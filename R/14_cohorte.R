library(DBI)
library(duckdb)
library(digest)
library(jsonlite)

SEMILLA <- 20260817
set.seed(SEMILLA)

BASE  <- path.expand("~/mimic-data/physionet.org/files/mimiciv/3.1")
MICRO <- file.path(BASE, "hosp", "microbiologyevents.csv.gz")
RX    <- file.path(BASE, "hosp", "prescriptions.csv.gz")
PAT   <- file.path(BASE, "hosp", "patients.csv.gz")
ICU   <- file.path(BASE, "icu",  "icustays.csv.gz")

OUT <- "outputs/fase2"
dir.create(OUT, recursive = TRUE, showWarnings = FALSE)

con <- dbConnect(duckdb::duckdb())
dbExecute(con, "PRAGMA threads=8")
dbExecute(con, "PRAGMA memory_limit='32GB'")

for (v in list(c("micro", MICRO), c("rx", RX),
               c("patients", PAT), c("icustays", ICU))) {
  dbExecute(con, sprintf(
    "CREATE VIEW %s AS SELECT * FROM read_csv('%s', all_varchar=true)",
    v[1], v[2]))
}

# ---------------------------------------------------------------------------
# Antibioticos sistemicos.
#
# Se identifican por raiz farmacologica en minusculas, no por nombre exacto,
# porque el mismo farmaco aparece con distinta capitalizacion (CefazoLIN y
# CeFAZolin son el mismo medicamento en dos filas separadas).
#
# Exclusiones aprobadas:
#   - Preparaciones oftalmicas y topicas: accion local, no indican sospecha
#     de infeccion sistemica.
#   - Vancomicina oral y enema: no se absorbe por via digestiva, se emplea
#     para Clostridioides difficile con efecto luminal.
#   - Neomicina oral y combinaciones topicas: descontaminacion intestinal o
#     uso topico, no tratamiento sistemico.
#   - Lock antibiotico de vancomicina: instilacion intraluminal en cateter.
#   - Eritromicina sin calificador: ambigua entre antibiotico y procinetico
#     gastrico. Se excluye por criterio conservador.
# ---------------------------------------------------------------------------

dbExecute(con, "
  CREATE VIEW atb AS
  SELECT subject_id, hadm_id, CAST(starttime AS TIMESTAMP) AS inicio, drug
  FROM rx
  WHERE drug_type = 'MAIN'
    AND route IN ('IV','IV DRIP','PO','PO/NG','IM')
    AND lower(drug) SIMILAR TO '.*(cillin|cephal|cefaz|cefep|ceftr|ceftaz|cefur|cefpo|mycin|micin|floxacin|penem|cycline|sulfamethox|metronidazol|linezolid|daptomycin|azithro|clarithro|clindamycin|nitrofurantoin|rifamp|aztreonam|colistin|polymyxin|tigecycline|fosfomycin).*'
    AND lower(drug) NOT SIMILAR TO '.*(ophth|oint|oral liquid|enema|lock|susp).*'
    AND lower(drug) NOT IN ('neomycin sulfate','neomycin-polymyxin-bacitracin','erythromycin')")

# ---------------------------------------------------------------------------
# Categorias del desenlace.
#
# Solo cultivos de sitio anatomico. Se excluyen tamizajes (MRSA SCREEN),
# serologias (no cultivan, org_name siempre nulo) y coprocultivo (responde
# a diarrea nosocomial, no localiza foco de sepsis).
#
# El liquido cefalorraquideo se etiqueta como otro_sitio y no como clase
# modelable: 222 cultivos con 12 positivos quedan muy por debajo del minimo
# requerido para calibracion conforme condicional por clase.
# ---------------------------------------------------------------------------

dbExecute(con, "
  CREATE VIEW categoria AS
  SELECT spec_type_desc,
    CASE
      WHEN spec_type_desc IN ('BLOOD CULTURE',
                              'BLOOD CULTURE ( MYCO/F LYTIC BOTTLE)',
                              'FLUID RECEIVED IN BLOOD CULTURE BOTTLES')
        THEN 'sangre'
      WHEN spec_type_desc IN ('SPUTUM','BRONCHOALVEOLAR LAVAGE','Mini-BAL',
                              'BRONCHIAL WASHINGS','TRACHEAL ASPIRATE')
        THEN 'respiratorio'
      WHEN spec_type_desc IN ('BILE','PERITONEAL FLUID','DIALYSIS FLUID')
        THEN 'intraabdominal'
      WHEN spec_type_desc IN ('URINE','URINE,KIDNEY','URINE,SUPRAPUBIC ASPIRATE')
        THEN 'urinario'
      WHEN spec_type_desc IN ('SWAB','TISSUE','ABSCESS','FOOT CULTURE',
                              'FOREIGN BODY','FLUID WOUND')
        THEN 'herida'
      ELSE 'otro_sitio'
    END AS clase
  FROM (SELECT DISTINCT spec_type_desc FROM micro)")

# ---------------------------------------------------------------------------
# Paso 1. Una estancia por paciente.
#
# La particion es por subject_id y no por hadm_id. Un paciente con dos
# hospitalizaciones aportaria dos registros correlacionados, lo que rompe
# la intercambiabilidad que sostiene la garantia de cobertura conforme.
# ---------------------------------------------------------------------------

dbExecute(con, "
  CREATE VIEW estancia AS
  SELECT subject_id, hadm_id, stay_id, first_careunit, intime, outtime
  FROM (
    SELECT subject_id, hadm_id, stay_id, first_careunit,
           CAST(intime AS TIMESTAMP)  AS intime,
           CAST(outtime AS TIMESTAMP) AS outtime,
           ROW_NUMBER() OVER (PARTITION BY subject_id
                              ORDER BY CAST(intime AS TIMESTAMP)) AS orden
    FROM icustays)
  WHERE orden = 1")

flujo <- list()
uno <- function(sql) dbGetQuery(con, sql)[[1]]

flujo$p1_estancias_unicas <- uno("SELECT COUNT(*) FROM estancia")

# ---------------------------------------------------------------------------
# Paso 2. Adultos.
# ---------------------------------------------------------------------------

dbExecute(con, "
  CREATE VIEW adultos AS
  SELECT e.*, CAST(p.anchor_age AS INTEGER) AS edad, p.gender
  FROM estancia e
  JOIN patients p ON e.subject_id = p.subject_id
  WHERE CAST(p.anchor_age AS INTEGER) >= 18")

flujo$p2_adultos <- uno("SELECT COUNT(*) FROM adultos")

# ---------------------------------------------------------------------------
# Paso 3. Cultivo dentro de la ventana de seis horas desde el ingreso a UCI.
# ---------------------------------------------------------------------------

dbExecute(con, "
  CREATE VIEW cultivo_ventana AS
  SELECT DISTINCT
    a.stay_id, a.subject_id, a.hadm_id, a.first_careunit, a.intime, a.edad,
    m.micro_specimen_id,
    CAST(m.charttime AS TIMESTAMP) AS hora_cultivo,
    m.spec_type_desc,
    m.org_name
  FROM adultos a
  JOIN micro m ON m.hadm_id = a.hadm_id
  WHERE CAST(m.charttime AS TIMESTAMP) >= a.intime
    AND CAST(m.charttime AS TIMESTAMP) <= a.intime + INTERVAL 6 HOUR")

flujo$p3_con_cultivo <- uno("SELECT COUNT(DISTINCT stay_id) FROM cultivo_ventana")

# ---------------------------------------------------------------------------
# Paso 4. Sospecha de infeccion.
#
# Criterio operacional: la muestra debe acompanarse de antibiotico sistemico
# en una ventana temporal. Si el cultivo precede al antibiotico, este debe
# iniciarse dentro de 72 horas. Si el antibiotico precede al cultivo, la
# muestra debe obtenerse dentro de 24 horas.
#
# La asimetria refleja el proceso clinico: tras obtener la muestra puede
# mediar deliberacion antes de tratar, mientras que si ya se decidio tratar
# la muestra debia tomarse de inmediato. Un intervalo mayor sugiere que
# ambos eventos pertenecen a episodios distintos.
# ---------------------------------------------------------------------------

dbExecute(con, "
  CREATE VIEW sospecha AS
  SELECT DISTINCT c.stay_id
  FROM cultivo_ventana c
  JOIN atb b ON b.hadm_id = c.hadm_id
  WHERE (b.inicio >= c.hora_cultivo AND b.inicio <= c.hora_cultivo + INTERVAL 72 HOUR)
     OR (b.inicio <  c.hora_cultivo AND b.inicio >= c.hora_cultivo - INTERVAL 24 HOUR)")

flujo$p4_sospecha_infeccion <- uno("SELECT COUNT(*) FROM sospecha")

# ---------------------------------------------------------------------------
# Paso 5. Etiqueta.
#
# Jerarquia de desempate cuando hay mas de un sitio positivo, fijada antes
# de observar resultado alguno de modelamiento:
#   sangre > respiratorio > intraabdominal > urinario > herida
#
# La bacteriemia encabeza porque determina duracion de tratamiento, obliga a
# descartar endocarditis y modifica el pronostico con independencia de que
# otro sitio resulte positivo. El orden restante desciende segun la carga de
# mortalidad y la necesidad de control de foco.
# ---------------------------------------------------------------------------

dbExecute(con, "
  CREATE VIEW etiqueta AS
  WITH positivos AS (
    SELECT c.stay_id, g.clase,
      CASE g.clase
        WHEN 'sangre'         THEN 1
        WHEN 'respiratorio'   THEN 2
        WHEN 'intraabdominal' THEN 3
        WHEN 'urinario'       THEN 4
        WHEN 'herida'         THEN 5
        ELSE 9
      END AS prioridad
    FROM cultivo_ventana c
    JOIN sospecha s   ON s.stay_id = c.stay_id
    JOIN categoria g  ON g.spec_type_desc = c.spec_type_desc
    WHERE c.org_name IS NOT NULL
  ),
  mejor AS (
    SELECT stay_id, clase,
           ROW_NUMBER() OVER (PARTITION BY stay_id ORDER BY prioridad) AS r
    FROM positivos
  )
  SELECT s.stay_id, COALESCE(m.clase, 'sin_crecimiento') AS clase
  FROM sospecha s
  LEFT JOIN mejor m ON m.stay_id = s.stay_id AND m.r = 1")

# ---------------------------------------------------------------------------
# Cohorte final
# ---------------------------------------------------------------------------

dbExecute(con, "
  CREATE VIEW cohorte AS
  SELECT DISTINCT
    a.stay_id, a.subject_id, a.hadm_id,
    a.first_careunit AS unidad, a.intime AS t0, a.edad, a.gender,
    e.clase
  FROM adultos a
  JOIN etiqueta e ON e.stay_id = a.stay_id")

flujo$p5_cohorte_final <- uno("SELECT COUNT(*) FROM cohorte")

cat("\n=== DIAGRAMA DE FLUJO ===\n")
flujo_df <- data.frame(paso = names(flujo), n = unlist(flujo))
print(flujo_df, row.names = FALSE)

cat("\n=== DISTRIBUCION DE CLASES ===\n")
clases <- dbGetQuery(con, "
  SELECT clase, COUNT(*) AS n,
         ROUND(100.0 * COUNT(*) / SUM(COUNT(*)) OVER (), 2) AS pct
  FROM cohorte GROUP BY 1 ORDER BY n DESC, clase")
print(clases, row.names = FALSE)

cat("\n=== CLASE POR UNIDAD ===\n")
cruce <- dbGetQuery(con, "
  SELECT unidad, clase, COUNT(*) AS n
  FROM cohorte GROUP BY 1,2 ORDER BY unidad, n DESC, clase")
print(head(cruce, 30), row.names = FALSE)

cat("\n=== TAMANO MINIMO DE CLASE POR UNIDAD ===\n")
minimos <- dbGetQuery(con, "
  SELECT unidad, COUNT(*) AS estancias, COUNT(DISTINCT clase) AS clases
  FROM cohorte GROUP BY 1 HAVING COUNT(*) >= 500 ORDER BY estancias DESC, unidad")
print(minimos, row.names = FALSE)

write.csv(flujo_df, file.path(OUT, "flujo.csv"),   row.names = FALSE)
write.csv(clases,   file.path(OUT, "clases.csv"),  row.names = FALSE)
write.csv(cruce,    file.path(OUT, "clase_por_unidad.csv"), row.names = FALSE)
write.csv(minimos,  file.path(OUT, "unidades_elegibles.csv"), row.names = FALSE)

manifiesto <- list(
  fase           = "2",
  ejecutado_en   = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
  semilla        = SEMILLA,
  r_version      = R.version.string,
  duckdb_version = as.character(packageVersion("duckdb")),
  decisiones = list(
    unidad_analisis = "primera estancia en UCI por paciente",
    t0              = "icustays.intime",
    ventana         = "6 horas",
    sospecha        = "cultivo con antibiotico sistemico, 72h despues o 24h antes",
    desempate       = "sangre, respiratorio, intraabdominal, urinario, herida",
    lcr             = "otro_sitio, reservado para evaluacion de abstencion"),
  sha256 = list(
    microbiologyevents = digest(file = MICRO, algo = "sha256"),
    prescriptions      = digest(file = RX,    algo = "sha256"),
    patients           = digest(file = PAT,   algo = "sha256"),
    icustays           = digest(file = ICU,   algo = "sha256")))

write_json(manifiesto, file.path(OUT, "manifiesto.json"),
           auto_unbox = TRUE, pretty = TRUE)

dbDisconnect(con, shutdown = TRUE)
cat("\nSalidas en", OUT, "\n")
