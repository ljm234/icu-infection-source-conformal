library(DBI)
library(duckdb)
library(digest)
library(jsonlite)

SEMILLA <- 20260818
set.seed(SEMILLA)

BASE  <- path.expand("~/mimic-data/physionet.org/files/mimiciv/3.1")
MICRO <- file.path(BASE, "hosp", "microbiologyevents.csv.gz")
RX    <- file.path(BASE, "hosp", "prescriptions.csv.gz")
PAT   <- file.path(BASE, "hosp", "patients.csv.gz")
LAB   <- file.path(BASE, "hosp", "labevents.csv.gz")
ICU   <- file.path(BASE, "icu",  "icustays.csv.gz")

OUT <- "outputs/fase4"
dir.create(OUT, recursive = TRUE, showWarnings = FALSE)

con <- dbConnect(duckdb::duckdb())
dbExecute(con, "PRAGMA threads=8")
dbExecute(con, "PRAGMA memory_limit='40GB'")

for (v in list(c("micro", MICRO), c("rx", RX), c("patients", PAT),
               c("lab", LAB), c("icustays", ICU))) {
  dbExecute(con, sprintf(
    "CREATE VIEW %s AS SELECT * FROM read_csv('%s', all_varchar=true)",
    v[1], v[2]))
}

# ---------------------------------------------------------------------------
# Cohorte. Replica las definiciones selladas en la fase anterior.
# ---------------------------------------------------------------------------

dbExecute(con, "
  CREATE VIEW atb AS
  SELECT hadm_id, CAST(starttime AS TIMESTAMP) AS inicio
  FROM rx
  WHERE drug_type = 'MAIN'
    AND route IN ('IV','IV DRIP','PO','PO/NG','IM')
    AND lower(drug) SIMILAR TO '.*(cillin|cephal|cefaz|cefep|ceftr|ceftaz|cefur|cefpo|mycin|micin|floxacin|penem|cycline|sulfamethox|metronidazol|linezolid|daptomycin|azithro|clarithro|clindamycin|nitrofurantoin|rifamp|aztreonam|colistin|polymyxin|tigecycline|fosfomycin).*'
    AND lower(drug) NOT SIMILAR TO '.*(ophth|oint|oral liquid|enema|lock|susp).*'
    AND lower(drug) NOT IN ('neomycin sulfate','neomycin-polymyxin-bacitracin','erythromycin')")

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
      WHEN spec_type_desc IN ('URINE','URINE,KIDNEY','URINE,SUPRAPUBIC ASPIRATE')
        THEN 'urinario'
      ELSE 'abstencion'
    END AS clase
  FROM (SELECT DISTINCT spec_type_desc FROM micro)")

dbExecute(con, "
  CREATE VIEW adultos AS
  SELECT e.subject_id, e.hadm_id, e.stay_id, e.first_careunit, e.intime,
         CAST(p.anchor_age AS INTEGER) AS edad, p.gender
  FROM (
    SELECT subject_id, hadm_id, stay_id, first_careunit,
           CAST(intime AS TIMESTAMP) AS intime,
           ROW_NUMBER() OVER (PARTITION BY subject_id
                              ORDER BY CAST(intime AS TIMESTAMP)) AS orden
    FROM icustays) e
  JOIN patients p ON e.subject_id = p.subject_id
  WHERE e.orden = 1 AND CAST(p.anchor_age AS INTEGER) >= 18")

dbExecute(con, "
  CREATE VIEW cultivo_ventana AS
  SELECT DISTINCT a.stay_id, a.hadm_id, m.micro_specimen_id,
    CAST(m.charttime AS TIMESTAMP) AS hora_cultivo,
    m.spec_type_desc, m.org_name
  FROM adultos a
  JOIN micro m ON m.hadm_id = a.hadm_id
  WHERE CAST(m.charttime AS TIMESTAMP) >= a.intime
    AND CAST(m.charttime AS TIMESTAMP) <= a.intime + INTERVAL 6 HOUR")

dbExecute(con, "
  CREATE VIEW sospecha AS
  SELECT DISTINCT c.stay_id
  FROM cultivo_ventana c
  JOIN atb b ON b.hadm_id = c.hadm_id
  WHERE (b.inicio >= c.hora_cultivo AND b.inicio <= c.hora_cultivo + INTERVAL 72 HOUR)
     OR (b.inicio <  c.hora_cultivo AND b.inicio >= c.hora_cultivo - INTERVAL 24 HOUR)")

dbExecute(con, "
  CREATE VIEW etiqueta AS
  WITH positivos AS (
    SELECT c.stay_id, g.clase,
      CASE g.clase
        WHEN 'sangre'       THEN 1
        WHEN 'respiratorio' THEN 2
        WHEN 'urinario'     THEN 3
        ELSE 9
      END AS prioridad
    FROM cultivo_ventana c
    JOIN sospecha s  ON s.stay_id = c.stay_id
    JOIN categoria g ON g.spec_type_desc = c.spec_type_desc
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
# Predictores de laboratorio.
#
# Se toma la primera medicion registrada dentro de la ventana. La regla se
# fijo tras comprobar que entre 76 y 93 por ciento de las estancias tienen
# una sola medicion por variable, salvo en gasometria. El primer valor es el
# unico cuya distribucion no depende del numero de determinaciones, lo que
# importa porque la intensidad de monitoreo varia entre unidades y el diseno
# contempla validacion dejando una unidad fuera.
#
# El filtro temporal usa storetime y no charttime. Un resultado obtenido a
# la quinta hora pero registrado a la novena no estaba disponible en el
# momento de decision, y emplearlo constituiria fuga de informacion.
# ---------------------------------------------------------------------------

items <- c(
  leucocitos    = "51301", hemoglobina = "51222", plaquetas   = "51265",
  rdw           = "51277", creatinina  = "50912", urea        = "51006",
  brecha_anion  = "50868", sodio       = "50983", potasio     = "50971",
  cloro         = "50902", bicarbonato = "50882", inr         = "51237",
  ttpa          = "51275", ph          = "50820", pco2        = "50818",
  lactato       = "50813", exceso_base = "50802")

lista_ids <- paste0("'", paste(items, collapse = "','"), "'")

dbExecute(con, sprintf("
  CREATE VIEW primer_valor AS
  SELECT stay_id, itemid, valuenum FROM (
    SELECT a.stay_id, l.itemid, CAST(l.valuenum AS DOUBLE) AS valuenum,
           ROW_NUMBER() OVER (PARTITION BY a.stay_id, l.itemid
                              ORDER BY CAST(l.storetime AS TIMESTAMP)) AS r
    FROM adultos a
    JOIN lab l ON l.hadm_id = a.hadm_id
    WHERE CAST(l.storetime AS TIMESTAMP) >= a.intime
      AND CAST(l.storetime AS TIMESTAMP) <= a.intime + INTERVAL 6 HOUR
      AND l.valuenum IS NOT NULL
      AND l.itemid IN (%s)) WHERE r = 1", lista_ids))

# El pivoteo convierte el formato largo, con una fila por medicion, en el
# formato ancho que requiere la matriz, con una fila por estancia y una
# columna por variable. MAX opera sobre un unico valor no nulo por celda.
columnas <- paste(sprintf(
  "MAX(CASE WHEN itemid = '%s' THEN valuenum END) AS %s",
  items, names(items)), collapse = ",\n    ")

dbExecute(con, sprintf("
  CREATE VIEW ancho AS
  SELECT stay_id,
    %s
  FROM primer_valor
  GROUP BY stay_id", columnas))

dbExecute(con, "
  CREATE VIEW matriz AS
  SELECT
    a.stay_id, a.subject_id, a.hadm_id,
    a.first_careunit AS unidad,
    a.edad, a.gender AS sexo,
    w.leucocitos, w.hemoglobina, w.plaquetas, w.rdw,
    w.creatinina, w.urea, w.brecha_anion,
    w.sodio, w.potasio, w.cloro, w.bicarbonato,
    w.inr, w.ttpa,
    w.ph, w.pco2, w.lactato, w.exceso_base,
    CASE WHEN w.lactato IS NULL THEN 0 ELSE 1 END AS lactato_medido,
    e.clase
  FROM adultos a
  JOIN etiqueta e ON e.stay_id = a.stay_id
  LEFT JOIN ancho w ON w.stay_id = a.stay_id")

# ---------------------------------------------------------------------------
# Materializacion y control de calidad
# ---------------------------------------------------------------------------

matriz <- dbGetQuery(con, "SELECT * FROM matriz")

cat("\n=== DIMENSIONES ===\n")
cat("Filas:", nrow(matriz), " Columnas:", ncol(matriz), "\n")

cat("\n=== DISTRIBUCION DE CLASES ===\n")
print(table(matriz$clase))

vars <- names(items)
faltantes <- data.frame(
  variable = vars,
  faltan   = sapply(vars, function(v) sum(is.na(matriz[[v]]))),
  pct      = round(100 * sapply(vars, function(v)
                  mean(is.na(matriz[[v]]))), 1),
  row.names = NULL)
faltantes <- faltantes[order(faltantes$pct), ]

cat("\n=== FALTANTES POR VARIABLE ===\n")
print(faltantes, row.names = FALSE)

cat("\n=== FALTANTES POR UNIDAD, LACTATO ===\n")
por_unidad <- aggregate(
  is.na(matriz$lactato) ~ matriz$unidad, FUN = mean)
names(por_unidad) <- c("unidad", "pct_falta_lactato")
por_unidad$pct_falta_lactato <- round(100 * por_unidad$pct_falta_lactato, 1)
print(por_unidad[order(-por_unidad$pct_falta_lactato), ], row.names = FALSE)

cat("\n=== CASOS COMPLETOS ===\n")
completos <- sum(complete.cases(matriz[, vars]))
cat("Sin ningun faltante:", completos,
    sprintf("(%.1f%%)", 100 * completos / nrow(matriz)), "\n")

cat("\n=== RANGOS, CONTROL DE PLAUSIBILIDAD ===\n")
rangos <- data.frame(
  variable = vars,
  minimo   = sapply(vars, function(v) min(matriz[[v]], na.rm = TRUE)),
  mediana  = sapply(vars, function(v) median(matriz[[v]], na.rm = TRUE)),
  maximo   = sapply(vars, function(v) max(matriz[[v]], na.rm = TRUE)),
  row.names = NULL)
print(rangos, row.names = FALSE)

write.csv(matriz,     file.path(OUT, "matriz.csv"),     row.names = FALSE)
write.csv(faltantes,  file.path(OUT, "faltantes.csv"),  row.names = FALSE)
write.csv(por_unidad, file.path(OUT, "faltantes_unidad.csv"), row.names = FALSE)
write.csv(rangos,     file.path(OUT, "rangos.csv"),     row.names = FALSE)

manifiesto <- list(
  fase           = "4",
  ejecutado_en   = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
  semilla        = SEMILLA,
  r_version      = R.version.string,
  duckdb_version = as.character(packageVersion("duckdb")),
  n_filas        = nrow(matriz),
  n_completos    = completos,
  agregacion     = "primera medicion registrada dentro de la ventana",
  filtro_tiempo  = "storetime, para excluir resultados no disponibles a las seis horas",
  itemids        = as.list(items),
  sha256 = list(
    microbiologyevents = digest(file = MICRO, algo = "sha256"),
    prescriptions      = digest(file = RX,    algo = "sha256"),
    patients           = digest(file = PAT,   algo = "sha256"),
    labevents          = digest(file = LAB,   algo = "sha256"),
    icustays           = digest(file = ICU,   algo = "sha256")))

write_json(manifiesto, file.path(OUT, "manifiesto.json"),
           auto_unbox = TRUE, pretty = TRUE)

dbDisconnect(con, shutdown = TRUE)
cat("\nMatriz escrita en", OUT, "\n")
