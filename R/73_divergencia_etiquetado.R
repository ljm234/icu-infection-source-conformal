library(DBI)
library(duckdb)
library(jsonlite)

# Divergencia de etiquetado entre la fase segunda y la cuarta.
#
# R/19 declara replicar las definiciones selladas en la fase anterior. Las de
# cohorte si las replica: el antibiotico sistemico, la primera estancia por
# paciente, la ventana de seis horas y el criterio de sospecha son identicos.
# La etiqueta no.
#
# R/14 clasifica en seis sitios y desempata sangre, respiratorio,
# intraabdominal, urinario, herida. R/19 conserva tres sitios, envia el resto
# a la categoria de abstencion y desempata sangre, respiratorio, urinario. El
# colapso de los sitios no modelados es deliberado. El reordenamiento no lo
# declara ningun comentario: al desaparecer intraabdominal de la escala,
# urinario asciende por encima de el, de modo que una estancia positiva en
# ambos sitios recibe en la fase segunda una etiqueta que no se modela y en
# la cuarta una que si.
#
# Este procedimiento cuantifica el efecto. Reconstruye ambas etiquetas desde
# las tablas de origen, con las definiciones de cohorte comunes calculadas
# una sola vez, y deposita el cruce completo.
#
# La cohorte no se reconstruye. Rehacerla despues de abrir el conjunto
# sellado invalidaria la validacion externa, y el proposito aqui es medir la
# discrepancia, no corregirla.
#
# La categoria residual no se escribe: se deduce como la unica presente en el
# vocabulario de la fase cuarta y ausente del de la segunda. Una etiqueta se
# considera esperada si se conserva, cuando la fase cuarta la reconoce, o si
# pasa a esa residual, cuando no la reconoce. Cualquier otro par constituye
# divergencia.

OUT <- "outputs/fase25"

BASE  <- path.expand("~/mimic-data/physionet.org/files/mimiciv/3.1")
MICRO <- file.path(BASE, "hosp", "microbiologyevents.csv.gz")
RX    <- file.path(BASE, "hosp", "prescriptions.csv.gz")
PAT   <- file.path(BASE, "hosp", "patients.csv.gz")
ICU   <- file.path(BASE, "icu",  "icustays.csv.gz")

RUTA_CLASES <- "outputs/fase2/clases.csv"
RUTA_FLUJO  <- "outputs/fase2/flujo.csv"
RUTA_MATRIZ <- "outputs/fase4/matriz.csv"

detener <- function(...) {
  cat("\n", ..., "\n", sep = "")
  cat("El procedimiento se detiene. No procede interpretar el resultado.\n")
  quit(status = 1)
}

for (r in c(MICRO, RX, PAT, ICU, RUTA_CLASES, RUTA_FLUJO, RUTA_MATRIZ))
  if (!file.exists(r)) detener("Fuente ausente: ", r)

con <- dbConnect(duckdb::duckdb())
dbExecute(con, "PRAGMA threads=8")
dbExecute(con, "PRAGMA memory_limit='40GB'")
for (v in list(c("micro", MICRO), c("rx", RX), c("patients", PAT),
               c("icustays", ICU)))
  dbExecute(con, sprintf(
    "CREATE VIEW %s AS SELECT * FROM read_csv('%s', all_varchar=true)",
    v[1], v[2]))

# ---------------------------------------------------------------------------
# Definiciones de cohorte. Identicas en R/14 y en R/19, y por tanto comunes a
# las dos etiquetas. Se transcriben sin variacion para que la comparacion
# aisle la etiqueta y no arrastre ninguna otra diferencia.
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
  CREATE VIEW adultos AS
  SELECT e.subject_id, e.hadm_id, e.stay_id, e.first_careunit, e.intime,
         CAST(p.anchor_age AS INTEGER) AS edad
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

cat("Consultando las tablas de origen. Esto toma varios minutos.\n")
inicio <- Sys.time()

flujo_obs <- dbGetQuery(con, "
  SELECT (SELECT COUNT(DISTINCT stay_id) FROM cultivo_ventana) AS p3,
         (SELECT COUNT(*) FROM sospecha) AS p4")

flujo_pub <- read.csv(RUTA_FLUJO, stringsAsFactors = FALSE)
p3_pub <- flujo_pub$n[flujo_pub$paso == "p3_con_cultivo"]
p4_pub <- flujo_pub$n[flujo_pub$paso == "p4_sospecha_infeccion"]
if (length(p3_pub) != 1 || length(p4_pub) != 1)
  detener("El embudo publicado no declara los pasos tercero y cuarto.")
if (flujo_obs$p3 != p3_pub || flujo_obs$p4 != p4_pub)
  detener("La reconstruccion no reproduce el embudo publicado.")
cat("Embudo reproducido: ", flujo_obs$p3, " con cultivo, ",
    flujo_obs$p4, " con sospecha.\n", sep = "")

# ---------------------------------------------------------------------------
# Las dos clasificaciones, transcritas de R/14 y de R/19.
# ---------------------------------------------------------------------------

dbExecute(con, "
  CREATE VIEW categoria_2 AS
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
    END AS clase,
    CASE
      WHEN spec_type_desc IN ('BLOOD CULTURE',
                              'BLOOD CULTURE ( MYCO/F LYTIC BOTTLE)',
                              'FLUID RECEIVED IN BLOOD CULTURE BOTTLES')
        THEN 1
      WHEN spec_type_desc IN ('SPUTUM','BRONCHOALVEOLAR LAVAGE','Mini-BAL',
                              'BRONCHIAL WASHINGS','TRACHEAL ASPIRATE')
        THEN 2
      WHEN spec_type_desc IN ('BILE','PERITONEAL FLUID','DIALYSIS FLUID')
        THEN 3
      WHEN spec_type_desc IN ('URINE','URINE,KIDNEY','URINE,SUPRAPUBIC ASPIRATE')
        THEN 4
      WHEN spec_type_desc IN ('SWAB','TISSUE','ABSCESS','FOOT CULTURE',
                              'FOREIGN BODY','FLUID WOUND')
        THEN 5
      ELSE 9
    END AS prioridad
  FROM (SELECT DISTINCT spec_type_desc FROM micro)")

dbExecute(con, "
  CREATE VIEW categoria_4 AS
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
    END AS clase,
    CASE
      WHEN spec_type_desc IN ('BLOOD CULTURE',
                              'BLOOD CULTURE ( MYCO/F LYTIC BOTTLE)',
                              'FLUID RECEIVED IN BLOOD CULTURE BOTTLES')
        THEN 1
      WHEN spec_type_desc IN ('SPUTUM','BRONCHOALVEOLAR LAVAGE','Mini-BAL',
                              'BRONCHIAL WASHINGS','TRACHEAL ASPIRATE')
        THEN 2
      WHEN spec_type_desc IN ('URINE','URINE,KIDNEY','URINE,SUPRAPUBIC ASPIRATE')
        THEN 3
      ELSE 9
    END AS prioridad
  FROM (SELECT DISTINCT spec_type_desc FROM micro)")

etiquetar <- function(vista) sprintf("
  SELECT s.stay_id, COALESCE(m.clase, 'sin_crecimiento') AS clase
  FROM sospecha s
  LEFT JOIN (
    SELECT stay_id, clase FROM (
      SELECT c.stay_id, g.clase,
             ROW_NUMBER() OVER (PARTITION BY c.stay_id
                                ORDER BY g.prioridad) AS r
      FROM cultivo_ventana c
      JOIN sospecha t   ON t.stay_id = c.stay_id
      JOIN %s g         ON g.spec_type_desc = c.spec_type_desc
      WHERE c.org_name IS NOT NULL)
    WHERE r = 1) m ON m.stay_id = s.stay_id", vista)

e2 <- dbGetQuery(con, etiquetar("categoria_2"))
e4 <- dbGetQuery(con, etiquetar("categoria_4"))

# Sitios distintos con cultivo positivo por estancia, segun la clasificacion
# de la fase segunda, que es la que distingue mas sitios.
multi <- dbGetQuery(con, "
  SELECT sitios, COUNT(*) AS estancias FROM (
    SELECT c.stay_id, COUNT(DISTINCT g.clase) AS sitios
    FROM cultivo_ventana c
    JOIN sospecha t  ON t.stay_id = c.stay_id
    JOIN categoria_2 g ON g.spec_type_desc = c.spec_type_desc
    WHERE c.org_name IS NOT NULL
    GROUP BY c.stay_id)
  GROUP BY sitios ORDER BY sitios, estancias")

dbDisconnect(con, shutdown = TRUE)
cat("Consulta terminada en",
    round(difftime(Sys.time(), inicio, units = "mins"), 1), "min\n")

# ---------------------------------------------------------------------------
# Comprobaciones antes de cruzar
# ---------------------------------------------------------------------------

if (nrow(e2) != nrow(e4) || !setequal(e2$stay_id, e4$stay_id))
  detener("Las dos etiquetas no cubren las mismas estancias.")

pub <- read.csv(RUTA_CLASES, stringsAsFactors = FALSE)
obs <- as.data.frame(table(e2$clase), stringsAsFactors = FALSE)
names(obs) <- c("clase", "n")
comparado <- merge(pub[, c("clase", "n")], obs, by = "clase",
                   suffixes = c("_publicado", "_reconstruido"), all = TRUE)
cat("\n=== ETIQUETA DE LA FASE SEGUNDA, RECONSTRUIDA ===\n")
print(comparado, row.names = FALSE)
if (any(is.na(comparado$n_publicado)) ||
    any(is.na(comparado$n_reconstruido)) ||
    any(comparado$n_publicado != comparado$n_reconstruido))
  detener("La reconstruccion no reproduce la distribucion publicada de la ",
          "fase segunda.")
cat("Reproduce la distribucion publicada.\n")

mat <- read.csv(RUTA_MATRIZ, stringsAsFactors = FALSE)
if (is.null(mat$stay_id) || is.null(mat$clase))
  detener("La matriz de la fase cuarta carece de estancia o de categoria.")
if (!setequal(mat$stay_id, e4$stay_id))
  detener("La matriz de la fase cuarta no cubre las mismas estancias.")
j <- merge(mat[, c("stay_id", "clase")], e4, by = "stay_id",
           suffixes = c("_matriz", "_reconstruida"))
discrepan <- sum(j$clase_matriz != j$clase_reconstruida)
cat("\nEstancias donde la etiqueta reconstruida difiere de la matriz:",
    discrepan, "\n")
if (discrepan > 0)
  detener("La reconstruccion de la fase cuarta no reproduce la matriz.")

# ---------------------------------------------------------------------------
# Cruce
# ---------------------------------------------------------------------------

cr <- merge(e2, e4, by = "stay_id", suffixes = c("_fase2", "_fase4"))
vocab2 <- sort(unique(cr$clase_fase2))
vocab4 <- sort(unique(cr$clase_fase4))
residual <- setdiff(vocab4, vocab2)
if (length(residual) != 1)
  detener("El vocabulario de la fase cuarta no define una unica categoria ",
          "residual.")
cat("\nCategoria residual deducida:", residual, "\n")

tb <- as.data.frame(table(cr$clase_fase2, cr$clase_fase4),
                    stringsAsFactors = FALSE)
names(tb) <- c("etiqueta_fase2", "etiqueta_fase4", "estancias")
tb <- tb[tb$estancias > 0, ]
tb$esperada <- ifelse(tb$etiqueta_fase2 %in% vocab4,
                      tb$etiqueta_fase4 == tb$etiqueta_fase2,
                      tb$etiqueta_fase4 == residual)
tb <- tb[order(-tb$esperada, -tb$estancias), ]
rownames(tb) <- NULL

cat("\n=== CRUCE DE ETIQUETAS ===\n")
print(tb, row.names = FALSE)

div <- tb[!tb$esperada, ]
cat("\nEstancias que cambian de etiqueta de forma no prevista:",
    sum(div$estancias), "de", nrow(cr), "\n")
if (nrow(div) > 0) {
  cat("Pares afectados:\n")
  for (i in seq_len(nrow(div)))
    cat(sprintf("  %-16s -> %-16s %6d\n", div$etiqueta_fase2[i],
                div$etiqueta_fase4[i], div$estancias[i]))
  cat("De ellas, las que pasan a una categoria modelada:",
      sum(div$estancias[div$etiqueta_fase4 != residual]), "\n")
}

names(multi) <- c("sitios_positivos", "estancias")
cat("\n=== SITIOS POSITIVOS DISTINTOS POR ESTANCIA ===\n")
print(multi, row.names = FALSE)
cat("Estancias con mas de un sitio positivo:",
    sum(multi$estancias[multi$sitios_positivos > 1]), "\n")

dir.create(OUT, recursive = TRUE, showWarnings = FALSE)
write.csv(tb, file.path(OUT, "divergencia_etiquetado.csv"), row.names = FALSE)
write.csv(multi, file.path(OUT, "multisitio_cohorte.csv"), row.names = FALSE)

writeLines(toJSON(list(
  fase = "25",
  ejecutado_en = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
  r_version = R.version.string,
  guarda_de_emparejamiento_parcial = isTRUE(getOption("warnPartialMatchDollar")),
  duckdb_version = as.character(packageVersion("duckdb")),
  proposito = paste("cuantificar la divergencia de etiquetado entre la fase",
                    "segunda y la cuarta"),
  definiciones_de_cohorte = "comunes a ambas fases, calculadas una sola vez",
  categoria_residual = residual,
  criterio_esperada = paste("se conserva la etiqueta cuando la fase cuarta la",
                            "reconoce; pasa a la residual cuando no"),
  definicion_multisitio = paste("sitios distintos con cultivo positivo segun",
                                "la clasificacion de la fase segunda"),
  estancias = nrow(cr),
  la_cohorte_no_se_reconstruye = TRUE), auto_unbox = TRUE, pretty = TRUE),
  file.path(OUT, "manifiesto.json"))

cat("\nDepositado en", OUT, "\n")
