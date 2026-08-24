library(DBI)
library(duckdb)

BASE  <- path.expand("~/mimic-data/physionet.org/files/mimiciv/3.1")
CHART <- file.path(BASE, "icu", "chartevents.csv.gz")
ICU   <- file.path(BASE, "icu", "icustays.csv.gz")

con <- dbConnect(duckdb::duckdb())
dbExecute(con, "PRAGMA threads=8")
dbExecute(con, "PRAGMA memory_limit='48GB'")
for (x in list(c("chart", CHART), c("icustays", ICU)))
  dbExecute(con, sprintf(
    "CREATE VIEW %s AS SELECT * FROM read_csv('%s', all_varchar=true)",
    x[1], x[2]))

dbExecute(con, "
  CREATE VIEW estancia AS
  SELECT subject_id, stay_id, intime FROM (
    SELECT subject_id, stay_id,
           CAST(intime AS TIMESTAMP) AS intime,
           ROW_NUMBER() OVER (PARTITION BY subject_id
                              ORDER BY CAST(intime AS TIMESTAMP)) AS orden
    FROM icustays) WHERE orden = 1")

# La columna numerica conserva la puntuacion de cada componente, de modo que
# no procede establecer correspondencia alguna con el texto. Se retiene sin
# embargo la expresion literal de la respuesta verbal, dado que la
# denominacion reservada al paciente con tubo endotraqueal recibe la misma
# puntuacion que la ausencia genuina de respuesta y solo el texto permite
# distinguir ambas situaciones.
cat("Extrayendo los tres componentes dentro de la ventana.\n")

g <- dbGetQuery(con, "
  SELECT stay_id,
    MAX(CASE WHEN codigo = '220739' THEN punto    END) AS ocular,
    MAX(CASE WHEN codigo = '223901' THEN punto    END) AS motora,
    MAX(CASE WHEN codigo = '223900' THEN punto    END) AS verbal,
    MAX(CASE WHEN codigo = '223900' THEN texto    END) AS verbal_texto,
    MAX(CASE WHEN codigo = '220739' THEN registro END) AS t_ocular,
    MAX(CASE WHEN codigo = '223901' THEN registro END) AS t_motora,
    MAX(CASE WHEN codigo = '223900' THEN registro END) AS t_verbal
  FROM (
    SELECT e.stay_id, c.itemid AS codigo, c.value AS texto,
           CAST(c.valuenum AS DOUBLE) AS punto,
           CAST(c.storetime AS TIMESTAMP) AS registro,
           ROW_NUMBER() OVER (PARTITION BY e.stay_id, c.itemid
                              ORDER BY CAST(c.storetime AS TIMESTAMP)) AS orden
    FROM estancia e
    JOIN chart c ON c.stay_id = e.stay_id
    WHERE c.itemid IN ('220739','223901','223900')
      AND c.valuenum IS NOT NULL
      AND CAST(c.storetime AS TIMESTAMP) >= e.intime
      AND CAST(c.storetime AS TIMESTAMP) <= e.intime + INTERVAL 6 HOUR)
  WHERE orden = 1
  GROUP BY stay_id")

dbDisconnect(con, shutdown = TRUE)

g$intubado <- as.integer(!is.na(g$verbal_texto) &
                         g$verbal_texto == "No Response-ETT")

g$gcs_total <- g$ocular + g$motora + g$verbal
g$gcs_em    <- g$ocular + g$motora

t1 <- as.numeric(g$t_ocular)
t2 <- as.numeric(g$t_motora)
t3 <- as.numeric(g$t_verbal)
g$dispersion_min <- (pmax(t1, t2, t3) - pmin(t1, t2, t3)) / 60

part <- read.csv("outputs/fase5/matriz_particionada.csv",
                 stringsAsFactors = FALSE)
m <- merge(part[, c("stay_id","clase","grupo","unidad")],
           g[, c("stay_id","ocular","motora","verbal","intubado",
                 "gcs_total","gcs_em","dispersion_min")],
           by = "stay_id", all.x = TRUE)

cat("\n=== COBERTURA EN LA COHORTE ===\n")
print(data.frame(
  variable = c("apertura ocular","respuesta motora","respuesta verbal",
               "glasgow total","ocular mas motora"),
  pct = round(100 * c(mean(!is.na(m$ocular)), mean(!is.na(m$motora)),
                      mean(!is.na(m$verbal)), mean(!is.na(m$gcs_total)),
                      mean(!is.na(m$gcs_em))), 1),
  row.names = NULL), row.names = FALSE)

# Los tres componentes deben proceder de una misma valoracion. Una dispersion
# temporal apreciable indicaria que la suma combina exploraciones realizadas
# en momentos distintos, en cuyo caso no constituiria una puntuacion de la
# escala sino un agregado de evaluaciones independientes.
cat("\n=== DISPERSION TEMPORAL ENTRE LOS TRES COMPONENTES ===\n")
q <- quantile(m$dispersion_min, c(0.5, 0.75, 0.9, 0.99), na.rm = TRUE)
print(data.frame(percentil = c("50","75","90","99"),
                 minutos = round(as.numeric(q), 1), row.names = NULL),
      row.names = FALSE)
cat("Valoraciones simultaneas, dispersion nula:",
    sprintf("%.1f por ciento\n",
            100 * mean(m$dispersion_min == 0, na.rm = TRUE)))

cat("\n=== DISTRIBUCION DE AMBAS VERSIONES ===\n")
print(data.frame(
  version = c("glasgow total","ocular mas motora"),
  minimo = c(min(m$gcs_total, na.rm = TRUE), min(m$gcs_em, na.rm = TRUE)),
  p25 = c(quantile(m$gcs_total, .25, na.rm = TRUE),
          quantile(m$gcs_em, .25, na.rm = TRUE)),
  mediana = c(median(m$gcs_total, na.rm = TRUE),
              median(m$gcs_em, na.rm = TRUE)),
  maximo = c(max(m$gcs_total, na.rm = TRUE), max(m$gcs_em, na.rm = TRUE)),
  row.names = NULL), row.names = FALSE)
cat("Correlacion entre ambas versiones:",
    round(cor(m$gcs_total, m$gcs_em, use = "complete.obs"), 4), "\n")

cat("\n=== INTUBACION POR UNIDAD ===\n")
u <- do.call(rbind, lapply(sort(unique(m$unidad)), function(un) {
  s <- m[m$unidad == un, ]
  if (nrow(s) < 500) return(NULL)
  data.frame(unidad = substr(un, 1, 34), n = nrow(s),
             pct_intubado = round(100 * mean(s$intubado, na.rm = TRUE), 1),
             gcs_total_mediano = median(s$gcs_total, na.rm = TRUE),
             gcs_em_mediano = median(s$gcs_em, na.rm = TRUE),
             row.names = NULL)
}))
print(u, row.names = FALSE)

# Comprobacion determinante. Si la proporcion de pacientes con tubo
# endotraqueal difiere entre las categorias del desenlace, la puntuacion que
# incorpora la respuesta verbal actuaria como indicador indirecto de la
# ventilacion mecanica, y esta a su vez de la obtencion de cultivo
# respiratorio. La asociacion resultante no reflejaria el estado neurologico
# sino el procedimiento aplicado.
cat("\n=== INTUBACION POR CATEGORIA DEL DESENLACE ===\n")
CL <- c("sin_crecimiento","urinario","respiratorio","sangre")
print(do.call(rbind, lapply(CL, function(k) {
  s <- m[m$clase == k, ]
  data.frame(clase = k, n = nrow(s),
             pct_intubado = round(100 * mean(s$intubado, na.rm = TRUE), 1),
             gcs_total_mediano = median(s$gcs_total, na.rm = TRUE),
             gcs_em_mediano = median(s$gcs_em, na.rm = TRUE),
             row.names = NULL)
})), row.names = FALSE)

write.csv(m, "data/derivados/glasgow.csv", row.names = FALSE)
dir.create("outputs/fase17", recursive = TRUE, showWarnings = FALSE)
write.csv(u, "outputs/fase17/intubacion_por_unidad.csv", row.names = FALSE)
cat("\nGuardado en data/derivados/glasgow.csv\n")
