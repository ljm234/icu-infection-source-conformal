library(DBI)
library(duckdb)

BASE  <- path.expand("~/mimic-data/physionet.org/files/mimiciv/3.1")
CHART <- file.path(BASE, "icu", "chartevents.csv.gz")
ICU   <- file.path(BASE, "icu", "icustays.csv.gz")

con <- dbConnect(duckdb::duckdb())
dbExecute(con, "PRAGMA threads=8")
dbExecute(con, "PRAGMA memory_limit='48GB'")

for (v in list(c("chart", CHART), c("icustays", ICU)))
  dbExecute(con, sprintf(
    "CREATE VIEW %s AS SELECT * FROM read_csv('%s', all_varchar=true)",
    v[1], v[2]))

dbExecute(con, "
  CREATE VIEW estancia AS
  SELECT subject_id, stay_id, intime FROM (
    SELECT subject_id, stay_id,
           CAST(intime AS TIMESTAMP) AS intime,
           ROW_NUMBER() OVER (PARTITION BY subject_id
                              ORDER BY CAST(intime AS TIMESTAMP)) AS orden
    FROM icustays) WHERE orden = 1")

# La ventana temporal se define sobre la hora de registro y no sobre la de
# anotacion. Un valor anotado dentro del intervalo pero incorporado al
# sistema con posterioridad no estaba disponible en el momento de la
# decision, y su empleo constituiria una anticipacion de informacion.
#
# La temperatura consta en dos escalas segun la unidad de procedencia. Las
# lecturas en grados Fahrenheit se convierten a Celsius antes de cualquier
# operacion posterior, y el resultado se somete a una comprobacion de rango
# fisiologico que revelaria una conversion defectuosa.
dbExecute(con, "
  CREATE VIEW crudo AS
  SELECT e.stay_id,
         CASE WHEN c.itemid = '223761' THEN '223762' ELSE c.itemid END AS codigo,
         CASE WHEN c.itemid = '223761'
              THEN (CAST(c.valuenum AS DOUBLE) - 32) * 5.0 / 9.0
              ELSE CAST(c.valuenum AS DOUBLE) END AS valor,
         CAST(c.storetime AS TIMESTAMP) AS registro
  FROM estancia e
  JOIN chart c ON c.stay_id = e.stay_id
  WHERE c.itemid IN ('223762','223761','220045','220210',
                     '220179','220181','220277')
    AND c.valuenum IS NOT NULL
    AND CAST(c.storetime AS TIMESTAMP) >= e.intime
    AND CAST(c.storetime AS TIMESTAMP) <= e.intime + INTERVAL 6 HOUR")

# De las determinaciones disponibles dentro de la ventana se retiene la
# primera de cada tipo. Un estadistico calculado sobre el conjunto, como la
# media o el valor extremo, dependeria del numero de mediciones efectuadas y
# por tanto de la intensidad de monitorizacion propia de cada unidad.
dbExecute(con, "
  CREATE VIEW primero AS
  SELECT stay_id, codigo, valor FROM (
    SELECT stay_id, codigo, valor,
           ROW_NUMBER() OVER (PARTITION BY stay_id, codigo
                              ORDER BY registro) AS orden
    FROM crudo) WHERE orden = 1")

cat("Consultando chartevents. Esto toma varios minutos.\n")

vit <- dbGetQuery(con, "
  SELECT stay_id,
    MAX(CASE WHEN codigo = '223762' THEN valor END) AS temperatura,
    MAX(CASE WHEN codigo = '220045' THEN valor END) AS frec_cardiaca,
    MAX(CASE WHEN codigo = '220210' THEN valor END) AS frec_respiratoria,
    MAX(CASE WHEN codigo = '220179' THEN valor END) AS presion_sistolica,
    MAX(CASE WHEN codigo = '220181' THEN valor END) AS presion_media,
    MAX(CASE WHEN codigo = '220277' THEN valor END) AS saturacion
  FROM primero GROUP BY stay_id")

dbDisconnect(con, shutdown = TRUE)

cat("Estancias con al menos un signo vital:", nrow(vit), "\n")

part <- read.csv("outputs/fase5/matriz_particionada.csv",
                 stringsAsFactors = FALSE)
m <- merge(part[, c("stay_id","clase","grupo","unidad")], vit,
           by = "stay_id", all.x = TRUE)

VITALES <- c("temperatura","frec_cardiaca","frec_respiratoria",
             "presion_sistolica","presion_media","saturacion")

cat("\n=== COMPROBACION DE RANGO FISIOLOGICO ===\n")
rangos <- list(temperatura = c(32, 42), frec_cardiaca = c(20, 250),
               frec_respiratoria = c(4, 60), presion_sistolica = c(40, 250),
               presion_media = c(25, 180), saturacion = c(50, 100))
print(do.call(rbind, lapply(VITALES, function(v) {
  x <- m[[v]][!is.na(m[[v]])]
  r <- rangos[[v]]
  data.frame(variable = v,
             minimo = round(min(x), 1), maximo = round(max(x), 1),
             mediana = round(median(x), 1),
             fuera_de_rango = sum(x < r[1] | x > r[2]),
             row.names = NULL)
})), row.names = FALSE)

cat("\n=== COBERTURA GLOBAL ===\n")
cob <- data.frame(
  variable = VITALES,
  disponible = sapply(VITALES, function(v) sum(!is.na(m[[v]]))),
  pct = round(100 * sapply(VITALES, function(v) mean(!is.na(m[[v]]))), 1),
  row.names = NULL)
print(cob, row.names = FALSE)

# El nombre de la unidad se recorta para que la tabla quepa en la consola, y
# solo ahi. Recortarlo en el dato dejaba el deposito con un identificador
# mutilado y obligaba a emparejar por prefijo aguas abajo.
corto <- function(x, n) substr(x, 1, n)

cat("\n=== COBERTURA POR UNIDAD ===\n")
u <- do.call(rbind, lapply(sort(unique(m$unidad)), function(un) {
  s <- m[m$unidad == un, ]
  if (nrow(s) < 500) return(NULL)
  data.frame(unidad = un, n = nrow(s),
             t(round(100 * sapply(VITALES, function(v) mean(!is.na(s[[v]]))), 1)),
             row.names = NULL)
}))
names(u)[3:8] <- c("temp","fc","fr","pas","pam","spo2")
print(transform(u, unidad = corto(unidad, 34)), row.names = FALSE)

cat("\n=== ESTANCIAS CON LOS SEIS SIGNOS ===\n")
completos <- rowSums(!is.na(m[, VITALES])) == 6
cat(sum(completos), sprintf("(%.1f por ciento)\n", 100 * mean(completos)))

write.csv(m, "data/derivados/vitales.csv", row.names = FALSE)
dir.create("outputs/fase17", recursive = TRUE, showWarnings = FALSE)
write.csv(cob, "outputs/fase17/cobertura_vitales.csv", row.names = FALSE)
write.csv(u, "outputs/fase17/cobertura_por_unidad.csv", row.names = FALSE)
cat("\nMatriz guardada en data/derivados/vitales.csv\n")
