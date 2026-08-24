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

# El diccionario declara los tres componentes de la escala como texto. Antes
# de establecer correspondencia alguna con su puntuacion conviene comprobar
# que expresiones se registran en la practica y si la tabla conserva ademas
# una version numerica del componente. Una correspondencia construida sobre
# la denominacion habitual de la escala, y no sobre el contenido efectivo del
# registro, dejaria sin traducir las expresiones que no coincidan.
cat("Enumerando las expresiones registradas en los componentes.\n")

vals <- dbGetQuery(con, "
  SELECT itemid,
         value,
         COUNT(*) AS n,
         MIN(CAST(valuenum AS DOUBLE)) AS num_min,
         MAX(CAST(valuenum AS DOUBLE)) AS num_max
  FROM chart
  WHERE itemid IN ('220739','223901','223900')
    AND value IS NOT NULL
  GROUP BY itemid, value
  ORDER BY itemid, n DESC")

nombres <- c("220739" = "apertura ocular",
             "223901" = "respuesta motora",
             "223900" = "respuesta verbal")

for (id in names(nombres)) {
  cat("\n===", toupper(nombres[id]), "  itemid", id, "===\n")
  s <- vals[vals$itemid == id, c("value","n","num_min","num_max")]
  print(s, row.names = FALSE)
}

dbExecute(con, "
  CREATE VIEW estancia AS
  SELECT subject_id, stay_id, intime FROM (
    SELECT subject_id, stay_id,
           CAST(intime AS TIMESTAMP) AS intime,
           ROW_NUMBER() OVER (PARTITION BY subject_id
                              ORDER BY CAST(intime AS TIMESTAMP)) AS orden
    FROM icustays) WHERE orden = 1")

# La disponibilidad se evalua dentro de la misma ventana y con el mismo
# criterio temporal aplicado al resto de las variables, de modo que la cifra
# resulte comparable con la cobertura ya establecida para las constantes.
cat("\nCalculando disponibilidad dentro de la ventana de seis horas.\n")

cob <- dbGetQuery(con, "
  SELECT
    COUNT(DISTINCT stay_id) AS estancias,
    COUNT(DISTINCT CASE WHEN codigo = '220739' THEN stay_id END) AS ocular,
    COUNT(DISTINCT CASE WHEN codigo = '223901' THEN stay_id END) AS motora,
    COUNT(DISTINCT CASE WHEN codigo = '223900' THEN stay_id END) AS verbal
  FROM (
    SELECT e.stay_id, c.itemid AS codigo
    FROM estancia e
    JOIN chart c ON c.stay_id = e.stay_id
    WHERE c.itemid IN ('220739','223901','223900')
      AND c.value IS NOT NULL
      AND CAST(c.storetime AS TIMESTAMP) >= e.intime
      AND CAST(c.storetime AS TIMESTAMP) <= e.intime + INTERVAL 6 HOUR)")

dbDisconnect(con, shutdown = TRUE)

part <- read.csv("outputs/fase5/matriz_particionada.csv",
                 stringsAsFactors = FALSE)

cat("\n=== DISPONIBILIDAD EN LA VENTANA ===\n")
print(data.frame(
  componente = c("apertura ocular","respuesta motora","respuesta verbal"),
  estancias = c(cob$ocular, cob$motora, cob$verbal),
  pct_de_las_que_tienen_alguno =
    round(100 * c(cob$ocular, cob$motora, cob$verbal) / cob$estancias, 1),
  row.names = NULL), row.names = FALSE)
cat("\nEstancias con al menos un componente:", cob$estancias, "\n")
cat("Estancias en la cohorte del estudio:", nrow(part), "\n")
