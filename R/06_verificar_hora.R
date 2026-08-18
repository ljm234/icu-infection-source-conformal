library(DBI)
library(duckdb)

BASE  <- path.expand("~/mimic-data/physionet.org/files/mimiciv/3.1")
MICRO <- file.path(BASE, "hosp", "microbiologyevents.csv.gz")

con <- dbConnect(duckdb::duckdb())
dbExecute(con, sprintf(
  "CREATE VIEW micro AS SELECT * FROM read_csv('%s', all_varchar=true)", MICRO))

cat("\n-- En toda la tabla, sin filtrar por UCI --\n")
print(dbGetQuery(con, "
  SELECT
    CASE WHEN charttime IS NULL      THEN 'nulo real'
         WHEN TRIM(charttime) = ''   THEN 'texto vacio'
         ELSE 'tiene valor' END        AS estado,
    COUNT(*)                           AS filas
  FROM micro
  GROUP BY 1"))

cat("\n-- Diez valores de ejemplo --\n")
print(dbGetQuery(con, "
  SELECT chartdate, charttime
  FROM micro
  LIMIT 10"))

dbDisconnect(con, shutdown = TRUE)
