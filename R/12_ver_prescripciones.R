library(DBI)
library(duckdb)

BASE <- path.expand("~/mimic-data/physionet.org/files/mimiciv/3.1")
RX   <- file.path(BASE, "hosp", "prescriptions.csv.gz")

con <- dbConnect(duckdb::duckdb())
dbExecute(con, sprintf(
  "CREATE VIEW rx AS SELECT * FROM read_csv('%s', all_varchar=true)", RX))

cat("\n-- Columnas --\n")
print(dbGetQuery(con, "DESCRIBE rx")$column_name)

cat("\n-- Cinco filas de ejemplo --\n")
print(dbGetQuery(con, "
  SELECT subject_id, hadm_id, starttime, drug, route
  FROM rx LIMIT 5"))

cat("\n-- Medicamentos mas frecuentes --\n")
print(dbGetQuery(con, "
  SELECT drug, COUNT(*) AS recetas
  FROM rx GROUP BY 1 ORDER BY recetas DESC LIMIT 25"))

cat("\n-- Vias de administracion --\n")
print(dbGetQuery(con, "
  SELECT route, COUNT(*) AS recetas
  FROM rx GROUP BY 1 ORDER BY recetas DESC LIMIT 10"))

dbDisconnect(con, shutdown = TRUE)
