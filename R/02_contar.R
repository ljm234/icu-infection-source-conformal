library(DBI)
library(duckdb)

MICRO <- path.expand("~/mimic-data/physionet.org/files/mimiciv/3.1/hosp/microbiologyevents.csv.gz")

con <- dbConnect(duckdb::duckdb())
dbExecute(con, sprintf(
  "CREATE VIEW micro AS SELECT * FROM read_csv('%s', all_varchar=true)", MICRO))

print(dbGetQuery(con, "
  SELECT
    COUNT(*)                          AS filas,
    COUNT(DISTINCT micro_specimen_id) AS cultivos,
    COUNT(DISTINCT hadm_id)           AS hospitalizaciones,
    COUNT(DISTINCT subject_id)        AS pacientes
  FROM micro"))

dbDisconnect(con, shutdown = TRUE)
