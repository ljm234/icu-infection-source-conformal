library(DBI)
library(duckdb)

BASE  <- path.expand("~/mimic-data/physionet.org/files/mimiciv/3.1")
MICRO <- file.path(BASE, "hosp", "microbiologyevents.csv.gz")
ICU   <- file.path(BASE, "icu",  "icustays.csv.gz")

con <- dbConnect(duckdb::duckdb())
dbExecute(con, sprintf(
  "CREATE VIEW micro AS SELECT * FROM read_csv('%s', all_varchar=true)", MICRO))
dbExecute(con, sprintf(
  "CREATE VIEW icustays AS SELECT * FROM read_csv('%s', all_varchar=true)", ICU))

print(dbGetQuery(con, "
  SELECT
    COUNT(DISTINCT m.micro_specimen_id) AS cultivos,
    COUNT(DISTINCT m.hadm_id)           AS hospitalizaciones
  FROM micro m
  JOIN icustays u ON m.hadm_id = u.hadm_id
  WHERE m.hadm_id IS NOT NULL"))

dbDisconnect(con, shutdown = TRUE)
