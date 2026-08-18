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
    CASE WHEN m.charttime IS NULL THEN 'solo fecha'
         ELSE 'fecha y hora' END      AS tiene,
    COUNT(DISTINCT m.micro_specimen_id) AS cultivos
  FROM micro m
  JOIN icustays u ON m.hadm_id = u.hadm_id
  GROUP BY 1"))

dbDisconnect(con, shutdown = TRUE)
