library(DBI)
library(duckdb)

MICRO <- path.expand("~/mimic-data/physionet.org/files/mimiciv/3.1/hosp/microbiologyevents.csv.gz")

con <- dbConnect(duckdb::duckdb())
dbExecute(con, sprintf(
  "CREATE VIEW micro AS SELECT * FROM read_csv('%s', all_varchar=true)", MICRO))

print(dbGetQuery(con, "
  SELECT
    CASE WHEN hadm_id IS NULL THEN 'sin hospitalizacion'
         ELSE 'con hospitalizacion' END AS grupo,
    COUNT(DISTINCT micro_specimen_id)    AS cultivos
  FROM micro
  GROUP BY 1"))

dbDisconnect(con, shutdown = TRUE)
