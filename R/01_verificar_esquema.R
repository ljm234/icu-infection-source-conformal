library(DBI)
library(duckdb)

MICRO <- path.expand("~/mimic-data/physionet.org/files/mimiciv/3.1/hosp/microbiologyevents.csv.gz")
ICU   <- path.expand("~/mimic-data/physionet.org/files/mimiciv/3.1/icu/icustays.csv.gz")

con <- dbConnect(duckdb::duckdb())

dbExecute(con, sprintf(
  "CREATE VIEW micro AS SELECT * FROM read_csv('%s', all_varchar=true)", MICRO))
dbExecute(con, sprintf(
  "CREATE VIEW icustays AS SELECT * FROM read_csv('%s', all_varchar=true)", ICU))

cat("\n=== COLUMNAS DE microbiologyevents ===\n")
print(dbGetQuery(con, "DESCRIBE micro")$column_name)

cat("\n=== COLUMNAS DE icustays ===\n")
print(dbGetQuery(con, "DESCRIBE icustays")$column_name)

dbDisconnect(con, shutdown = TRUE)
