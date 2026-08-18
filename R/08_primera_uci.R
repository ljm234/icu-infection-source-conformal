library(DBI)
library(duckdb)

BASE  <- path.expand("~/mimic-data/physionet.org/files/mimiciv/3.1")
MICRO <- file.path(BASE, "hosp", "microbiologyevents.csv.gz")
ICU   <- file.path(BASE, "icu",  "icustays.csv.gz")
ADM   <- file.path(BASE, "hosp", "admissions.csv.gz")

con <- dbConnect(duckdb::duckdb())
dbExecute(con, sprintf(
  "CREATE VIEW micro AS SELECT * FROM read_csv('%s', all_varchar=true)", MICRO))
dbExecute(con, sprintf(
  "CREATE VIEW icustays AS SELECT * FROM read_csv('%s', all_varchar=true)", ICU))
dbExecute(con, sprintf(
  "CREATE VIEW adm AS SELECT * FROM read_csv('%s', all_varchar=true)", ADM))

cat("\n-- Opcion A: t0 = ingreso a UCI, solo primera estancia --\n")
print(dbGetQuery(con, "
  WITH primera AS (
    SELECT hadm_id, stay_id, first_careunit,
           CAST(intime AS TIMESTAMP) AS intime,
           ROW_NUMBER() OVER (PARTITION BY hadm_id
                              ORDER BY CAST(intime AS TIMESTAMP)) AS orden
    FROM icustays
  )
  SELECT
    CASE
      WHEN CAST(m.charttime AS TIMESTAMP) < p.intime
        THEN 'antes de UCI'
      WHEN CAST(m.charttime AS TIMESTAMP) <= p.intime + INTERVAL 6 HOUR
        THEN 'primeras 6h'
      ELSE 'despues de 6h'
    END                                 AS momento,
    COUNT(DISTINCT m.micro_specimen_id) AS cultivos
  FROM micro m
  JOIN primera p ON m.hadm_id = p.hadm_id AND p.orden = 1
  GROUP BY 1
  ORDER BY cultivos DESC"))

cat("\n-- Opcion B: t0 = ingreso al hospital --\n")
print(dbGetQuery(con, "
  SELECT
    CASE
      WHEN CAST(m.charttime AS TIMESTAMP) < CAST(a.admittime AS TIMESTAMP)
        THEN 'antes de admision'
      WHEN CAST(m.charttime AS TIMESTAMP)
           <= CAST(a.admittime AS TIMESTAMP) + INTERVAL 6 HOUR
        THEN 'primeras 6h'
      ELSE 'despues de 6h'
    END                                 AS momento,
    COUNT(DISTINCT m.micro_specimen_id) AS cultivos
  FROM micro m
  JOIN adm a ON m.hadm_id = a.hadm_id
  JOIN icustays u ON m.hadm_id = u.hadm_id
  GROUP BY 1
  ORDER BY cultivos DESC"))

dbDisconnect(con, shutdown = TRUE)
