library(DBI)
library(duckdb)

BASE <- path.expand("~/mimic-data/physionet.org/files/mimiciv/3.1")
LAB  <- file.path(BASE, "hosp", "labevents.csv.gz")
ICU  <- file.path(BASE, "icu",  "icustays.csv.gz")

con <- dbConnect(duckdb::duckdb())
dbExecute(con, "PRAGMA threads=8")
dbExecute(con, "PRAGMA memory_limit='40GB'")

for (v in list(c("lab", LAB), c("icustays", ICU))) {
  dbExecute(con, sprintf(
    "CREATE VIEW %s AS SELECT * FROM read_csv('%s', all_varchar=true)",
    v[1], v[2]))
}

dbExecute(con, "
  CREATE VIEW estancia AS
  SELECT subject_id, hadm_id, stay_id, first_careunit, intime FROM (
    SELECT subject_id, hadm_id, stay_id, first_careunit,
           CAST(intime AS TIMESTAMP) AS intime,
           ROW_NUMBER() OVER (PARTITION BY subject_id
                              ORDER BY CAST(intime AS TIMESTAMP)) AS orden
    FROM icustays) WHERE orden = 1")

cat("\n-- Solicitud de lactato por unidad --\n")
print(dbGetQuery(con, "
  SELECT
    e.first_careunit AS unidad,
    COUNT(DISTINCT e.stay_id) AS estancias,
    COUNT(DISTINCT CASE WHEN l.itemid IN ('50813','52442','53154')
                        THEN e.stay_id END) AS con_lactato,
    ROUND(100.0 * COUNT(DISTINCT CASE WHEN l.itemid IN ('50813','52442','53154')
                        THEN e.stay_id END)
          / NULLIF(COUNT(DISTINCT e.stay_id),0), 1) AS pct
  FROM estancia e
  LEFT JOIN lab l ON l.hadm_id = e.hadm_id
    AND CAST(l.storetime AS TIMESTAMP) >= e.intime
    AND CAST(l.storetime AS TIMESTAMP) <= e.intime + INTERVAL 6 HOUR
    AND l.valuenum IS NOT NULL
  GROUP BY 1
  HAVING COUNT(DISTINCT e.stay_id) >= 500
  ORDER BY estancias DESC"), row.names = FALSE)

dbDisconnect(con, shutdown = TRUE)
