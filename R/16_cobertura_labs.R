library(DBI)
library(duckdb)

BASE <- path.expand("~/mimic-data/physionet.org/files/mimiciv/3.1")
DLAB <- file.path(BASE, "hosp", "d_labitems.csv.gz")
LAB  <- file.path(BASE, "hosp", "labevents.csv.gz")
ICU  <- file.path(BASE, "icu",  "icustays.csv.gz")

con <- dbConnect(duckdb::duckdb())
dbExecute(con, "PRAGMA threads=8")
dbExecute(con, "PRAGMA memory_limit='40GB'")

for (v in list(c("dlab", DLAB), c("lab", LAB), c("icustays", ICU))) {
  dbExecute(con, sprintf(
    "CREATE VIEW %s AS SELECT * FROM read_csv('%s', all_varchar=true)",
    v[1], v[2]))
}

dbExecute(con, "
  CREATE VIEW estancia AS
  SELECT subject_id, hadm_id, stay_id, intime FROM (
    SELECT subject_id, hadm_id, stay_id,
           CAST(intime AS TIMESTAMP) AS intime,
           ROW_NUMBER() OVER (PARTITION BY subject_id
                              ORDER BY CAST(intime AS TIMESTAMP)) AS orden
    FROM icustays) WHERE orden = 1")

cat("\n-- Cobertura por examen, filtrando por storetime --\n")
res <- dbGetQuery(con, "
  SELECT
    d.itemid, d.label, d.fluid, d.category,
    COUNT(DISTINCT e.stay_id) AS estancias,
    ROUND(100.0 * COUNT(DISTINCT e.stay_id) / 65366.0, 1) AS pct
  FROM estancia e
  JOIN lab l  ON l.hadm_id = e.hadm_id
  JOIN dlab d ON d.itemid  = l.itemid
  WHERE CAST(l.storetime AS TIMESTAMP) >= e.intime
    AND CAST(l.storetime AS TIMESTAMP) <= e.intime + INTERVAL 6 HOUR
    AND l.valuenum IS NOT NULL
  GROUP BY 1,2,3,4
  HAVING COUNT(DISTINCT e.stay_id) >= 3000
  ORDER BY estancias DESC, d.itemid")
print(res, row.names = FALSE)

cat("\nExamenes con cobertura suficiente:", nrow(res), "\n")

dir.create("outputs/fase3", recursive = TRUE, showWarnings = FALSE)
write.csv(res, "outputs/fase3/cobertura_labs.csv", row.names = FALSE)

dbDisconnect(con, shutdown = TRUE)
