library(DBI)
library(duckdb)

BASE <- path.expand("~/mimic-data/physionet.org/files/mimiciv/3.1")
LAB  <- file.path(BASE, "hosp", "labevents.csv.gz")
DLAB <- file.path(BASE, "hosp", "d_labitems.csv.gz")
ICU  <- file.path(BASE, "icu",  "icustays.csv.gz")

con <- dbConnect(duckdb::duckdb())
dbExecute(con, "PRAGMA threads=8")
dbExecute(con, "PRAGMA memory_limit='40GB'")

for (v in list(c("lab", LAB), c("dlab", DLAB), c("icustays", ICU))) {
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

dbExecute(con, "
  CREATE VIEW medidas AS
  SELECT e.stay_id, l.itemid, COUNT(*) AS n_mediciones
  FROM estancia e
  JOIN lab l ON l.hadm_id = e.hadm_id
  WHERE CAST(l.storetime AS TIMESTAMP) >= e.intime
    AND CAST(l.storetime AS TIMESTAMP) <= e.intime + INTERVAL 6 HOUR
    AND l.valuenum IS NOT NULL
    AND l.itemid IN ('51301','51222','51265','51277',
                     '50912','51006','50868',
                     '50983','50971','50902','50882',
                     '51237','51275',
                     '50820','50818','50813','50802')
  GROUP BY 1,2")

cat("\n-- Mediciones por variable dentro de la ventana --\n")
res <- dbGetQuery(con, "
  SELECT
    d.label,
    COUNT(*)                                              AS estancias,
    ROUND(AVG(m.n_mediciones), 2)                         AS promedio,
    MAX(m.n_mediciones)                                   AS maximo,
    ROUND(100.0 * SUM(CASE WHEN m.n_mediciones = 1 THEN 1 ELSE 0 END)
          / COUNT(*), 1)                                  AS pct_una_sola,
    ROUND(100.0 * SUM(CASE WHEN m.n_mediciones >= 3 THEN 1 ELSE 0 END)
          / COUNT(*), 1)                                  AS pct_tres_o_mas
  FROM medidas m
  JOIN dlab d ON d.itemid = m.itemid
  GROUP BY 1
  ORDER BY promedio DESC, itemid")
print(res, row.names = FALSE)

dir.create("outputs/fase3", recursive = TRUE, showWarnings = FALSE)
write.csv(res, "outputs/fase3/repeticion.csv", row.names = FALSE)

dbDisconnect(con, shutdown = TRUE)
