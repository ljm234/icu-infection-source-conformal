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

res <- dbGetQuery(con, "
  WITH primera AS (
    SELECT hadm_id,
           CAST(intime AS TIMESTAMP) AS intime,
           ROW_NUMBER() OVER (PARTITION BY hadm_id
                              ORDER BY CAST(intime AS TIMESTAMP)) AS orden
    FROM icustays
  ),
  ventana AS (
    SELECT DISTINCT m.micro_specimen_id, m.spec_type_desc, m.org_name
    FROM micro m
    JOIN primera p ON m.hadm_id = p.hadm_id AND p.orden = 1
    WHERE CAST(m.charttime AS TIMESTAMP) >= p.intime
      AND CAST(m.charttime AS TIMESTAMP) <= p.intime + INTERVAL 6 HOUR
  )
  SELECT
    spec_type_desc                    AS tipo_muestra,
    COUNT(DISTINCT micro_specimen_id) AS cultivos,
    COUNT(DISTINCT CASE WHEN org_name IS NOT NULL
                        THEN micro_specimen_id END) AS con_crecimiento
  FROM ventana
  GROUP BY 1
  ORDER BY cultivos DESC, tipo_muestra")

print(res, row.names = FALSE)
cat("\nTipos distintos:", nrow(res), "\n")
cat("Cultivos totales:", sum(res$cultivos), "\n")

write.csv(res, "outputs/tipos_muestra_ventana6h.csv", row.names = FALSE)

dbDisconnect(con, shutdown = TRUE)
