library(DBI)
library(duckdb)
library(digest)
library(jsonlite)

BASE  <- path.expand("~/mimic-data/physionet.org/files/mimiciv/3.1")
MICRO <- file.path(BASE, "hosp", "microbiologyevents.csv.gz")
ICU   <- file.path(BASE, "icu",  "icustays.csv.gz")
PAT   <- file.path(BASE, "hosp", "patients.csv.gz")

dir.create("outputs", showWarnings = FALSE)

con <- dbConnect(duckdb::duckdb())
for (v in list(c("micro", MICRO), c("icustays", ICU), c("patients", PAT))) {
  dbExecute(con, sprintf(
    "CREATE VIEW %s AS SELECT * FROM read_csv('%s', all_varchar=true)",
    v[1], v[2]))
}

cat("\n-- N real: estancias, no cultivos --\n")
n_real <- dbGetQuery(con, "
  WITH primera AS (
    SELECT hadm_id, stay_id, subject_id, first_careunit,
           CAST(intime AS TIMESTAMP) AS intime,
           ROW_NUMBER() OVER (PARTITION BY hadm_id
                              ORDER BY CAST(intime AS TIMESTAMP)) AS orden
    FROM icustays
  )
  SELECT
    COUNT(DISTINCT p.stay_id)           AS estancias,
    COUNT(DISTINCT p.subject_id)        AS pacientes,
    COUNT(DISTINCT m.micro_specimen_id) AS cultivos
  FROM primera p
  JOIN micro m ON m.hadm_id = p.hadm_id
  WHERE p.orden = 1
    AND CAST(m.charttime AS TIMESTAMP) >= p.intime
    AND CAST(m.charttime AS TIMESTAMP) <= p.intime + INTERVAL 6 HOUR")
print(n_real)

cat("\n-- Cuantos sitios positivos distintos por estancia --\n")
multi <- dbGetQuery(con, "
  WITH primera AS (
    SELECT hadm_id, stay_id,
           CAST(intime AS TIMESTAMP) AS intime,
           ROW_NUMBER() OVER (PARTITION BY hadm_id
                              ORDER BY CAST(intime AS TIMESTAMP)) AS orden
    FROM icustays
  ),
  pos AS (
    SELECT DISTINCT p.stay_id, m.spec_type_desc
    FROM primera p
    JOIN micro m ON m.hadm_id = p.hadm_id
    WHERE p.orden = 1
      AND m.org_name IS NOT NULL
      AND CAST(m.charttime AS TIMESTAMP) >= p.intime
      AND CAST(m.charttime AS TIMESTAMP) <= p.intime + INTERVAL 6 HOUR
  )
  SELECT sitios_positivos, COUNT(*) AS estancias
  FROM (SELECT stay_id, COUNT(*) AS sitios_positivos FROM pos GROUP BY stay_id)
  GROUP BY 1 ORDER BY 1")
print(multi)

cat("\n-- Unidades de cuidado, analogo de sede --\n")
unidades <- dbGetQuery(con, "
  SELECT first_careunit AS unidad, COUNT(*) AS estancias
  FROM icustays GROUP BY 1 ORDER BY estancias DESC")
print(unidades)

write.csv(n_real,   "outputs/n_real.csv",   row.names = FALSE)
write.csv(multi,    "outputs/multisitio.csv", row.names = FALSE)
write.csv(unidades, "outputs/unidades.csv", row.names = FALSE)

manifiesto <- list(
  ejecutado_en   = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
  r_version      = R.version.string,
  duckdb_version = as.character(packageVersion("duckdb")),
  sha256 = list(
    microbiologyevents = digest(file = MICRO, algo = "sha256"),
    icustays           = digest(file = ICU,   algo = "sha256"),
    patients           = digest(file = PAT,   algo = "sha256")))
write_json(manifiesto, "outputs/manifiesto.json", auto_unbox = TRUE, pretty = TRUE)

dbDisconnect(con, shutdown = TRUE)
cat("\nListo. Salidas en outputs/\n")
