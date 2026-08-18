library(DBI)
library(duckdb)

BASE <- path.expand("~/mimic-data/physionet.org/files/mimiciv/3.1")
DLAB <- file.path(BASE, "hosp", "d_labitems.csv.gz")
LAB  <- file.path(BASE, "hosp", "labevents.csv.gz")

con <- dbConnect(duckdb::duckdb())
dbExecute(con, "PRAGMA threads=8")
dbExecute(con, sprintf(
  "CREATE VIEW dlab AS SELECT * FROM read_csv('%s', all_varchar=true)", DLAB))
dbExecute(con, sprintf(
  "CREATE VIEW lab AS SELECT * FROM read_csv('%s', all_varchar=true)", LAB))

cat("\n-- Columnas de d_labitems --\n")
print(dbGetQuery(con, "DESCRIBE dlab")$column_name)

cat("\n-- Columnas de labevents --\n")
print(dbGetQuery(con, "DESCRIBE lab")$column_name)

cat("\n-- Examenes candidatos para sepsis --\n")
print(dbGetQuery(con, "
  SELECT itemid, label, fluid, category
  FROM dlab
  WHERE lower(label) SIMILAR TO '.*(white blood|neutrophil|lymphocyte|platelet|lactate|creatinine|bilirubin|albumin|c-reactive|procalcitonin|bicarbonate|urea nitrogen|glucose|hemoglobin|band|ph$).*'
    AND lower(fluid) IN ('blood','urine')
  ORDER BY category, label
  LIMIT 60"), row.names = FALSE)

dbDisconnect(con, shutdown = TRUE)
