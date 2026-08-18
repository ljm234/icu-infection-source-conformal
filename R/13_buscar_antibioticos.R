library(DBI)
library(duckdb)

BASE <- path.expand("~/mimic-data/physionet.org/files/mimiciv/3.1")
RX   <- file.path(BASE, "hosp", "prescriptions.csv.gz")

con <- dbConnect(duckdb::duckdb())
dbExecute(con, sprintf(
  "CREATE VIEW rx AS SELECT * FROM read_csv('%s', all_varchar=true)", RX))

cat("\n-- Que es drug_type --\n")
print(dbGetQuery(con, "
  SELECT drug_type, COUNT(*) AS recetas
  FROM rx GROUP BY 1 ORDER BY recetas DESC"))

cat("\n-- Candidatos a antibiotico por raiz de nombre --\n")
print(dbGetQuery(con, "
  SELECT drug, COUNT(*) AS recetas
  FROM rx
  WHERE lower(drug) SIMILAR TO
    '.*(cillin|cephal|cefaz|cefep|ceftr|ceftaz|cefur|cefpo|mycin|micin|floxacin|penem|cycline|sulfamethox|metronidazol|linezolid|daptomycin|azithro|clarithro|clindamycin|nitrofurantoin|rifamp|aztreonam|colistin|polymyxin|tigecycline|fosfomycin).*'
  GROUP BY 1
  ORDER BY recetas DESC
  LIMIT 60"))

dbDisconnect(con, shutdown = TRUE)
