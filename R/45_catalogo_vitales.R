library(DBI)
library(duckdb)

BASE  <- path.expand("~/mimic-data/physionet.org/files/mimiciv/3.1")
DITEM <- file.path(BASE, "icu", "d_items.csv.gz")

con <- dbConnect(duckdb::duckdb())
dbExecute(con, "PRAGMA threads=8")
dbExecute(con, sprintf(
  "CREATE VIEW ditem AS SELECT * FROM read_csv('%s', all_varchar=true)",
  DITEM))

# Antes de consultar la tabla de registros clinicos conviene establecer con
# exactitud que identificadores corresponden a cada signo. El diccionario
# contiene denominaciones proximas entre si que difieren en la unidad de
# medida o en el metodo de obtencion, de modo que una seleccion inadvertida
# mezclaria escalas distintas o introduciria una variable cuya presencia
# depende de la gravedad del paciente.

cat("=== TEMPERATURA ===\n")
print(dbGetQuery(con, "
  SELECT itemid, label, unitname, param_type
  FROM ditem
  WHERE lower(label) LIKE '%temperature%'
    AND lower(linksto) = 'chartevents'
  ORDER BY label"), row.names = FALSE)

cat("\n=== FRECUENCIA CARDIACA ===\n")
print(dbGetQuery(con, "
  SELECT itemid, label, unitname, param_type
  FROM ditem
  WHERE lower(label) LIKE '%heart rate%'
    AND lower(linksto) = 'chartevents'
  ORDER BY label"), row.names = FALSE)

cat("\n=== FRECUENCIA RESPIRATORIA ===\n")
print(dbGetQuery(con, "
  SELECT itemid, label, unitname, param_type
  FROM ditem
  WHERE lower(label) LIKE '%respiratory rate%'
    AND lower(linksto) = 'chartevents'
  ORDER BY label"), row.names = FALSE)

cat("\n=== PRESION ARTERIAL ===\n")
print(dbGetQuery(con, "
  SELECT itemid, label, unitname, param_type
  FROM ditem
  WHERE lower(label) SIMILAR TO '.*(blood pressure|arterial pressure).*'
    AND lower(linksto) = 'chartevents'
    AND lower(label) SIMILAR TO '.*(systolic|diastolic|mean).*'
  ORDER BY label"), row.names = FALSE)

cat("\n=== SATURACION DE OXIGENO ===\n")
print(dbGetQuery(con, "
  SELECT itemid, label, unitname, param_type
  FROM ditem
  WHERE lower(label) SIMILAR TO '.*(o2 saturation|spo2).*'
    AND lower(linksto) = 'chartevents'
  ORDER BY label"), row.names = FALSE)

cat("\n=== ESCALA DE COMA DE GLASGOW ===\n")
print(dbGetQuery(con, "
  SELECT itemid, label, unitname, param_type
  FROM ditem
  WHERE lower(label) LIKE '%gcs%'
    AND lower(linksto) = 'chartevents'
  ORDER BY label"), row.names = FALSE)

dbDisconnect(con, shutdown = TRUE)
