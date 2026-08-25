library(DBI)
library(duckdb)

# Dos ejecuciones de la extraccion de presion arterial produjeron
# correlaciones globales de 0.3446 y 0.3456 sobre el mismo numero de
# estancias. Identico recuento con resultado distinto implica que los valores
# retenidos no fueron los mismos.
#
# La seleccion de la primera determinacion se apoya en una funcion de ventana
# ordenada por hora de registro. Cuando dos mediciones comparten esa hora, el
# orden entre ellas no queda determinado por la especificacion del lenguaje, y
# ejecuciones sucesivas pueden retener valores distintos.
#
# El presente procedimiento cuantifica la frecuencia de tales coincidencias.
# La cuestion excede a la presion arterial: la totalidad de las extracciones
# del proyecto emplea el mismo patron, incluidas las diecisiete
# determinaciones bioquimicas.

BASE  <- path.expand("~/mimic-data/physionet.org/files/mimiciv/3.1")
CHART <- file.path(BASE, "icu",  "chartevents.csv.gz")
LAB   <- file.path(BASE, "hosp", "labevents.csv.gz")
ICU   <- file.path(BASE, "icu",  "icustays.csv.gz")

con <- dbConnect(duckdb::duckdb())
dbExecute(con, "PRAGMA threads=8")
dbExecute(con, "PRAGMA memory_limit='48GB'")
for (x in list(c("chart", CHART), c("lab", LAB), c("icustays", ICU)))
  dbExecute(con, sprintf(
    "CREATE VIEW %s AS SELECT * FROM read_csv('%s', all_varchar=true)",
    x[1], x[2]))

dbExecute(con, "
  CREATE VIEW estancia AS
  SELECT subject_id, hadm_id, stay_id, intime FROM (
    SELECT subject_id, hadm_id, stay_id,
           CAST(intime AS TIMESTAMP) AS intime,
           ROW_NUMBER() OVER (PARTITION BY subject_id
                              ORDER BY CAST(intime AS TIMESTAMP)) AS orden
    FROM icustays) WHERE orden = 1")

cat("=== COINCIDENCIAS EN LA HORA DE REGISTRO ===\n")
cat("Constantes procedentes de la tabla de registros clinicos\n\n")

emp_chart <- dbGetQuery(con, "
  SELECT codigo,
         COUNT(DISTINCT stay_id) AS estancias,
         COUNT(DISTINCT CASE WHEN empatadas > 1 THEN stay_id END) AS con_coincidencia,
         MAX(empatadas) AS maximo_coincidentes
  FROM (
    SELECT stay_id, codigo, COUNT(*) AS empatadas
    FROM (
      SELECT e.stay_id, c.itemid AS codigo,
             CAST(c.storetime AS TIMESTAMP) AS registro,
             MIN(CAST(c.storetime AS TIMESTAMP)) OVER
               (PARTITION BY e.stay_id, c.itemid) AS primero
      FROM estancia e
      JOIN chart c ON c.stay_id = e.stay_id
      WHERE c.itemid IN ('220181','220052','223762','220045','220210','220277')
        AND c.valuenum IS NOT NULL
        AND CAST(c.storetime AS TIMESTAMP) >= e.intime
        AND CAST(c.storetime AS TIMESTAMP) <= e.intime + INTERVAL 6 HOUR)
    WHERE registro = primero
    GROUP BY stay_id, codigo)
  GROUP BY codigo
  ORDER BY codigo")

nombres <- c("220045" = "frecuencia cardiaca",
             "220052" = "presion media invasiva",
             "220181" = "presion media no invasiva",
             "220210" = "frecuencia respiratoria",
             "220277" = "saturacion",
             "223762" = "temperatura")
emp_chart$variable <- nombres[as.character(emp_chart$codigo)]
emp_chart$pct <- round(100 * emp_chart$con_coincidencia / emp_chart$estancias, 2)
print(emp_chart[, c("variable","estancias","con_coincidencia","pct",
                    "maximo_coincidentes")], row.names = FALSE)

cat("\nDeterminaciones bioquimicas\n\n")

emp_lab <- dbGetQuery(con, "
  SELECT COUNT(DISTINCT stay_id) AS estancias,
         COUNT(DISTINCT CASE WHEN empatadas > 1 THEN stay_id END) AS con_coincidencia,
         MAX(empatadas) AS maximo_coincidentes
  FROM (
    SELECT stay_id, codigo, COUNT(*) AS empatadas
    FROM (
      SELECT e.stay_id, l.itemid AS codigo,
             CAST(l.storetime AS TIMESTAMP) AS registro,
             MIN(CAST(l.storetime AS TIMESTAMP)) OVER
               (PARTITION BY e.stay_id, l.itemid) AS primero
      FROM estancia e
      JOIN lab l ON l.hadm_id = e.hadm_id
      WHERE l.itemid IN ('51301','51222','51265','51277','50912','51006',
                         '50868','50983','50971','50902','50882','51237',
                         '51275','50820','50818','50813','50802')
        AND l.valuenum IS NOT NULL
        AND CAST(l.storetime AS TIMESTAMP) >= e.intime
        AND CAST(l.storetime AS TIMESTAMP) <= e.intime + INTERVAL 6 HOUR)
    WHERE registro = primero
    GROUP BY stay_id, codigo)")

emp_lab$pct <- round(100 * emp_lab$con_coincidencia / emp_lab$estancias, 2)
print(emp_lab, row.names = FALSE)

# Cuando varias determinaciones comparten la hora de registro, la magnitud de
# la discrepancia entre ellas determina si la eleccion resulta indiferente. Si
# los valores coincidentes fuesen identicos, el orden careceria de
# consecuencias.
cat("\n=== DISPERSION ENTRE DETERMINACIONES COINCIDENTES ===\n")

disp <- dbGetQuery(con, "
  SELECT codigo,
         COUNT(*) AS grupos,
         ROUND(AVG(rango), 3) AS rango_medio,
         ROUND(MAX(rango), 3) AS rango_maximo,
         SUM(CASE WHEN rango = 0 THEN 1 ELSE 0 END) AS identicos
  FROM (
    SELECT stay_id, codigo, MAX(valor) - MIN(valor) AS rango
    FROM (
      SELECT e.stay_id, c.itemid AS codigo,
             CAST(c.valuenum AS DOUBLE) AS valor,
             CAST(c.storetime AS TIMESTAMP) AS registro,
             MIN(CAST(c.storetime AS TIMESTAMP)) OVER
               (PARTITION BY e.stay_id, c.itemid) AS primero
      FROM estancia e
      JOIN chart c ON c.stay_id = e.stay_id
      WHERE c.itemid IN ('220181','220052','223762','220045','220210','220277')
        AND c.valuenum IS NOT NULL
        AND CAST(c.storetime AS TIMESTAMP) >= e.intime
        AND CAST(c.storetime AS TIMESTAMP) <= e.intime + INTERVAL 6 HOUR)
    WHERE registro = primero
    GROUP BY stay_id, codigo
    HAVING COUNT(*) > 1)
  GROUP BY codigo
  ORDER BY codigo")

if (nrow(disp) == 0) {
  cat("No consta ninguna coincidencia en la hora de registro.\n")
} else {
  disp$variable <- nombres[as.character(disp$codigo)]
  disp$pct_identicos <- round(100 * disp$identicos / disp$grupos, 1)
  print(disp[, c("variable","grupos","rango_medio","rango_maximo",
                 "pct_identicos")], row.names = FALSE)
}

dbDisconnect(con, shutdown = TRUE)

dir.create("outputs/fase20", recursive = TRUE, showWarnings = FALSE)
write.csv(emp_chart, "outputs/fase20/empates_constantes.csv", row.names = FALSE)
write.csv(emp_lab, "outputs/fase20/empates_laboratorio.csv", row.names = FALSE)
if (nrow(disp) > 0)
  write.csv(disp, "outputs/fase20/dispersion_empates.csv", row.names = FALSE)

cat("\n=== CONCLUSION ===\n")
if (max(emp_chart$con_coincidencia) == 0 && emp_lab$con_coincidencia == 0) {
  cat("No existen coincidencias. La discrepancia observada obedece a otra\n")
  cat("causa, que procede investigar antes de cerrar el componente.\n")
} else {
  cat("Existen coincidencias en la hora de registro. La seleccion de la\n")
  cat("primera determinacion no queda por tanto determinada, y el conjunto\n")
  cat("de procedimientos no resulta reproducible bit a bit sin un criterio\n")
  cat("de desempate explicito.\n")
}
