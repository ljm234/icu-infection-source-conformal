library(DBI)
library(duckdb)
library(digest)
set.seed(20260818)

BASE <- path.expand("~/mimic-data/physionet.org/files/mimiciv/3.1")
LAB  <- file.path(BASE, "hosp", "labevents.csv.gz")
ICU  <- file.path(BASE, "icu",  "icustays.csv.gz")

con <- dbConnect(duckdb::duckdb())
dbExecute(con, "PRAGMA threads=8")
dbExecute(con, "PRAGMA memory_limit='40GB'")
for (v in list(c("lab", LAB), c("icustays", ICU)))
  dbExecute(con, sprintf(
    "CREATE VIEW %s AS SELECT * FROM read_csv('%s', all_varchar=true)",
    v[1], v[2]))

dbExecute(con, "
  CREATE VIEW estancia AS
  SELECT subject_id, hadm_id, stay_id, intime FROM (
    SELECT subject_id, hadm_id, stay_id,
           CAST(intime AS TIMESTAMP) AS intime,
           ROW_NUMBER() OVER (PARTITION BY subject_id
                              ORDER BY CAST(intime AS TIMESTAMP)) AS orden
    FROM icustays) WHERE orden = 1")

items <- c(leucocitos="51301", hemoglobina="51222", plaquetas="51265",
           rdw="51277", creatinina="50912", urea="51006",
           brecha_anion="50868", sodio="50983", potasio="50971",
           cloro="50902", bicarbonato="50882", inr="51237",
           ttpa="51275", ph="50820", pco2="50818",
           lactato="50813", exceso_base="50802")
ids <- paste0("'", paste(items, collapse="','"), "'")

# Direccion de la alteracion fisiologica para cada determinacion. En las
# variables cuyo compromiso se manifiesta por elevacion se retiene el maximo,
# y en las restantes el minimo. La eleccion sigue el criterio empleado en los
# indices de disfuncion organica.
alto <- c("leucocitos","rdw","creatinina","urea","brecha_anion","potasio",
          "inr","ttpa","pco2","lactato")
bajo <- c("hemoglobina","plaquetas","sodio","cloro","bicarbonato","ph",
          "exceso_base")

cols <- paste(sapply(names(items), function(v) {
  f <- if (v %in% alto) "MAX" else "MIN"
  sprintf("%s(CASE WHEN itemid = '%s' THEN valuenum END) AS %s",
          f, items[v], v)
}), collapse = ",\n    ")

dbExecute(con, sprintf("
  CREATE VIEW peor AS
  SELECT stay_id,
    %s
  FROM (
    SELECT e.stay_id, l.itemid, CAST(l.valuenum AS DOUBLE) AS valuenum
    FROM estancia e
    JOIN lab l ON l.hadm_id = e.hadm_id
    WHERE CAST(l.storetime AS TIMESTAMP) >= e.intime
      AND CAST(l.storetime AS TIMESTAMP) <= e.intime + INTERVAL 6 HOUR
      AND l.valuenum IS NOT NULL
      AND l.itemid IN (%s))
  GROUP BY stay_id", cols, ids))

pv <- dbGetQuery(con, "SELECT * FROM peor")
dbDisconnect(con, shutdown = TRUE)

part <- read.csv("outputs/fase5/matriz_particionada.csv",
                 stringsAsFactors = FALSE)
vars <- names(items)

m <- merge(part[, c("stay_id","clase","grupo","unidad", vars)],
           pv, by = "stay_id", suffixes = c("_primero","_peor"))

cat("=== DIFERENCIA ENTRE AMBAS REGLAS ===\n")
dif <- do.call(rbind, lapply(vars, function(v) {
  a <- m[[paste0(v, "_primero")]]; b <- m[[paste0(v, "_peor")]]
  ok <- !is.na(a) & !is.na(b)
  data.frame(variable = v,
             direccion = if (v %in% alto) "maximo" else "minimo",
             pct_identicos = round(100 * mean(a[ok] == b[ok]), 1),
             dif_mediana = round(median(b[ok] - a[ok]), 3),
             correlacion = round(cor(a[ok], b[ok]), 4),
             row.names = NULL)
}))
print(dif, row.names = FALSE)

# El sesgo que motiva la eleccion de la primera determinacion consiste en que
# el valor extremo depende del numero de determinaciones realizadas. Se
# examina si la magnitud de la diferencia entre ambas reglas guarda relacion
# con la unidad de cuidado, cuya intensidad de monitorizacion difiere.
cat("\n=== DIFERENCIA POR UNIDAD, LACTATO ===\n")
m$d_lac <- m$lactato_peor - m$lactato_primero
u <- aggregate(d_lac ~ unidad, data = m[!is.na(m$d_lac), ],
               FUN = function(x) round(c(media = mean(x),
                                         pct_cambia = 100*mean(x != 0)), 3))
print(u, row.names = FALSE)

dir.create("outputs/fase15", recursive = TRUE, showWarnings = FALSE)
write.csv(dif, "outputs/fase15/comparacion_agregacion.csv", row.names = FALSE)
write.csv(m[, c("stay_id","clase","grupo","unidad",
                paste0(vars, "_peor"))],
          "outputs/fase15/matriz_peor_valor.csv", row.names = FALSE)
cat("\nMatriz de peor valor escrita.\n")
