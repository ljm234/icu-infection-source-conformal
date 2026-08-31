library(DBI)
library(duckdb)
library(jsonlite)

# Recuento de casos completos entre las candidatas de laboratorio.
#
# El deposito de la fase trigesima afirmaba que una comparacion por casos
# completos sobre las setenta y tres no era viable, y sostenia esa afirmacion
# en una cota: la candidata menos frecuente aparece en tres mil ciento setenta
# y siete de las primeras estancias en cuidados intensivos. La cota es cierta
# y la conclusion no se seguia de ella. Tres mil ciento setenta y siete es el
# trece por ciento de la cohorte y del tamano del conjunto de prueba sobre el
# que se publica todo, de modo que una comparacion de ese tamano no seria
# despreciable. Faltaba contar.
#
# Aqui se cuenta. Un solo barrido establece cuantas estancias de la cohorte
# tienen las setenta y tres determinaciones dentro de la ventana, y cuantas
# tienen el subconjunto de veintinueve que interesa a la comparacion: las
# diecisiete retenidas mas las descartadas cuya cobertura supera a la de la
# retenida menos frecuente. Ese subconjunto es el que responde a la pregunta
# afilada, que no es si setenta y tres rinden mas que diecisiete, sino por que
# quedo fuera una determinacion mas frecuente que varias de las incluidas.
#
# La pertenencia a la cohorte se lee de la matriz de la fase cuarta en lugar
# de reconstruirse: es la definicion efectiva y evita repetir la cadena de
# sospecha de infeccion. La ventana y el filtro temporal son los de R/19: seis
# horas desde el ingreso a la primera estancia en cuidados intensivos, sobre
# la hora de registro y no la de anotacion.

BASE <- path.expand("~/mimic-data/physionet.org/files/mimiciv/3.1")
LAB  <- file.path(BASE, "hosp", "labevents.csv.gz")
ICU  <- file.path(BASE, "icu",  "icustays.csv.gz")

OUT <- "outputs/fase31"
RUTA_CAND <- "outputs/fase30/determinaciones_candidatas.csv"
RUTA_MAT  <- "outputs/fase4/matriz.csv"
RUTA_FLU  <- "outputs/fase2/flujo.csv"

detener <- function(...) {
  cat("\n", ..., "\n", sep = "")
  cat("El procedimiento se detiene. No procede concluir sobre esta base.\n")
  quit(status = 1)
}

for (r in c(LAB, ICU, RUTA_CAND, RUTA_MAT, RUTA_FLU))
  if (!file.exists(r)) detener("Fuente ausente: ", r)

cand <- read.csv(RUTA_CAND, stringsAsFactors = FALSE)
cand$itemid <- as.character(cand$itemid)
flu <- read.csv(RUTA_FLU, stringsAsFactors = FALSE)

fi <- which(flu$paso == "p5_cohorte_final")
if (length(fi) != 1) detener("El embudo no declara la cohorte final.")
N_COHORTE <- flu$n[fi]

# El subconjunto de veintinueve. La retenida de menor cobertura fija el
# liston, y se incorporan las descartadas que lo superan.
minima <- min(cand$cobertura_pct[cand$retenida])
sub <- cand$retenida | cand$cobertura_pct > minima
if (sum(cand$retenida) != 17) detener("Las retenidas no son diecisiete.")
cat("=== CONJUNTOS ===\n")
cat("Candidatas:", nrow(cand), "\n")
cat("Retenidas:", sum(cand$retenida),
    " cobertura de la menos frecuente:", minima, "por ciento\n")
cat("Descartadas por encima de ese valor:", sum(sub & !cand$retenida), "\n")
cat("Subconjunto ampliado:", sum(sub), "\n")

ids73 <- cand$itemid
ids29 <- cand$itemid[sub]

mat <- read.csv(RUTA_MAT, stringsAsFactors = FALSE)
if (is.null(mat$stay_id)) detener("La matriz no declara la estancia.")
estancias <- unique(mat$stay_id)
if (length(estancias) != N_COHORTE)
  detener("La matriz no cubre la cohorte final del embudo.")

con <- dbConnect(duckdb::duckdb())
dbExecute(con, "PRAGMA threads=8")
dbExecute(con, "PRAGMA memory_limit='40GB'")
for (v in list(c("lab", LAB), c("icustays", ICU)))
  dbExecute(con, sprintf(
    "CREATE VIEW %s AS SELECT * FROM read_csv('%s', all_varchar=true)",
    v[1], v[2]))

dbWriteTable(con, "cohorte", data.frame(stay_id = as.character(estancias)))

dbExecute(con, "
  CREATE VIEW estancia AS
  SELECT e.subject_id, e.hadm_id, e.stay_id, e.intime FROM (
    SELECT subject_id, hadm_id, stay_id,
           CAST(intime AS TIMESTAMP) AS intime,
           ROW_NUMBER() OVER (PARTITION BY subject_id
                              ORDER BY CAST(intime AS TIMESTAMP)) AS orden
    FROM icustays) e
  JOIN cohorte c ON c.stay_id = e.stay_id
  WHERE e.orden = 1")

n_est <- dbGetQuery(con, "SELECT COUNT(*) AS n FROM estancia")$n
if (n_est != N_COHORTE)
  detener("La union con la cohorte no recupera todas las estancias.")

lista <- function(x) paste0("'", paste(x, collapse = "','"), "'")

cat("\nRecorriendo las determinaciones de laboratorio. Toma varios minutos.\n")
inicio <- Sys.time()
res <- dbGetQuery(con, sprintf("
  SELECT e.stay_id,
         COUNT(DISTINCT CASE WHEN l.itemid IN (%s) THEN l.itemid END) AS n73,
         COUNT(DISTINCT CASE WHEN l.itemid IN (%s) THEN l.itemid END) AS n29
  FROM estancia e
  LEFT JOIN lab l
    ON l.hadm_id = e.hadm_id
   AND l.valuenum IS NOT NULL
   AND l.itemid IN (%s)
   AND CAST(l.storetime AS TIMESTAMP) >= e.intime
   AND CAST(l.storetime AS TIMESTAMP) <= e.intime + INTERVAL 6 HOUR
  GROUP BY e.stay_id", lista(ids73), lista(ids29), lista(ids73)))
dbDisconnect(con, shutdown = TRUE)
cat("Tiempo:", round(difftime(Sys.time(), inicio, units = "mins"), 1), "min\n")

if (nrow(res) != N_COHORTE)
  detener("El recuento no cubre la cohorte entera.")
if (max(res$n73) > length(ids73) || max(res$n29) > length(ids29))
  detener("Alguna estancia excede el numero de determinaciones posibles.")

completas73 <- sum(res$n73 == length(ids73))
completas29 <- sum(res$n29 == length(ids29))

cat("\n=== CASOS COMPLETOS ===\n")
cat("Con las", length(ids73), "candidatas:", completas73, "de", N_COHORTE,
    sprintf("(%.2f por ciento)\n", 100 * completas73 / N_COHORTE))
cat("Con las", length(ids29), "del subconjunto:", completas29, "de",
    N_COHORTE, sprintf("(%.2f por ciento)\n", 100 * completas29 / N_COHORTE))

cat("\n=== DISTRIBUCION DEL NUMERO DE DETERMINACIONES PRESENTES ===\n")
cortes <- c(0, 10, 20, 30, 40, 50, 60, 70, length(ids73))
dist <- do.call(rbind, lapply(seq_len(length(cortes) - 1), function(i) {
  lo <- cortes[i]; hi <- cortes[i + 1]
  data.frame(desde = lo + 1, hasta = hi,
             estancias = sum(res$n73 > lo & res$n73 <= hi), row.names = NULL)
}))
dist <- rbind(data.frame(desde = 0, hasta = 0, estancias = sum(res$n73 == 0)),
              dist)
print(dist, row.names = FALSE)
cat("Maximo observado de las", length(ids73), ":", max(res$n73), "\n")

recuento <- data.frame(
  conjunto = c("candidatas", "subconjunto ampliado"),
  determinaciones = c(length(ids73), length(ids29)),
  estancias_completas = c(completas73, completas29),
  denominador = N_COHORTE,
  pct_de_la_cohorte = round(100 * c(completas73, completas29) / N_COHORTE, 2),
  row.names = NULL)

cat("\n=== RECUENTO DEPOSITADO ===\n")
print(recuento, row.names = FALSE)

dir.create(OUT, recursive = TRUE, showWarnings = FALSE)
write.csv(recuento, file.path(OUT, "casos_completos.csv"), row.names = FALSE)
write.csv(dist, file.path(OUT, "distribucion_presentes.csv"),
          row.names = FALSE)

writeLines(toJSON(list(
  fase = "31",
  ejecutado_en = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
  r_version = R.version.string,
  duckdb_version = as.character(packageVersion("duckdb")),
  proposito = paste("contar las estancias de la cohorte que tienen todas las",
                    "candidatas dentro de la ventana, para establecer si una",
                    "comparacion por casos completos es posible"),
  ventana = "seis horas desde el ingreso a la primera estancia en la unidad",
  filtro_temporal = "hora de registro, no de anotacion",
  pertenencia_a_la_cohorte = RUTA_MAT,
  denominador = N_COHORTE,
  subconjunto_ampliado = paste("las retenidas mas las descartadas cuya",
                               "cobertura supera a la de la retenida menos",
                               "frecuente"),
  determinaciones_del_subconjunto = length(ids29)), auto_unbox = TRUE,
  pretty = TRUE, digits = 15), file.path(OUT, "manifiesto.json"))

cat("\nDepositado en", OUT, "\n")
