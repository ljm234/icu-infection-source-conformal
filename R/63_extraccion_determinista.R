library(DBI)
library(duckdb)

# La ordenacion por hora de registro no determina la seleccion cuando varias
# determinaciones comparten ese instante, circunstancia que afecta a entre el
# veinticinco y el cuarenta por ciento de las estancias por efecto de la
# validacion por lotes de los registros de enfermeria.
#
# El criterio de desempate se establece en cuatro niveles. La hora de registro
# encabeza la ordenacion por ser el criterio que impide anticipar informacion.
# La hora de anotacion actua en segundo lugar: entre determinaciones validadas
# en un mismo lote, la anotada antes fue obtenida antes. El tercer criterio es
# el valor ya convertido a la escala comun, no el valor tal como consta en la
# tabla: la temperatura reune dos identificadores en escalas distintas, de
# modo que ordenar por la cifra sin convertir situa juntas lecturas que
# expresan temperaturas muy diferentes y deja el desempate sin resolver. El
# identificador cierra la ordenacion.
#
# Con el valor convertido incorporado a la clave, dos filas que empaten en los
# tres primeros criterios presentan por definicion el mismo valor, de modo que
# el resultado queda determinado con independencia de cual se retenga.

BASE  <- path.expand("~/mimic-data/physionet.org/files/mimiciv/3.1")
CHART <- file.path(BASE, "icu", "chartevents.csv.gz")
ICU   <- file.path(BASE, "icu", "icustays.csv.gz")

con <- dbConnect(duckdb::duckdb())
dbExecute(con, "PRAGMA threads=8")
dbExecute(con, "PRAGMA memory_limit='48GB'")
for (x in list(c("chart", CHART), c("icustays", ICU)))
  dbExecute(con, sprintf(
    "CREATE VIEW %s AS SELECT * FROM read_csv('%s', all_varchar=true)",
    x[1], x[2]))

dbExecute(con, "
  CREATE VIEW estancia AS
  SELECT subject_id, stay_id, intime FROM (
    SELECT subject_id, stay_id,
           CAST(intime AS TIMESTAMP) AS intime,
           ROW_NUMBER() OVER (PARTITION BY subject_id
                              ORDER BY CAST(intime AS TIMESTAMP),
                                       stay_id) AS orden
    FROM icustays) WHERE orden = 1")

dbExecute(con, "
  CREATE VIEW crudo AS
  SELECT e.stay_id,
         CASE WHEN c.itemid = '223761' THEN '223762'
              ELSE c.itemid END AS codigo,
         CASE WHEN c.itemid = '223761'
              THEN (CAST(c.valuenum AS DOUBLE) - 32) * 5.0 / 9.0
              ELSE CAST(c.valuenum AS DOUBLE) END AS valor,
         CAST(c.storetime AS TIMESTAMP) AS registro,
         CAST(c.charttime AS TIMESTAMP) AS anotacion,
         c.itemid AS identificador
  FROM estancia e
  JOIN chart c ON c.stay_id = e.stay_id
  WHERE c.itemid IN ('223762','223761','220045','220210',
                     '220179','220181','220277')
    AND c.valuenum IS NOT NULL
    AND CAST(c.storetime AS TIMESTAMP) >= e.intime
    AND CAST(c.storetime AS TIMESTAMP) <= e.intime + INTERVAL 6 HOUR")

# Comprobacion estructural del determinismo. Se examina si existe algun grupo
# en que varias determinaciones compartan hora de registro y de anotacion
# presentando valores convertidos distintos. Un recuento nulo acredita que el
# resultado queda determinado por construccion y no unicamente por la
# coincidencia de dos ejecuciones.
cat("=== COMPROBACION ESTRUCTURAL ===\n")
amb <- dbGetQuery(con, "
  SELECT COUNT(*) AS grupos_ambiguos FROM (
    SELECT stay_id, codigo, registro, anotacion
    FROM crudo
    GROUP BY stay_id, codigo, registro, anotacion
    HAVING COUNT(DISTINCT valor) > 1)")
cat("Grupos con valores distintos bajo la misma clave temporal:",
    amb$grupos_ambiguos, "\n")
cat("Un recuento no nulo se resuelve mediante el valor convertido,",
    "incorporado a la clave de ordenacion.\n")

CONSULTA <- "
  SELECT stay_id,
    MAX(CASE WHEN codigo = '223762' THEN valor END) AS temperatura,
    MAX(CASE WHEN codigo = '220045' THEN valor END) AS frec_cardiaca,
    MAX(CASE WHEN codigo = '220210' THEN valor END) AS frec_respiratoria,
    MAX(CASE WHEN codigo = '220179' THEN valor END) AS presion_sistolica,
    MAX(CASE WHEN codigo = '220181' THEN valor END) AS presion_media,
    MAX(CASE WHEN codigo = '220277' THEN valor END) AS saturacion
  FROM (
    SELECT stay_id, codigo, valor FROM (
      SELECT stay_id, codigo, valor,
             ROW_NUMBER() OVER (
               PARTITION BY stay_id, codigo
               ORDER BY registro, anotacion, valor, identificador) AS orden
      FROM crudo)
    WHERE orden = 1)
  GROUP BY stay_id
  ORDER BY stay_id"

cat("\nEjecutando la consulta tres veces.\n")
a <- dbGetQuery(con, CONSULTA)
b <- dbGetQuery(con, CONSULTA)
d3 <- dbGetQuery(con, CONSULTA)
dbDisconnect(con, shutdown = TRUE)

cat("\n=== COMPROBACION POR EJECUCION ===\n")
cat("Estancias recuperadas:", nrow(a), nrow(b), nrow(d3), "\n")
ig12 <- identical(a, b); ig13 <- identical(a, d3)
cat("Primera frente a segunda:", if (ig12) "identicas" else "DIFIEREN", "\n")
cat("Primera frente a tercera:", if (ig13) "identicas" else "DIFIEREN", "\n")
if (!ig12) print(all.equal(a, b, tolerance = 0))
if (!ig13) print(all.equal(a, d3, tolerance = 0))

# El resultado de esta comparacion solo constaba por pantalla, de modo que la
# afirmacion que la documentacion hace sobre ella no era verificable por un
# lector. Se deposita. Se calcula aqui, antes de que la tabla se modifique con
# la recuperacion de la escala Fahrenheit y el marcado de implausibles, para
# que compare lo que la consulta devolvio y no una version ya tratada.
ejecuciones <- list(a, b, d3)
determinismo <- data.frame(
  ejecuciones            = length(ejecuciones),
  identicas_a_la_primera = sum(vapply(ejecuciones,
                                      function(x) identical(a, x), logical(1))),
  estancias              = nrow(a),
  columnas               = ncol(a),
  grupos_ambiguos        = amb$grupos_ambiguos,
  row.names = NULL)

VITALES <- c("temperatura","frec_cardiaca","frec_respiratoria",
             "presion_sistolica","presion_media","saturacion")

LIMITES <- list(
  temperatura = c(32, 42), frec_cardiaca = c(20, 250),
  frec_respiratoria = c(4, 60), presion_sistolica = c(40, 250),
  presion_media = c(25, 180), saturacion = c(50, 100))

fh <- !is.na(a$temperatura) & a$temperatura > 50
a$temperatura[fh] <- (a$temperatura[fh] - 32) * 5 / 9
cat("\nLecturas termometricas recuperadas de la escala Fahrenheit:",
    sum(fh), "\n")

for (nom in VITALES) {
  lim <- LIMITES[[nom]]
  fuera <- !is.na(a[[nom]]) & (a[[nom]] < lim[1] | a[[nom]] > lim[2])
  a[[nom]][fuera] <- NA
}

# La divergencia se cuantifica con dos umbrales. El primero, de una
# millonesima, detecta si se retuvo una fila distinta. El segundo, especifico
# de cada constante, detecta si el cambio alcanza magnitud clinicamente
# distinguible. La diferencia entre ambos recuentos corresponde por entero a
# la representacion binaria de los numeros decimales: la conversion desde
# grados Fahrenheit produce cifras como 36.699999999999996 alli donde el
# registro directo en escala Celsius consigna 36.7.
TOL <- list(temperatura = 0.05, frec_cardiaca = 0.5,
            frec_respiratoria = 0.5, presion_sistolica = 0.5,
            presion_media = 0.5, saturacion = 0.5)

viejo <- read.csv("data/derivados/vitales_limpios.csv", stringsAsFactors = FALSE)
cmp <- merge(a, viejo[, c("stay_id", VITALES)], by = "stay_id",
             suffixes = c("_nuevo", "_viejo"))
cat("Estancias comparables:", nrow(cmp), "\n")

cat("\n=== DIVERGENCIA RESPECTO DE LA EXTRACCION ANTERIOR ===\n")
div <- do.call(rbind, lapply(VITALES, function(v) {
  x <- cmp[[paste0(v, "_nuevo")]]; y <- cmp[[paste0(v, "_viejo")]]
  ok <- !is.na(x) & !is.na(y)
  dif <- abs(x - y)
  num <- ok & dif > 1e-6
  mat <- ok & dif > TOL[[v]]
  data.frame(variable = v, comparables = sum(ok),
             distintos_numericamente = sum(num),
             distintos_materialmente = sum(mat),
             pct_material = round(100 * sum(mat) / sum(ok), 3),
             dif_mediana = if (sum(mat) > 0)
               round(median(dif[mat]), 2) else 0,
             dif_p90 = if (sum(mat) > 0)
               round(as.numeric(quantile(dif[mat], 0.9)), 2) else 0,
             row.names = NULL)
}))
print(div, row.names = FALSE)

m <- sapply(VITALES, function(v) {
  x <- cmp[[paste0(v, "_nuevo")]]; y <- cmp[[paste0(v, "_viejo")]]
  ok <- !is.na(x) & !is.na(y)
  ok & abs(x - y) > TOL[[v]]
})
pct_est <- 100 * mean(rowSums(m) > 0)
cat("\nEstancias con al menos un cambio material:",
    sum(rowSums(m) > 0), sprintf("(%.2f por ciento)\n", pct_est))

cat("\n=== COBERTURA, ANTERIOR FRENTE A DETERMINISTA ===\n")
part <- read.csv("outputs/fase5/matriz_particionada.csv", stringsAsFactors = FALSE)
mn <- merge(part[, c("stay_id","clase","grupo","unidad")], a,
            by = "stay_id", all.x = TRUE)

# Se consignan los denominadores junto a los porcentajes. Las dos columnas no
# proceden de la misma tabla, y sin el recuento un lector no puede establecer
# si la comparacion se realiza sobre el mismo conjunto de estancias.
cobertura <- do.call(rbind, lapply(VITALES, function(v)
  data.frame(variable = v,
             n_anterior = nrow(viejo),
             pct_anterior = round(100 * mean(!is.na(viejo[[v]])), 1),
             n_determinista = nrow(mn),
             pct_determinista = round(100 * mean(!is.na(mn[[v]])), 1),
             row.names = NULL)))
cobertura$diferencia <- round(cobertura$pct_determinista -
                              cobertura$pct_anterior, 1)
print(cobertura, row.names = FALSE)

write.csv(mn, "data/derivados/vitales_deterministas.csv", row.names = FALSE)
dir.create("outputs/fase20", recursive = TRUE, showWarnings = FALSE)
write.csv(div, "outputs/fase20/divergencia_extraccion.csv", row.names = FALSE)
write.csv(determinismo, "outputs/fase20/determinismo_extraccion.csv",
          row.names = FALSE)
write.csv(cobertura, "outputs/fase20/cobertura_extraccion.csv",
          row.names = FALSE)

cat("\n=== VALORACION ===\n")
if (!ig12 || !ig13) {
  cat("La consulta no resulta determinista. Procede resolver esa cuestion\n")
  cat("antes de valorar la divergencia.\n")
} else if (pct_est < 1) {
  cat(sprintf("La divergencia material alcanza el %.2f por ciento de las\n",
              pct_est))
  cat("estancias. Procede declarar la correccion y conservar el bloque de\n")
  cat("extension, dado que el efecto sobre los resultados es despreciable.\n")
} else {
  cat(sprintf("La divergencia material alcanza el %.2f por ciento de las\n",
              pct_est))
  cat("estancias. Procede rehacer el bloque de extension sobre la\n")
  cat("extraccion determinista.\n")
}
