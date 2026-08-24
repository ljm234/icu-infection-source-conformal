library(DBI)
library(duckdb)

v <- read.csv("data/derivados/vitales.csv", stringsAsFactors = FALSE)

VITALES <- c("temperatura","frec_cardiaca","frec_respiratoria",
             "presion_sistolica","presion_media","saturacion")

# Limites de plausibilidad fisiologica declarados de forma explicita. Los
# valores situados fuera del intervalo se consideran errores de registro y se
# marcan como ausentes, sin sustitucion ni correccion, dado que su valor real
# se desconoce.
LIMITES <- list(
  temperatura = c(32, 42), frec_cardiaca = c(20, 250),
  frec_respiratoria = c(4, 60), presion_sistolica = c(40, 250),
  presion_media = c(25, 180), saturacion = c(50, 100))

# Una parte de las lecturas termometricas consta en grados Fahrenheit bajo el
# identificador correspondiente a la escala Celsius. Un valor superior a
# cincuenta resulta incompatible con la supervivencia expresado en Celsius y
# admite conversion, de modo que se recupera en lugar de descartarse.
fh <- !is.na(v$temperatura) & v$temperatura > 50
cat("Lecturas termometricas en escala Fahrenheit recuperadas:", sum(fh), "\n")
v$temperatura[fh] <- (v$temperatura[fh] - 32) * 5 / 9

cat("\n=== VALORES IMPLAUSIBLES MARCADOS COMO AUSENTES ===\n")
marcados <- do.call(rbind, lapply(VITALES, function(nom) {
  lim <- LIMITES[[nom]]
  x <- v[[nom]]
  fuera <- !is.na(x) & (x < lim[1] | x > lim[2])
  v[[nom]][fuera] <<- NA
  data.frame(variable = nom, limite_inferior = lim[1],
             limite_superior = lim[2], marcados = sum(fuera),
             row.names = NULL)
}))
print(marcados, row.names = FALSE)

cat("\n=== DISTRIBUCION TRAS LA LIMPIEZA ===\n")
print(do.call(rbind, lapply(VITALES, function(nom) {
  x <- v[[nom]][!is.na(v[[nom]])]
  q <- quantile(x, c(0.01, 0.25, 0.5, 0.75, 0.99))
  data.frame(variable = nom, n = length(x),
             p1 = round(q[1], 1), p25 = round(q[2], 1),
             mediana = round(q[3], 1), p75 = round(q[4], 1),
             p99 = round(q[5], 1), row.names = NULL)
})), row.names = FALSE)

# La presion arterial obtenida por cateter se incorpora unicamente para
# cuantificar que proporcion de la cohorte quedaria cubierta al combinar
# ambos metodos de obtencion. La decision sobre su empleo se adopta despues,
# a la vista de esa cifra.
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
                              ORDER BY CAST(intime AS TIMESTAMP)) AS orden
    FROM icustays) WHERE orden = 1")

cat("\nConsultando presion arterial invasiva.\n")

inv <- dbGetQuery(con, "
  SELECT stay_id,
    MAX(CASE WHEN codigo = '220050' THEN valor END) AS pas_invasiva,
    MAX(CASE WHEN codigo = '220052' THEN valor END) AS pam_invasiva
  FROM (
    SELECT stay_id, codigo, valor FROM (
      SELECT e.stay_id, c.itemid AS codigo,
             CAST(c.valuenum AS DOUBLE) AS valor,
             ROW_NUMBER() OVER (PARTITION BY e.stay_id, c.itemid
                                ORDER BY CAST(c.storetime AS TIMESTAMP)) AS orden
      FROM estancia e
      JOIN chart c ON c.stay_id = e.stay_id
      WHERE c.itemid IN ('220050','220052')
        AND c.valuenum IS NOT NULL
        AND CAST(c.storetime AS TIMESTAMP) >= e.intime
        AND CAST(c.storetime AS TIMESTAMP) <= e.intime + INTERVAL 6 HOUR)
    WHERE orden = 1)
  GROUP BY stay_id")

dbDisconnect(con, shutdown = TRUE)

v <- merge(v, inv, by = "stay_id", all.x = TRUE)
v$pas_invasiva[!is.na(v$pas_invasiva) &
  (v$pas_invasiva < 40 | v$pas_invasiva > 250)] <- NA
v$pam_invasiva[!is.na(v$pam_invasiva) &
  (v$pam_invasiva < 25 | v$pam_invasiva > 180)] <- NA

v$pas_compuesta <- ifelse(!is.na(v$presion_sistolica),
                          v$presion_sistolica, v$pas_invasiva)
v$pam_compuesta <- ifelse(!is.na(v$presion_media),
                          v$presion_media, v$pam_invasiva)
v$metodo_invasivo <- as.integer(is.na(v$presion_media) &
                                !is.na(v$pam_invasiva))

cat("\n=== COBERTURA DE LA PRESION ARTERIAL SEGUN ESTRATEGIA ===\n")
print(data.frame(
  estrategia = c("solo no invasiva","solo invasiva","combinada"),
  pct_sistolica = round(100 * c(mean(!is.na(v$presion_sistolica)),
                                mean(!is.na(v$pas_invasiva)),
                                mean(!is.na(v$pas_compuesta))), 1),
  pct_media = round(100 * c(mean(!is.na(v$presion_media)),
                            mean(!is.na(v$pam_invasiva)),
                            mean(!is.na(v$pam_compuesta))), 1),
  row.names = NULL), row.names = FALSE)

cat("\n=== COBERTURA COMBINADA Y USO DEL CATETER POR UNIDAD ===\n")
u <- do.call(rbind, lapply(sort(unique(v$unidad)), function(un) {
  s <- v[v$unidad == un, ]
  if (nrow(s) < 500) return(NULL)
  data.frame(unidad = substr(un, 1, 34), n = nrow(s),
             pct_combinada = round(100 * mean(!is.na(s$pam_compuesta)), 1),
             pct_por_cateter = round(100 * mean(s$metodo_invasivo), 1),
             row.names = NULL)
}))
print(u, row.names = FALSE)

cat("\n=== DIFERENCIA ENTRE METODOS EN QUIENES TIENEN AMBOS ===\n")
amb <- !is.na(v$presion_media) & !is.na(v$pam_invasiva)
cat("Estancias con ambas determinaciones:", sum(amb), "\n")
cat("Diferencia mediana invasiva menos no invasiva:",
    round(median(v$pam_invasiva[amb] - v$presion_media[amb]), 2), "mmHg\n")
cat("Correlacion entre metodos:",
    round(cor(v$pam_invasiva[amb], v$presion_media[amb]), 4), "\n")

cat("\n=== COBERTURA FINAL DE LOS SEIS SIGNOS ===\n")
FINALES <- c("temperatura","frec_cardiaca","frec_respiratoria",
             "pas_compuesta","pam_compuesta","saturacion")
print(data.frame(
  variable = FINALES,
  pct = round(100 * sapply(FINALES, function(x) mean(!is.na(v[[x]]))), 1),
  row.names = NULL), row.names = FALSE)
cat("\nEstancias con los seis:",
    sum(rowSums(!is.na(v[, FINALES])) == 6),
    sprintf("(%.1f por ciento)\n",
            100 * mean(rowSums(!is.na(v[, FINALES])) == 6)))

write.csv(v, "data/derivados/vitales_limpios.csv", row.names = FALSE)
write.csv(marcados, "outputs/fase17/implausibles.csv", row.names = FALSE)
write.csv(u, "outputs/fase17/cobertura_presion.csv", row.names = FALSE)
cat("\nGuardado en data/derivados/vitales_limpios.csv\n")
