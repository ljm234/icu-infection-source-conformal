library(DBI)
library(duckdb)

v <- read.csv("data/derivados/vitales_limpios.csv", stringsAsFactors = FALSE)

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

# La concordancia entre ambos metodos de determinacion se evalua conservando
# la hora de registro de cada primera medicion. Sin ese dato no cabe
# distinguir entre una discrepancia atribuible al procedimiento y una
# variacion real de la presion arterial ocurrida en el intervalo que separa
# ambas lecturas.
cat("Consultando presiones con su hora de registro.\n")

d <- dbGetQuery(con, "
  SELECT stay_id,
    MAX(CASE WHEN codigo = '220181' THEN valor END)    AS pam_ni,
    MAX(CASE WHEN codigo = '220181' THEN registro END) AS t_ni,
    MAX(CASE WHEN codigo = '220052' THEN valor END)    AS pam_inv,
    MAX(CASE WHEN codigo = '220052' THEN registro END) AS t_inv
  FROM (
    SELECT stay_id, codigo, valor, registro FROM (
      SELECT e.stay_id, c.itemid AS codigo,
             CAST(c.valuenum AS DOUBLE) AS valor,
             CAST(c.storetime AS TIMESTAMP) AS registro,
             ROW_NUMBER() OVER (PARTITION BY e.stay_id, c.itemid
                                ORDER BY CAST(c.storetime AS TIMESTAMP)) AS orden
      FROM estancia e
      JOIN chart c ON c.stay_id = e.stay_id
      WHERE c.itemid IN ('220181','220052')
        AND c.valuenum IS NOT NULL
        AND CAST(c.storetime AS TIMESTAMP) >= e.intime
        AND CAST(c.storetime AS TIMESTAMP) <= e.intime + INTERVAL 6 HOUR)
    WHERE orden = 1)
  GROUP BY stay_id")

dbDisconnect(con, shutdown = TRUE)

d <- d[d$stay_id %in% v$stay_id, ]
d <- d[!is.na(d$pam_ni) & !is.na(d$pam_inv), ]
d <- d[d$pam_ni  >= 25 & d$pam_ni  <= 180 &
       d$pam_inv >= 25 & d$pam_inv <= 180, ]

d$desfase_min <- as.numeric(difftime(d$t_inv, d$t_ni, units = "mins"))

cat("\nEstancias de la cohorte con ambas determinaciones:", nrow(d), "\n")

cat("\n=== DESFASE TEMPORAL ENTRE AMBAS PRIMERAS MEDICIONES ===\n")
q <- quantile(abs(d$desfase_min), c(0.10, 0.25, 0.50, 0.75, 0.90))
print(data.frame(percentil = c("10","25","50","75","90"),
                 minutos = round(as.numeric(q), 1), row.names = NULL),
      row.names = FALSE)

# Si la discordancia obedece al intervalo transcurrido entre lecturas, la
# concordancia debe aumentar de forma apreciable al restringir el analisis a
# mediciones proximas en el tiempo. De mantenerse baja incluso entre lecturas
# separadas por pocos minutos, la discrepancia seria atribuible al metodo.
cat("\n=== CONCORDANCIA SEGUN PROXIMIDAD TEMPORAL ===\n")
tramos <- list("menos de 15 min" = c(0, 15), "de 15 a 60 min" = c(15, 60),
               "de 1 a 3 horas"  = c(60, 180), "mas de 3 horas" = c(180, Inf))
print(do.call(rbind, lapply(names(tramos), function(nom) {
  tr <- tramos[[nom]]
  s <- d[abs(d$desfase_min) >= tr[1] & abs(d$desfase_min) < tr[2], ]
  if (nrow(s) < 30) return(data.frame(tramo = nom, n = nrow(s),
    correlacion = NA, dif_mediana = NA, dif_absoluta_mediana = NA,
    row.names = NULL))
  data.frame(tramo = nom, n = nrow(s),
             correlacion = round(cor(s$pam_inv, s$pam_ni), 4),
             dif_mediana = round(median(s$pam_inv - s$pam_ni), 2),
             dif_absoluta_mediana = round(median(abs(s$pam_inv - s$pam_ni)), 2),
             row.names = NULL)
})), row.names = FALSE)

# La correlacion desciende de manera artificial cuando la variable presenta
# poca dispersion en el subconjunto analizado. Se compara la desviacion
# tipica del subgrupo con la de la cohorte completa para descartar que la
# concordancia observada responda a una restriccion del rango.
cat("\n=== DISPERSION DEL SUBGRUPO FRENTE A LA COHORTE ===\n")
print(data.frame(
  serie = c("no invasiva en el subgrupo", "no invasiva en la cohorte",
            "invasiva en el subgrupo"),
  desviacion = round(c(sd(d$pam_ni), sd(v$presion_media, na.rm = TRUE),
                       sd(d$pam_inv)), 2),
  row.names = NULL), row.names = FALSE)
cat("Cociente de dispersion, subgrupo sobre cohorte:",
    round(sd(d$pam_ni) / sd(v$presion_media, na.rm = TRUE), 3), "\n")

# El nombre de la unidad se recorta para que la tabla quepa en la consola, y
# solo ahi. Recortarlo en el dato dejaba el deposito con un identificador
# mutilado y obligaba a emparejar por prefijo aguas abajo.
corto <- function(x, n) substr(x, 1, n)

cat("\n=== CONCORDANCIA POR UNIDAD ===\n")
uni <- v[, c("stay_id","unidad")]
d2 <- merge(d, uni, by = "stay_id")
print(transform(do.call(rbind, lapply(sort(unique(d2$unidad)), function(un) {
  s <- d2[d2$unidad == un, ]
  if (nrow(s) < 50) return(NULL)
  data.frame(unidad = un, n = nrow(s),
             correlacion = round(cor(s$pam_inv, s$pam_ni), 4),
             desfase_mediano_min = round(median(abs(s$desfase_min)), 1),
             row.names = NULL)
})), unidad = corto(unidad, 34)), row.names = FALSE)

write.csv(d, "data/derivados/concordancia_presion.csv", row.names = FALSE)
cat("\nGuardado en data/derivados/concordancia_presion.csv\n")
