library(jsonlite)

# Cobertura de las constantes vitales, por etapa.
#
# La cifra publicada procedia de outputs/fase17/cobertura_vitales.csv, que
# R/46 escribe sobre la tabla recien extraida. R/47 anula despues los valores
# que caen fuera de los limites de plausibilidad, y es esa tabla limpia la que
# alimenta la imputacion y el modelo. La cobertura publicada describia por
# tanto una tabla que el ajuste no vio, y contaba como disponibles valores que
# el propio procedimiento descarta.
#
# La extraccion cruda no es un error y no se retira: dice cuanto midio el
# hospital. La tabla limpia dice cuanto pudo usar el modelo. Son dos
# cantidades distintas y el deposito las separa en lugar de elegir una.
#
# No se reejecuta nada. Las dos etapas se derivan de agregados ya versionados:
# el recuento disponible de la extraccion y el numero de valores que los
# limites anulan. La derivacion se contrasta contra un testigo independiente,
# la columna anterior de outputs/fase20/cobertura_extraccion.csv, que R/63
# calculo sobre la tabla limpia por un camino distinto. Si ambos no
# coincidieran, la resta no describiria la limpieza y el procedimiento se
# detiene.

OUT <- "outputs/fase28"

RUTA_FLU <- "outputs/fase2/flujo.csv"
RUTA_COB <- "outputs/fase17/cobertura_vitales.csv"
RUTA_IMP <- "outputs/fase17/implausibles.csv"
RUTA_EXT <- "outputs/fase20/cobertura_extraccion.csv"

detener <- function(...) {
  cat("\n", ..., "\n", sep = "")
  cat("El procedimiento se detiene. No procede publicar sobre esta base.\n")
  quit(status = 1)
}

for (r in c(RUTA_FLU, RUTA_COB, RUTA_IMP, RUTA_EXT))
  if (!file.exists(r)) detener("Fuente ausente: ", r)

flu <- read.csv(RUTA_FLU, stringsAsFactors = FALSE)
cob <- read.csv(RUTA_COB, stringsAsFactors = FALSE)
imp <- read.csv(RUTA_IMP, stringsAsFactors = FALSE)
ext <- read.csv(RUTA_EXT, stringsAsFactors = FALSE)

# El denominador es la cohorte final, que es sobre la que R/46 calcula la
# proporcion. Se lee del embudo y no se escribe aqui.
fi <- which(flu$paso == "p5_cohorte_final")
if (length(fi) != 1) detener("El embudo no declara la cohorte final.")
N <- flu$n[fi]

if (!setequal(cob$variable, imp$variable))
  detener("Las dos fuentes no cubren las mismas constantes.")
if (!setequal(cob$variable, ext$variable))
  detener("El testigo no cubre las mismas constantes.")

o <- match(cob$variable, imp$variable)
oe <- match(cob$variable, ext$variable)

etapa <- data.frame(
  variable = cob$variable,
  estancias = N,
  disponible_crudo = cob$disponible,
  pct_crudo = round(100 * cob$disponible / N, 1),
  marcados = imp$marcados[o],
  disponible_limpio = cob$disponible - imp$marcados[o],
  row.names = NULL)
etapa$pct_limpio <- round(100 * etapa$disponible_limpio / N, 1)

# Primera comprobacion. La proporcion cruda recompuesta ha de reproducir la
# que el archivo de origen publica; si no, el denominador no es el que aquel
# empleo y toda la derivacion carece de base.
if (max(abs(etapa$pct_crudo - cob$pct)) > 1e-9)
  detener("La proporcion cruda recompuesta no reproduce la publicada.")

# Segunda comprobacion, la que importa. La proporcion limpia ha de coincidir
# con la que R/63 calculo directamente sobre la tabla limpia. Son dos caminos
# independientes hacia la misma cantidad.
d <- max(abs(etapa$pct_limpio - ext$pct_anterior[oe]))
cat("=== CONTRASTE CON EL TESTIGO INDEPENDIENTE ===\n")
cat("Discrepancia maxima con la cobertura calculada sobre la tabla limpia:",
    signif(d, 3), "\n")
if (d > 1e-9)
  detener("La resta no reproduce la cobertura de la tabla limpia.")
cat("Los dos caminos coinciden en las", nrow(etapa), "constantes.\n")

if (any(etapa$disponible_limpio < 0) || any(etapa$marcados < 0))
  detener("Algun recuento resulta negativo.")
if (any(etapa$pct_limpio > etapa$pct_crudo + 1e-9))
  detener("La limpieza no puede aumentar la cobertura.")

cat("\n=== COBERTURA POR ETAPA ===\n")
print(etapa, row.names = FALSE)
cat("\nValores anulados por los limites de plausibilidad:",
    sum(etapa$marcados), "\n")

dir.create(OUT, recursive = TRUE, showWarnings = FALSE)
write.csv(etapa, file.path(OUT, "cobertura_vitales_por_etapa.csv"),
          row.names = FALSE)

writeLines(toJSON(list(
  fase = "28",
  ejecutado_en = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
  r_version = R.version.string,
  proposito = paste("separar la cobertura de la extraccion cruda de la que",
                    "presenta la tabla sobre la que se ajusta el modelo"),
  denominador = "cohorte final del embudo de seleccion",
  etapa_cruda = paste("recuento disponible tras la extraccion, antes de",
                      "anular los valores implausibles"),
  etapa_limpia = paste("el mismo recuento menos los valores que los limites",
                       "de plausibilidad anulan; es la tabla que alimenta la",
                       "imputacion y el modelo"),
  testigo = paste("la columna anterior de",
                  "outputs/fase20/cobertura_extraccion.csv, calculada por",
                  "R/63 sobre la tabla limpia por un camino distinto"),
  se_reejecuta = FALSE), auto_unbox = TRUE, pretty = TRUE, digits = 15),
  file.path(OUT, "manifiesto.json"))

cat("\nDepositado en", OUT, "\n")
