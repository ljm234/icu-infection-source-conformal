library(jsonlite)

options(scipen = 999)

# Positividad de los cultivos por tipo de muestra.
#
# El documento afirmaba que la positividad varia en un orden de magnitud entre
# tipos de muestra, y ningun archivo lo sostenia: el deposito de origen trae
# cincuenta y ocho tipos, muchos con un punado de cultivos, entre los que la
# proporcion va del cero al cien por cien. Sobre esa tabla entera la
# afirmacion no significa nada, ni a favor ni en contra.
#
# Lo que si significa algo es la variacion entre los tipos que sostienen el
# recuento. El corte se declara y no se elige a ojo: entran los que por si
# solos reunen al menos el uno por ciento de los cultivos. Un tipo con
# veinte cultivos da una proporcion que se mueve con dos casos.
#
# El deposito lleva la regla, el denominador, los tipos retenidos y la razon
# entre los extremos, de modo que quien lea la afirmacion pueda comprobarla
# sin rehacerla.

OUT <- "outputs/fase43"
FUENTE <- "outputs/tipos_muestra_ventana6h.csv"
PESO_MINIMO <- 1

detener <- function(...) {
  cat("\n", ..., "\n", sep = "")
  cat("El procedimiento se detiene. No procede publicar sobre esta base.\n")
  quit(status = 1)
}

if (!file.exists(FUENTE)) detener("Fuente ausente: ", FUENTE)

d <- read.csv(FUENTE, stringsAsFactors = FALSE)
for (col in c("tipo_muestra", "cultivos", "con_crecimiento"))
  if (!col %in% names(d)) detener("La fuente no trae la columna ", col)
if (any(d$con_crecimiento > d$cultivos))
  detener("Algun tipo declara mas crecimientos que cultivos.")

total <- sum(d$cultivos)
if (total <= 0) detener("La fuente no relaciona cultivo alguno.")
d$pct_de_los_cultivos <- round(100 * d$cultivos / total, 3)
d$positividad_pct <- round(100 * d$con_crecimiento / d$cultivos, 2)

k <- d[d$pct_de_los_cultivos >= PESO_MINIMO, ]
k <- k[order(-k$positividad_pct), ]
if (nrow(k) < 2)
  detener("Menos de dos tipos alcanzan el peso minimo. No hay variacion que ",
          "declarar.")

resumen <- data.frame(
  tipos_en_la_fuente = nrow(d),
  cultivos = total,
  peso_minimo_pct = PESO_MINIMO,
  tipos_retenidos = nrow(k),
  cultivos_retenidos_pct = round(100 * sum(k$cultivos) / total, 2),
  positividad_minima_pct = min(k$positividad_pct),
  positividad_maxima_pct = max(k$positividad_pct),
  razon_entre_extremos = round(max(k$positividad_pct) /
                               min(k$positividad_pct), 2),
  row.names = NULL)

cat("=== POSITIVIDAD POR TIPO DE MUESTRA ===\n")
cat("Tipos en la fuente:", nrow(d), " cultivos:", total, "\n")
cat("Retenidos por reunir al menos", PESO_MINIMO, "por ciento:", nrow(k),
    " que cubren", resumen$cultivos_retenidos_pct, "por ciento\n\n")
print(k[, c("tipo_muestra", "cultivos", "con_crecimiento",
            "positividad_pct", "pct_de_los_cultivos")], row.names = FALSE)
cat("\nRango:", resumen$positividad_minima_pct, "a",
    resumen$positividad_maxima_pct, " razon:",
    resumen$razon_entre_extremos, "\n")

dir.create(OUT, recursive = TRUE, showWarnings = FALSE)
write.csv(k[, c("tipo_muestra", "cultivos", "con_crecimiento",
                "positividad_pct", "pct_de_los_cultivos")],
          file.path(OUT, "positividad_retenidos.csv"), row.names = FALSE)
write.csv(resumen, file.path(OUT, "resumen_positividad.csv"), row.names = FALSE)

writeLines(toJSON(list(
  fase = "43",
  ejecutado_en = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
  r_version = R.version.string,
  guarda_de_emparejamiento_parcial = isTRUE(getOption("warnPartialMatchDollar")),
  proposito = paste("acotar la variacion de la positividad entre tipos de",
                    "muestra con un denominador declarado, para que la",
                    "afirmacion del documento descanse en un archivo"),
  fuente = FUENTE,
  regla = paste("entran los tipos que por si solos reunen al menos el uno por",
                "ciento de los cultivos; sobre la tabla entera la proporcion",
                "va del cero al cien por cien entre tipos de un punado de",
                "cultivos, y no significa nada"),
  denominador = paste("los cultivos de la ventana de seis horas, no la",
                      "cohorte"),
  cultivos = total,
  tipos_en_la_fuente = nrow(d),
  tipos_retenidos = nrow(k),
  razon_entre_extremos = resumen$razon_entre_extremos),
  auto_unbox = TRUE, pretty = TRUE), file.path(OUT, "manifiesto.json"))

cat("\nDepositado en", OUT, "\n")
