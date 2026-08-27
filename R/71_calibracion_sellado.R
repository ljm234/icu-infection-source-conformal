library(jsonlite)

# Calibracion por categoria en la unidad reservada.
#
# La documentacion afirma que la sobrecobertura de la categoria mayoritaria
# procede de un desplazamiento de prevalencia, y concreta la afirmacion: la
# probabilidad media que el modelo predice para esa categoria cae por debajo
# de su frecuencia observada. R/36 calcula esa tabla y la imprime por
# pantalla, de modo que ningun archivo versionado la sostiene y un lector no
# puede comprobarla.
#
# Este procedimiento la deposita. No reevalua la unidad reservada ni vuelve a
# predecir sobre ella: lee la evaluacion que R/36 dejo escrita, que es el
# registro directo de aquella unica apertura. Reejecutar la prediccion no
# anadiria nada y multiplicaria sin necesidad los accesos al conjunto que el
# sellado protege.
#
# El deposito es agregado. Contiene una fila por categoria y ningun
# identificador.

OUT <- "outputs/fase23"

RUTA_ESP <- "outputs/fase7/especificacion.rds"
RUTA_EVA <- "outputs/fase11/sellado_evaluado.csv"
RUTA_COB <- "outputs/fase11/cobertura_sellado.csv"

detener <- function(...) {
  cat("\n", ..., "\n", sep = "")
  cat("El procedimiento se detiene. No procede interpretar el resultado.\n")
  quit(status = 1)
}

for (r in c(RUTA_ESP, RUTA_EVA, RUTA_COB))
  if (!file.exists(r)) detener("Fuente ausente: ", r)

esp <- readRDS(RUTA_ESP)
if (is.null(esp$clases)) detener("La especificacion no declara categorias.")
CLASES <- esp$clases

sd_ <- read.csv(RUTA_EVA, stringsAsFactors = FALSE)
cob <- read.csv(RUTA_COB, stringsAsFactors = FALSE)

faltan <- setdiff(c(CLASES, "clase"), names(sd_))
if (length(faltan) > 0)
  detener("La evaluacion carece de columnas: ", paste(faltan, collapse = ", "))
if (!all(sd_$clase %in% CLASES))
  detener("La evaluacion contiene categorias ajenas a la especificacion.")

# Las probabilidades proceden del promedio de las imputaciones sobre la
# escala de probabilidad, de modo que cada fila debe seguir sumando la
# unidad. Una fila que no lo haga indicaria que las columnas no son las que
# se suponen, y toda la tabla que sigue careceria de sentido.
suma <- rowSums(sd_[, CLASES])
if (max(abs(suma - 1)) > 1e-6)
  detener("Alguna fila no suma la unidad sobre las categorias modeladas.")
if (any(is.na(sd_[, CLASES])) || any(is.na(sd_$clase)))
  detener("La evaluacion contiene valores ausentes.")

# Contraste con el archivo ya publicado. Si los recuentos por categoria no
# coincidieran, las dos tablas se referirian a evaluaciones distintas y el
# deposito no seria comparable con la cobertura que el trabajo reporta.
n_eva <- as.integer(table(factor(sd_$clase, levels = CLASES)))
n_cob <- as.integer(cob$n[match(CLASES, cob$clase)])
if (any(is.na(n_cob)) || !identical(n_eva, n_cob))
  detener("Los recuentos por categoria no coinciden con la cobertura ",
          "publicada.")

cat("=== UNIDAD RESERVADA ===\n")
cat("Estancias evaluadas:", nrow(sd_), "\n")
cat("Categorias:", paste(CLASES, collapse = ", "), "\n")

calib <- do.call(rbind, lapply(CLASES, function(k) {
  prev <- mean(sd_$clase == k)
  pm   <- mean(sd_[[k]])
  data.frame(clase = k,
             n = sum(sd_$clase == k),
             frecuencia_observada = round(prev, 4),
             probabilidad_media = round(pm, 4),
             diferencia = round(pm - prev, 4),
             row.names = NULL)
}))

cat("\n=== CALIBRACION POR CATEGORIA ===\n")
print(calib, row.names = FALSE)

# La afirmacion que el deposito ha de sostener o desmentir se evalua sobre
# las cantidades sin redondear, no sobre las que la tabla imprime.
mayor <- CLASES[which.max(table(factor(sd_$clase, levels = CLASES)))]
prev_may <- mean(sd_$clase == mayor)
pm_may   <- mean(sd_[[mayor]])

cat("\n=== AFIRMACION CONTRASTADA ===\n")
cat("Categoria mayoritaria en la unidad reservada:", mayor, "\n")
cat("Frecuencia observada:", round(prev_may, 4), "\n")
cat("Probabilidad media predicha:", round(pm_may, 4), "\n")
if (pm_may < prev_may) {
  cat("La probabilidad media queda por debajo de la frecuencia observada.\n")
  cat("El archivo sostiene la afirmacion de la documentacion.\n")
} else {
  cat("La probabilidad media NO queda por debajo de la frecuencia ",
      "observada.\n", sep = "")
  cat("El archivo desmiente la afirmacion de la documentacion.\n")
}

dir.create(OUT, recursive = TRUE, showWarnings = FALSE)
write.csv(calib, file.path(OUT, "calibracion_sellado.csv"), row.names = FALSE)

writeLines(toJSON(list(
  fase = "23",
  ejecutado_en = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
  r_version = R.version.string,
  proposito = paste("deposito de la calibracion por categoria en la unidad",
                    "reservada"),
  fuente = RUTA_EVA,
  origen = paste("evaluacion unica escrita por R/36_sellado.R; no se repite",
                 "la prediccion"),
  definicion_frecuencia = paste("proporcion de estancias cuya categoria",
                                "observada es la de la fila"),
  definicion_probabilidad = paste("media de la probabilidad predicha para la",
                                  "categoria de la fila"),
  estancias = nrow(sd_),
  categorias = length(CLASES)), auto_unbox = TRUE, pretty = TRUE),
  file.path(OUT, "manifiesto.json"))

cat("\nDepositado en", OUT, "\n")
