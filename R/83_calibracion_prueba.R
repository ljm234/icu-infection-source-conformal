library(jsonlite)

# Las diferencias de esta fase son de milesimas, y la notacion por defecto las
# deposita en forma exponencial mientras la fase vigesimo tercera, cuyos
# valores son mayores, las deposita en forma decimal. Dos notaciones distintas
# para la misma columna impiden leer las dos tablas enfrentadas, que es lo que
# el esquema comun persigue.
options(scipen = 999)

# Calibracion por categoria en el conjunto de prueba.
#
# El trabajo declara evaluar cuatro dominios y el documento del protocolo lo
# enuncia expresamente: capacidad de ordenar casos, concordancia entre
# probabilidad predicha y frecuencia observada, cobertura de los conjuntos y
# proporcion de casos resueltos. Tres de los cuatro constan en archivos
# versionados sobre el conjunto de prueba. El segundo no: la unica calibracion
# depositada es la de la unidad reservada, en la fase vigesimo tercera. Sin
# este deposito la afirmacion de los cuatro dominios no se sostiene sobre el
# conjunto en el que el trabajo reporta discriminacion y cobertura.
#
# Este procedimiento lo deposita. No reajusta ni vuelve a predecir: lee las
# probabilidades por estancia que R/33_conformal.R dejo escritas al calibrar
# el modelo final, que son las mismas de las que salen la cobertura y la
# discriminacion publicadas. Reajustar produciria una tabla que describiria un
# modelo distinto del que el trabajo reporta.
#
# La forma es la de R/71_calibracion_sellado.R, columna por columna y sin
# variacion. Esa igualdad es el punto: las dos tablas se leen enfrentadas, y
# anadir aqui una cantidad que alli no consta impediria compararlas, que es
# la clase de asimetria que este desarrollo ya tuvo que corregir dos veces.
#
# El deposito es agregado. Contiene una fila por categoria y ningun
# identificador.

OUT <- "outputs/fase34"

RUTA_ESP <- "outputs/fase7/especificacion.rds"
RUTA_PRU <- "outputs/fase8/prueba_con_conjuntos.csv"
RUTA_COB <- "outputs/fase8/cobertura.csv"
RUTA_SEL <- "outputs/fase23/calibracion_sellado.csv"

detener <- function(...) {
  cat("\n", ..., "\n", sep = "")
  cat("El procedimiento se detiene. No procede interpretar el resultado.\n")
  quit(status = 1)
}

for (r in c(RUTA_ESP, RUTA_PRU, RUTA_COB, RUTA_SEL))
  if (!file.exists(r)) detener("Fuente ausente: ", r)

esp <- readRDS(RUTA_ESP)
if (is.null(esp$clases)) detener("La especificacion no declara categorias.")
CLASES <- esp$clases

pru <- read.csv(RUTA_PRU, stringsAsFactors = FALSE)
cob <- read.csv(RUTA_COB, stringsAsFactors = FALSE)

faltan <- setdiff(c(CLASES, "clase"), names(pru))
if (length(faltan) > 0)
  detener("La evaluacion carece de columnas: ", paste(faltan, collapse = ", "))
if (!all(pru$clase %in% CLASES))
  detener("La evaluacion contiene categorias ajenas a la especificacion.")

# Las probabilidades proceden del promedio de las imputaciones sobre la escala
# de probabilidad, de modo que cada fila debe seguir sumando la unidad. Una
# fila que no lo haga indicaria que las columnas no son las que se suponen, y
# toda la tabla que sigue careceria de sentido.
suma <- rowSums(pru[, CLASES])
if (max(abs(suma - 1)) > 1e-6)
  detener("Alguna fila no suma la unidad sobre las categorias modeladas.")
if (any(is.na(pru[, CLASES])) || any(is.na(pru$clase)))
  detener("La evaluacion contiene valores ausentes.")

# Contraste con el archivo ya publicado. Si los recuentos por categoria no
# coincidieran, las dos tablas se referirian a evaluaciones distintas y el
# deposito no seria comparable con la cobertura que el trabajo reporta.
n_eva <- as.integer(table(factor(pru$clase, levels = CLASES)))
n_cob <- as.integer(cob$n[match(CLASES, cob$clase)])
if (any(is.na(n_cob)) || !identical(n_eva, n_cob))
  detener("Los recuentos por categoria no coinciden con la cobertura ",
          "publicada.")

cat("=== CONJUNTO DE PRUEBA ===\n")
cat("Estancias evaluadas:", nrow(pru), "\n")
cat("Categorias:", paste(CLASES, collapse = ", "), "\n")

calib <- do.call(rbind, lapply(CLASES, function(k) {
  prev <- mean(pru$clase == k)
  pm   <- mean(pru[[k]])
  data.frame(clase = k,
             n = sum(pru$clase == k),
             frecuencia_observada = round(prev, 4),
             probabilidad_media = round(pm, 4),
             diferencia = round(pm - prev, 4),
             row.names = NULL)
}))

cat("\n=== CALIBRACION POR CATEGORIA ===\n")
print(calib, row.names = FALSE)

# La afirmacion que el deposito ha de sostener o desmentir se evalua sobre las
# cantidades sin redondear, no sobre las que la tabla imprime.
mayor <- CLASES[which.max(table(factor(pru$clase, levels = CLASES)))]
prev_may <- mean(pru$clase == mayor)
pm_may   <- mean(pru[[mayor]])

cat("\n=== AFIRMACION CONTRASTADA ===\n")
cat("Categoria mayoritaria en el conjunto de prueba:", mayor, "\n")
cat("Frecuencia observada:", round(prev_may, 4), "\n")
cat("Probabilidad media predicha:", round(pm_may, 4), "\n")

# Contraste con la unidad reservada. El trabajo atribuye la sobrecobertura de
# la categoria mayoritaria alli a un desplazamiento de prevalencia. Esa
# lectura exige que el desajuste sea propio de aquella sede y no del modelo,
# lo que solo puede verse enfrentando las dos tablas. Se informa la
# comparacion; no se concluye de ella, que para eso haria falta un intervalo
# que ninguna de las dos deposita.
sel <- read.csv(RUTA_SEL, stringsAsFactors = FALSE)
if (!all(CLASES %in% sel$clase))
  detener("La calibracion de la unidad reservada no cubre las categorias.")
d_pru <- calib$diferencia[calib$clase == mayor]
d_sel <- sel$diferencia[sel$clase == mayor]

cat("\n=== DESAJUSTE DE LA MAYORITARIA, LAS DOS EVALUACIONES ===\n")
cat("Conjunto de prueba:  ", sprintf("%+.4f", d_pru), "\n")
cat("Unidad reservada:    ", sprintf("%+.4f", d_sel), "\n")
cat("El desajuste de la unidad reservada es",
    sprintf("%.1f", abs(d_sel) / abs(d_pru)),
    "veces el del conjunto de prueba.\n")

dir.create(OUT, recursive = TRUE, showWarnings = FALSE)
write.csv(calib, file.path(OUT, "calibracion_prueba.csv"), row.names = FALSE)

writeLines(toJSON(list(
  fase = "34",
  ejecutado_en = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
  r_version = R.version.string,
  guarda_de_emparejamiento_parcial = isTRUE(getOption("warnPartialMatchDollar")),
  proposito = paste("deposito de la calibracion por categoria en el conjunto",
                    "de prueba, que es el dominio que faltaba de los cuatro",
                    "que el trabajo declara evaluar"),
  fuente = RUTA_PRU,
  origen = paste("probabilidades por estancia escritas por R/33_conformal.R",
                 "al calibrar el modelo final; no se reajusta ni se vuelve a",
                 "predecir"),
  definicion_frecuencia = paste("proporcion de estancias cuya categoria",
                                "observada es la de la fila"),
  definicion_probabilidad = paste("media de la probabilidad predicha para la",
                                  "categoria de la fila"),
  forma = paste("identica a la de outputs/fase23/calibracion_sellado.csv,",
                "para que las dos evaluaciones se lean enfrentadas"),
  no_deposita = paste("intervalo alguno sobre la diferencia; la comparacion",
                      "entre las dos evaluaciones se informa y no se concluye"),
  estancias = nrow(pru),
  categorias = length(CLASES)), auto_unbox = TRUE, pretty = TRUE),
  file.path(OUT, "manifiesto.json"))

cat("\nDepositado en", OUT, "\n")
