library(jsonlite)

options(scipen = 999)

# Completitud analitica por conjunto de la particion, sobre las diecisiete
# determinaciones del modelo publicado.
#
# La fase trigesimo segunda deposita la completitud sobre las veintinueve de
# la comparacion de especificaciones, y alli la unidad reservada resulta muy
# por debajo de los conjuntos de desarrollo. Esa cifra pertenece a otra
# pregunta: la comparacion emplea veintinueve determinaciones y el modelo
# publicado emplea diecisiete. Lo que decide si el modelo publicado se
# aplica con mas imputacion en la unidad reservada que en el conjunto donde
# se ajusto son las diecisiete, y son las que aqui se miden.
#
# Se miden dos cosas distintas y se depositan por separado. La primera es
# cuantas estancias tienen las diecisiete a la vez, que es la cantidad
# analoga a la de la fase anterior. La segunda es que fraccion de las celdas
# esta observada, que es la que gobierna cuanta imputacion recibe cada
# conjunto: una estancia a la que le falta una determinacion y otra a la que
# le faltan doce cuentan igual en la primera y muy distinto en la segunda.
#
# No se reajusta nada ni se vuelve a imputar. Se lee la matriz particionada,
# que es la que la imputacion recibio.
#
# ESTE PROCEDIMIENTO NO INTERPRETA. Deposita recuentos y proporciones. Que
# mecanismo explique la sobrecobertura de la categoria mayoritaria en la
# unidad reservada no se decide aqui, y este deposito no lo decide: aporta
# una asimetria que hace falta declarar, no una conclusion sobre su causa.

OUT <- "outputs/fase36"

RUTA_ESP  <- "outputs/fase7/especificacion.rds"
RUTA_PART <- "outputs/fase5/matriz_particionada.csv"
RUTA_FLU  <- "outputs/fase2/flujo.csv"

# Los conjuntos de la particion se agrupan en tres bloques. El texto compara
# la unidad reservada contra el desarrollo, y esa comparacion ha de leerse de
# una fila del deposito y no componerse sumando columnas al redactar.
BLOQUE <- c(entrenamiento = "desarrollo", calibracion = "desarrollo",
            prueba = "desarrollo", sellado = "unidad reservada",
            excluido = "unidades descartadas")

detener <- function(...) {
  cat("\n", ..., "\n", sep = "")
  cat("El procedimiento se detiene. No procede interpretar el resultado.\n")
  quit(status = 1)
}

for (r in c(RUTA_ESP, RUTA_PART, RUTA_FLU))
  if (!file.exists(r)) detener("Fuente ausente: ", r)

esp <- readRDS(RUTA_ESP)
vars <- c(esp$con_spline, esp$lineales)
if (length(vars) != 17)
  detener("La especificacion no declara diecisiete determinaciones.")

part <- read.csv(RUTA_PART, stringsAsFactors = FALSE)
falta <- setdiff(c("grupo", vars), names(part))
if (length(falta) > 0)
  detener("La particion carece de: ", paste(falta, collapse = ", "))

flu <- read.csv(RUTA_FLU, stringsAsFactors = FALSE)
N_COHORTE <- flu$n[flu$paso == "p5_cohorte_final"]
if (nrow(part) != N_COHORTE)
  detener("La matriz particionada no cubre la cohorte final del embudo.")
if (!all(part$grupo %in% names(BLOQUE)))
  detener("La particion declara un conjunto que este procedimiento no ",
          "clasifica: ", paste(setdiff(part$grupo, names(BLOQUE)),
                               collapse = ", "))

M <- as.matrix(!is.na(part[, vars]))
presentes <- rowSums(M)

resumir <- function(s, etiqueta, columna) {
  n <- sum(s)
  data.frame(
    setNames(list(etiqueta), columna),
    determinaciones = length(vars),
    estancias = n,
    completas = sum(s & presentes == length(vars)),
    pct_completas = round(100 * sum(s & presentes == length(vars)) / n, 2),
    determinaciones_medias = round(mean(presentes[s]), 2),
    valores_posibles = n * length(vars),
    valores_presentes = sum(M[s, ]),
    pct_valores_presentes = round(100 * sum(M[s, ]) /
                                  (n * length(vars)), 2),
    row.names = NULL)
}

por_grupo <- do.call(rbind, lapply(sort(unique(part$grupo)), function(g)
  resumir(part$grupo == g, g, "grupo")))
por_bloque <- do.call(rbind, lapply(unique(BLOQUE), function(b)
  resumir(BLOQUE[part$grupo] == b, b, "conjunto")))

# Que determinacion falta donde. Sin este desglose, la asimetria entre
# conjuntos queda como un agregado que nadie puede situar.
por_variable <- do.call(rbind, lapply(unique(BLOQUE), function(b) {
  s <- BLOQUE[part$grupo] == b
  do.call(rbind, lapply(vars, function(v)
    data.frame(conjunto = b, determinacion = v, estancias = sum(s),
               presentes = sum(!is.na(part[[v]][s])),
               pct_presentes = round(100 * sum(!is.na(part[[v]][s])) /
                                     sum(s), 2), row.names = NULL)))
}))

if (sum(por_grupo$estancias) != N_COHORTE)
  detener("Los conjuntos no suman la cohorte.")
if (sum(por_bloque$estancias) != N_COHORTE)
  detener("Los bloques no suman la cohorte.")
if (sum(por_bloque$valores_presentes) != sum(M))
  detener("El recuento de valores presentes no cuadra con la matriz.")

# El total de casos completos, depositado y no sumado al redactar.
#
# Los sumandos ya estaban aqui y el total no, de modo que quien escribiera la
# cifra tenia que sumar filas a mano. Esa es la clase de cifra contra la que
# existe R/59. Y el texto la usa restandola del recuento anterior a los
# limites de plausibilidad, que vive en el manifiesto de la cuarta fase: la
# resta cruzaba dos fases y no la comprobaba nadie.
#
# Se exige ademas que los dos repartos coincidan. Son particiones distintas de
# la misma cohorte, de modo que un total que dependiera de cual se sumase
# indicaria que uno de los dos no la cubre entera.
if (sum(por_bloque$completas) != sum(por_grupo$completas))
  detener("Los dos repartos no dan el mismo total de casos completos.")
total_completas <- sum(por_bloque$completas)
por_total <- data.frame(
  determinaciones = length(vars),
  estancias       = N_COHORTE,
  completas       = total_completas,
  pct_completas   = round(100 * total_completas / N_COHORTE, 2),
  row.names = NULL)

cat("=== COMPLETITUD SOBRE LAS", length(vars), "DEL MODELO PUBLICADO ===\n")
cat("Estancias:", nrow(part), "\n\n")
cat("Por conjunto de la particion:\n")
print(por_grupo[, c("grupo", "estancias", "completas", "pct_completas",
                    "determinaciones_medias", "pct_valores_presentes")],
      row.names = FALSE)
cat("\nPor bloque:\n")
print(por_bloque[, c("conjunto", "estancias", "completas", "pct_completas",
                     "determinaciones_medias", "pct_valores_presentes")],
      row.names = FALSE)

cat("\nEstancias completas en las", length(vars), ":",
    total_completas, "de", N_COHORTE, "\n")

des <- por_bloque[por_bloque$conjunto == "desarrollo", ]
sel <- por_bloque[por_bloque$conjunto == "unidad reservada", ]
cat("\n=== ASIMETRIA ENTRE DESARROLLO Y UNIDAD RESERVADA ===\n")
cat("Estancias completas en las", length(vars), ":",
    sprintf("%.2f frente a %.2f por ciento\n",
            des$pct_completas, sel$pct_completas))
cat("Valores observados:",
    sprintf("%.2f frente a %.2f por ciento\n",
            des$pct_valores_presentes, sel$pct_valores_presentes))
cat("Determinaciones por estancia:",
    sprintf("%.2f frente a %.2f\n",
            des$determinaciones_medias, sel$determinaciones_medias))
cat("\nNo se deposita lectura alguna de estas cifras.\n")

dir.create(OUT, recursive = TRUE, showWarnings = FALSE)
write.csv(por_grupo, file.path(OUT, "completitud_por_grupo.csv"),
          row.names = FALSE)
write.csv(por_bloque, file.path(OUT, "completitud_por_conjunto.csv"),
          row.names = FALSE)
write.csv(por_variable, file.path(OUT, "completitud_por_determinacion.csv"),
          row.names = FALSE)
write.csv(por_total, file.path(OUT, "completitud_total.csv"),
          row.names = FALSE)

writeLines(toJSON(list(
  fase = "36",
  ejecutado_en = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
  r_version = R.version.string,
  guarda_de_emparejamiento_parcial = isTRUE(getOption("warnPartialMatchDollar")),
  proposito = paste("medir la completitud analitica de cada conjunto de la",
                    "particion sobre las determinaciones del modelo",
                    "publicado, que son las que gobiernan cuanta imputacion",
                    "recibe cada uno"),
  fuente = RUTA_PART,
  determinaciones = length(vars),
  por_que_diecisiete = paste("la comparacion de especificaciones mide sobre",
                             "veintinueve, que no son las del modelo",
                             "publicado; la imputacion que el modelo recibe",
                             "depende de las diecisiete que emplea"),
  completas = "estancias con las diecisiete determinaciones a la vez",
  valores_presentes = paste("fraccion de celdas observadas sobre el producto",
                            "de estancias por determinaciones; es la que",
                            "gobierna cuanta imputacion recibe el conjunto,",
                            "porque distingue a quien le falta una de a quien",
                            "le faltan doce"),
  bloques = "entrenamiento, calibracion y prueba forman el desarrollo",
  lectura = paste("no se deposita ninguna. Que mecanismo explique la",
                  "sobrecobertura de la categoria mayoritaria en la unidad",
                  "reservada no se decide aqui"),
  estancias = nrow(part)), auto_unbox = TRUE, pretty = TRUE),
  file.path(OUT, "manifiesto.json"))

cat("\nDepositado en", OUT, "\n")
