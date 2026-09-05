library(jsonlite)

options(scipen = 999)

# Caracteristicas basales por conjunto de la particion.
#
# Es el primer deposito del manuscrito y el que ninguna guia de reporte
# perdona. Hasta aqui el trabajo describia la cohorte entera y describia la
# unidad reservada por separado, pero no ponia lado a lado los conjuntos sobre
# los que el modelo se ajusta, se evalua y se transporta. Sin esa tabla nadie
# puede juzgar si los tres son comparables, que es la primera pregunta que un
# revisor hace y la unica que el resto del documento da por respondida.
#
# QUE SE DEPOSITA Y QUE NO. Solo agregados: recuentos, proporciones, medianas
# y cuartiles. Ninguna fila, ningun identificador, ninguna fecha. La matriz
# particionada se lee para contar y no se copia.
#
# LA REGLA DE LAS CINCO. Una celda de recuento con menos de cinco casos puede
# permitir reidentificar por cruce, de modo que ninguna se publica. Las
# categorias que caen por debajo se agrupan en una sola, y el deposito declara
# cuantas se agruparon y en que bloque. Agrupar y no suprimir es deliberado:
# suprimir una sola celda de una fila cuyo total se publica la deja
# reconstruible por resta.
#
# LOS BLOQUES. Los cinco conjuntos de la particion, uno por fila y sin
# agrupar: entrenamiento, donde el modelo se ajusta; calibracion, de donde
# salen los umbrales conformes; prueba, donde se evalua; la unidad reservada,
# donde se transporta; y las unidades descartadas, que no entran en ningun
# analisis y figuran porque sin ellas los totales no cuadran con el embudo y
# un revisor tendria que preguntar donde fueron las estancias que faltan.

OUT <- "outputs/fase45"

RUTA_ESP  <- "outputs/fase7/especificacion.rds"
RUTA_PART <- "outputs/fase5/matriz_particionada.csv"
RUTA_FLU  <- "outputs/fase2/flujo.csv"

MINIMO <- 5

# Los bloques son los cinco conjuntos de la particion, sin agrupar. La
# calibracion es un conjunto con funcion propia —de ella salen los umbrales
# conformes— y fundirla con el entrenamiento la esconde detras de una palabra.
#
# Y hay una razon mas fuerte: "desarrollo" ya nombra otra cantidad en este
# deposito. La fase que mide la completitud por conjunto llama desarrollo a
# entrenamiento mas calibracion mas prueba, que son 18.054 estancias; aqui
# nombraba a entrenamiento mas calibracion, que son 14.442. La misma palabra
# para dos cantidades distintas, en dos archivos que un mismo parrafo cita.
# Se retira de aqui y se conserva alli, que es la acepcion mayoritaria.
BLOQUE <- c(entrenamiento = "entrenamiento", calibracion = "calibracion",
            prueba = "prueba", sellado = "unidad reservada",
            excluido = "unidades descartadas")
ORDEN <- c("entrenamiento", "calibracion", "prueba", "unidad reservada",
           "unidades descartadas")

# Ninguna otra parte de este procedimiento puede reintroducir la palabra por
# la puerta de atras: se comprueba sobre las etiquetas y sobre la descripcion
# del manifiesto, que es donde ya ocurrio una vez.
TEXTO_BLOQUES <- paste("entrenamiento es donde el modelo se ajusta, calibracion",
                       "donde se fijan los umbrales, prueba donde se evalua una",
                       "sola vez, la unidad reservada donde se transporta; se",
                       "incluye el bloque descartado para que los totales",
                       "cuadren con el embudo")
if (any(grepl("desarrollo", c(ORDEN, TEXTO_BLOQUES), fixed = TRUE)))
  stop("Este deposito no nombra desarrollo a ningun bloque.")

detener <- function(...) {
  cat("\n", ..., "\n", sep = "")
  cat("El procedimiento se detiene. No procede publicar sobre esta base.\n")
  quit(status = 1)
}

for (r in c(RUTA_ESP, RUTA_PART, RUTA_FLU))
  if (!file.exists(r)) detener("Fuente ausente: ", r)

esp <- readRDS(RUTA_ESP)
vars <- c(esp$con_spline, esp$lineales)
if (length(vars) != 17)
  detener("La especificacion no declara diecisiete determinaciones.")

part <- read.csv(RUTA_PART, stringsAsFactors = FALSE)
falta <- setdiff(c("grupo", "edad", "sexo", "clase", vars), names(part))
if (length(falta) > 0)
  detener("La matriz particionada no trae: ", paste(falta, collapse = ", "))
if (!all(part$grupo %in% names(BLOQUE)))
  detener("La particion trae un grupo que esta tabla no sabe agrupar: ",
          paste(setdiff(unique(part$grupo), names(BLOQUE)), collapse = ", "))

part$bloque <- factor(BLOQUE[part$grupo], levels = ORDEN)
if (any(is.na(part$bloque))) detener("Alguna estancia queda sin bloque.")

flujo <- read.csv(RUTA_FLU, stringsAsFactors = FALSE)
if (nrow(part) != min(flujo$n))
  detener("La matriz particionada no cubre la cohorte final del embudo.")

cat("=== BLOQUES ===\n")
print(table(part$bloque), row.names = FALSE)

# ---------------------------------------------------------------------------
# Recuento, edad y sexo.
# ---------------------------------------------------------------------------

resumen <- do.call(rbind, lapply(ORDEN, function(b) {
  s <- part[part$bloque == b, ]
  q <- as.numeric(quantile(s$edad, c(0.25, 0.5, 0.75), na.rm = TRUE))
  data.frame(
    bloque = b,
    estancias = nrow(s),
    pct_de_la_cohorte = round(100 * nrow(s) / nrow(part), 2),
    edad_q1 = round(q[1], 1), edad_mediana = round(q[2], 1),
    edad_q3 = round(q[3], 1),
    mujeres = sum(s$sexo == "F"), hombres = sum(s$sexo == "M"),
    pct_mujeres = round(100 * mean(s$sexo == "F"), 2),
    row.names = NULL)
}))

cat("\n=== RECUENTO, EDAD Y SEXO ===\n")
print(resumen, row.names = FALSE)

# ---------------------------------------------------------------------------
# Categorias de desenlace, con la regla de las cinco.
# ---------------------------------------------------------------------------

clases <- sort(unique(part$clase))
agrupadas <- list()
cfilas <- list()
for (b in ORDEN) {
  s <- part[part$bloque == b, ]
  n <- setNames(sapply(clases, function(k) sum(s$clase == k)), clases)
  peq <- names(n)[n > 0 & n < MINIMO]
  if (length(peq) > 0) {
    agrupadas[[b]] <- peq
    n <- c(n[!(names(n) %in% peq)],
           setNames(sum(n[peq]), "categorias_agrupadas"))
  }
  for (k in names(n))
    cfilas[[length(cfilas)+1]] <- data.frame(
      bloque = b, categoria = k, estancias = as.integer(n[[k]]),
      pct_del_bloque = round(100 * n[[k]] / nrow(s), 2), row.names = NULL)
}
categorias <- do.call(rbind, cfilas)

cat("\n=== CATEGORIAS DE DESENLACE ===\n")
print(categorias, row.names = FALSE)

if (length(agrupadas) > 0) {
  cat("\nCategorias agrupadas por quedar bajo el minimo de", MINIMO, ":\n")
  for (b in names(agrupadas))
    cat("  ", b, ": ", paste(agrupadas[[b]], collapse = ", "), "\n", sep = "")
} else {
  cat("\nNinguna categoria quedo bajo el minimo de", MINIMO,
      "en ningun bloque.\n")
}

# Ninguna celda de recuento publicada puede quedar bajo el minimo, ni en el
# desglose de categorias ni en el de sexo.
malas <- categorias$estancias > 0 & categorias$estancias < MINIMO
if (any(malas))
  detener("Alguna celda de categoria queda bajo el minimo tras agrupar.")
sx <- c(resumen$mujeres, resumen$hombres)
if (any(sx > 0 & sx < MINIMO))
  detener("Alguna celda de sexo queda bajo el minimo.")

# ---------------------------------------------------------------------------
# Las diecisiete determinaciones: mediana y ausencia por bloque.
# ---------------------------------------------------------------------------

dfilas <- list()
for (v in sort(vars)) {
  for (b in ORDEN) {
    x <- part[[v]][part$bloque == b]
    dfilas[[length(dfilas)+1]] <- data.frame(
      determinacion = v, bloque = b,
      mediana = if (all(is.na(x))) NA_real_
                else round(median(x, na.rm = TRUE), 3),
      pct_ausente = round(100 * mean(is.na(x)), 2),
      observadas = sum(!is.na(x)), row.names = NULL)
  }
}
determinaciones <- do.call(rbind, dfilas)

cat("\n=== DETERMINACIONES: AUSENCIA POR BLOQUE ===\n")
anch <- reshape(determinaciones[, c("determinacion", "bloque", "pct_ausente")],
                idvar = "determinacion", timevar = "bloque",
                direction = "wide")
names(anch) <- sub("^pct_ausente[.]", "", names(anch))
print(anch, row.names = FALSE)

# Una mediana calculada sobre menos del minimo describiria un punado de
# estancias y no un bloque.
pocas <- determinaciones$observadas > 0 & determinaciones$observadas < MINIMO
if (any(pocas)) {
  determinaciones$mediana[pocas] <- NA_real_
  cat("\nMedianas retiradas por descansar en menos de", MINIMO,
      "observaciones:", sum(pocas), "\n")
}

# El deposito no puede llevar identificadores. Se comprueba sobre las columnas
# que se van a escribir y no sobre la intencion de no haberlos puesto.
IDENT <- c("stay_id", "subject_id", "hadm_id")
for (d in list(resumen, categorias, determinaciones))
  if (any(names(d) %in% IDENT))
    detener("Una tabla de este deposito lleva un identificador.")

if (sum(resumen$estancias) != nrow(part))
  detener("Los bloques no suman la cohorte.")

dir.create(OUT, recursive = TRUE, showWarnings = FALSE)
write.csv(resumen, file.path(OUT, "basales_por_conjunto.csv"),
          row.names = FALSE)
write.csv(categorias, file.path(OUT, "categorias_por_conjunto.csv"),
          row.names = FALSE)
write.csv(determinaciones, file.path(OUT, "determinaciones_por_conjunto.csv"),
          row.names = FALSE)

writeLines(toJSON(list(
  fase = "45",
  ejecutado_en = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
  r_version = R.version.string,
  guarda_de_emparejamiento_parcial = isTRUE(getOption("warnPartialMatchDollar")),
  proposito = paste("caracteristicas basales por conjunto de la particion,",
                    "que es el primer deposito que una guia de reporte exige",
                    "y que ningun documento de este trabajo tenia"),
  bloques = TEXTO_BLOQUES,
  contenido = paste("solo agregados: recuentos, proporciones, medianas y",
                    "cuartiles; ninguna fila, ningun identificador"),
  regla_de_las_cinco = paste("ninguna celda de recuento publicada baja de",
                             MINIMO, "casos; las categorias que caen se",
                             "agrupan en una sola, y agrupar y no suprimir es",
                             "deliberado, porque suprimir una celda de una",
                             "fila cuyo total se publica la deja",
                             "reconstruible por resta"),
  minimo = MINIMO,
  categorias_agrupadas = length(agrupadas),
  medianas_retiradas = sum(pocas),
  determinaciones = length(vars),
  estancias = nrow(part)), auto_unbox = TRUE, pretty = TRUE),
  file.path(OUT, "manifiesto.json"))

cat("\nDepositado en", OUT, "\n")
