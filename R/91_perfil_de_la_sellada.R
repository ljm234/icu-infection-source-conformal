library(jsonlite)

options(scipen = 999)

# Perfil de la unidad reservada frente a las demas.
#
# El documento dice que la eleccion fue un juicio y no la aplicacion de una
# regla, y eso se mantiene. Pero omite un hecho comprobable que juega a su
# favor: la unidad que se aparto es la mas disimil de las seis, por el mismo
# estadistico que informo el juicio y por el reparto de categorias. Quien lo
# descubra por su cuenta se preguntara por que no lo decia el documento.
#
# Lo que ese hecho establece, y lo que no. Establece que la validacion externa
# no salio de un sorteo favorable: se hizo sobre la unidad mas alejada del
# resto, de modo que es el caso peor entre los disponibles. No establece que la
# eleccion se hiciera por esa razon, y este procedimiento no lo insinua: la
# distancia se calculo antes, el juicio la tuvo delante, y ninguna regla la
# convierte en criterio.
#
# Tambien acota la validacion por sedes: las cinco que entran en ella caen
# dentro de un rango de disimilitud mas estrecho que el que la reservada
# representa, de modo que la heterogeneidad que aquella explora es menor que
# la que una sede nueva podria presentar.
#
# CUAL ES LA RESERVADA NO SE ESCRIBE AQUI. Es la que figura en el perfil de
# unidades y no en la validacion por sedes, que es precisamente lo que
# significa haberla reservado.

OUT <- "outputs/fase42"
DIST <- "outputs/fase5/distancia_unidades.csv"
CLAS <- "outputs/fase5/clases_por_unidad.csv"
LOUO <- "outputs/fase10/cobertura_louo.csv"

detener <- function(...) {
  cat("\n", ..., "\n", sep = "")
  cat("El procedimiento se detiene. No procede publicar sobre esta base.\n")
  quit(status = 1)
}

for (f in c(DIST, CLAS, LOUO))
  if (!file.exists(f)) detener("Fuente ausente: ", f)

d <- read.csv(DIST, stringsAsFactors = FALSE)
k <- read.csv(CLAS, stringsAsFactors = FALSE, check.names = FALSE)
l <- read.csv(LOUO, stringsAsFactors = FALSE)

names(k)[1] <- "unidad"
if (!setequal(d$unidad, k$unidad))
  detener("El perfil de distancias y el de categorias no cubren las mismas ",
          "unidades.")

sellada <- setdiff(d$unidad, l$unidad)
if (length(sellada) != 1)
  detener("Las unidades que no entran en la validacion por sedes son ",
          length(sellada), ". La reservada no queda determinada.")

cat("=== LA UNIDAD RESERVADA, DERIVADA ===\n")
cat("Esta en el perfil y no en la validacion por sedes:\n  ", sellada, "\n")

otras <- d$unidad != sellada
d_sel <- d$distancia[d$unidad == sellada]
d_max <- max(d$distancia[otras])
d_min <- min(d$distancia[otras])
rango <- data.frame(
  unidades = nrow(d),
  distancia_de_la_reservada = d_sel,
  mayor_de_las_demas = d_max,
  menor_de_las_demas = d_min,
  veces_la_siguiente = round(d_sel / d_max, 2),
  es_la_mas_distante = d_sel > d_max,
  fuera_del_rango_de_las_demas = d_sel > d_max,
  row.names = NULL)

cat("\n=== DISTANCIA AL RESTO DE LA COHORTE ===\n")
print(d[order(-d$distancia), ], row.names = FALSE)

# El reparto de categorias. Se cuenta en cuantas columnas la reservada toma el
# valor extremo, por arriba o por abajo, sin decidir de antemano cual.
clases <- setdiff(names(k), "unidad")
if (length(clases) == 0) detener("El perfil de categorias no trae columnas.")
fila <- k[k$unidad == sellada, ]
ext <- do.call(rbind, lapply(clases, function(cl) {
  v <- k[[cl]]; s <- fila[[cl]]; o <- v[k$unidad != sellada]
  data.frame(categoria = cl, reservada = s,
             menor_de_las_demas = min(o), mayor_de_las_demas = max(o),
             extremo = if (s > max(o)) "por arriba"
                       else if (s < min(o)) "por abajo" else "no",
             row.names = NULL)
}))
ext$es_extremo <- ext$extremo != "no"

cat("\n=== REPARTO DE CATEGORIAS ===\n")
print(ext, row.names = FALSE)
cat("\nCategorias en que la reservada toma el valor extremo:",
    sum(ext$es_extremo), "de", nrow(ext), "\n")

resumen <- data.frame(
  unidad_reservada = sellada,
  categorias = nrow(ext),
  categorias_en_que_es_extremo = sum(ext$es_extremo),
  extrema_en_todas = all(ext$es_extremo),
  sedes_en_la_validacion = length(unique(l$unidad)),
  row.names = NULL)

cat("\n=== LO QUE ESTO ESTABLECE ===\n")
if (rango$es_la_mas_distante && resumen$extrema_en_todas) {
  cat("La unidad apartada es la mas alejada del resto y toma el valor\n")
  cat("extremo en todas las categorias. La validacion externa se hizo\n")
  cat("sobre el caso peor entre los disponibles.\n")
} else {
  cat("La unidad apartada NO es la mas disimil en todo. El documento no\n")
  cat("puede afirmarlo, y el generador se detendra si lo intenta.\n")
}
cat("Lo que no establece: que se eligiera por esa razon.\n")

dir.create(OUT, recursive = TRUE, showWarnings = FALSE)
write.csv(rango, file.path(OUT, "distancia_de_la_reservada.csv"),
          row.names = FALSE)
write.csv(ext, file.path(OUT, "extremos_por_categoria.csv"), row.names = FALSE)
write.csv(resumen, file.path(OUT, "resumen.csv"), row.names = FALSE)

writeLines(toJSON(list(
  fase = "42",
  ejecutado_en = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
  r_version = R.version.string,
  guarda_de_emparejamiento_parcial = isTRUE(getOption("warnPartialMatchDollar")),
  proposito = paste("situar a la unidad reservada entre las demas por el",
                    "estadistico que informo el juicio y por el reparto de",
                    "categorias, para que el documento pueda decir un hecho",
                    "favorable que hoy calla"),
  identificacion = paste("la reservada se deriva de estar en el perfil de",
                         "unidades y no en la validacion por sedes; no se",
                         "escribe aqui"),
  estadistico = paste("diferencia estandarizada media entre la unidad y el",
                      "resto de la cohorte, promediada sobre las",
                      "determinaciones, tal como la calcula el perfil de",
                      "unidades"),
  lo_que_establece = paste("que la validacion externa se hizo sobre la unidad",
                           "mas alejada del resto, de modo que es el caso peor",
                           "entre los disponibles, y que las sedes de la",
                           "validacion cubren un rango de disimilitud mas",
                           "estrecho"),
  lo_que_no_establece = paste("que la unidad se eligiera por esa razon; la",
                              "eleccion fue un juicio y ninguna regla la",
                              "convierte en criterio"),
  unidades = nrow(d),
  distancia_de_la_reservada = d_sel,
  mayor_de_las_demas = d_max,
  extrema_en_todas_las_categorias = all(ext$es_extremo)),
  auto_unbox = TRUE, pretty = TRUE), file.path(OUT, "manifiesto.json"))

cat("\nDepositado en", OUT, "\n")
