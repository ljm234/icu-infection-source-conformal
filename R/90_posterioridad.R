library(jsonlite)

options(scipen = 999)

# Posterioridad de los analisis, derivada del historial.
#
# El documento enumeraba siete procedimientos posteriores a la fijacion de la
# seleccion y titulaba el apartado como si fuera un censo. No lo era: hay
# muchos mas, y la lista escrita a mano no crece con el trabajo.
#
# Aqui se deriva, y con tres categorias y no una, porque no comprometen lo
# mismo:
#
#   posterior a la fijacion    la seleccion de determinaciones ya estaba fija
#   posterior a la apertura    ademas, la unidad reservada ya se habia abierto
#   ademas lee filas           ademas lee alguna tabla a nivel de paciente
#
# La tercera es la que un revisor mira, y por eso se separa.
#
# LA COLUMNA QUE EVITA UNA ALARMA SIN MOTIVO. A estas alturas casi todo el
# trabajo es posterior a la apertura, porque son las rondas de verificacion y
# de documentacion. Eso no compromete nada mientras ninguno reajuste ni
# reescriba el modelo publicado, de modo que tambien se deriva quien escribe
# un artefacto suyo y quien invoca un ajuste. Sin esa distincion la lista
# asusta; con ella dice lo que importa.
#
# Las dos no son la misma cosa y el resultado lo demuestra: seis procedimientos
# posteriores a la apertura invocan un ajuste y ninguno escribe donde vive el
# modelo publicado. Ajustan el suyo, leyendo la especificacion congelada.
#
# NINGUNA FECHA SE ESCRIBE AQUI. La de la fijacion se lee del deposito que la
# acredita; la de la apertura sale del alta del unico agregado versionado de
# la fase que evaluo la unidad reservada. Y las fases donde vive el modelo
# publicado tampoco se nombran: son aquellas en que hay un objeto ajustado
# bajo control de versiones.

OUT <- "outputs/fase41"
PROC <- "outputs/fase33/procedencia_seleccion.csv"
FASE_SELLADO <- "outputs/fase11"

detener <- function(...) {
  cat("\n", ..., "\n", sep = "")
  cat("El procedimiento se detiene. No procede publicar sobre esta base.\n")
  quit(status = 1)
}

if (!dir.exists(".git")) detener("No hay historial que consultar.")
if (!file.exists(PROC)) detener("Fuente ausente: ", PROC)

ejecutar <- function(cmd) {
  r <- suppressWarnings(system(cmd, intern = TRUE))
  if (!is.null(attr(r, "status")) && attr(r, "status") != 0)
    detener("El comando fallo: ", cmd)
  r
}

es_fecha <- function(x) grepl("^20[0-9]{2}-[0-9]{2}-[0-9]{2}$", x)

# Un archivo que el historial no registra todavia es, por construccion, el
# mas nuevo del deposito: es el que se esta introduciendo. Se anota como tal
# en vez de inventarle fecha, y R/59 exige que no quede ninguno al publicar,
# de modo que un deposito compuesto antes de comprometer el trabajo no pasa
# por bueno.
alta_de <- function(ruta) {
  h <- ejecutar(paste("git log --diff-filter=A --format=%ad --date=short --",
                      shQuote(ruta)))
  if (length(h) == 0) "sin registrar" else h[length(h)]
}

# La fijacion, del deposito que la acredita.
FIJADA <- read.csv(PROC, stringsAsFactors = FALSE)$fecha[1]
if (!es_fecha(FIJADA)) detener("La fecha de fijacion no tiene forma de fecha.")

# La apertura, del alta del agregado que la fase del sellado publica. Ha de
# haber uno y solo uno: si hubiera varios, cual marca la apertura seria una
# eleccion y no un hecho.
agr <- ejecutar(sprintf("git ls-files %s", shQuote(paste0(FASE_SELLADO, "/*"))))
agr <- agr[!grepl("manifiesto[.]json$", agr)]
if (length(agr) != 1)
  detener("La fase del sellado no publica un agregado unico, sino ",
          length(agr), ". La fecha de apertura no seria un hecho.")
APERTURA <- alta_de(agr)
if (!es_fecha(APERTURA)) detener("La fecha de apertura no tiene forma de fecha.")
if (!(APERTURA > FIJADA))
  detener("La apertura no es posterior a la fijacion. El orden que este ",
          "analisis supone no se sostiene.")

cat("=== LAS DOS FECHAS, DERIVADAS ===\n")
cat("Fijacion de la seleccion:", FIJADA, " de", PROC, "\n")
cat("Apertura de la reservada:", APERTURA, " del alta de", agr, "\n")

# Las fases del modelo publicado: aquellas con un objeto ajustado versionado.
objetos <- ejecutar("git ls-files 'outputs/*/*.rds'")
if (length(objetos) == 0)
  detener("No hay objeto ajustado versionado. Las fases del modelo no ",
          "pueden derivarse.")
FASES_MODELO <- sort(unique(dirname(objetos)))
cat("Fases del modelo publicado:", paste(FASES_MODELO, collapse = ", "), "\n")

# Las rutas a nivel de paciente, por el lector unico.
source("R/00_rutas_reservadas.R")
reservadas <- rutas_reservadas()$ruta

fuentes <- sort(list.files("R", pattern = "[.]R$", full.names = TRUE))
texto <- setNames(lapply(fuentes, function(f)
  paste(readLines(f, warn = FALSE), collapse = "\n")), basename(fuentes))

# Los archivos cuyas cifras el documento publica. No se enumeran: un generador
# de prosa es el que escribe un documento y lo compone con la guardia que
# rechaza cifras literales, y lo que publica es lo que lee. Preguntarselo a la
# relacion de comprobacion en vez de a los generadores seria preguntarle a un
# intermediario, y ademas ataria esta fase a otra que a su vez la comprueba.
#
# La guardia es lo que distingue un documento de prosa de un registro de
# verificacion: el segundo tambien escribe un archivo de texto, pero es un
# libro de asientos y no afirma nada por su cuenta. La distincion se lee del
# codigo y no de los nombres.
generadores <- names(texto)[sapply(texto, grepl,
  pattern = 'writeLines\\([^,]+,\\s*"[^"]+[.]md"') &
  sapply(texto, grepl, pattern = "cifra <- +function")]
if (length(generadores) == 0)
  detener("Ningun procedimiento compone un documento de prosa. Lo publicado ",
          "no puede derivarse.")

# Rutas que una llamada LEE o ESCRIBE. Nombrar una ruta no es leerla: la
# auditoria las nombra para comprobar que no esten versionadas, y contar eso
# como lectura metia en la categoria mas grave a quien hace lo contrario. La posicion del argumento difiere: en la
# lectura es el primero y en la escritura el segundo, porque el primero es el
# dato.
# Casi ningun procedimiento escribe la ruta dentro de la llamada: la declara
# como constante arriba y usa el nombre. Resolver esas constantes no es un
# adorno, es la diferencia entre ver y no ver: sin ello los dos procedimientos
# que la auditoria senalo por leer matrices a nivel de fila salian limpios.
rutas_de <- function(txt, fn, pos = 1) {
  asign <- regmatches(txt, gregexpr(
    "[A-Za-z_][A-Za-z0-9_.]*\\s*<-\\s*\"[^\"]+\"", txt))[[1]]
  mapa <- setNames(sub('.*"([^"]+)".*', "\\1", asign),
                   sub("\\s*<-.*", "", asign))
  uno <- "(\"[^\"]+\"|[A-Za-z_][A-Za-z0-9_.]*)"
  pre <- if (pos == 1) "" else paste0(uno, "\\s*,\\s*")
  arg <- regmatches(txt, gregexpr(sprintf("%s\\(\\s*%s%s", fn, pre, uno),
                                  txt))[[1]]
  arg <- sub(sprintf("^%s\\(\\s*", fn), "", arg)
  if (pos != 1) arg <- sub(sprintf("^%s\\s*,\\s*", uno), "", arg)
  lit <- grepl('^"', arg)
  c(gsub('"', "", arg[lit]), unname(mapa[arg[!lit]]))
}

bajo_reservada <- function(rutas) {
  any(rutas %in% reservadas) ||
    any(sapply(reservadas, function(r)
      grepl("/$", r) && any(startsWith(rutas, r))))
}

publicadas <- unique(unlist(lapply(texto[generadores], function(t)
  c(rutas_de(t, "leer"), rutas_de(t, "read\\.csv")))))
publicadas <- publicadas[!is.na(publicadas) & grepl("^outputs/", publicadas)]
if (length(publicadas) == 0)
  detener("Los generadores no leen deposito alguno. Lo publicado no puede ",
          "derivarse.")
cat("Generadores de prosa:", length(generadores),
    " archivos que leen:", length(publicadas), "\n")

filas <- do.call(rbind, lapply(fuentes, function(f) {
  txt <- texto[[basename(f)]]
  lee <- c(rutas_de(txt, "read\\.csv"), rutas_de(txt, "readRDS"),
           rutas_de(txt, "dbConnect", pos = 2))
  esc <- c(rutas_de(txt, "write\\.csv", pos = 2),
           rutas_de(txt, "saveRDS", pos = 2))
  # Un nombre que no resuelve a ninguna constante no es una ruta: se descarta
  # en vez de arrastrarse como ausente y volver ausente a la clasificacion.
  lee <- setdiff(lee[!is.na(lee)], esc)
  esc <- esc[!is.na(esc)]
  o <- regmatches(txt, regexpr('OUT\\s*<-\\s*"[^"]+"', txt))
  if (length(o) == 1) esc <- c(esc, file.path(sub('.*"([^"]+)".*', "\\1", o),
    sub('.*"([^"]+)".*', "\\1",
        regmatches(txt, gregexpr('file\\.path\\(OUT,\\s*"[^"]+"', txt))[[1]])))
  a <- alta_de(f)
  data.frame(
    procedimiento = basename(f),
    alta = a,
    registrado = a != "sin registrar",
    lee_filas_por_paciente = bajo_reservada(lee),
    escribe_artefacto_del_modelo = any(dirname(esc) %in% FASES_MODELO),
    ajusta_un_modelo = grepl("(^|[^A-Za-z._])(cv[.])?glmnet\\(", txt),
    # Sin esta columna, decir que sesenta y seis analisis son posteriores
    # suena a sesenta y seis analisis nuevos. La mayoria son comprobaciones, y
    # no depositan nada que el documento publique.
    produce_cifra_publicada = any(esc %in% publicadas),
    row.names = NULL)
}))

filas$posterior_a_la_fijacion <- !filas$registrado | filas$alta > FIJADA
filas$posterior_a_la_apertura <- !filas$registrado | filas$alta > APERTURA
filas$categoria <- ifelse(
  filas$posterior_a_la_apertura & filas$lee_filas_por_paciente,
  "posterior a la apertura y lee filas por paciente",
  ifelse(filas$posterior_a_la_apertura, "posterior a la apertura",
  ifelse(filas$posterior_a_la_fijacion, "posterior a la fijacion",
         "anterior a la fijacion")))

ORDEN <- c("posterior a la apertura y lee filas por paciente",
           "posterior a la apertura", "posterior a la fijacion",
           "anterior a la fijacion")
filas <- filas[order(match(filas$categoria, ORDEN), filas$procedimiento), ]

cat("\n=== REPARTO ===\n")
for (k in ORDEN)
  cat(sprintf("  %-48s %3d\n", k, sum(filas$categoria == k)))
cat(sprintf("  %-48s %3d\n", "procedimientos contrastados", nrow(filas)))
cat(sprintf("  %-48s %3d\n", "aun sin registrar en el historial",
            sum(!filas$registrado)))

# La comprobacion que decide si esta contabilidad es tranquilizadora o grave.
post <- filas[filas$posterior_a_la_apertura, ]
cat("\n=== POSTERIORES A LA APERTURA: QUE TOCAN ===\n")
cat("Invocan un ajuste:               ", sum(post$ajusta_un_modelo), "\n")
cat("Escriben artefacto del modelo:   ",
    sum(post$escribe_artefacto_del_modelo), "\n")
if (sum(post$escribe_artefacto_del_modelo) > 0) {
  for (p in post$procedimiento[post$escribe_artefacto_del_modelo])
    cat("  ", p, "\n")
  detener("Un procedimiento posterior a la apertura escribe un artefacto ",
          "del modelo publicado. El orden del trabajo no es el que se ",
          "documenta.")
}
if (sum(post$ajusta_un_modelo) > 0)
  cat("Los que ajustan lo hacen sobre su propio modelo: leen la",
      "especificacion\ncongelada y depositan fuera de sus fases.\n")

cat("\n=== CUANTOS PRODUCEN UNA CIFRA PUBLICADA ===\n")
for (k in ORDEN)
  cat(sprintf("  %-48s %3d de %3d\n", k,
              sum(filas$categoria == k & filas$produce_cifra_publicada),
              sum(filas$categoria == k)))

cat("\n=== TERCERA CATEGORIA ===\n")
ter <- filas[filas$categoria == ORDEN[1], ]
if (nrow(ter) > 0) print(ter[, c("procedimiento", "alta", "ajusta_un_modelo")],
                         row.names = FALSE)

dir.create(OUT, recursive = TRUE, showWarnings = FALSE)
write.csv(filas[, c("procedimiento", "alta", "registrado", "categoria",
                    "posterior_a_la_fijacion", "posterior_a_la_apertura",
                    "lee_filas_por_paciente", "escribe_artefacto_del_modelo",
                    "ajusta_un_modelo", "produce_cifra_publicada")],
          file.path(OUT, "posterioridad.csv"), row.names = FALSE)
write.csv(data.frame(
  fijacion_de_la_seleccion = FIJADA,
  apertura_de_la_reservada = APERTURA,
  agregado_que_marca_la_apertura = agr,
  fases_del_modelo = paste(FASES_MODELO, collapse = " "),
  row.names = NULL), file.path(OUT, "fechas.csv"), row.names = FALSE)

writeLines(toJSON(list(
  fase = "41",
  ejecutado_en = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
  r_version = R.version.string,
  guarda_de_emparejamiento_parcial = isTRUE(getOption("warnPartialMatchDollar")),
  proposito = paste("derivar del historial que analisis son posteriores a la",
                    "fijacion de la seleccion y cuales ademas a la apertura de",
                    "la unidad reservada, en lugar de enumerarlos"),
  categorias = paste("posterior a la fijacion; posterior a la apertura;",
                     "posterior a la apertura y ademas lee filas a nivel de",
                     "paciente, que es la que compromete"),
  origen_de_las_fechas = paste("la fijacion se lee del deposito que la",
                               "acredita y la apertura del alta del unico",
                               "agregado versionado de la fase que evaluo la",
                               "unidad reservada; ninguna se escribe aqui"),
  fases_del_modelo = paste("no se nombran: son aquellas con un objeto",
                           "ajustado versionado"),
  por_que_la_columna = paste("casi todo el trabajo es posterior a la",
                             "apertura porque son rondas de verificacion y",
                             "documentacion; lo que importa es si alguno",
                             "reajusta o reescribe el modelo publicado"),
  fijacion = FIJADA,
  apertura = APERTURA,
  procedimientos = nrow(filas),
  sin_registrar = sum(!filas$registrado),
  posteriores_a_la_fijacion = sum(filas$posterior_a_la_fijacion),
  posteriores_a_la_apertura = sum(filas$posterior_a_la_apertura),
  ademas_leen_filas = sum(filas$categoria == ORDEN[1]),
  producen_cifra_publicada = sum(filas$produce_cifra_publicada),
  posteriores_que_producen = sum(filas$posterior_a_la_apertura &
                                 filas$produce_cifra_publicada),
  lo_publicado = paste("un archivo es publicado si lo lee un generador de",
                       "prosa, y un generador de prosa es el que escribe un",
                       "documento y lo compone con la guardia que rechaza",
                       "cifras literales; ninguno de los dos se enumera"),
  ajustan_tras_la_apertura = sum(post$ajusta_un_modelo),
  escriben_artefacto_tras_la_apertura = 0),
  auto_unbox = TRUE, pretty = TRUE), file.path(OUT, "manifiesto.json"))

cat("\nDepositado en", OUT, "\n")
