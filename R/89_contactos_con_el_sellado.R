library(jsonlite)

options(scipen = 999)

# Contabilidad de los contactos con la unidad reservada.
#
# El documento enumeraba tres y son cinco, y el recuento volvia a quedarse
# corto cada vez que alguien leia el registro de aquella evaluacion: ya paso
# al anadir la curva de calibracion. Enumerar a mano un recuento que crece con
# el trabajo no es sostenible, de modo que aqui se deriva del codigo.
#
# Se deriva por dos vias y solo esas dos. Leer el registro de la evaluacion, y
# filtrar por el grupo sellado sobre un archivo a nivel de paciente. No cuenta
# leer un archivo que contenga sus filas entre otras: casi todo el deposito lo
# hace en algun punto, y contarlo inflaria el numero hasta no significar nada.
# Lo que distingue un archivo a nivel de paciente de un agregado no se escribe
# aqui: son las rutas que el archivo de exclusiones reserva, que es donde esa
# distincion ya vive.
#
# Las clases y su compromiso son distintos, y el documento ha de decirlo:
#
#   define la etiqueta      la escribe, y esta aguas arriba de toda evaluacion
#   predice sobre la unidad la abre una vez, con el modelo ya congelado
#   reutiliza lo almacenado  no vuelve a predecir
#   la describe sin predecir sobre otra tabla a nivel de paciente
#
# Quien define queda fuera del recuento de contactos, y la razon se deriva y
# no se escribe: escribe el archivo que filtra. Quien escribe la etiqueta la
# define; quien la lee despues, la usa. Excluirlo con una excepcion a mano
# seria volver a enumerar, que es lo que esta derivacion existe para evitar.

OUT <- "outputs/fase40"
EVAL <- "outputs/fase11/sellado_evaluado.csv"
APARTE <- "outputs/fase5/SELLADO_NO_ABRIR.csv"
ETIQUETA <- "sellado"

detener <- function(...) {
  cat("\n", ..., "\n", sep = "")
  cat("El procedimiento se detiene. No procede publicar sobre esta base.\n")
  quit(status = 1)
}

# Las rutas a nivel de paciente, por el lector unico. Antes se analizaban
# aqui, y el directorio de derivados se anadia a mano una linea mas abajo:
# esa linea era exactamente la clase de excepcion escrita que este trabajo
# evita, y hacia que este procedimiento contase catorce rutas donde la
# documentacion contaba trece.
source("R/00_rutas_reservadas.R")
reservadas <- rutas_reservadas()$ruta
if (!EVAL %in% reservadas) detener("El registro de la evaluacion no figura ",
                                   "entre las rutas reservadas.")

fuentes <- sort(list.files("R", pattern = "[.]R$", full.names = TRUE))

# Lecturas y escrituras efectivas. Una ruta nombrada dentro de una relacion no
# es una lectura: la auditoria las enumera para comprobar que no esten
# versionadas, y contarla como contacto seria contar lo contrario de lo que
# hace.
# El argumento que lleva la ruta no ocupa la misma posicion en las dos: en la
# lectura es el primero y en la escritura el segundo, porque el primero es el
# dato. Tomar el primero en las dos devolvia el nombre del objeto y dejaba a
# quien escribe sin escrituras.
rutas_de <- function(txt, fn, pos = 1) {
  asign <- regmatches(txt, gregexpr(
    "[A-Za-z_][A-Za-z0-9_.]*\\s*<-\\s*\"[^\"]+\"", txt))[[1]]
  mapa <- setNames(sub('.*"([^"]+)".*', "\\1", asign),
                   sub("\\s*<-.*", "", asign))
  uno <- "(\"[^\"]+\"|[A-Za-z_][A-Za-z0-9_.]*)"
  pre <- if (pos == 1) "" else paste0(uno, "\\s*,\\s*")
  arg <- regmatches(txt, gregexpr(
    sprintf("%s\\(\\s*%s%s", fn, pre, uno), txt))[[1]]
  arg <- sub(sprintf("^%s\\(\\s*", fn), "", arg)
  if (pos != 1) arg <- sub(sprintf("^%s\\s*,\\s*", uno), "", arg)
  lit <- grepl('^"', arg)
  c(gsub('"', "", arg[lit]), unname(mapa[arg[!lit]]))
}

filas <- do.call(rbind, lapply(fuentes, function(f) {
  txt <- paste(readLines(f, warn = FALSE), collapse = "\n")
  lee    <- unique(na.omit(rutas_de(txt, "read\\.csv")))
  escribe <- unique(na.omit(rutas_de(txt, "write\\.csv", pos = 2)))
  # file.path(OUT, "x") tambien escribe; se resuelve con el OUT declarado.
  o <- regmatches(txt, regexpr('OUT\\s*<-\\s*"[^"]+"', txt))
  if (length(o) == 1) {
    d <- sub('.*"([^"]+)".*', "\\1", o)
    escribe <- c(escribe, file.path(d, sub('.*"([^"]+)".*', "\\1",
      regmatches(txt, gregexpr('file\\.path\\(OUT,\\s*"[^"]+"', txt))[[1]])))
  }
  data.frame(
    procedimiento = basename(f),
    lee_la_evaluacion = EVAL %in% lee,
    escribe_la_evaluacion = EVAL %in% escribe,
    filtra_la_etiqueta = grepl(sprintf('== *"%s"', ETIQUETA), txt),
    lee_a_nivel_de_paciente = any(lee %in% reservadas) ||
      any(sapply(reservadas, function(r)
        endsWith(r, "/") && any(startsWith(lee, r)))),
    escribe_lo_que_filtra = any(escribe %in% reservadas),
    row.names = NULL)
}))

toca <- filas$lee_la_evaluacion | filas$escribe_la_evaluacion |
        (filas$filtra_la_etiqueta & filas$lee_a_nivel_de_paciente)
filas <- filas[toca, ]

filas$clase <- ifelse(
  filas$filtra_la_etiqueta & filas$escribe_lo_que_filtra &
    !filas$escribe_la_evaluacion, "define la etiqueta",
  ifelse(filas$escribe_la_evaluacion, "predice sobre la unidad",
  ifelse(filas$lee_la_evaluacion, "reutiliza lo almacenado",
         "la describe sin predecir")))
filas <- filas[order(match(filas$clase,
  c("define la etiqueta", "predice sobre la unidad",
    "reutiliza lo almacenado", "la describe sin predecir")),
  filas$procedimiento), ]

cat("=== CONTACTOS CON LA UNIDAD RESERVADA ===\n")
print(filas[, c("procedimiento", "clase")], row.names = FALSE)

if (!any(filas$clase == "predice sobre la unidad"))
  detener("Ningun procedimiento predice sobre la unidad reservada. La ",
          "derivacion no describe lo que el trabajo hizo.")
if (sum(filas$clase == "predice sobre la unidad") != 1)
  detener("Mas de un procedimiento predice sobre la unidad reservada.")
if (sum(filas$clase == "define la etiqueta") != 1)
  detener("La etiqueta del grupo sellado no se define en un solo sitio.")

contactos <- filas[filas$clase != "define la etiqueta", ]

cat("\nContactos, sin contar a quien define la etiqueta:", nrow(contactos), "\n")
for (cl in unique(contactos$clase))
  cat(sprintf("  %-24s %s\n", cl,
              paste(contactos$procedimiento[contactos$clase == cl],
                    collapse = ", ")))

# La particion aparte. Se escribe una vez y no la lee ningun procedimiento:
# el archivo cuyo nombre promete que no se abre, en efecto no se abre.
CMD <- sprintf("git grep -l -- %s -- 'R/*.R'", shQuote(APARTE))
enc <- suppressWarnings(system(CMD, intern = TRUE))
enc <- basename(enc)
esc <- filas$procedimiento[filas$escribe_lo_que_filtra]
lect <- setdiff(enc, esc)
# La auditoria la nombra para comprobar que no este versionada, no para leerla.
lect <- lect[sapply(lect, function(p) {
  txt <- paste(readLines(file.path("R", p), warn = FALSE), collapse = "\n")
  APARTE %in% rutas_de(txt, "read\\.csv")
})]

aparte <- data.frame(
  archivo = APARTE,
  procedimientos_que_lo_nombran = length(enc),
  procedimientos_que_lo_leen = length(lect),
  # Quienes lo nombran, para que el documento tampoco tenga que decirlo a
  # mano. Salen ordenados por ruta, de modo que el que lo escribe va primero.
  quienes_lo_nombran = paste0("R/", sort(enc), collapse = ", "),
  comando = CMD, row.names = NULL)

cat("\n=== EL CONJUNTO APARTADO ===\n")
cat("Procedimientos que lo nombran:", length(enc),
    " que lo leen:", length(lect), "\n")
if (length(lect) > 0)
  detener("Algun procedimiento lee el conjunto apartado: ",
          paste(lect, collapse = ", "))

dir.create(OUT, recursive = TRUE, showWarnings = FALSE)
write.csv(filas[, c("procedimiento", "clase", "lee_la_evaluacion",
                    "escribe_la_evaluacion", "filtra_la_etiqueta",
                    "escribe_lo_que_filtra")],
          file.path(OUT, "contactos_con_el_sellado.csv"), row.names = FALSE)
write.csv(aparte, file.path(OUT, "conjunto_apartado.csv"), row.names = FALSE)

writeLines(toJSON(list(
  fase = "40",
  ejecutado_en = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
  r_version = R.version.string,
  guarda_de_emparejamiento_parcial = isTRUE(getOption("warnPartialMatchDollar")),
  proposito = paste("derivar del codigo cuantos procedimientos tocan la",
                    "unidad reservada y con que compromiso, en lugar de",
                    "enumerarlos, porque el recuento crece con el trabajo"),
  vias = paste("leer el registro de la evaluacion, y filtrar por el grupo",
               "sellado sobre un archivo a nivel de paciente"),
  no_cuenta = paste("leer un archivo que contenga sus filas entre otras, ni",
                    "nombrar una ruta dentro de una relacion sin leerla"),
  nivel_de_paciente = paste("se toma de las rutas que el archivo de",
                            "exclusiones reserva, que es donde esa distincion",
                            "ya vive"),
  exclusion_de_quien_define = paste("queda fuera del recuento quien escribe el",
                                    "archivo que filtra: define la etiqueta en",
                                    "lugar de usarla, y esta aguas arriba de",
                                    "toda evaluacion. La razon se deriva y no",
                                    "se escribe"),
  contactos = nrow(contactos),
  clases = length(unique(contactos$clase)),
  conjunto_apartado = paste("se escribe una vez y ningun procedimiento lo",
                            "lee"),
  comando = CMD), auto_unbox = TRUE, pretty = TRUE),
  file.path(OUT, "manifiesto.json"))

cat("\nDepositado en", OUT, "\n")
