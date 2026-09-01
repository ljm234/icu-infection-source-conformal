# Lector unico de las rutas reservadas a nivel de paciente.
#
# La lista vive en .gitignore, que es donde la exclusion surte efecto. Tres
# procedimientos la necesitan: la documentacion de decisiones la publica, la
# auditoria de publicacion recorre el historial en su busca, y la contabilidad
# de contactos la usa para distinguir un archivo a nivel de paciente de un
# agregado.
#
# Hasta aqui cada uno la obtenia por su cuenta, dos analizando el archivo y el
# tercero transcribiendola, y los tres discrepaban: trece rutas, catorce y
# catorce, segun cada cual anadiera o no el directorio de derivados. No era un
# riesgo futuro sino una divergencia ya presente. Y tres analisis del mismo
# archivo pueden diferir tambien en donde entienden que acaba la seccion, que
# es el mismo defecto un nivel mas abajo.
#
# De ahi este archivo, que es el unico del deposito que no es un procedimiento
# sino un lector. No deposita nada, no imprime nada al cargarse y no tiene mas
# efecto que definir la funcion.
#
# LA CADENA DE ANALISIS NO LO USA, y no debe usarlo. Solo lo cargan los
# procedimientos que auditan o documentan. La razon es concreta y ya se ha
# cobrado su utilidad: R/88 recupera del historial una version antigua de un
# procedimiento y la ejecuta en un directorio aparte con lo minimo. Un
# procedimiento de la cadena que dependiera de este archivo dejaria de poder
# comprobarse asi.
#
# Que siga siendo el unico lector no queda confiado a esta nota. R/59 lo
# comprueba, y comprueba ademas que llamarlo desde otro directorio devuelva la
# misma lista.

RESERVADAS_ENCABEZADO <- "Patient level derived data"

rutas_reservadas <- function() {
  alto <- function(...) {
    cat("\n", ..., "\n", sep = "")
    cat("La lista de rutas reservadas no puede establecerse. Se detiene.\n")
    quit(status = 1)
  }

  # La raiz se resuelve por el propio repositorio y no por el directorio de
  # trabajo, de modo que quien llame obtenga la misma lista se llame desde
  # donde se llame.
  raiz <- suppressWarnings(system("git rev-parse --show-toplevel",
                                  intern = TRUE, ignore.stderr = TRUE))
  if (length(raiz) != 1 || !nzchar(raiz))
    alto("No hay repositorio del que leer el archivo de exclusiones.")
  arch <- file.path(raiz, ".gitignore")
  if (!file.exists(arch)) alto("Fuente ausente: ", arch)

  gi <- readLines(arch, warn = FALSE)
  i <- grep(RESERVADAS_ENCABEZADO, gi, fixed = TRUE)
  if (length(i) != 1)
    alto("El archivo de exclusiones no delimita la seccion una sola vez.")

  # La seccion llega hasta la linea en blanco o el comentario siguientes, y no
  # hasta el final del archivo: asi otra seccion posterior no la contamina.
  resto <- if (i < length(gi)) gi[(i + 1):length(gi)] else character(0)
  fin <- which(!nzchar(trimws(resto)) | grepl("^\\s*#", resto))
  if (length(fin) > 0) resto <- resto[seq_len(fin[1] - 1)]
  rutas <- trimws(resto)
  rutas <- rutas[nzchar(rutas)]
  if (length(rutas) == 0)
    alto("La seccion reservada no relaciona ruta alguna.")
  if (any(duplicated(rutas)))
    alto("La seccion reservada repite alguna ruta.")

  # Lo que el archivo declara y lo que git aplica pueden separarse: una ruta
  # mal escrita se lee aqui y no excluye nada. Se contrasta contra la propia
  # maquinaria de exclusion, que es la unica autoridad sobre esa pregunta.
  aplicadas <- suppressWarnings(system(paste(
    "git -C", shQuote(raiz), "check-ignore --no-index --",
    paste(shQuote(rutas), collapse = " ")), intern = TRUE,
    ignore.stderr = TRUE))
  if (!setequal(aplicadas, rutas))
    alto("Alguna ruta declarada no la excluye git: ",
         paste(setdiff(rutas, aplicadas), collapse = ", "))

  data.frame(ruta = rutas,
             directorio_entero = grepl("/$", rutas),
             row.names = NULL, stringsAsFactors = FALSE)
}
