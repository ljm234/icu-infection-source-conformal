library(jsonlite)

# Acreditacion de que la seleccion de determinaciones no se modifico despues.
#
# La sospecha natural ante una seleccion sin criterio documentado es que se
# ajustara al ver resultados. El historial la responde por anticipado, y esa
# respuesta debe constar en el deposito y no solo en la memoria de quien la
# comprobo.
#
# EL METODO ANTERIOR NO ACREDITABA LO QUE AFIRMABA. Este procedimiento
# buscaba en el historial los commits en que aparecia o desaparecia la cadena
# que abre la lista, y se detenia si habia mas de uno. Esa busqueda cuenta
# apariciones de un token: editar los identificadores dentro del bloque no
# altera cuantas veces aparece la cadena que lo abre, de modo que una lista
# reescrita por completo habria devuelto igualmente un solo commit. La
# comprobacion no podia fallar, y una comprobacion que no puede fallar no
# comprueba nada. El archivo, ademas, si se modifico despues, en un segundo
# commit que aquella busqueda no veia.
#
# Lo que sigue compara el contenido. Para cada version del archivo que el
# historial conserva se extrae el bloque y se resume, y se exige que todos
# los resumenes coincidan entre si y con el bloque en disco. El resumen se
# obtiene con git, de modo que el comando consignado produce exactamente la
# misma cadena que aqui se deposita, en cualquier maquina y sin herramienta
# externa alguna.

OUT <- "outputs/fase33"
ARCHIVO <- "R/19_matriz.R"
BLOQUE <- "de la linea que abre items hasta la que define lista_ids"
SED <- "/^items <- c(/,/^lista_ids/p"

detener <- function(...) {
  cat("\n", ..., "\n", sep = "")
  cat("El procedimiento se detiene. No procede acreditar nada.\n")
  quit(status = 1)
}

if (!file.exists(ARCHIVO)) detener("Fuente ausente: ", ARCHIVO)
if (!dir.exists(".git")) detener("No hay historial que consultar.")

ejecutar <- function(cmd) {
  r <- suppressWarnings(system(cmd, intern = TRUE))
  if (!is.null(attr(r, "status")) && attr(r, "status") != 0)
    detener("El comando fallo: ", cmd)
  r
}

# El comando que se acredita. Se compone una sola vez y se consigna tal cual,
# de modo que lo depositado y lo ejecutado sean la misma cadena.
CMD_LISTA <- paste0(
  "git log --all --format=%H -- ", ARCHIVO,
  " | while read c; do printf '%s %s\\n' \"$c\" \"$(git show $c:", ARCHIVO,
  " | sed -n '", SED, "' | git hash-object --stdin)\"; done")

cat("=== COMANDO ACREDITADO ===\n")
cat("  ", CMD_LISTA, "\n", sep = "")

commits <- ejecutar(paste0("git log --all --format='%H %ad' --date=short -- ",
                           ARCHIVO))
if (length(commits) == 0) detener("El historial no registra el archivo.")

alta <- ejecutar(paste0("git log --all --format='%H %ad' --date=short ",
                        "--diff-filter=A -- ", ARCHIVO))
if (length(alta) != 1) detener("El archivo consta dado de alta mas de una vez.")
sha_alta <- strsplit(alta, " ")[[1]][1]

# Resumen del bloque en cada version, y numero de identificadores que declara.
# Ambos se leen del propio historial: transcribirlos permitiria que el
# deposito describiera una lista que el archivo ya no contiene.
resumen_de <- function(sha)
  ejecutar(sprintf("git show %s:%s | sed -n '%s' | git hash-object --stdin",
                   sha, ARCHIVO, SED))

ids_de <- function(sha) {
  src <- ejecutar(sprintf("git show %s:%s", sha, ARCHIVO))
  i <- grep("^items <- c\\(", src)
  j <- grep("^lista_ids", src)
  if (length(i) != 1 || length(j) != 1 || j <= i)
    detener("No se localiza el bloque en la version ", sha)
  blk <- paste(src[i:j], collapse = " ")
  m <- gregexpr("[a-z][a-z0-9_]*[[:space:]]*=[[:space:]]*\"[0-9]+\"", blk)
  length(regmatches(blk, m)[[1]])
}

campos <- do.call(rbind, lapply(commits, function(l) {
  p <- strsplit(l, " ")[[1]]
  data.frame(commit = p[1], fecha = p[2], es_alta = p[1] == sha_alta,
             resumen_del_bloque = resumen_de(p[1]),
             identificadores = ids_de(p[1]), row.names = NULL)
}))

# El bloque en disco ha de coincidir con el del historial. Si difiriera, el
# deposito describiria una version que no es la que el analisis empleo.
resumen_disco <- ejecutar(sprintf("sed -n '%s' %s | git hash-object --stdin",
                                  SED, ARCHIVO))
campos$coincide_con_el_actual <- campos$resumen_del_bloque == resumen_disco

cat("\n=== VERSIONES DEL BLOQUE ===\n")
print(campos, row.names = FALSE)

distintos <- length(unique(campos$resumen_del_bloque))
cat("\nCommits que tocan el archivo:", nrow(campos), "\n")
cat("Resumenes distintos del bloque:", distintos, "\n")
cat("Resumen del bloque en disco:", resumen_disco, "\n")

if (distintos != 1)
  detener("El bloque cambia de contenido entre versiones. La afirmacion de ",
          "que la seleccion no se modifico es falsa.")
if (!all(campos$coincide_con_el_actual))
  detener("Alguna version del bloque no coincide con la que hay en disco.")
if (length(unique(campos$identificadores)) != 1)
  detener("El numero de identificadores varia entre versiones.")
if (!any(campos$es_alta))
  detener("El alta del archivo no figura entre los commits que lo tocan.")
if (campos$resumen_del_bloque[campos$es_alta] != resumen_disco)
  detener("El bloque no quedo fijado en el alta del archivo.")

fila_alta <- campos[campos$es_alta, ]

# No se deposita el numero de commits posteriores en la rama. Es una cifra
# autorreferencial: envejece con cada commit y ya se desfaso tres veces en una
# sola sesion de trabajo. El documento principal se niega a publicar los
# recuentos de archivos y de lineas por esa misma razon, y no cabe aplicar un
# criterio en un sitio y el contrario en otro. Quien quiera la cifra la obtiene
# del historial, que es donde no envejece.

acta <- data.frame(
  archivo = ARCHIVO,
  bloque = BLOQUE,
  commit = fila_alta$commit,
  fecha = fila_alta$fecha,
  commits_que_tocan_el_archivo = nrow(campos),
  commits_que_tocan_la_lista = distintos,
  identificadores = fila_alta$identificadores,
  resumen_del_bloque = resumen_disco,
  coincide_con_el_bloque_en_disco = TRUE,
  fijada_en_el_alta_del_archivo = TRUE,
  comando = CMD_LISTA,
  row.names = NULL)

cat("\n=== ACTA ===\n")
cat("La lista se fijo en", fila_alta$commit, "el", fila_alta$fecha, "\n")
cat("Identificadores que declara:", fila_alta$identificadores, "\n")
cat("Commits que tocan el archivo desde entonces:", nrow(campos) - 1, "\n")
cat("De ellos, los que cambian la lista:", distintos - 1, "\n")

dir.create(OUT, recursive = TRUE, showWarnings = FALSE)
write.csv(acta, file.path(OUT, "procedencia_seleccion.csv"), row.names = FALSE)
write.csv(campos, file.path(OUT, "versiones_del_bloque.csv"), row.names = FALSE)

writeLines(toJSON(list(
  fase = "33",
  ejecutado_en = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
  r_version = R.version.string,
  guarda_de_emparejamiento_parcial = isTRUE(getOption("warnPartialMatchDollar")),
  proposito = paste("acreditar que la seleccion de determinaciones no se",
                    "modifico tras fijarse, que es lo que responde a la",
                    "sospecha de que se ajustara al ver resultados"),
  archivo = ARCHIVO,
  bloque = BLOQUE,
  comando = CMD_LISTA,
  metodo = paste("se resume el contenido del bloque en cada version que el",
                 "historial conserva y se exige que todos los resumenes",
                 "coincidan entre si y con el bloque en disco"),
  metodo_anterior = paste("una busqueda por contenido de la cadena que abre",
                          "la lista. Contaba apariciones de un token y no",
                          "podia detectar una edicion dentro del bloque, de",
                          "modo que no acreditaba lo que se le atribuia"),
  interpretacion = paste("un solo resumen distinto entre todas las versiones",
                         "significa que el bloque no cambio de contenido; que",
                         "ese resumen sea el del alta significa que quedo",
                         "fijado al escribirse"),
  lo_que_no_acredita = paste("que la seleccion fuera correcta o estuviera",
                             "razonada; solo que no se altero despues"),
  commits_que_tocan_el_archivo = nrow(campos),
  commits_que_tocan_la_lista = distintos),
  auto_unbox = TRUE, pretty = TRUE),
  file.path(OUT, "manifiesto.json"))

cat("\nDepositado en", OUT, "\n")
