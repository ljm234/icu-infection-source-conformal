library(jsonlite)

# Acreditacion de que la seleccion de determinaciones no se modifico despues.
#
# La sospecha natural ante una seleccion sin criterio documentado es que se
# ajustara al ver resultados. El historial la responde por anticipado, y esa
# respuesta debe constar en el deposito y no solo en la memoria de quien la
# comprobo.
#
# La lista de identificadores vive en una sola expresion de R/19_matriz.R. La
# busqueda por contenido del historial devuelve los commits en que esa
# expresion aparece o desaparece, de modo que si la lista se hubiera tocado
# despues de fijarse habria mas de uno. El procedimiento ejecuta esa busqueda,
# deposita lo que devuelve y consigna el comando, para que cualquiera con el
# deposito clonado lo repita sin depender de esta ejecucion.
#
# Se detiene si aparece mas de un commit. En ese caso la afirmacion seria
# falsa y no procede depositarla.

OUT <- "outputs/fase33"
ARCHIVO <- "R/19_matriz.R"
EXPRESION <- "items <- c("

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
CMD_LISTA <- sprintf(
  "git log --all --format='%%H %%ad %%an' --date=short -S'%s' -- %s",
  EXPRESION, ARCHIVO)
CMD_ALTA <- sprintf(
  "git log --all --format='%%H %%ad %%an' --date=short --diff-filter=A -- %s",
  ARCHIVO)

cat("=== COMANDOS EJECUTADOS ===\n")
cat("  ", CMD_LISTA, "\n", sep = "")
cat("  ", CMD_ALTA, "\n", sep = "")

lista <- ejecutar(CMD_LISTA)
alta <- ejecutar(CMD_ALTA)

cat("\n=== COMMITS QUE TOCAN LA LISTA ===\n")
if (length(lista) == 0) detener("La busqueda no devuelve commit alguno.")
for (l in lista) cat("  ", l, "\n", sep = "")
cat("\n=== ALTA DEL ARCHIVO ===\n")
for (l in alta) cat("  ", l, "\n", sep = "")

if (length(lista) != 1)
  detener("La lista de identificadores se toco en mas de un commit. La ",
          "afirmacion de que no se modifico es falsa.")
if (length(alta) != 1)
  detener("El archivo consta dado de alta mas de una vez.")

campos <- strsplit(lista, " ")[[1]]
campos_alta <- strsplit(alta, " ")[[1]]
if (campos[1] != campos_alta[1])
  detener("La lista no se fijo en el mismo commit que dio de alta el ",
          "archivo. Procede examinar el historial antes de acreditar nada.")

# Numero de commits posteriores en la rama, para dimensionar cuanto trabajo
# se hizo despues sin tocar la seleccion.
posteriores <- length(ejecutar(sprintf("git rev-list %s..HEAD", campos[1])))

acta <- data.frame(
  archivo = ARCHIVO,
  expresion = EXPRESION,
  commit = campos[1],
  fecha = campos[2],
  commits_que_tocan_la_lista = length(lista),
  fijada_en_el_alta_del_archivo = TRUE,
  commits_posteriores_en_la_rama = posteriores,
  comando = CMD_LISTA,
  row.names = NULL)

cat("\n=== ACTA ===\n")
cat("La lista se fijo en", campos[1], "el", campos[2], "\n")
cat("Commits que la tocan desde entonces:", length(lista) - 1, "\n")
cat("Commits posteriores en la rama:", posteriores, "\n")
cat("La seleccion no se modifico despues de fijarse.\n")

dir.create(OUT, recursive = TRUE, showWarnings = FALSE)
write.csv(acta, file.path(OUT, "procedencia_seleccion.csv"), row.names = FALSE)

writeLines(toJSON(list(
  fase = "33",
  ejecutado_en = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
  r_version = R.version.string,
  proposito = paste("acreditar que la seleccion de determinaciones no se",
                    "modifico tras fijarse, que es lo que responde a la",
                    "sospecha de que se ajustara al ver resultados"),
  archivo = ARCHIVO,
  expresion = EXPRESION,
  commit = campos[1],
  fecha = campos[2],
  comando = CMD_LISTA,
  interpretacion = paste("la busqueda por contenido devuelve los commits en",
                         "que la expresion aparece o desaparece; que devuelva",
                         "uno solo, y que sea el del alta del archivo,",
                         "significa que la lista no se ha tocado desde que se",
                         "escribio"),
  lo_que_no_acredita = paste("que la seleccion fuera correcta o estuviera",
                             "razonada; solo que no se altero despues")),
  auto_unbox = TRUE, pretty = TRUE, digits = 15),
  file.path(OUT, "manifiesto.json"))

cat("\nDepositado en", OUT, "\n")
