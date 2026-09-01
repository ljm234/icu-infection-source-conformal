library(jsonlite)

options(scipen = 999)

# Orden total de los depositos que salen de una consulta.
#
# Una clausula de orden que no distingue todas las filas deja el resto a la
# implementacion, y duckdb agrega en paralelo: dos ejecuciones devuelven
# ordenes distintos. Este desarrollo tuvo esa clase de defecto en nueve
# consultas a la vez, y la mas grave hacia que la particion, que asigna por
# posicion de fila, repartiera pacientes distintos con la misma semilla.
#
# Hay dos modos de fallo y solo uno avisa:
#
#   clave inexistente o ambigua   la consulta falla y se ve
#   clave existente pero no unica no falla, reproduce dos veces seguidas por
#                                 casualidad del dato, y el defecto sigue vivo
#
# El segundo es el que este procedimiento persigue. La regla es que las
# columnas de ordenacion sean conjuntamente unicas en el resultado: si lo son,
# el orden queda determinado por el contenido y no por la implementacion.
#
# La comprobacion se hace sobre el deposito y no sobre la consulta, porque es
# el deposito lo que se publica. Las columnas de ordenacion se leen del codigo
# que lo escribe, de modo que nadie tiene que mantener una lista aparte. Si no
# se pueden leer, el procedimiento se detiene en lugar de dar por bueno lo que
# no ha comprobado.

OUT <- "outputs/fase38"

detener <- function(...) {
  cat("\n", ..., "\n", sep = "")
  cat("El procedimiento se detiene. No procede publicar sobre esta base.\n")
  quit(status = 1)
}

if (!dir.exists(".git")) detener("No hay historial que consultar.")

versionados <- system("git ls-files 'outputs/*.csv' 'outputs/*/*.csv'",
                      intern = TRUE)
if (length(versionados) == 0) detener("No hay depositos versionados.")

fuentes <- list.files("R", pattern = "[.]R$", full.names = TRUE)

# Para cada deposito: el procedimiento que lo escribe, la variable que
# escribe, y si esa variable viene de una consulta. Solo esas entran: las que
# se componen en R llevan el orden que el propio codigo les da.
localizar <- function(dep) {
  for (f in fuentes) {
    s <- paste(readLines(f, warn = FALSE), collapse = "\n")
    # Se empareja por ruta y no por nombre: hay nombres de archivo que se
    # repiten en directorios distintos, y emparejar por el nombre atribuyo a
    # una consulta un deposito que se compone en R.
    base <- basename(dep)
    dir  <- dirname(dep)
    esc  <- function(x) gsub("([.]|/)", "[\\1]", x)
    pat <- sprintf('write\\.csv\\(\\s*([A-Za-z_][A-Za-z0-9_.]*)\\s*,\\s*(file[.]path\\(OUT,\\s*)?"(%s/)?%s"',
                   esc(dir), esc(base))
    if (!any(grepl(sprintf('OUT\\s*<-\\s*"%s"', esc(dir)), s)))
      pat <- sprintf('write\\.csv\\(\\s*([A-Za-z_][A-Za-z0-9_.]*)\\s*,\\s*"%s"',
                     esc(dep))
    m <- regmatches(s, regexpr(pat, s))
    if (length(m) == 0) next
    var <- sub(".*write\\.csv\\(\\s*([A-Za-z_][A-Za-z0-9_.]*)\\s*,.*", "\\1", m)
    q <- regmatches(s, regexpr(sprintf(
      '%s\\s*<-\\s*dbGetQuery\\(\\s*con\\s*,\\s*(sprintf\\()?\\s*"(\\\\.|[^"])*"',
      gsub("[.]", "[.]", var)), s))
    if (length(q) == 0) return(list(archivo = basename(f), var = var, sql = NA))
    # Solo el cuerpo de la consulta. Conservar el envoltorio de R haria que el
    # parentesis de dbGetQuery contara como profundidad, y ninguna clausula
    # externa se reconoceria como tal.
    cuerpo <- sub('^.*?dbGetQuery\\([^"]*"', "", q)
    return(list(archivo = basename(f), var = var, sql = cuerpo))
  }
  NULL
}

# La clausula de orden externa, resuelta a nombres de columna del deposito.
claves <- function(sql, cols) {
  u <- toupper(sql)
  p <- gregexpr("ORDER BY", u)[[1]]
  if (p[1] == -1) return(character(0))
  # la ultima que no este dentro de una ventana OVER(...)
  # La clausula externa es la que esta a profundidad cero de parentesis. Las
  # que aparecen dentro de OVER(...) o de una subconsulta ordenan otra cosa y
  # no determinan el orden del resultado.
  ch <- strsplit(sql, "")[[1]]
  prof <- cumsum((ch == "(") - (ch == ")"))
  cl <- NULL
  for (i in rev(seq_along(p))) {
    if (prof[p[i]] != 0) next
    cl <- substring(sql, p[i] + 8)
    break
  }
  if (is.null(cl)) return(character(0))
  cl <- sub('".*$', "", cl)
  cl <- gsub("\\s+", " ", trimws(cl))
  piezas <- trimws(strsplit(cl, ",")[[1]])
  piezas <- piezas[nzchar(piezas)]
  sapply(piezas, function(x) {
    x <- trimws(sub("(?i)\\s+(DESC|ASC)\\s*$", "", x, perl = TRUE))
    x <- sub("^[A-Za-z_][A-Za-z0-9_]*\\.", "", x)
    if (grepl("^[0-9]+$", x)) {
      i <- as.integer(x)
      if (i < 1 || i > length(cols))
        detener("Una referencia posicional del orden cae fuera del deposito.")
      return(cols[i])
    }
    x
  }, USE.NAMES = FALSE)
}

filas <- list()
for (dep in versionados) {
  loc <- localizar(dep)
  if (is.null(loc) || is.na(loc$sql[1])) next
  d <- read.csv(dep, stringsAsFactors = FALSE)
  k <- claves(loc$sql, names(d))
  falta <- setdiff(k, names(d))
  if (length(falta) > 0)
    detener("El deposito ", dep, " se ordena por columnas que no contiene: ",
            paste(falta, collapse = ", "),
            ". La comprobacion no puede darse por hecha.")
  if (length(k) == 0) {
    unica <- nrow(d) <= 1
    nota <- if (unica) "una sola fila" else "sin clausula de orden"
  } else {
    v <- do.call(paste, c(d[, k, drop = FALSE], sep = "\r"))
    unica <- !any(duplicated(v))
    nota <- if (unica) "orden total" else "la clave de orden repite valores"
  }
  filas[[length(filas)+1]] <- data.frame(
    deposito = dep, escrito_por = loc$archivo, filas = nrow(d),
    claves_de_orden = if (length(k)) paste(k, collapse = "+") else "",
    orden_total = unica, nota = nota, row.names = NULL)
}

tab <- do.call(rbind, filas)
tab <- tab[order(tab$orden_total, tab$deposito), ]

cat("=== ORDEN DE LOS DEPOSITOS QUE SALEN DE UNA CONSULTA ===\n")
print(tab, row.names = FALSE)

malos <- tab[!tab$orden_total, ]
cat("\nDepositos contrastados:", nrow(tab), "\n")
cat("Con orden total:", sum(tab$orden_total), "\n")
cat("Sin orden total:", nrow(malos), "\n")

dir.create(OUT, recursive = TRUE, showWarnings = FALSE)
write.csv(tab, file.path(OUT, "orden_total.csv"), row.names = FALSE)

writeLines(toJSON(list(
  fase = "38",
  ejecutado_en = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
  r_version = R.version.string,
  guarda_de_emparejamiento_parcial = isTRUE(getOption("warnPartialMatchDollar")),
  proposito = paste("comprobar que todo deposito que sale de una consulta",
                    "queda ordenado por columnas conjuntamente unicas, de",
                    "modo que su orden lo determine el contenido y no la",
                    "implementacion de la base"),
  regla = paste("las columnas de la clausula de orden han de ser",
                "conjuntamente unicas en el resultado; si no lo son, el orden",
                "entre las filas que empatan queda sin determinar"),
  por_que = paste("una clave inexistente o ambigua hace fallar la consulta y",
                  "se ve; una clave existente pero no unica no falla, y",
                  "reproduce dos veces seguidas por casualidad del dato"),
  origen_de_las_claves = paste("se leen del codigo que escribe cada deposito;",
                               "si no pueden leerse, el procedimiento se",
                               "detiene en lugar de dar por bueno lo que no ha",
                               "comprobado"),
  depositos = nrow(tab),
  con_orden_total = sum(tab$orden_total)), auto_unbox = TRUE, pretty = TRUE),
  file.path(OUT, "manifiesto.json"))

if (nrow(malos) > 0) {
  cat("\nLa clave de orden repite valores en:\n")
  for (i in seq_len(nrow(malos)))
    cat("  ", malos$deposito[i], "  claves:", malos$claves_de_orden[i], "\n")
  detener("Algun deposito no queda totalmente ordenado por su propia clave.")
}

cat("\nDepositado en", OUT, "\n")
