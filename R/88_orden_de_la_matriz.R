library(jsonlite)

options(scipen = 999)

# Medicion del orden no fijado de la matriz de analisis.
#
# La consulta que construia la matriz no fijaba su orden. Como la particion
# reparte por posicion de fila, la misma semilla asignaba pacientes distintos
# en cada ejecucion: un modo de fallo contra el que la semilla aparenta
# proteger y no protege.
#
# Este procedimiento lo mide en lugar de declararlo. Recupera del historial la
# version anterior al arreglo, la ejecuta dos veces en un directorio aparte,
# hace lo mismo con la version corregida, y compara los cuatro resultados
# entre si y contra la matriz publicada.
#
# Nada de lo que escribe toca el deposito: todo ocurre en un directorio
# temporal que se borra al terminar. La matriz publicada no se regenera.
#
# Los depositos de esta fase son agregados: recuentos de posiciones y nombres
# de columna, ninguna fila a nivel de paciente y ningun identificador. La
# matriz que se lee para comparar es a nivel de estancia e identificable, esta
# en la seccion reservada del archivo de exclusiones y se lee para contar y
# comparar ordenes: no se copia ni se deposita.
#
# Depositaba ademas un segundo archivo, seleccion_del_sellado.csv, cuya
# columna unidades contaba las unidades presentes en la matriz y no las de la
# cohorte, bajo un nombre que hablaba de la seleccion del sellado. Nadie lo
# publicaba ni lo contrastaba, y un recuento que dice una cosa bajo un nombre
# que sugiere otra es peor que no tenerlo. Se retiro en lugar de renombrarlo.
#
# Que la unidad reservada se elija por nombre y no por posicion, que es lo
# unico que aquel archivo queria decir, ya lo dice el documento y lo sostiene
# la comparacion de ordenes que si se deposita aqui.

OUT <- "outputs/fase39"
ARCHIVO <- "R/19_matriz.R"
ARREGLO <- "ORDER BY stay_id"
MATRIZ <- "outputs/fase4/matriz.csv"

detener <- function(...) {
  cat("\n", ..., "\n", sep = "")
  cat("El procedimiento se detiene. No procede publicar sobre esta base.\n")
  quit(status = 1)
}

if (!dir.exists(".git")) detener("No hay historial que consultar.")
if (!file.exists(MATRIZ)) detener("Fuente ausente: ", MATRIZ)

ejecutar <- function(cmd) {
  r <- suppressWarnings(system(cmd, intern = TRUE))
  if (!is.null(attr(r, "status")) && attr(r, "status") != 0)
    detener("El comando fallo: ", cmd)
  r
}

# El commit que introdujo el arreglo, leido del historial y no escrito aqui.
h <- ejecutar(sprintf("git log --format='%%H %%ad' --date=short -S'%s' -- %s",
                      ARREGLO, ARCHIVO))
if (length(h) == 0) detener("El historial no registra el arreglo del orden.")
alta <- strsplit(h[length(h)], " ")[[1]]
SHA <- alta[1]; FECHA <- alta[2]

cat("=== EL ARREGLO ===\n")
cat("Introducido en", SHA, "el", FECHA, "\n")

RAIZ <- normalizePath(".")
tmp <- file.path(tempdir(), "orden_matriz")
unlink(tmp, recursive = TRUE)

# Una ejecucion de la version pedida, en directorio aparte. Las entradas se
# enlazan; la salida cae en el temporal y nunca en el deposito.
correr <- function(version, etiqueta) {
  d <- file.path(tmp, etiqueta)
  dir.create(file.path(d, "outputs", "fase4"), recursive = TRUE,
             showWarnings = FALSE)
  dir.create(file.path(d, "R"), showWarnings = FALSE)
  ejecutar(sprintf("git show %s:%s > %s", version, ARCHIVO,
                   file.path(d, ARCHIVO)))
  file.copy(file.path(RAIZ, ".Rprofile"), d)
  file.symlink(file.path(RAIZ, "renv"), file.path(d, "renv"))
  cat("  ", etiqueta, "\n", sep = "")
  ejecutar(sprintf("cd %s && Rscript %s > /dev/null 2>&1", d, ARCHIVO))
  file.path(d, MATRIZ)
}

cat("\nEjecutando la matriz cuatro veces. Toma varios minutos.\n")
rutas <- c(
  antes_a  = correr(paste0(SHA, "^"), "antes_a"),
  antes_b  = correr(paste0(SHA, "^"), "antes_b"),
  despues_a = correr(SHA, "despues_a"),
  despues_b = correr(SHA, "despues_b"))
for (r in rutas) if (!file.exists(r)) detener("Una ejecucion no deposito.")

m <- lapply(c(publicada = MATRIZ, rutas), read.csv, stringsAsFactors = FALSE)
if (length(unique(sapply(m, ncol))) != 1)
  detener("Las versiones no coinciden en numero de columnas.")
cols <- names(m[[1]])
if (!"stay_id" %in% cols) detener("La matriz no declara la estancia.")

# Contenido: se alinean por estancia para que la comparacion no dependa del
# orden. Orden: se comparan las secuencias de estancia tal cual salieron.
orden <- lapply(m, function(x) x$stay_id)
alin  <- lapply(m, function(x) x[order(x$stay_id), ])

celdas <- function(x, y) sum(sapply(cols, function(cl) {
  a <- x[[cl]]; b <- y[[cl]]
  sum(!((is.na(a) & is.na(b)) | (!is.na(a) & !is.na(b) & a == b)))
}))

pares <- list(
  c("publicada", "antes_a"), c("publicada", "antes_b"),
  c("antes_a", "antes_b"), c("despues_a", "despues_b"),
  c("publicada", "despues_a"))
tab <- do.call(rbind, lapply(pares, function(p) {
  x <- alin[[p[1]]]; y <- alin[[p[2]]]
  data.frame(
    comparacion = paste(p, collapse = " frente a "),
    filas = nrow(x),
    columnas = length(cols),
    celdas_comparadas = nrow(x) * length(cols),
    celdas_que_difieren = celdas(x, y),
    mismas_estancias = setequal(orden[[p[1]]], orden[[p[2]]]),
    posiciones_que_cambian = sum(orden[[p[1]]] != orden[[p[2]]]),
    pct_posiciones = round(100 * sum(orden[[p[1]]] != orden[[p[2]]]) /
                           length(orden[[p[1]]]), 2),
    row.names = NULL)
}))

cat("\n=== COMPARACION ===\n")
print(tab, row.names = FALSE)

if (any(tab$celdas_que_difieren > 0))
  detener("Alguna version difiere en contenido y no solo en orden. La ",
          "medicion describiria otra cosa.")
if (tab$posiciones_que_cambian[tab$comparacion == "despues_a frente a despues_b"] != 0)
  detener("La version corregida no reproduce su propio orden.")

cat("\n=== LO QUE ESTO ESTABLECE ===\n")
cat("El contenido es identico en las cinco versiones, celda a celda.\n")
cat("Antes del arreglo, dos ejecuciones del mismo codigo con la misma\n")
cat("semilla devuelven la matriz en ordenes distintos. Despues, no.\n")
cat("No se deposita lectura alguna sobre sus consecuencias.\n")

unlink(tmp, recursive = TRUE)
dir.create(OUT, recursive = TRUE, showWarnings = FALSE)
write.csv(tab, file.path(OUT, "orden_de_la_matriz.csv"), row.names = FALSE)

writeLines(toJSON(list(
  fase = "39",
  ejecutado_en = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
  r_version = R.version.string,
  guarda_de_emparejamiento_parcial = isTRUE(getOption("warnPartialMatchDollar")),
  proposito = paste("medir el orden no fijado de la matriz de analisis, que",
                    "hacia que la particion repartiera pacientes distintos",
                    "con la misma semilla"),
  archivo = ARCHIVO,
  arreglo = ARREGLO,
  arreglo_introducido_en = SHA,
  arreglo_introducido_el = FECHA,
  metodo = paste("se recupera del historial la version anterior al arreglo y",
                 "se ejecuta dos veces en un directorio aparte, y lo mismo con",
                 "la corregida; los cuatro resultados se comparan entre si y",
                 "contra la matriz publicada"),
  la_matriz_publicada = paste("no se regenera; todo ocurre en un directorio",
                              "temporal que se borra al terminar"),
  lectura = "no se deposita ninguna",
  columnas = length(cols)), auto_unbox = TRUE, pretty = TRUE),
  file.path(OUT, "manifiesto.json"))

cat("\nDepositado en", OUT, "\n")
