library(jsonlite)

options(scipen = 999)

# Depositos publicados, contrastados, y ninguna de las dos cosas.
#
# El deposito acumula ciento veintitantos archivos versionados. Algunos
# sostienen cifras del documento, otros existen para que una comprobacion
# tenga contra que correr, y otros no hacen ni lo uno ni lo otro porque el
# trabajo siguio por otro camino. Las tres situaciones son legitimas; lo que
# no lo es, es no saber cual es cual.
#
# El caso grave es el archivo que sostiene una cifra publicada y al que
# ninguna comprobacion alcanza: la cifra viaja sin red. Ese caso detiene este
# procedimiento.
#
# Ninguna de las dos listas se escribe aqui. Publicado es lo que lee un
# generador de prosa, que es el que escribe un documento y lo compone con la
# guardia que rechaza cifras literales. Contrastado es lo que figura en la
# relacion de fuentes de la verificacion de cifras, leida de su codigo.

OUT <- "outputs/fase44"
VERIF <- "R/59_verificar_cifras.R"

detener <- function(...) {
  cat("\n", ..., "\n", sep = "")
  cat("El procedimiento se detiene. No procede publicar sobre esta base.\n")
  quit(status = 1)
}

if (!dir.exists(".git")) detener("No hay historial que consultar.")
if (!file.exists(VERIF)) detener("Fuente ausente: ", VERIF)

fuentes <- sort(list.files("R", pattern = "[.]R$", full.names = TRUE))
texto <- setNames(lapply(fuentes, function(f)
  paste(readLines(f, warn = FALSE), collapse = "\n")), basename(fuentes))

generadores <- names(texto)[
  sapply(texto, grepl, pattern = 'writeLines\\([^,]+,\\s*"[^"]+[.]md"') &
  sapply(texto, grepl, pattern = "cifra <- +function")]
if (length(generadores) == 0)
  detener("Ningun generador de prosa compone documento.")

rutas_leidas <- function(t) {
  a <- regmatches(t, gregexpr('(leer|read[.]csv|fromJSON)\\(\\s*"[^"]+"', t))[[1]]
  unique(sub('.*"([^"]+)".*', "\\1", a))
}

publicadas <- unique(unlist(lapply(texto[generadores], rutas_leidas)))
publicadas <- publicadas[grepl("^outputs/", publicadas)]

# La relacion de fuentes de la verificacion. Se lee de su codigo porque es
# donde vive; copiarla aqui seria crear la segunda lista que este trabajo ya
# ha pagado dos veces.
contrastadas <- rutas_leidas(texto[[basename(VERIF)]])
contrastadas <- unique(c(contrastadas,
  sub('.*"([^"]+)".*', "\\1",
      regmatches(texto[[basename(VERIF)]],
                 gregexpr('=\\s*"outputs/[^"]+"',
                          texto[[basename(VERIF)]]))[[1]])))
contrastadas <- contrastadas[grepl("^outputs/", contrastadas)]
if (length(contrastadas) == 0)
  detener("No se puede leer la relacion de fuentes de la verificacion.")

versionados <- system("git ls-files 'outputs/*.csv' 'outputs/*/*.csv'",
                      intern = TRUE)
if (length(versionados) == 0) detener("No hay depositos versionados.")

tab <- data.frame(
  deposito = versionados,
  publicado = versionados %in% publicadas,
  contrastado = versionados %in% contrastadas,
  row.names = NULL)
tab$situacion <- ifelse(tab$publicado & tab$contrastado,
                        "publicado y contrastado",
                 ifelse(tab$publicado & !tab$contrastado,
                        "publicado sin contraste",
                 ifelse(!tab$publicado & tab$contrastado,
                        "contrasta lo que nadie publica",
                        "ni publicado ni contrastado")))
ORDEN <- c("publicado sin contraste", "publicado y contrastado",
           "contrasta lo que nadie publica", "ni publicado ni contrastado")
tab <- tab[order(match(tab$situacion, ORDEN), tab$deposito), ]

cat("=== DEPOSITOS VERSIONADOS ===\n")
for (k in ORDEN)
  cat(sprintf("  %-34s %3d\n", k, sum(tab$situacion == k)))
cat(sprintf("  %-34s %3d\n", "en total", nrow(tab)))

malos <- tab[tab$situacion == "publicado sin contraste", ]
if (nrow(malos) > 0) {
  cat("\nSostienen una cifra publicada y ninguna comprobacion los alcanza:\n")
  for (d in malos$deposito) cat("  ", d, "\n")
}

cat("\n=== NI PUBLICADO NI CONTRASTADO ===\n")
h <- tab$deposito[tab$situacion == "ni publicado ni contrastado"]
if (length(h) == 0) cat("  ninguno\n") else for (d in h) cat("  ", d, "\n")

dir.create(OUT, recursive = TRUE, showWarnings = FALSE)
write.csv(tab, file.path(OUT, "depositos.csv"), row.names = FALSE)
write.csv(data.frame(
  situacion = ORDEN,
  depositos = sapply(ORDEN, function(k) sum(tab$situacion == k)),
  row.names = NULL), file.path(OUT, "recuento.csv"), row.names = FALSE)

writeLines(toJSON(list(
  fase = "44",
  ejecutado_en = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
  r_version = R.version.string,
  guarda_de_emparejamiento_parcial = isTRUE(getOption("warnPartialMatchDollar")),
  proposito = paste("saber de cada deposito versionado si sostiene una cifra",
                    "publicada, si alguna comprobacion lo alcanza, o ninguna",
                    "de las dos cosas"),
  publicado = paste("lo lee un generador de prosa, que es el que escribe un",
                    "documento y lo compone con la guardia que rechaza cifras",
                    "literales"),
  contrastado = paste("figura en la relacion de fuentes de la verificacion de",
                      "cifras, leida de su codigo y no copiada aqui"),
  caso_grave = paste("publicado sin contraste: la cifra viaja sin red, y",
                     "detiene este procedimiento"),
  depositos = nrow(tab),
  publicados = sum(tab$publicado),
  contrastados = sum(tab$contrastado),
  huerfanos = sum(tab$situacion == "ni publicado ni contrastado")),
  auto_unbox = TRUE, pretty = TRUE), file.path(OUT, "manifiesto.json"))

if (nrow(malos) > 0)
  detener("Algun deposito sostiene una cifra publicada sin que ninguna ",
          "comprobacion lo alcance.")

cat("\nDepositado en", OUT, "\n")
