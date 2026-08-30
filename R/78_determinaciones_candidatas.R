# ESTE ARCHIVO ES UNA RECONSTRUCCION POSTERIOR. Se redacta el 2026-08-30 para
# documentar que distingue a las determinaciones retenidas de las que no lo
# fueron. La seleccion se fijo el 2026-08-18, en R/19_matriz.R, doce dias
# antes. Lo que sigue NO es el criterio con que se eligieron: ese criterio no
# consta en el codigo, en ningun comentario ni en ningun manifiesto, y este
# procedimiento no lo suple ni pretende suplirlo. Describe atributos
# registrados, nada mas.

library(jsonlite)

# La lista de las setenta y tres procede de R/16_cobertura_labs.R, que retuvo
# los examenes presentes en tres mil estancias o mas dentro de la ventana de
# seis horas. Ese umbral es el unico criterio de seleccion que el deposito
# documenta, y opera antes del paso que aqui se describe.
#
# La lista de las diecisiete se lee del propio R/19_matriz.R en lugar de
# transcribirse. Transcrita, este archivo podria dejar de describir el modelo
# sin que nada lo advirtiera.
#
# Ninguna de las cifras que siguen mide desempeno. Nadie ajusto un modelo con
# las setenta y tres, de modo que no existe comparacion entre esa
# especificacion y la que se publica, y este procedimiento no la construye.
# La ausencia de esa comparacion se declara en el deposito.

OUT <- "outputs/fase30"
RUTA_COB <- "outputs/fase3/cobertura_labs.csv"
RUTA_MAT <- "R/19_matriz.R"
UMBRAL_ESTANCIAS <- 3000

detener <- function(...) {
  cat("\n", ..., "\n", sep = "")
  cat("El procedimiento se detiene. No procede publicar sobre esta base.\n")
  quit(status = 1)
}

for (r in c(RUTA_COB, RUTA_MAT))
  if (!file.exists(r)) detener("Fuente ausente: ", r)

cob <- read.csv(RUTA_COB, stringsAsFactors = FALSE)
cob$itemid <- as.character(cob$itemid)

# Extraccion de los identificadores retenidos. Se acota al bloque que los
# declara y se admiten digitos en el nombre de la variable, que los hay.
src <- readLines(RUTA_MAT, warn = FALSE)
i <- grep("^items <- c\\(", src)
j <- grep("^lista_ids", src)
if (length(i) != 1 || length(j) != 1 || j <= i)
  detener("No se localiza el bloque de identificadores en ", RUTA_MAT)
blk <- paste(src[i:(j-1)], collapse = " ")
m <- gregexpr("[a-z][a-z0-9_]*[[:space:]]*=[[:space:]]*\"[0-9]+\"", blk)
pares <- regmatches(blk, m)[[1]]
if (length(pares) == 0) detener("El bloque no declara identificador alguno.")
retenidos <- sub('.*"([0-9]+)".*', "\\1", pares)
nombres <- sub("[[:space:]]*=.*", "", pares)

cat("=== IDENTIFICADORES RETENIDOS, LEIDOS DE R/19 ===\n")
cat("Numero:", length(retenidos), "\n")
if (length(unique(retenidos)) != length(retenidos))
  detener("Hay identificadores repetidos en el bloque.")
if (!all(retenidos %in% cob$itemid))
  detener("Algun identificador retenido no figura entre los candidatos.")

cob$retenida <- cob$itemid %in% retenidos
cob$variable <- ifelse(cob$retenida,
                       nombres[match(cob$itemid, retenidos)], NA_character_)

tab <- data.frame(
  itemid = cob$itemid,
  etiqueta = cob$label,
  variable_en_el_modelo = cob$variable,
  fluido = cob$fluid,
  panel = cob$category,
  estancias = cob$estancias,
  cobertura_pct = cob$pct,
  retenida = cob$retenida,
  row.names = NULL)
tab <- tab[order(-tab$retenida, -tab$cobertura_pct), ]
row.names(tab) <- NULL

if (min(tab$estancias) < UMBRAL_ESTANCIAS)
  detener("Alguna candidata no alcanza el umbral que las selecciono.")

ret <- tab[tab$retenida, ]
des <- tab[!tab$retenida, ]

cat("\n=== RECUENTOS ===\n")
cat("Candidatas:", nrow(tab), "  retenidas:", nrow(ret),
    "  descartadas:", nrow(des), "\n")
cat("Cobertura de las retenidas:  ", min(ret$cobertura_pct), "a",
    max(ret$cobertura_pct), "por ciento\n")
cat("Cobertura de las descartadas:", min(des$cobertura_pct), "a",
    max(des$cobertura_pct), "por ciento\n")

# ---------------------------------------------------------------------------
# Separacion. Para cada atributo registrado se establece si distingue a los
# dos grupos. Un atributo separa si ningun valor suyo aparece a ambos lados;
# la cobertura separa si el maximo de las descartadas queda por debajo del
# minimo de las retenidas. No se construye ningun criterio: se comprueba si
# los atributos que constan bastarian para reproducir la particion.
# ---------------------------------------------------------------------------

separa_categoria <- function(v) {
  a <- unique(v[tab$retenida]); b <- unique(v[!tab$retenida])
  list(compartidos = length(intersect(a, b)),
       solo_retenidas = paste(sort(setdiff(a, b)), collapse = "; "),
       solo_descartadas = paste(sort(setdiff(b, a)), collapse = "; "),
       separa = length(intersect(a, b)) == 0)
}

sf <- separa_categoria(tab$fluido)
sp <- separa_categoria(tab$panel)
sc_separa <- max(des$cobertura_pct) < min(ret$cobertura_pct)
por_encima <- sum(des$cobertura_pct > min(ret$cobertura_pct))

sep <- data.frame(
  atributo = c("fluido", "panel", "cobertura"),
  valores_compartidos = c(sf$compartidos, sp$compartidos, NA),
  solo_en_retenidas = c(sf$solo_retenidas, sp$solo_retenidas, ""),
  solo_en_descartadas = c(sf$solo_descartadas, sp$solo_descartadas, ""),
  descartadas_por_encima_de_la_retenida_minima =
    c(NA, NA, por_encima),
  separa_los_grupos = c(sf$separa, sp$separa, sc_separa),
  row.names = NULL)

cat("\n=== SEPARACION POR ATRIBUTO ===\n")
print(sep, row.names = FALSE)

alguno <- any(sep$separa_los_grupos)
cat("\n=== LO QUE ESTO ESTABLECE ===\n")
if (alguno) {
  cat("Algun atributo registrado separa los dos grupos:\n")
  for (a in sep$atributo[sep$separa_los_grupos]) cat("  ", a, "\n")
} else {
  cat("Ningun atributo registrado separa los dos grupos. Ni el tipo de\n")
  cat("muestra, ni el panel, ni la cobertura reproducen la particion: cada\n")
  cat("uno toma valores comunes a retenidas y descartadas. La descripcion\n")
  cat("no permite por tanto reconstruir la regla, y el deposito lo declara\n")
  cat("en lugar de ofrecer una explicacion que los datos no sostienen.\n")
}
cat("\nDescartadas con cobertura superior a la retenida de menor cobertura:",
    por_encima, "\n")
cat("La de mayor cobertura entre las candidatas fue",
    if (tab$retenida[which.max(tab$cobertura_pct)]) "retenida" else
      "descartada", "\n")

dir.create(OUT, recursive = TRUE, showWarnings = FALSE)
write.csv(tab, file.path(OUT, "determinaciones_candidatas.csv"),
          row.names = FALSE)
write.csv(sep, file.path(OUT, "separacion_candidatas.csv"), row.names = FALSE)

writeLines(toJSON(list(
  fase = "30",
  ejecutado_en = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
  r_version = R.version.string,
  naturaleza = paste("reconstruccion posterior redactada el 2026-08-30; la",
                     "seleccion se fijo el 2026-08-18 en R/19_matriz.R"),
  no_es_el_criterio = paste("el criterio con que se eligieron las retenidas",
                            "no consta en el codigo ni en ningun manifiesto;",
                            "este deposito describe atributos registrados y",
                            "no lo suple"),
  umbral_previo = UMBRAL_ESTANCIAS,
  origen_del_umbral = "R/16_cobertura_labs.R",
  candidatas = nrow(tab),
  retenidas = nrow(ret),
  descartadas = nrow(des),
  algun_atributo_separa = alguno,
  comparacion_de_especificaciones = paste("no existe: no se ajusto modelo",
                                          "alguno con las candidatas no",
                                          "retenidas, de modo que no se ha",
                                          "medido si una especificacion mas",
                                          "amplia rendiria mejor")),
  auto_unbox = TRUE, pretty = TRUE, digits = 15),
  file.path(OUT, "manifiesto.json"))

cat("\nDepositado en", OUT, "\n")
