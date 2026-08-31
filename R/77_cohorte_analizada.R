library(jsonlite)

# Composicion de la cohorte analizada, unidad por unidad.
#
# La documentacion declara que las unidades con menos de quinientas estancias
# se descartan en el paso de particion, y que la cohorte analizada es por
# tanto menor que la ultima fila del embudo. La cifra no constaba en ningun
# archivo: R/22 imprime los tamanos por pantalla y deposita unicamente las
# matrices por paciente, excluidas del versionado.
#
# Aqui se deriva de agregados ya versionados, sin reejecutar nada. El cruce
# de categoria por unidad de la fase segunda contiene las dieciseis unidades
# de la cohorte, incluidas las que el umbral descarta, de modo que la
# composicion entera se obtiene de el.
#
# El umbral no es un resultado sino una eleccion de diseno, y consta en
# R/14_cohorte.R y en R/22_particion.R. Se declara aqui y se comprueba contra
# el archivo de unidades elegibles: si el conjunto que el umbral retiene no
# coincidiera exactamente con el que aquella fase publico, la derivacion no
# describiria la regla que se aplico.

UMBRAL <- 500
OUT <- "outputs/fase29"

RUTA_CRU <- "outputs/fase2/clase_por_unidad.csv"
RUTA_ELE <- "outputs/fase2/unidades_elegibles.csv"
RUTA_FLU <- "outputs/fase2/flujo.csv"
RUTA_LOU <- "outputs/fase10/cobertura_louo.csv"

detener <- function(...) {
  cat("\n", ..., "\n", sep = "")
  cat("El procedimiento se detiene. No procede publicar sobre esta base.\n")
  quit(status = 1)
}

for (r in c(RUTA_CRU, RUTA_ELE, RUTA_FLU, RUTA_LOU))
  if (!file.exists(r)) detener("Fuente ausente: ", r)

cru <- read.csv(RUTA_CRU, stringsAsFactors = FALSE)
ele <- read.csv(RUTA_ELE, stringsAsFactors = FALSE)
flu <- read.csv(RUTA_FLU, stringsAsFactors = FALSE)
lou <- read.csv(RUTA_LOU, stringsAsFactors = FALSE)

fi <- which(flu$paso == "p5_cohorte_final")
if (length(fi) != 1) detener("El embudo no declara la cohorte final.")
N <- flu$n[fi]

tot <- aggregate(n ~ unidad, data = cru, FUN = sum)
names(tot) <- c("unidad", "estancias")
tot <- tot[order(-tot$estancias), ]

if (sum(tot$estancias) != N)
  detener("El cruce por unidad no suma la cohorte final del embudo.")

tot$retenida <- tot$estancias >= UMBRAL

# El conjunto que el umbral retiene ha de ser el que la fase segunda publico.
if (!setequal(tot$unidad[tot$retenida], ele$unidad))
  detener("El umbral no reproduce las unidades elegibles publicadas.")

# La unidad reservada es la retenida que no figura entre las evaluadas
# dejando una fuera. Se identifica asi, y no por su nombre escrito aqui, para
# que la derivacion siga a los archivos. Los nombres publicados vienen
# truncados, de modo que se casan por prefijo y se exige correspondencia
# unica.
casar <- function(corto, largos) {
  i <- which(substr(largos, 1, nchar(corto)) == corto)
  if (length(i) != 1) detener("La sede no se identifica sin ambiguedad: ",
                              corto)
  largos[i]
}
desarrollo <- sapply(lou$unidad, casar, largos = tot$unidad[tot$retenida])
sellada <- setdiff(tot$unidad[tot$retenida], desarrollo)
if (length(sellada) != 1)
  detener("Las unidades retenidas no dejan una sola sede reservada.")

tot$grupo <- ifelse(!tot$retenida, "descartada",
             ifelse(tot$unidad == sellada, "reservada", "desarrollo"))
row.names(tot) <- NULL

cat("=== COMPOSICION DE LA COHORTE ===\n")
cat("Umbral de retencion:", UMBRAL, "estancias\n")
print(tot, row.names = FALSE)

resumen <- data.frame(
  grupo = c("retenidas", "descartadas", "cohorte final"),
  unidades = c(sum(tot$retenida), sum(!tot$retenida), nrow(tot)),
  estancias = c(sum(tot$estancias[tot$retenida]),
                sum(tot$estancias[!tot$retenida]),
                sum(tot$estancias)),
  row.names = NULL)

detalle <- data.frame(
  umbral = UMBRAL,
  unidades_retenidas = sum(tot$retenida),
  estancias_retenidas = sum(tot$estancias[tot$retenida]),
  unidades_descartadas = sum(!tot$retenida),
  estancias_descartadas = sum(tot$estancias[!tot$retenida]),
  estancias_cohorte_final = N,
  unidades_de_desarrollo = sum(tot$grupo == "desarrollo"),
  estancias_de_desarrollo = sum(tot$estancias[tot$grupo == "desarrollo"]),
  estancias_reservadas = sum(tot$estancias[tot$grupo == "reservada"]),
  row.names = NULL)

if (detalle$estancias_retenidas + detalle$estancias_descartadas != N)
  detener("Los dos grupos no suman la cohorte final.")
if (detalle$estancias_de_desarrollo + detalle$estancias_reservadas !=
    detalle$estancias_retenidas)
  detener("Desarrollo y reservada no suman las estancias retenidas.")

cat("\n=== RESUMEN ===\n")
print(resumen, row.names = FALSE)
cat("\nDe las retenidas,", detalle$estancias_de_desarrollo,
    "estancias en las unidades de desarrollo y",
    detalle$estancias_reservadas, "en la reservada.\n")

dir.create(OUT, recursive = TRUE, showWarnings = FALSE)
write.csv(tot, file.path(OUT, "unidades_cohorte.csv"), row.names = FALSE)
write.csv(detalle, file.path(OUT, "cohorte_analizada.csv"), row.names = FALSE)

writeLines(toJSON(list(
  fase = "29",
  ejecutado_en = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
  r_version = R.version.string,
  guarda_de_emparejamiento_parcial = isTRUE(getOption("warnPartialMatchDollar")),
  proposito = paste("declarar la composicion de la cohorte analizada tras",
                    "descartar las unidades por debajo del umbral"),
  umbral = UMBRAL,
  origen_del_umbral = "R/14_cohorte.R y R/22_particion.R",
  derivado_de = RUTA_CRU,
  se_reejecuta = FALSE,
  unidad_reservada = paste("identificada como la retenida que no figura",
                           "entre las evaluadas dejando una sede fuera")),
  auto_unbox = TRUE, pretty = TRUE, digits = 15),
  file.path(OUT, "manifiesto.json"))

cat("\nDepositado en", OUT, "\n")
