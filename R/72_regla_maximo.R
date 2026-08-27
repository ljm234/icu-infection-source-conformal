library(glmnet)
library(splines)
library(jsonlite)

# La regla del maximo, por metodo.
#
# La documentacion afirma que la regla del maximo no nombra jamas un foco, y
# que el resultado se observa bajo las dos reglas de penalizacion, bajo
# arboles potenciados y bajo el modelo ampliado. Las cuatro comprobaciones
# existen unicamente como impresion de consola en R/32, R/44 y R/55, de modo
# que ningun archivo versionado las sostiene.
#
# Este procedimiento reune las cuatro en una tabla. Tres de ellas no exigen
# reajuste alguno: las dos penalizaciones dejaron escritas las probabilidades
# por estancia sobre el conjunto de prueba, y el modelo ampliado quedo
# guardado, de modo que basta volver a predecir con el. La cuarta, la de
# arboles potenciados, no dejo modelo ni probabilidades, y su fila procede
# del deposito que R/44 escribe al reajustarse.
#
# Cada fila se contrasta contra la discriminacion ya publicada de su propio
# metodo antes de admitirse. Sin esa comprobacion, la tabla podria describir
# un modelo distinto del que el trabajo reporta y nada lo advertiria.
#
# Definiciones, declaradas aqui y repetidas en el manifiesto:
#   conclusiones minoritarias: estancias cuya categoria de probabilidad
#     maxima no es la categoria mayoritaria del conjunto de prueba.
#   exactitud aparente: proporcion de estancias cuya categoria de
#     probabilidad maxima coincide con la observada.

OUT <- "outputs/fase24"

RUTA_ESP  <- "outputs/fase7/especificacion.rds"
RUTA_1SE  <- "outputs/fase7/prob_prueba.csv"
RUTA_MIN  <- "outputs/fase8/prueba_con_conjuntos.csv"
RUTA_CMP  <- "outputs/fase7/comparacion_lambda.csv"
RUTA_GBM  <- "outputs/fase16/gbm_regla_maximo.csv"
RUTA_AMPM <- "data/derivados/modelo_ampliado.rds"
RUTA_AMPI <- "data/derivados/imputaciones_ampliadas.rds"
RUTA_AMPC <- "outputs/fase18/comparacion_ampliado.csv"

detener <- function(...) {
  cat("\n", ..., "\n", sep = "")
  cat("El procedimiento se detiene. No procede interpretar el resultado.\n")
  quit(status = 1)
}

tolerancia <- function(decimales) 10^(-decimales) / 2
TOL <- tolerancia(4)

for (r in c(RUTA_ESP, RUTA_1SE, RUTA_MIN, RUTA_CMP, RUTA_AMPM, RUTA_AMPI,
            RUTA_AMPC))
  if (!file.exists(r)) detener("Fuente ausente: ", r)

if (!file.exists(RUTA_GBM))
  detener("Falta el deposito de arboles potenciados: ", RUTA_GBM, "\n",
          "Procede ejecutar antes R/44_gbm.R, que lo escribe al reajustarse.")

esp <- readRDS(RUTA_ESP)
if (is.null(esp$clases)) detener("La especificacion no declara categorias.")
CLASES <- esp$clases

auc <- function(p, yb) {
  r <- rank(p); n1 <- sum(yb); n0 <- length(yb) - n1
  if (n1 == 0 || n0 == 0) return(NA)
  (sum(r[yb == 1]) - n1*(n1+1)/2) / (n1*n0)
}

# La categoria mayoritaria se determina sobre el propio conjunto evaluado y
# no se escribe aqui. El enunciado que se contrasta es que la regla del
# maximo no abandona esa categoria, cualquiera que resulte ser.
resumir <- function(m, etiqueta) {
  faltan <- setdiff(c(CLASES, "clase"), names(m))
  if (length(faltan) > 0)
    detener(etiqueta, ": faltan columnas ", paste(faltan, collapse = ", "))
  if (any(is.na(m[, CLASES])) || any(is.na(m$clase)))
    detener(etiqueta, ": hay valores ausentes.")
  if (!all(m$clase %in% CLASES))
    detener(etiqueta, ": hay categorias ajenas a la especificacion.")
  if (max(abs(rowSums(m[, CLASES]) - 1)) > 1e-6)
    detener(etiqueta, ": alguna fila no suma la unidad.")
  mayor <- CLASES[which.max(table(factor(m$clase, levels = CLASES)))]
  argm <- CLASES[apply(m[, CLASES], 1, which.max)]
  list(etiqueta = etiqueta,
       n = nrow(m),
       mayoritaria = mayor,
       minoritarias = sum(argm != mayor),
       exactitud = mean(argm == as.character(m$clase)),
       auc = sapply(CLASES, function(k) auc(m[[k]], as.integer(m$clase == k))))
}

# Contraste de procedencia. La discriminacion recompuesta ha de coincidir con
# la que el archivo publicado de ese metodo consigna, dentro del margen que
# permiten sus cuatro decimales.
verificar <- function(r, esperado, fuente) {
  d <- max(abs(r$auc[CLASES] - esperado[CLASES]))
  cat(sprintf("  %-22s discrepancia maxima con %s: %.6f\n",
              r$etiqueta, fuente, d))
  if (d >= TOL)
    detener(r$etiqueta, ": la discriminacion recompuesta no coincide con ",
            fuente, ".")
  invisible(TRUE)
}

cmp <- read.csv(RUTA_CMP, stringsAsFactors = FALSE)
if (!all(CLASES %in% cmp$clase))
  detener("La comparacion de penalizaciones no cubre las categorias.")
e1se <- setNames(cmp$auc_1se, cmp$clase)
emin <- setNames(cmp$auc_min, cmp$clase)

cat("=== REGLA DEL MAXIMO POR METODO ===\n\n")
cat("Comprobacion de procedencia\n")

r1 <- resumir(read.csv(RUTA_1SE, stringsAsFactors = FALSE),
              "penalizacion 1se")
verificar(r1, e1se, "outputs/fase7/comparacion_lambda.csv")

r2 <- resumir(read.csv(RUTA_MIN, stringsAsFactors = FALSE),
              "penalizacion minima")
verificar(r2, emin, "outputs/fase7/comparacion_lambda.csv")

# ---------------------------------------------------------------------------
# Modelo ampliado. Se recupera el objeto guardado y se vuelve a predecir. No
# se reajusta: reajustarlo exigiria reconstruir la distribucion nula de
# veinte permutaciones y no alteraria un solo coeficiente.
# ---------------------------------------------------------------------------

amp  <- readRDS(RUTA_AMPM)
impa <- readRDS(RUTA_AMPI)
Ma <- length(impa)
for (campo in c("modelo", "con_spline", "nudos_vit"))
  if (is.null(amp[[campo]]))
    detener("El modelo ampliado carece del campo: ", campo)

VITALES <- names(amp$nudos_vit)

base_spline <- function(x, nu, nombre) {
  b <- ns(x, knots = nu[2:3], Boundary.knots = nu[c(1,4)])
  colnames(b) <- paste0(nombre, "_s", seq_len(ncol(b)))
  as.matrix(b)
}

construir_amp <- function(d, vit_spline) {
  bl <- list()
  for (v in esp$con_spline) bl[[v]] <- base_spline(d[[v]], esp$nudos[[v]], v)
  for (v in VITALES)
    if (v %in% vit_spline)
      bl[[v]] <- base_spline(d[[v]], amp$nudos_vit[[v]], v)
  lineales_vit <- setdiff(VITALES, vit_spline)
  lin <- as.matrix(d[, c(esp$lineales, lineales_vit), drop = FALSE])
  otr <- data.frame(edad = d$edad,
                    lactato_medido = d$lactato_medido,
                    sexo_M = as.integer(d$sexo == "M"))
  uni <- model.matrix(~ unidad - 1, data = d)[, -1, drop = FALSE]
  cbind(do.call(cbind, bl), lin, as.matrix(otr), uni)
}

pru <- do.call(rbind, lapply(impa, function(x) x[x$grupo == "prueba", ]))
pru <- pru[pru$clase %in% CLASES, ]
pru$clase <- factor(as.character(pru$clase), levels = CLASES)
if (nrow(pru) == 0) detener("El conjunto de prueba ampliado esta vacio.")

Xp <- construir_amp(pru, amp$con_spline)
coef_amp <- coef(amp$modelo)
if (nrow(coef_amp[[1]]) - 1 != ncol(Xp))
  detener("La matriz reconstruida no tiene las columnas del modelo ampliado.")
if (!identical(rownames(coef_amp[[1]])[-1], colnames(Xp)))
  detener("Las columnas reconstruidas no coinciden con las del modelo ",
          "ampliado.")

p <- predict(amp$modelo, newx = Xp, type = "response")[, , 1]
ag <- aggregate(p, by = list(stay_id = pru$stay_id), FUN = mean)
et <- unique(pru[, c("stay_id", "clase")])
ma <- merge(ag, et, by = "stay_id")
if (nrow(ma) != length(unique(pru$stay_id)))
  detener("El promedio por estancia no recupera el conjunto de prueba ",
          "ampliado.")

r4 <- resumir(ma, "modelo ampliado")
ampc <- read.csv(RUTA_AMPC, stringsAsFactors = FALSE)
verificar(r4, setNames(ampc$auc_ampliado, ampc$clase), RUTA_AMPC)

# ---------------------------------------------------------------------------
# Arboles potenciados. Fila leida del deposito que R/44 escribe.
# ---------------------------------------------------------------------------

g <- read.csv(RUTA_GBM, stringsAsFactors = FALSE)
if (nrow(g) != 1) detener("El deposito de arboles potenciados no es una fila.")
for (cl in c("n", "mayoritaria", "conclusiones_minoritarias",
             "exactitud_aparente"))
  if (is.null(g[[cl]])) detener("El deposito de arboles carece de: ", cl)
cat("  arboles potenciados    fila leida de ", RUTA_GBM, "\n", sep = "")

filas <- list(r1, r2,
              list(etiqueta = "arboles potenciados", n = g$n,
                   mayoritaria = g$mayoritaria,
                   minoritarias = g$conclusiones_minoritarias,
                   exactitud = g$exactitud_aparente),
              r4)

tab <- do.call(rbind, lapply(filas, function(r)
  data.frame(metodo = r$etiqueta,
             n = as.integer(r$n),
             mayoritaria = r$mayoritaria,
             conclusiones_minoritarias = as.integer(r$minoritarias),
             exactitud_aparente = round(r$exactitud, 4),
             row.names = NULL)))

cat("\n=== CONCLUSIONES POR LA REGLA DEL MAXIMO ===\n")
print(tab, row.names = FALSE)

cat("\n=== AFIRMACION CONTRASTADA ===\n")
if (all(tab$conclusiones_minoritarias == 0)) {
  cat("Ninguna configuracion nombra un foco por la regla del maximo.\n")
  cat("Las cuatro coinciden y el archivo sostiene la afirmacion.\n")
} else {
  cat("Alguna configuracion si nombra un foco por la regla del maximo:\n")
  s <- tab[tab$conclusiones_minoritarias > 0, ]
  print(s[, c("metodo", "n", "conclusiones_minoritarias")], row.names = FALSE)
  cat("El archivo desmiente la afirmacion tal como esta redactada.\n")
}

dir.create(OUT, recursive = TRUE, showWarnings = FALSE)
write.csv(tab, file.path(OUT, "regla_maximo.csv"), row.names = FALSE)

writeLines(toJSON(list(
  fase = "24",
  ejecutado_en = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
  r_version = R.version.string,
  glmnet_version = as.character(packageVersion("glmnet")),
  proposito = paste("deposito de la regla del maximo en las cuatro",
                    "configuraciones evaluadas"),
  conjunto = "prueba",
  definicion_conclusion = paste("estancia cuya categoria de probabilidad",
                                "maxima no es la mayoritaria del conjunto"),
  definicion_exactitud = paste("proporcion de estancias cuya categoria de",
                               "probabilidad maxima coincide con la",
                               "observada"),
  procedencia = paste("cada fila se contrasta contra la discriminacion",
                      "publicada de su metodo antes de admitirse"),
  tolerancia_contraste = TOL,
  imputaciones_ampliadas = Ma), auto_unbox = TRUE, pretty = TRUE),
  file.path(OUT, "manifiesto.json"))

cat("\nDepositado en", OUT, "\n")
