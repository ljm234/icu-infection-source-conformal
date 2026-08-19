library(glmnet)
library(splines)
library(jsonlite)

SEMILLA <- 20260818
set.seed(SEMILLA)

ALFA_CONFORMAL <- 0.10

OUT <- "outputs/fase8"
dir.create(OUT, recursive = TRUE, showWarnings = FALSE)

esp  <- readRDS("outputs/fase7/especificacion.rds")
imps <- readRDS("outputs/fase6/imputaciones.rds")
M <- length(imps)
CLASES <- esp$clases

base_ent <- imps[[1]][imps[[1]]$grupo == "entrenamiento", ]
niveles_ent <- sort(unique(base_ent$unidad))

imps <- lapply(imps, function(x) {
  x$unidad_mod <- factor(ifelse(x$unidad %in% niveles_ent,
                                as.character(x$unidad), niveles_ent[1]),
                         levels = niveles_ent)
  x
})

construir <- function(datos) {
  bl <- list()
  for (v in esp$con_spline) {
    nu <- esp$nudos[[v]]
    b <- ns(datos[[v]], knots = nu[2:3], Boundary.knots = nu[c(1,4)])
    colnames(b) <- paste0(v, "_s", seq_len(ncol(b)))
    bl[[v]] <- as.matrix(b)
  }
  lin <- as.matrix(datos[, esp$lineales, drop = FALSE])
  otr <- data.frame(edad = datos$edad,
                    lactato_medido = datos$lactato_medido,
                    sexo_M = as.integer(datos$sexo == "M"))
  uni <- model.matrix(~ unidad_mod - 1, data = datos)[, -1, drop = FALSE]
  cbind(do.call(cbind, bl), lin, as.matrix(otr), uni)
}

apilar <- function(g) {
  p <- do.call(rbind, lapply(imps, function(x) x[x$grupo == g, ]))
  p <- p[p$clase %in% CLASES, ]
  p$clase <- factor(as.character(p$clase), levels = CLASES)
  p
}

# ---------------------------------------------------------------------------
# Ajuste definitivo
#
# Se adopta lambda.min tras la comparacion preespecificada frente a
# lambda.1se, que arrojo una mejora de 0.0395 en el area bajo la curva
# promediada sobre las clases minoritarias, sin que ninguna clase perdiera
# mas de 0.02. La regla de un error estandar resulta excesivamente
# conservadora bajo el desbalance presente, dado que el error estandar queda
# dominado por la clase mayoritaria y suprime la senal de las restantes.
# ---------------------------------------------------------------------------

ent <- apilar("entrenamiento")
X <- construir(ent); y <- ent$clase; w <- rep(1/M, nrow(X))

pacientes <- unique(ent$stay_id)
asign <- sample(rep(1:10, length.out = length(pacientes)))
names(asign) <- pacientes
foldid <- asign[as.character(ent$stay_id)]

cat("Ajustando el modelo definitivo.\n")
cv <- cv.glmnet(X, y, family = "multinomial", alpha = 1, weights = w,
                foldid = foldid, type.measure = "deviance", standardize = TRUE)
LAMBDA <- cv$lambda.min

modelo <- glmnet(X, y, family = "multinomial", alpha = 1,
                 weights = w, lambda = LAMBDA, standardize = TRUE)

cat("Lambda:", signif(LAMBDA, 4), "\n")
cat("Coeficientes no nulos:",
    sum(sapply(coef(modelo), function(z) sum(z[-1] != 0))), "\n")

predecir <- function(grupo) {
  g <- apilar(grupo)
  p <- predict(modelo, newx = construir(g), type = "response")[, , 1]
  ag <- aggregate(p, by = list(stay_id = g$stay_id), FUN = mean)
  et <- unique(g[, c("stay_id","clase","unidad")])
  merge(ag, et, by = "stay_id")
}

cal <- predecir("calibracion")
pru <- predecir("prueba")

# ---------------------------------------------------------------------------
# Prediccion conforme condicional por clase
#
# La puntuacion de no conformidad de un caso respecto de una clase es el
# complemento de la probabilidad que el modelo le asigna. Un valor bajo
# indica que el modelo no encuentra al caso extrano bajo esa clase.
#
#   s(x, k) = 1 - p(k | x)
#
# El umbral se calcula por separado dentro de cada clase, sobre el conjunto
# de calibracion. La correccion de muestra finita emplea el cuantil de orden
# ceil((n+1)(1-alpha))/n, que se deriva de que la puntuacion del caso nuevo
# ocupa una posicion uniforme entre las n+1 puntuaciones intercambiables.
#
# La calibracion separada por clase confiere la garantia
#
#   P(Y en C(X) | Y = k) >= 1 - alpha  para toda k
#
# La version marginal resulta insuficiente en esta cohorte: con el ochenta y
# ocho por ciento de los casos concentrado en una sola clase, un umbral
# global puede satisfacer la cobertura nominal cubriendo bien la clase
# mayoritaria mientras las restantes quedan por debajo.
# ---------------------------------------------------------------------------

umbral_conforme <- function(scores, alpha) {
  n <- length(scores)
  k <- ceiling((n + 1) * (1 - alpha))
  if (k > n) return(Inf)
  sort(scores)[k]
}

cat("\n=== CALIBRACION CONDICIONAL POR CLASE ===\n")
umbrales <- sapply(CLASES, function(k) {
  sub <- cal[cal$clase == k, ]
  s <- 1 - sub[[k]]
  umbral_conforme(s, ALFA_CONFORMAL)
})

tab_um <- data.frame(
  clase = CLASES,
  n_calibracion = sapply(CLASES, function(k) sum(cal$clase == k)),
  umbral = round(umbrales, 4),
  prob_minima_para_entrar = round(1 - umbrales, 4),
  sd_cobertura = round(sqrt(ALFA_CONFORMAL * (1 - ALFA_CONFORMAL) /
                       sapply(CLASES, function(k) sum(cal$clase == k))), 4),
  row.names = NULL)
print(tab_um, row.names = FALSE)

# ---------------------------------------------------------------------------
# Construccion de los conjuntos de prediccion
# ---------------------------------------------------------------------------

construir_conjunto <- function(datos) {
  pert <- sapply(CLASES, function(k) (1 - datos[[k]]) <= umbrales[k])
  colnames(pert) <- CLASES
  pert
}

pert_pru <- construir_conjunto(pru)

pru$tamano <- rowSums(pert_pru)
pru$cubierto <- sapply(seq_len(nrow(pru)), function(i)
  pert_pru[i, as.character(pru$clase[i])])
pru$conjunto <- apply(pert_pru, 1, function(r)
  if (sum(r) == 0) "vacio" else paste(CLASES[r], collapse = "+"))

cat("\n=== COBERTURA POR CLASE EN EL CONJUNTO DE PRUEBA ===\n")
cob <- do.call(rbind, lapply(CLASES, function(k) {
  s <- pru[pru$clase == k, ]
  n <- nrow(s); c1 <- sum(s$cubierto)
  ic <- if (n > 0) binom.test(c1, n)$conf.int else c(NA, NA)
  data.frame(clase = k, n = n,
             cobertura = round(c1 / n, 4),
             ic_inferior = round(ic[1], 4),
             ic_superior = round(ic[2], 4),
             nominal = 1 - ALFA_CONFORMAL,
             tamano_medio = round(mean(s$tamano), 3),
             row.names = NULL)
}))
print(cob, row.names = FALSE)

cat("\nCobertura marginal:", round(mean(pru$cubierto), 4), "\n")

cat("\n=== DISTRIBUCION DEL TAMANO DEL CONJUNTO ===\n")
print(table(tamano = pru$tamano))

cat("\n=== CONJUNTOS MAS FRECUENTES ===\n")
print(head(sort(table(pru$conjunto), decreasing = TRUE), 10))

# ---------------------------------------------------------------------------
# Abstencion
#
# El sistema emite conclusion unicamente cuando el conjunto contiene una sola
# clase. Un conjunto de mayor tamano significa que la informacion disponible
# no permite discriminar con la certeza requerida, y el caso se deriva. El
# conjunto vacio indica que ninguna clase resulta compatible con el perfil
# observado y tambien conduce a derivacion.
# ---------------------------------------------------------------------------

pru$decide <- pru$tamano == 1
decididos <- pru[pru$decide, ]
unica <- sapply(which(pru$decide), function(i) CLASES[pert_pru[i, ]][1])

cat("\n=== COMPORTAMIENTO DE LA ABSTENCION ===\n")
cat("Casos con conclusion:", sum(pru$decide),
    sprintf("(%.1f%%)\n", 100 * mean(pru$decide)))
cat("Casos derivados:", sum(!pru$decide),
    sprintf("(%.1f%%)\n", 100 * mean(!pru$decide)))
if (sum(pru$decide) > 0) {
  acierto <- mean(unica == as.character(decididos$clase))
  cat("Acierto entre los casos con conclusion:", round(acierto, 4), "\n")
  cat("Error entre los casos con conclusion:", round(1 - acierto, 4), "\n")
  cat("\nDistribucion de las conclusiones emitidas:\n")
  print(table(emitida = unica, real = decididos$clase))
}

# La curva de riesgo y cobertura describe el compromiso entre la proporcion
# de casos retenidos y el error cometido entre ellos. Se construye ordenando
# los casos por la probabilidad maxima predicha y evaluando el error a
# medida que se admiten fracciones crecientes de la cohorte.
cat("\n=== CURVA DE RIESGO Y COBERTURA ===\n")
pmax_v <- apply(pru[, CLASES], 1, max)
argmax_v <- CLASES[apply(pru[, CLASES], 1, which.max)]
correcto <- argmax_v == as.character(pru$clase)
ord <- order(pmax_v, decreasing = TRUE)
fracs <- seq(0.1, 1, by = 0.1)
rc <- do.call(rbind, lapply(fracs, function(f) {
  k <- max(1, floor(f * nrow(pru)))
  data.frame(fraccion_retenida = f,
             error = round(1 - mean(correcto[ord[1:k]]), 4),
             row.names = NULL)
}))
print(rc, row.names = FALSE)
cat("Area bajo la curva de riesgo y cobertura:",
    round(mean(rc$error), 4), "\n")

saveRDS(modelo, file.path(OUT, "modelo_final.rds"))
write.csv(cal,    file.path(OUT, "prob_calibracion.csv"), row.names = FALSE)
write.csv(pru,    file.path(OUT, "prueba_con_conjuntos.csv"), row.names = FALSE)
write.csv(tab_um, file.path(OUT, "umbrales.csv"), row.names = FALSE)
write.csv(cob,    file.path(OUT, "cobertura.csv"), row.names = FALSE)
write.csv(rc,     file.path(OUT, "riesgo_cobertura.csv"), row.names = FALSE)

writeLines(toJSON(list(
  fase = "8",
  ejecutado_en = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
  semilla = SEMILLA,
  alfa = ALFA_CONFORMAL,
  cobertura_nominal = 1 - ALFA_CONFORMAL,
  lambda = LAMBDA,
  regla_lambda = "min",
  justificacion_lambda = "comparacion preespecificada frente a 1se",
  puntuacion = "1 - probabilidad predicha",
  calibracion = "condicional por clase",
  umbrales = as.list(round(umbrales, 6)),
  cobertura_marginal = round(mean(pru$cubierto), 4),
  proporcion_con_conclusion = round(mean(pru$decide), 4)),
  auto_unbox = TRUE, pretty = TRUE),
  file.path(OUT, "manifiesto.json"))

cat("\nGuardado en", OUT, "\n")
