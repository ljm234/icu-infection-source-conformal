library(glmnet)
library(splines)
library(jsonlite)

# Validacion interna de la eleccion de la penalizacion.
#
# R/32 comparo lambda.1se con lambda.min evaluando en el conjunto de prueba,
# y la discriminacion y la cobertura que el trabajo publica se miden sobre
# ese mismo conjunto. La eleccion de la penalizacion es, por tanto, una
# decision de diseno adoptada a la vista del conjunto de evaluacion.
#
# Este procedimiento pregunta si la decision habria sido la misma sin mirar
# prueba. Reproduce la comparacion entera dentro del conjunto de
# entrenamiento: una porcion ajusta, la otra evalua, y el criterio
# preespecificado se aplica sin modificacion sobre esa segunda porcion.
#
# Repetir la comparacion con la devianza de validacion cruzada no responderia
# nada. lambda.min es por definicion el minimo de esa devianza y ganaria por
# construccion. El criterio ha de medirse con la misma cantidad que empleo el
# original, el area bajo la curva promediada sobre las categorias poco
# frecuentes, y sobre observaciones que no intervinieron en el ajuste.
#
# Los nudos proceden de outputs/fase7/especificacion.rds y no se recalculan,
# como exige la comparabilidad con la especificacion original. Se fijaron
# sobre la totalidad del entrenamiento, de modo que las estancias que aqui
# forman la validacion interna contribuyeron a situarlos. El efecto es comun
# a ambas penalizaciones y no favorece a ninguna, pero conviene consignarlo:
# la validacion interna no es enteramente ciega respecto de la base de
# splines. Recalcular los nudos sobre la porcion de ajuste produciria una
# base distinta y la comparacion dejaria de referirse al modelo publicado.
#
# La comparacion no es simetrica, y conviene advertirlo antes de leer el
# resultado. La porcion de ajuste son dos tercios del entrenamiento, mientras
# que el modelo publicado se estimo sobre el entrenamiento completo. Con menos
# observaciones la penalizacion optima aumenta, de modo que el procedimiento
# favorece a lambda.1se. La asimetria determina lo que cada desenlace autoriza
# a concluir. Si prevalece lambda.min, lo hace pese a esa desventaja y el
# resultado es concluyente en esa direccion. Si prevalece lambda.1se, el
# resultado no distingue entre una decision original erronea y el efecto del
# ajuste reducido, y no autoriza a afirmar lo primero.
#
# Ninguna cifra de resultado se escribe a mano. Las dos unicas constantes
# numericas que este procedimiento declara son los umbrales del criterio
# preespecificado, que proceden de R/32 y se reproducen sin alteracion.

SEMILLA <- 20260818
set.seed(SEMILLA)

PROPORCION_AJUSTE <- 2 / 3
PLIEGUES <- 10

# Criterio preespecificado, tal como consta en R/32: se adopta el valor
# minimo si la mejora promedio en las categorias poco frecuentes supera el
# umbral y ninguna de ellas empeora mas de esa misma cantidad.
MEJORA_MINIMA  <- 0.02
PERDIDA_MAXIMA <- 0.02

OUT <- "outputs/fase22"

detener <- function(...) {
  cat("\n", ..., "\n", sep = "")
  cat("El procedimiento se detiene. No procede interpretar el resultado.\n")
  quit(status = 1)
}

# ---------------------------------------------------------------------------
# Lectura y comprobacion de las fuentes
# ---------------------------------------------------------------------------

RUTA_ESP <- "outputs/fase7/especificacion.rds"
RUTA_IMP <- "outputs/fase6/imputaciones.rds"
RUTA_MAN <- "outputs/fase7/manifiesto.json"
RUTA_HIP <- "outputs/fase7/busqueda_hiperparametros.csv"
RUTA_CMP <- "outputs/fase7/comparacion_lambda.csv"

for (r in c(RUTA_ESP, RUTA_IMP, RUTA_MAN, RUTA_HIP, RUTA_CMP))
  if (!file.exists(r)) detener("Fuente ausente: ", r)

esp <- readRDS(RUTA_ESP)
for (campo in c("nudos", "con_spline", "lineales", "niveles", "clases"))
  if (is.null(esp[[campo]]))
    detener("La especificacion carece del campo: ", campo)

CLASES <- esp$clases
MINORITARIAS <- CLASES[-1]
if (length(CLASES) != 4 || length(MINORITARIAS) != 3)
  detener("La especificacion no declara cuatro categorias modeladas.")

for (v in esp$con_spline)
  if (length(esp$nudos[[v]]) != 4)
    detener("Los nudos de ", v, " no son cuatro.")

# El parametro de mezcla se lee del manifiesto de la fase que lo eligio, en
# lugar de repetirse aqui. Si la fase septima cambiara de alfa sin que este
# procedimiento se actualizara, la comparacion dejaria de referirse al modelo
# publicado y el desajuste pasaria inadvertido.
man <- fromJSON(RUTA_MAN)
if (is.null(man$alpha)) detener("El manifiesto de la fase 7 no declara alfa.")
ALFA <- as.numeric(man$alpha)

hip <- read.csv(RUTA_HIP, stringsAsFactors = FALSE)
fila_alfa <- which(abs(hip$alpha - ALFA) < 1e-9)
if (length(fila_alfa) != 1)
  detener("La busqueda de hiperparametros no contiene una fila para alfa.")

imps <- readRDS(RUTA_IMP)
M <- length(imps)
if (M < 2) detener("Las imputaciones no forman un conjunto multiple.")

cat("=== FUENTES ===\n")
cat("Categorias modeladas:", paste(CLASES, collapse = ", "), "\n")
cat("Imputaciones apiladas:", M, "\n")
cat("Parametro de mezcla leido del manifiesto:", ALFA, "\n")

# ---------------------------------------------------------------------------
# Especificacion. Se reproduce la de R/32 y R/33 sin variacion.
# ---------------------------------------------------------------------------

base_ent <- imps[[1]][imps[[1]]$grupo == "entrenamiento", ]
niveles_ent <- sort(unique(base_ent$unidad))

if (!identical(as.character(niveles_ent), as.character(esp$niveles)))
  detener("Los niveles de unidad no coinciden con los de la especificacion.")

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
  # Este procedimiento no debe alcanzar calibracion, prueba ni el conjunto
  # sellado. La restriccion se impone aqui, y no queda confiada a que ninguna
  # linea posterior solicite otra cosa.
  if (g != "entrenamiento")
    detener("Se ha solicitado un conjunto ajeno al entrenamiento: ", g)
  p <- do.call(rbind, lapply(imps, function(x) x[x$grupo == g, ]))
  p <- p[p$clase %in% CLASES, ]
  p$clase <- factor(as.character(p$clase), levels = CLASES)
  p
}

auc <- function(p, yb) {
  r <- rank(p); n1 <- sum(yb); n0 <- length(yb) - n1
  if (n1 == 0 || n0 == 0) return(NA)
  (sum(r[yb == 1]) - n1*(n1+1)/2) / (n1*n0)
}

# ---------------------------------------------------------------------------
# Conjunto de trabajo. Solo entrenamiento.
# ---------------------------------------------------------------------------

ent <- apilar("entrenamiento")

if (!all(ent$grupo == "entrenamiento"))
  detener("El conjunto de trabajo contiene filas ajenas al entrenamiento.")

copias <- table(ent$stay_id)
if (!all(copias == M))
  detener("Alguna estancia no aparece exactamente una vez por imputacion.")

etiquetas <- unique(ent[, c("stay_id", "clase")])
etiquetas <- etiquetas[order(etiquetas$stay_id), ]
if (nrow(etiquetas) != length(copias))
  detener("Alguna estancia presenta mas de una categoria.")

n_pacientes <- nrow(etiquetas)
cat("Estancias de entrenamiento en las categorias modeladas:",
    n_pacientes, "\n")
cat("Filas apiladas:", nrow(ent), "\n")

# El criterio promedia sobre las categorias poco frecuentes, definidas como
# todas menos la primera de la especificacion. Se comprueba que esa primera
# sea en efecto la mayoritaria en lugar de darlo por supuesto: si el orden
# de la especificacion cambiara, el promedio se calcularia sobre el conjunto
# equivocado sin que nada lo advirtiera.
frecuencia <- table(etiquetas$clase)
if (names(frecuencia)[which.max(frecuencia)] != CLASES[1])
  detener("La primera categoria de la especificacion no es la mayoritaria.")

# ---------------------------------------------------------------------------
# Particion interna, por estancia y estratificada por categoria
#
# La particion se realiza sobre la estancia y no sobre la fila: cada estancia
# aparece M veces en la matriz apilada, y repartir sus copias entre ajuste y
# validacion evaluaria pacientes ya vistos. Se estratifica por categoria
# siguiendo el criterio de R/22, dado que sin estratificar las categorias
# poco frecuentes podrian quedar concentradas en una porcion por azar, y son
# precisamente esas las que el criterio pondera.
#
# Se emplea sample.int sobre la longitud en lugar de sample sobre el vector:
# ante un vector de un solo elemento, la segunda forma permutaria la sucesion
# de enteros hasta ese valor en lugar del propio vector.
# ---------------------------------------------------------------------------

ajuste <- unlist(lapply(CLASES, function(k) {
  ids <- etiquetas$stay_id[etiquetas$clase == k]
  if (length(ids) < 2)
    detener("La categoria ", k, " no admite particion en entrenamiento.")
  ids <- ids[sample.int(length(ids))]
  ids[seq_len(floor(PROPORCION_AJUSTE * length(ids)))]
}))

validacion <- setdiff(etiquetas$stay_id, ajuste)

if (length(intersect(ajuste, validacion)) > 0)
  detener("Las dos porciones comparten estancias.")
if (length(ajuste) + length(validacion) != n_pacientes)
  detener("La particion no cubre el conjunto de entrenamiento.")

recuento <- function(ids)
  as.integer(table(factor(as.character(etiquetas$clase[
    etiquetas$stay_id %in% ids]), levels = CLASES)))

part <- data.frame(
  porcion = c("ajuste", "validacion interna"),
  pacientes = c(length(ajuste), length(validacion)),
  rbind(recuento(ajuste), recuento(validacion)),
  row.names = NULL)
names(part)[3:(2 + length(CLASES))] <- CLASES

cat("\n=== PARTICION INTERNA ===\n")
print(part, row.names = FALSE)

if (any(part[, MINORITARIAS] < 1))
  detener("Alguna categoria poco frecuente queda vacia en una porcion.")

fa <- ent[ent$stay_id %in% ajuste, ]
fv <- ent[ent$stay_id %in% validacion, ]

X_aj <- construir(fa); y_aj <- fa$clase; w_aj <- rep(1/M, nrow(X_aj))
X_val <- construir(fv)

if (!identical(colnames(X_aj), colnames(X_val)))
  detener("Las dos porciones no producen la misma matriz de diseno.")

cat("Matriz de ajuste:", nrow(X_aj), "filas,", ncol(X_aj), "columnas\n")
cat("Matriz de validacion interna:", nrow(X_val), "filas\n")

# ---------------------------------------------------------------------------
# Ambas penalizaciones, obtenidas solo sobre la porcion de ajuste
# ---------------------------------------------------------------------------

pac_aj <- unique(fa$stay_id)
asign <- sample(rep(seq_len(PLIEGUES), length.out = length(pac_aj)))
names(asign) <- pac_aj
foldid <- asign[as.character(fa$stay_id)]

cat("\nAjustando la ruta completa de lambda sobre la porcion de ajuste.\n")
inicio <- Sys.time()
cv <- cv.glmnet(X_aj, y_aj, family = "multinomial", alpha = ALFA,
                weights = w_aj, foldid = foldid,
                type.measure = "deviance", standardize = TRUE)
cat("Tiempo:", round(difftime(Sys.time(), inicio, units = "mins"), 1),
    "min\n")

if (!(cv$lambda.1se >= cv$lambda.min))
  detener("La regla de un error estandar no domina al minimo.")

# ---------------------------------------------------------------------------
# Evaluacion en la validacion interna, con el criterio del original
# ---------------------------------------------------------------------------

evaluar <- function(lam) {
  mod <- glmnet(X_aj, y_aj, family = "multinomial", alpha = ALFA,
                weights = w_aj, lambda = lam, standardize = TRUE)
  p <- predict(mod, newx = X_val, type = "response")[, , 1]
  ag <- aggregate(p, by = list(stay_id = fv$stay_id), FUN = mean)
  et <- unique(fv[, c("stay_id", "clase")])
  m <- merge(ag, et, by = "stay_id")
  if (nrow(m) != length(validacion))
    detener("El promedio por estancia no recupera la validacion interna.")
  a <- sapply(CLASES, function(k) auc(m[[k]], as.integer(m$clase == k)))
  nz <- sum(sapply(coef(mod), function(z) sum(z[-1] != 0)))
  list(auc = a, no_nulos = nz,
       pred_min = sum(apply(m[, CLASES], 1, which.max) != 1))
}

r1 <- evaluar(cv$lambda.1se)
r2 <- evaluar(cv$lambda.min)

if (any(is.na(r1$auc)) || any(is.na(r2$auc)))
  detener("Alguna categoria no admite estimacion del area en validacion.")

tab <- data.frame(
  clase = CLASES,
  auc_1se = round(r1$auc, 4),
  auc_min = round(r2$auc, 4),
  diferencia = round(r2$auc - r1$auc, 4),
  row.names = NULL)

cat("\n=== COMPARACION EN VALIDACION INTERNA ===\n")
print(tab, row.names = FALSE)

prom1 <- mean(r1$auc[MINORITARIAS])
prom2 <- mean(r2$auc[MINORITARIAS])
mejora <- prom2 - prom1
peor <- min(r2$auc[MINORITARIAS] - r1$auc[MINORITARIAS])

cat("\nArea promedio en categorias poco frecuentes\n")
cat("  lambda.1se:", round(prom1, 4), "\n")
cat("  lambda.min:", round(prom2, 4), "\n")
cat("  diferencia:", round(mejora, 4), "\n")
cat("\nCoeficientes no nulos: 1se =", r1$no_nulos,
    " min =", r2$no_nulos, "\n")
cat("Estancias con categoria poco frecuente como argmax: 1se =",
    r1$pred_min, " min =", r2$pred_min, "\n")

cumple_mejora  <- mejora > MEJORA_MINIMA
cumple_perdida <- peor > -PERDIDA_MAXIMA
decision_interna <- if (cumple_mejora && cumple_perdida)
  "lambda.min" else "lambda.1se"

cat("\n=== DECISION SEGUN EL CRITERIO PREESPECIFICADO ===\n")
cat("Mejora exigida:", MEJORA_MINIMA, " observada:", round(mejora, 4),
    if (cumple_mejora) " cumple\n" else " no cumple\n")
cat("Perdida admitida:", PERDIDA_MAXIMA, " peor cambio:", round(peor, 4),
    if (cumple_perdida) " cumple\n" else " no cumple\n")
cat("RESULTADO:", decision_interna, "\n")

# ---------------------------------------------------------------------------
# La decision original, recompuesta desde su propio archivo
#
# No se transcribe: se lee de outputs/fase7/comparacion_lambda.csv y se le
# aplica el mismo criterio. Si el archivo no sostuviera la decision que el
# proyecto declara haber tomado, la comparacion carecerian de referencia y el
# procedimiento se detiene.
# ---------------------------------------------------------------------------

cmp <- read.csv(RUTA_CMP, stringsAsFactors = FALSE)
if (!all(CLASES %in% cmp$clase))
  detener("La comparacion original no cubre las categorias modeladas.")

o1 <- setNames(cmp$auc_1se, cmp$clase)[MINORITARIAS]
o2 <- setNames(cmp$auc_min, cmp$clase)[MINORITARIAS]
mejora_orig <- mean(o2) - mean(o1)
peor_orig <- min(o2 - o1)
decision_original <- if (mejora_orig > MEJORA_MINIMA &&
                         peor_orig > -PERDIDA_MAXIMA)
  "lambda.min" else "lambda.1se"

if (decision_original != "lambda.min")
  detener("El archivo de la fase 7 no sostiene la decision publicada.")

coinciden <- decision_interna == decision_original

cat("\n=== CONTRASTE CON LA DECISION ORIGINAL ===\n")
cat("Decision en prueba, recompuesta desde la fase 7:",
    decision_original, "\n")
cat("  mejora:", round(mejora_orig, 4),
    " peor cambio:", round(peor_orig, 4), "\n")
cat("Decision en validacion interna:", decision_interna, "\n")
cat("  mejora:", round(mejora, 4), " peor cambio:", round(peor, 4), "\n")
cat("Coinciden:", if (coinciden) "si" else "NO", "\n")

if (coinciden) {
  cat("\nLa eleccion de la penalizacion no depende de haber mirado prueba:\n")
  cat("el mismo criterio, aplicado sobre datos apartados dentro del propio\n")
  cat("entrenamiento, conduce a la misma penalizacion.\n")
} else {
  cat("\nLa eleccion de la penalizacion no se reproduce sin mirar prueba.\n")
  cat("La decision publicada depende del conjunto sobre el que despues se\n")
  cat("reportan la discriminacion y la cobertura, y procede consignarlo.\n")
}

# ---------------------------------------------------------------------------
# Deposito
# ---------------------------------------------------------------------------

dec <- data.frame(
  pacientes_ajuste          = length(ajuste),
  pacientes_validacion      = length(validacion),
  lambda_1se_interno        = cv$lambda.1se,
  lambda_min_interno        = cv$lambda.min,
  lambda_1se_fase7          = hip$lambda_1se[fila_alfa],
  lambda_min_fase7          = hip$lambda_min[fila_alfa],
  no_nulos_1se              = r1$no_nulos,
  no_nulos_min              = r2$no_nulos,
  argmax_minoritario_1se    = r1$pred_min,
  argmax_minoritario_min    = r2$pred_min,
  promedio_minoritarias_1se = round(prom1, 4),
  promedio_minoritarias_min = round(prom2, 4),
  mejora_interna            = round(mejora, 4),
  peor_cambio_interno       = round(peor, 4),
  mejora_original           = round(mejora_orig, 4),
  peor_cambio_original      = round(peor_orig, 4),
  umbral_mejora             = MEJORA_MINIMA,
  umbral_perdida            = PERDIDA_MAXIMA,
  cumple_mejora             = cumple_mejora,
  cumple_perdida            = cumple_perdida,
  decision_interna          = decision_interna,
  decision_original         = decision_original,
  coinciden                 = coinciden,
  row.names = NULL)

dir.create(OUT, recursive = TRUE, showWarnings = FALSE)
write.csv(tab, file.path(OUT, "comparacion_lambda_interna.csv"),
          row.names = FALSE)
write.csv(dec, file.path(OUT, "decision_lambda.csv"), row.names = FALSE)
write.csv(part, file.path(OUT, "particion_interna.csv"), row.names = FALSE)

writeLines(toJSON(list(
  fase = "22",
  ejecutado_en = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
  semilla = SEMILLA,
  r_version = R.version.string,
  glmnet_version = as.character(packageVersion("glmnet")),
  proposito = paste("contraste de la eleccion de lambda sin emplear el",
                    "conjunto de prueba"),
  conjunto_empleado = "entrenamiento",
  particion = "por estancia, estratificada por categoria",
  proporcion_ajuste = PROPORCION_AJUSTE,
  pliegues_validacion_cruzada = PLIEGUES,
  alpha = ALFA,
  imputaciones_apiladas = M,
  criterio = paste("area bajo la curva promediada sobre las categorias",
                   "poco frecuentes"),
  umbral_mejora = MEJORA_MINIMA,
  umbral_perdida = PERDIDA_MAXIMA,
  nudos = "heredados de outputs/fase7/especificacion.rds, sin recalcular",
  columnas_diseno = ncol(X_aj)), auto_unbox = TRUE, pretty = TRUE),
  file.path(OUT, "manifiesto.json"))

cat("\nDepositado en", OUT, "\n")
