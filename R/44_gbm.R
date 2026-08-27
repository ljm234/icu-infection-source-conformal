library(xgboost)
set.seed(20260818)

detener <- function(...) {
  cat("\n", ..., "\n", sep = "")
  cat("El procedimiento se detiene. No procede sobrescribir el archivo.\n")
  quit(status = 1)
}

RUTA_ESP <- "outputs/fase7/especificacion.rds"
RUTA_IMP <- "outputs/fase6/imputaciones.rds"
RUTA_CMP <- "outputs/fase7/comparacion_lambda.csv"
RUTA_REF <- "outputs/fase13/referencias.csv"
RUTA_OUT <- "outputs/fase16/gbm_comparacion.csv"
RUTA_MAX <- "outputs/fase16/gbm_regla_maximo.csv"

for (r in c(RUTA_ESP, RUTA_IMP, RUTA_CMP))
  if (!file.exists(r)) detener("Fuente ausente: ", r)

esp  <- readRDS(RUTA_ESP)
imps <- readRDS(RUTA_IMP)
M <- length(imps)
CLASES <- esp$clases

base_ent <- imps[[1]][imps[[1]]$grupo == "entrenamiento", ]
niveles <- sort(unique(base_ent$unidad))
imps <- lapply(imps, function(x) {
  x$unidad_mod <- factor(ifelse(x$unidad %in% niveles,
                                as.character(x$unidad), niveles[1]),
                         levels = niveles)
  x
})

apilar <- function(g) {
  p <- do.call(rbind, lapply(imps, function(x) x[x$grupo == g, ]))
  p <- p[p$clase %in% CLASES, ]
  p$clase <- factor(as.character(p$clase), levels = CLASES)
  p
}
ent <- apilar("entrenamiento"); pru <- apilar("prueba")

# Comparador con arboles potenciados por gradiente, previsto en el protocolo
# como analisis secundario. Recibe las mismas variables que el modelo
# principal pero en forma cruda, sin splines: el metodo aprende por si mismo
# las no linealidades y las interacciones. Si un metodo flexible tampoco
# supera de forma apreciable la discriminacion del modelo lineal penalizado,
# la limitacion reside en la informacion disponible y no en la forma
# funcional elegida.
vars <- c(esp$con_spline, esp$lineales)

matriz <- function(d) {
  uni <- model.matrix(~ unidad_mod - 1, data = d)[, -1, drop = FALSE]
  as.matrix(cbind(d[, vars],
                  edad = d$edad,
                  sexo_M = as.integer(d$sexo == "M"),
                  lactato_medido = d$lactato_medido,
                  uni))
}

Xe <- matriz(ent); Xp <- matriz(pru)
ye <- as.integer(ent$clase) - 1
we <- rep(1/M, nrow(Xe))

pac <- unique(ent$stay_id)
asign <- sample(rep(1:10, length.out = length(pac)))
names(asign) <- pac
foldid <- asign[as.character(ent$stay_id)]
folds <- lapply(1:10, function(k) which(foldid == k))

dtrain <- xgb.DMatrix(Xe, label = ye, weight = we)

# El numero optimo de rondas se lee del registro de evaluacion, que es la
# fuente primaria del procedimiento, en lugar de recurrir a campos de
# conveniencia cuya denominacion varia entre versiones de la biblioteca.
mejor_ronda <- function(cv) {
  log <- cv$evaluation_log
  i <- which.min(log$test_mlogloss_mean)
  list(rondas = log$iter[i], mlogloss = log$test_mlogloss_mean[i])
}

cat("=== SELECCION DE PROFUNDIDAD POR VALIDACION CRUZADA ===\n")
sel <- do.call(rbind, lapply(c(3, 4, 6), function(d) {
  par <- list(objective = "multi:softprob", num_class = 4,
              eta = 0.05, max_depth = d,
              subsample = 0.8, colsample_bytree = 0.8,
              min_child_weight = 20, eval_metric = "mlogloss",
              nthread = 8)
  cv <- xgb.cv(par, dtrain, nrounds = 3000, folds = folds,
               early_stopping_rounds = 50, verbose = 0)
  b <- mejor_ronda(cv)
  cat("  profundidad", d, "completada,", b$rondas, "rondas\n")
  data.frame(max_depth = d, rondas = b$rondas,
             mlogloss = round(b$mlogloss, 5), row.names = NULL)
}))
cat("\n")
print(sel, row.names = FALSE)

mejor <- sel[which.min(sel$mlogloss), ]
cat("\nConfiguracion elegida: profundidad", mejor$max_depth,
    "con", mejor$rondas, "rondas\n")

par_f <- list(objective = "multi:softprob", num_class = 4,
              eta = 0.05, max_depth = mejor$max_depth,
              subsample = 0.8, colsample_bytree = 0.8,
              min_child_weight = 20, nthread = 8)
gbm <- xgb.train(par_f, dtrain, nrounds = mejor$rondas)

# La forma del objeto devuelto por predict difiere entre versiones de la
# biblioteca: unas entregan una matriz de n filas por clase y otras un vector
# plano en orden por filas. Se contemplan ambos casos y se comprueba despues
# que cada fila sume la unidad y que la probabilidad media por clase se
# aproxime a la prevalencia observada. Esa segunda comprobacion revela de
# inmediato cualquier desordenamiento de las predicciones.
pr <- predict(gbm, Xp)
if (is.null(dim(pr))) pr <- matrix(pr, ncol = length(CLASES), byrow = TRUE)
stopifnot(nrow(pr) == nrow(Xp), ncol(pr) == length(CLASES))
stopifnot(max(abs(rowSums(pr) - 1)) < 1e-6)
colnames(pr) <- CLASES

cat("\n=== COMPROBACION DE LAS PREDICCIONES ===\n")
print(data.frame(
  clase = CLASES,
  prevalencia = round(as.numeric(table(pru$clase)[CLASES]) / nrow(pru), 4),
  prob_media = round(colMeans(pr), 4),
  row.names = NULL), row.names = FALSE)
ag <- aggregate(pr, by = list(stay_id = pru$stay_id), FUN = mean)
et <- unique(pru[, c("stay_id","clase")])
mm <- merge(ag, et, by = "stay_id")

auc <- function(p, yb) {
  r <- rank(p); n1 <- sum(yb); n0 <- length(yb) - n1
  (sum(r[yb == 1]) - n1*(n1+1)/2) / (n1*n0)
}
a_gbm <- sapply(CLASES, function(k) auc(mm[[k]], as.integer(mm$clase == k)))

# Valores del modelo lineal penalizado en el mismo conjunto de prueba. Se
# leen del archivo que los produjo, no se transcriben: escritos a mano
# dejarian de seguir a su fuente y llegarian sin aviso al archivo que esta
# fase deposita, que es uno de los que la verificacion de cifras contrasta.
#
# La lectura se contrasta ademas contra la fila del modelo completo de la
# tabla de referencias, que ha de consignar la misma discriminacion. Dos
# archivos que discrepasen sobre el mismo modelo indicarian que uno de los
# dos quedo sin regenerar.
cmp <- read.csv(RUTA_CMP, stringsAsFactors = FALSE)
if (!all(CLASES %in% cmp$clase))
  detener("La comparacion de penalizaciones no cubre las categorias.")
a_lin <- setNames(cmp$auc_min, cmp$clase)[CLASES]
if (any(is.na(a_lin)))
  detener("La comparacion de penalizaciones no declara alguna categoria.")

if (file.exists(RUTA_REF)) {
  ref <- read.csv(RUTA_REF, stringsAsFactors = FALSE)
  fc <- ref[ref$modelo == "modelo completo", ]
  if (nrow(fc) == 1 && all(CLASES %in% names(fc))) {
    d <- max(abs(as.numeric(fc[1, CLASES]) - a_lin))
    cat("\nDiscrepancia con la tabla de referencias:", signif(d, 3), "\n")
    if (d >= 5e-05)
      detener("Los dos archivos discrepan sobre el modelo completo.")
  }
}

comp <- data.frame(clase = CLASES,
                   auc_lineal = as.numeric(a_lin[CLASES]),
                   auc_gbm = round(a_gbm, 4),
                   diferencia = round(a_gbm - a_lin[CLASES], 4),
                   row.names = NULL)

cat("\n=== DISCRIMINACION EN PRUEBA: LINEAL CONTRA ARBOLES ===\n")
print(comp, row.names = FALSE)
cat("\nPromedio minoritarias lineal:", round(mean(a_lin[-1]), 4), "\n")
cat("Promedio minoritarias arboles:", round(mean(a_gbm[-1]), 4), "\n")
cat("Diferencia:", round(mean(a_gbm[-1]) - mean(a_lin[-1]), 4), "\n")

# La regla del maximo. El recuento solo constaba por pantalla, de modo que la
# afirmacion que la documentacion hace sobre el no era comprobable por un
# lector. Se deposita. La categoria mayoritaria se determina sobre el propio
# conjunto evaluado y no se escribe aqui.
mayor <- CLASES[which.max(table(factor(mm$clase, levels = CLASES)))]
argm <- CLASES[apply(mm[, CLASES], 1, which.max)]
maxima <- data.frame(
  metodo = "arboles potenciados",
  n = nrow(mm),
  mayoritaria = mayor,
  conclusiones_minoritarias = sum(argm != mayor),
  exactitud_aparente = round(mean(argm == as.character(mm$clase)), 4),
  max_depth = mejor$max_depth,
  rondas = mejor$rondas,
  row.names = NULL)

cat("\n=== REGLA DEL MAXIMO ===\n")
print(maxima, row.names = FALSE)

dir.create("outputs/fase16", recursive = TRUE, showWarnings = FALSE)
write.csv(maxima, RUTA_MAX, row.names = FALSE)

# Contraste de reproduccion antes de sobrescribir la discriminacion ya
# publicada. Los arboles potenciados son el unico metodo del proyecto que no
# dejo modelo guardado, de modo que esta fase debe reajustarse entera y su
# resultado podria no coincidir con el anterior. Si no coincide, el archivo
# publicado se deja intacto y procede resolver esa cuestion antes que
# ninguna otra: la fila que se acaba de depositar describiria entonces un
# ajuste distinto del que el trabajo reporta.
if (file.exists(RUTA_OUT)) {
  ant <- read.csv(RUTA_OUT, stringsAsFactors = FALSE)
  o <- match(CLASES, ant$clase)
  if (any(is.na(o)))
    detener("El archivo publicado no cubre las categorias modeladas.")
  d <- max(abs(as.numeric(a_gbm[CLASES]) - ant$auc_gbm[o]))
  cat("\n=== CONTRASTE DE REPRODUCCION ===\n")
  cat("Discrepancia maxima con la discriminacion publicada:",
      signif(d, 3), "\n")
  if (d >= 5e-05) {
    cat("El reajuste no reproduce el resultado publicado. El archivo\n")
    cat("publicado queda intacto. La fila de la regla del maximo si se ha\n")
    cat("depositado, y describe este reajuste y no el anterior.\n")
    detener("Los arboles potenciados no se reproducen.")
  }
  cat("El reajuste reproduce el resultado publicado.\n")
}

write.csv(comp, RUTA_OUT, row.names = FALSE)
