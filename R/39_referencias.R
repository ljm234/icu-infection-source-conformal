library(glmnet)
library(splines)
library(jsonlite)
set.seed(20260818)

# Recuento de parametros. Las dos primeras referencias fijaban su recuento a
# mano, y no bajo el mismo criterio: una contaba los coeficientes sin los
# interceptos y la otra los contaba con ellos, de modo que la columna mezclaba
# definiciones y no admitia comparacion entre filas. Las dos ultimas ya lo
# derivaban del objeto ajustado.
#
# Se adopta una sola definicion, la que esas dos ultimas ya empleaban:
# coeficientes no nulos, excluidos los interceptos, sumados sobre los bloques
# de clase. Es la que corresponde a un modelo penalizado, donde un coeficiente
# anulado no consume grado de libertad alguno. Se deriva del ajuste en las
# cuatro referencias, sin excepcion.
parametros <- function(mod)
  sum(sapply(coef(mod), function(z) sum(z[-1] != 0)))

detener <- function(...) {
  cat("\n", ..., "\n", sep = "")
  cat("El procedimiento se detiene. No procede sobrescribir el archivo.\n")
  quit(status = 1)
}

RUTA_ESP <- "outputs/fase7/especificacion.rds"
RUTA_IMP <- "outputs/fase6/imputaciones.rds"
RUTA_MAN <- "outputs/fase7/manifiesto.json"
RUTA_HIP <- "outputs/fase7/busqueda_hiperparametros.csv"
RUTA_REF <- "outputs/fase13/referencias.csv"

for (r in c(RUTA_ESP, RUTA_IMP, RUTA_MAN, RUTA_HIP))
  if (!file.exists(r)) detener("Fuente ausente: ", r)

esp  <- readRDS(RUTA_ESP)
imps <- readRDS(RUTA_IMP)
M <- length(imps)
CLASES <- esp$clases

# La penalizacion de las dos ultimas referencias es la del modelo publicado.
# Se lee del registro de la busqueda de hiperparametros en lugar de repetirse
# aqui: escrita a mano, dejaria de seguir a la fase que la eligio.
man <- fromJSON(RUTA_MAN)
if (is.null(man$alpha)) detener("El manifiesto de la fase 7 no declara alfa.")
hip <- read.csv(RUTA_HIP, stringsAsFactors = FALSE)
fila <- which(abs(hip$alpha - as.numeric(man$alpha)) < 1e-9)
if (length(fila) != 1)
  detener("La busqueda de hiperparametros no contiene una fila para alfa.")
LAMBDA <- hip$lambda_min[fila]

# Las dos primeras referencias no se penalizan de forma apreciable: su
# proposito es medir cuanto explica un punado de variables, no seleccionar
# entre ellas. El valor es una eleccion de diseno y no un resultado.
LAMBDA_REFERENCIA <- 0.001

cat("Alfa leido del manifiesto:", man$alpha, "\n")
cat("Penalizacion leida del registro de la busqueda:", signif(LAMBDA, 4), "\n")

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

ent <- apilar("entrenamiento")
pru <- apilar("prueba")
w <- rep(1/M, nrow(ent))

auc <- function(p, yb) {
  r <- rank(p); n1 <- sum(yb); n0 <- length(yb) - n1
  if (n1 < 3 || n0 < 3) return(NA)
  (sum(r[yb == 1]) - n1*(n1+1)/2) / (n1*n0)
}

evaluar <- function(mod, Xp, etiqueta, k_param) {
  p <- predict(mod, newx = Xp, type = "response")[, , 1]
  ag <- aggregate(p, by = list(stay_id = pru$stay_id), FUN = mean)
  et <- unique(pru[, c("stay_id","clase")])
  m <- merge(ag, et, by = "stay_id")
  a <- sapply(CLASES, function(k) auc(m[[k]], as.integer(m$clase == k)))
  data.frame(modelo = etiqueta, parametros = k_param,
             t(round(a, 4)),
             promedio_minoritarias = round(mean(a[-1]), 4),
             row.names = NULL)
}

# Referencia uno. Solo edad y sexo. Establece el desempeno atribuible a la
# composicion demografica de la cohorte, sin informacion de laboratorio.
X1 <- as.matrix(data.frame(edad = ent$edad,
                           sexo_M = as.integer(ent$sexo == "M")))
X1p <- as.matrix(data.frame(edad = pru$edad,
                            sexo_M = as.integer(pru$sexo == "M")))
m1 <- glmnet(X1, ent$clase, family = "multinomial", alpha = 0,
             lambda = LAMBDA_REFERENCIA, weights = w, standardize = TRUE)
r1 <- evaluar(m1, X1p, "demografia", parametros(m1))

# Referencia dos. Tres marcadores de uso corriente en la valoracion inicial
# de la sepsis, en forma lineal y sin penalizacion apreciable.
v3 <- c("leucocitos","lactato","creatinina")
X2  <- as.matrix(ent[, v3]); X2p <- as.matrix(pru[, v3])
m2 <- glmnet(X2, ent$clase, family = "multinomial", alpha = 0,
             lambda = LAMBDA_REFERENCIA, weights = w, standardize = TRUE)
r2 <- evaluar(m2, X2p, "tres marcadores", parametros(m2))

# Referencia tres. Las diecisiete variables en forma lineal, sin splines y
# sin la unidad. Aisla la contribucion de la forma funcional y del efecto de
# sede respecto de la informacion bioquimica sin transformar.
v17 <- c(esp$con_spline, esp$lineales)
X3  <- as.matrix(ent[, v17]); X3p <- as.matrix(pru[, v17])
m3 <- glmnet(X3, ent$clase, family = "multinomial", alpha = 1,
             lambda = LAMBDA, weights = w, standardize = TRUE)
r3 <- evaluar(m3, X3p, "lineal sin unidad", parametros(m3))

# Modelo completo, con splines, unidad e indicador de determinacion de
# lactato.
construir <- function(d) {
  bl <- list()
  for (v in esp$con_spline) {
    nu <- esp$nudos[[v]]
    b <- ns(d[[v]], knots = nu[2:3], Boundary.knots = nu[c(1,4)])
    colnames(b) <- paste0(v, "_s", seq_len(ncol(b)))
    bl[[v]] <- as.matrix(b)
  }
  lin <- as.matrix(d[, esp$lineales, drop = FALSE])
  otr <- data.frame(edad = d$edad, lactato_medido = d$lactato_medido,
                    sexo_M = as.integer(d$sexo == "M"))
  uni <- model.matrix(~ unidad_mod - 1, data = d)[, -1, drop = FALSE]
  cbind(do.call(cbind, bl), lin, as.matrix(otr), uni)
}
X4 <- construir(ent); X4p <- construir(pru)
m4 <- glmnet(X4, ent$clase, family = "multinomial", alpha = 1,
             lambda = LAMBDA, weights = w, standardize = TRUE)
r4 <- evaluar(m4, X4p, "modelo completo", parametros(m4))

comp <- rbind(r1, r2, r3, r4)
names(comp)[3:6] <- CLASES

cat("\n=== COMPARACION CONTRA REFERENCIAS ===\n")
print(comp, row.names = FALSE)

cat("\n=== GANANCIA SOBRE LA REFERENCIA MAS SIMPLE ===\n")
base_v <- comp$promedio_minoritarias[1]
for (i in seq_len(nrow(comp))) {
  cat(sprintf("%-20s %3d parametros  AUC %.4f  ganancia %+.4f\n",
      comp$modelo[i], comp$parametros[i],
      comp$promedio_minoritarias[i],
      comp$promedio_minoritarias[i] - base_v))
}

cat("\n=== COSTE POR UNIDAD DE GANANCIA ===\n")
for (i in 2:nrow(comp)) {
  g <- comp$promedio_minoritarias[i] - comp$promedio_minoritarias[i-1]
  dp <- comp$parametros[i] - comp$parametros[i-1]
  cat(sprintf("%-20s +%3d parametros  ganancia %+.4f\n",
      comp$modelo[i], dp, g))
}

# Contraste con el archivo ya publicado. La discriminacion no debe cambiar:
# los ajustes son los mismos y solo se ha modificado de donde procede la
# penalizacion y como se cuenta el parametro. Si cambiara, el ajuste no seria
# reproducible y no procederia sobrescribir nada. El recuento de parametros si
# puede cambiar, y de ese cambio se informa.
if (file.exists(RUTA_REF)) {
  ant <- read.csv(RUTA_REF, stringsAsFactors = FALSE)
  if (!setequal(ant$modelo, comp$modelo))
    detener("El archivo publicado no contiene las mismas referencias.")
  o <- match(comp$modelo, ant$modelo)
  d <- max(abs(comp$promedio_minoritarias - ant$promedio_minoritarias[o]))
  cat("\n=== CONTRASTE CON EL ARCHIVO PUBLICADO ===\n")
  cat("Discrepancia maxima en la discriminacion:", signif(d, 3), "\n")
  if (d >= 5e-05)
    detener("La discriminacion recompuesta no reproduce la publicada.")
  cambios <- which(comp$parametros != ant$parametros[o])
  if (length(cambios) == 0) {
    cat("El recuento de parametros no varia.\n")
  } else {
    cat("El recuento de parametros varia al unificar la definicion:\n")
    for (i in cambios)
      cat(sprintf("  %-20s %4d -> %4d\n", comp$modelo[i],
                  ant$parametros[o][i], comp$parametros[i]))
    cat("Procede regenerar la documentacion y actualizar el valor esperado\n")
    cat("en R/59_verificar_cifras.R antes de publicar.\n")
  }
}

dir.create("outputs/fase13", recursive = TRUE, showWarnings = FALSE)
write.csv(comp, RUTA_REF, row.names = FALSE)
