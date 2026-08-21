library(glmnet)
library(splines)
set.seed(20260818)

esp  <- readRDS("outputs/fase7/especificacion.rds")
imps <- readRDS("outputs/fase6/imputaciones.rds")
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
             lambda = 0.001, weights = w, standardize = TRUE)
r1 <- evaluar(m1, X1p, "demografia", 8)

# Referencia dos. Tres marcadores de uso corriente en la valoracion inicial
# de la sepsis, en forma lineal y sin penalizacion apreciable.
v3 <- c("leucocitos","lactato","creatinina")
X2  <- as.matrix(ent[, v3]); X2p <- as.matrix(pru[, v3])
m2 <- glmnet(X2, ent$clase, family = "multinomial", alpha = 0,
             lambda = 0.001, weights = w, standardize = TRUE)
r2 <- evaluar(m2, X2p, "tres marcadores", 16)

# Referencia tres. Las diecisiete variables en forma lineal, sin splines y
# sin la unidad. Aisla la contribucion de la forma funcional y del efecto de
# sede respecto de la informacion bioquimica sin transformar.
v17 <- c(esp$con_spline, esp$lineales)
X3  <- as.matrix(ent[, v17]); X3p <- as.matrix(pru[, v17])
m3 <- glmnet(X3, ent$clase, family = "multinomial", alpha = 1,
             lambda = 0.000191, weights = w, standardize = TRUE)
r3 <- evaluar(m3, X3p, "lineal sin unidad", sum(sapply(coef(m3),
                                              function(z) sum(z[-1] != 0))))

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
             lambda = 0.000191, weights = w, standardize = TRUE)
r4 <- evaluar(m4, X4p, "modelo completo", sum(sapply(coef(m4),
                                             function(z) sum(z[-1] != 0))))

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

dir.create("outputs/fase13", recursive = TRUE, showWarnings = FALSE)
write.csv(comp, "outputs/fase13/referencias.csv", row.names = FALSE)
