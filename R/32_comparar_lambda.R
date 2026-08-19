library(glmnet)
library(splines)
set.seed(20260818)

esp <- readRDS("outputs/fase7/especificacion.rds")
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

ent <- apilar("entrenamiento")
X <- construir(ent); y <- ent$clase; w <- rep(1/M, nrow(X))

pacientes <- unique(ent$stay_id)
asign <- sample(rep(1:10, length.out = length(pacientes)))
names(asign) <- pacientes
foldid <- asign[as.character(ent$stay_id)]

cat("Ajustando la ruta completa de lambda con alfa = 1.\n")
cv <- cv.glmnet(X, y, family = "multinomial", alpha = 1, weights = w,
                foldid = foldid, type.measure = "deviance", standardize = TRUE)

auc <- function(p, yb) {
  r <- rank(p); n1 <- sum(yb); n0 <- length(yb) - n1
  if (n1 == 0 || n0 == 0) return(NA)
  (sum(r[yb == 1]) - n1*(n1+1)/2) / (n1*n0)
}

evaluar <- function(lam) {
  mod <- glmnet(X, y, family = "multinomial", alpha = 1,
                weights = w, lambda = lam, standardize = TRUE)
  pr <- apilar("prueba")
  Xp <- construir(pr)
  p <- predict(mod, newx = Xp, type = "response")[, , 1]
  ag <- aggregate(p, by = list(stay_id = pr$stay_id), FUN = mean)
  et <- unique(pr[, c("stay_id","clase")])
  m <- merge(ag, et, by = "stay_id")
  a <- sapply(CLASES, function(k) auc(m[[k]], as.integer(m$clase == k)))
  nz <- sum(sapply(coef(mod), function(z) sum(z[-1] != 0)))
  list(auc = a, no_nulos = nz,
       pred_min = sum(apply(m[, CLASES], 1, which.max) != 1))
}

r1 <- evaluar(cv$lambda.1se)
r2 <- evaluar(cv$lambda.min)

cat("\n=== COMPARACION ===\n")
tab <- data.frame(
  clase = CLASES,
  auc_1se = round(r1$auc, 4),
  auc_min = round(r2$auc, 4),
  diferencia = round(r2$auc - r1$auc, 4),
  row.names = NULL)
print(tab, row.names = FALSE)

min_cl <- CLASES[-1]
prom1 <- mean(r1$auc[min_cl]); prom2 <- mean(r2$auc[min_cl])

cat("\nAUC promedio en clases minoritarias\n")
cat("  lambda.1se:", round(prom1, 4), "\n")
cat("  lambda.min:", round(prom2, 4), "\n")
cat("  diferencia:", round(prom2 - prom1, 4), "\n")

cat("\nCoeficientes no nulos: 1se =", r1$no_nulos,
    " min =", r2$no_nulos, "\n")
cat("Pacientes con clase minoritaria como argmax: 1se =", r1$pred_min,
    " min =", r2$pred_min, "\n")

peor <- min(r2$auc[min_cl] - r1$auc[min_cl])
cat("\n=== DECISION SEGUN CRITERIO PREESPECIFICADO ===\n")
cat("Umbral de mejora relevante: 0.02\n")
cat("Ninguna clase puede empeorar mas de 0.02\n")
if (prom2 - prom1 > 0.02 && peor > -0.02) {
  cat("RESULTADO: adoptar lambda.min\n")
} else {
  cat("RESULTADO: conservar lambda.1se\n")
}

write.csv(tab, "outputs/fase7/comparacion_lambda.csv", row.names = FALSE)
