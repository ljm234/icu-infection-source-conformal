imps <- readRDS("outputs/fase6/imputaciones.rds")
d <- imps[[1]]
ent <- d[d$grupo == "entrenamiento", ]

vars <- c("leucocitos","hemoglobina","plaquetas","rdw","creatinina","urea",
          "brecha_anion","sodio","potasio","cloro","bicarbonato","inr",
          "ttpa","ph","pco2","lactato","exceso_base")

ent$conf <- ifelse(ent$clase == "sin_crecimiento", 0, 1)

cat("\n=== 1. NO LINEALIDAD POR VARIABLE ===\n")
# Se compara un modelo lineal contra uno con spline cubica restringida de
# cuatro nudos mediante prueba de razon de verosimilitud. Un valor bajo
# indica que la forma no lineal aporta ajuste por encima de la recta.
if (!requireNamespace("splines", quietly = TRUE)) stop("falta splines")
library(splines)

nl <- do.call(rbind, lapply(vars, function(v) {
  x <- ent[[v]]
  nudos <- quantile(x, c(0.05, 0.35, 0.65, 0.95), na.rm = TRUE)
  m0 <- glm(conf ~ x, family = binomial, data = ent)
  m1 <- try(glm(conf ~ ns(x, knots = nudos[2:3],
                          Boundary.knots = nudos[c(1,4)]),
                family = binomial, data = ent), silent = TRUE)
  if (inherits(m1, "try-error")) return(NULL)
  p <- anova(m0, m1, test = "LRT")$`Pr(>Chi)`[2]
  data.frame(variable = v,
             dev_lineal = round(deviance(m0), 1),
             dev_spline = round(deviance(m1), 1),
             ganancia   = round(deviance(m0) - deviance(m1), 2),
             p_valor    = format.pval(p, digits = 3),
             row.names  = NULL)
}))
print(nl[order(-nl$ganancia), ], row.names = FALSE)

cat("\n=== 2. COLINEALIDAD ===\n")
# La correlacion alta entre predictores justifica retener el componente
# cuadratico de la penalizacion, que reparte el coeficiente entre variables
# correlacionadas en lugar de escoger una de forma inestable.
cm <- cor(ent[, vars])
diag(cm) <- 0
altas <- which(abs(cm) > 0.6, arr.ind = TRUE)
altas <- altas[altas[,1] < altas[,2], , drop = FALSE]
if (nrow(altas) > 0) {
  print(data.frame(
    var1 = vars[altas[,1]], var2 = vars[altas[,2]],
    r = round(cm[altas], 3))[order(-abs(cm[altas])), ], row.names = FALSE)
} else {
  cat("Sin pares por encima de 0.6\n")
}

cat("\nCorrelacion absoluta media:", round(mean(abs(cm[upper.tri(cm)])), 3), "\n")

cat("\n=== 3. VARIABILIDAD ENTRE IMPUTACIONES ===\n")
# Si las imputaciones difieren poco entre si, promediar coeficientes y
# apilar conjuntos conducen a resultados semejantes. Una diferencia amplia
# favorece el apilado, que preserva esa incertidumbre en un unico ajuste.
disp <- do.call(rbind, lapply(vars, function(v) {
  medias <- sapply(imps, function(x) mean(x[[v]][x$grupo == "entrenamiento"]))
  data.frame(variable = v,
             media_global = round(mean(medias), 3),
             sd_entre_imp = round(sd(medias), 4),
             cv_pct = round(100 * sd(medias) / abs(mean(medias)), 3),
             row.names = NULL)
}))
print(disp[order(-disp$cv_pct), ], row.names = FALSE)

cat("\n=== 4. TAMANO DE LA MATRIZ SEGUN ESPECIFICACION ===\n")
n_ent <- nrow(ent)
esc <- data.frame(
  especificacion = c("lineal", "splines en todas", "splines solo no lineales"),
  columnas = c(length(vars) + 3,
               length(vars) * 3 + 3,
               NA),
  filas_apiladas = n_ent * 20)
print(esc, row.names = FALSE)
cat("Filas de entrenamiento por imputacion:", n_ent, "\n")
cat("Clase minoritaria en entrenamiento:",
    min(table(ent$clase[ent$clase != "abstencion"])), "\n")

dir.create("outputs/fase7", recursive = TRUE, showWarnings = FALSE)
write.csv(nl,   "outputs/fase7/no_linealidad.csv", row.names = FALSE)
write.csv(disp, "outputs/fase7/dispersion_imputaciones.csv", row.names = FALSE)
