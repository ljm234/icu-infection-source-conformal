library(splines)
set.seed(20260818)

imps <- readRDS("outputs/fase6/imputaciones.rds")
d <- imps[[1]]
ent <- d[d$grupo == "entrenamiento", ]
ent$conf <- ifelse(ent$clase == "sin_crecimiento", 0, 1)

vars <- c("leucocitos","hemoglobina","plaquetas","rdw","creatinina","urea",
          "brecha_anion","sodio","potasio","cloro","bicarbonato","inr",
          "ttpa","ph","pco2","lactato","exceso_base")

ganancia <- function(v, datos) {
  x <- datos[[v]]
  nu <- quantile(x, c(0.05,0.35,0.65,0.95), na.rm = TRUE)
  m0 <- glm(conf ~ x, family = binomial, data = datos)
  m1 <- try(glm(conf ~ ns(x, knots = nu[2:3], Boundary.knots = nu[c(1,4)]),
                family = binomial, data = datos), silent = TRUE)
  if (inherits(m1, "try-error")) return(NA)
  deviance(m0) - deviance(m1)
}

cat("\n=== 1. GANANCIA BAJO PERMUTACION DEL DESENLACE ===\n")
# Al permutar el desenlace se destruye toda relacion con los predictores.
# La ganancia que persiste corresponde a ruido, y su distribucion define
# el umbral por encima del cual una ganancia observada es interpretable.
nulo <- replicate(200, {
  perm <- ent
  perm$conf <- sample(perm$conf)
  ganancia(sample(vars, 1), perm)
})
cat("Percentiles de la ganancia nula:\n")
print(round(quantile(nulo, c(0.5, 0.90, 0.95, 0.99), na.rm = TRUE), 2))
umbral_nulo <- quantile(nulo, 0.95, na.rm = TRUE)

# El umbral adoptado para admitir un spline. Vivia en un comentario de R/30,
# que es donde lo encontraria quien lo buscase y donde nadie lo contrasta. Se
# nombra aqui, junto a la nula que lo justifica, y se deposita abajo.
UMBRAL_ADOPTADO <- 10

cat("\n=== 2. ESTABILIDAD ENTRE IMPUTACIONES ===\n")
# Una no linealidad real debe reproducirse en las veinte imputaciones. Si la
# ganancia varia de forma amplia entre ellas, procede de valores rellenados
# y no de la relacion subyacente.
est <- do.call(rbind, lapply(vars, function(v) {
  g <- sapply(imps, function(x) {
    e <- x[x$grupo == "entrenamiento", ]
    e$conf <- ifelse(e$clase == "sin_crecimiento", 0, 1)
    ganancia(v, e)
  })
  data.frame(variable = v,
             media = round(mean(g), 2),
             minimo = round(min(g), 2),
             sd = round(sd(g), 2),
             supera_nulo_en = sum(g > umbral_nulo),
             row.names = NULL)
}))
print(est[order(-est$media), ], row.names = FALSE)

cat("\n=== 3. VALIDACION CRUZADA: SPLINE MEJORA PREDICCION? ===\n")
# La prueba de razon de verosimilitud mide ajuste dentro de la muestra. Lo
# relevante es si la forma no lineal mejora la prediccion fuera de ella.
k <- 10
pliegue <- sample(rep(1:k, length.out = nrow(ent)))

cv <- do.call(rbind, lapply(vars, function(v) {
  dl <- ds <- 0
  for (i in 1:k) {
    tr <- ent[pliegue != i, ]; te <- ent[pliegue == i, ]
    nu <- quantile(tr[[v]], c(0.05,0.35,0.65,0.95), na.rm = TRUE)
    m0 <- glm(conf ~ tr[[v]], family = binomial, data = tr)
    p0 <- plogis(coef(m0)[1] + coef(m0)[2] * te[[v]])
    b  <- try(ns(tr[[v]], knots = nu[2:3], Boundary.knots = nu[c(1,4)]),
              silent = TRUE)
    if (inherits(b, "try-error")) return(NULL)
    m1 <- glm(te_y ~ ., family = binomial,
              data = data.frame(te_y = tr$conf, as.matrix(b)))
    bt <- predict(b, te[[v]])
    p1 <- plogis(cbind(1, bt) %*% coef(m1))
    dl <- dl - 2*sum(te$conf*log(pmax(p0,1e-9)) +
                     (1-te$conf)*log(pmax(1-p0,1e-9)))
    ds <- ds - 2*sum(te$conf*log(pmax(p1,1e-9)) +
                     (1-te$conf)*log(pmax(1-p1,1e-9)))
  }
  data.frame(variable = v,
             dev_cv_lineal = round(dl, 1),
             dev_cv_spline = round(ds, 1),
             ganancia_cv = round(dl - ds, 2),
             row.names = NULL)
}))
print(cv[order(-cv$ganancia_cv), ], row.names = FALSE)

cat("\n=== 4. DECISION ===\n")
cat("Umbral por permutacion, percentil 95:", round(umbral_nulo, 2), "\n")
sel <- cv$variable[cv$ganancia_cv > 0]
cat("Variables con ganancia positiva fuera de muestra:", length(sel), "\n")
print(sel)

dir.create("outputs/fase7", recursive = TRUE, showWarnings = FALSE)
write.csv(est, "outputs/fase7/estabilidad_nolinealidad.csv", row.names = FALSE)
write.csv(cv,  "outputs/fase7/ganancia_cv.csv", row.names = FALSE)

# El umbral de la nula, depositado.
#
# Hasta aqui se calculaba, se usaba para contar en cuantas imputaciones una
# ganancia lo supera, y se imprimia por consola. Lo que quedaba en disco era
# ese recuento derivado y no el valor, de modo que el umbral que el documento
# cita no tenia archivo detras: vivia en la salida de una ejecucion, que se
# pierde, y en un comentario, que nadie contrasta. De las dos senales, la que
# sobrevivia era la que no se comprueba.
#
# Se depositan los cuatro percentiles y no solo el que gobierna. Elegir el
# percentil es parte del criterio, y un archivo que guarde unicamente el
# elegido no permite ver que hubo eleccion.
write.csv(data.frame(
  replicas        = length(nulo),
  p50             = round(as.numeric(quantile(nulo, 0.50, na.rm = TRUE)), 2),
  p90             = round(as.numeric(quantile(nulo, 0.90, na.rm = TRUE)), 2),
  p95             = round(as.numeric(quantile(nulo, 0.95, na.rm = TRUE)), 2),
  p99             = round(as.numeric(quantile(nulo, 0.99, na.rm = TRUE)), 2),
  umbral_adoptado = UMBRAL_ADOPTADO,
  row.names = NULL),
  "outputs/fase7/umbral_permutacion.csv", row.names = FALSE)
