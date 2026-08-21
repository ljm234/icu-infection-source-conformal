library(mice)
set.seed(20260818)

d <- read.csv("outputs/fase5/matriz_particionada.csv", stringsAsFactors = FALSE)
d <- d[d$grupo == "entrenamiento", ]

vars <- c("leucocitos","hemoglobina","plaquetas","rdw","creatinina","urea",
          "brecha_anion","sodio","potasio","cloro","bicarbonato","inr",
          "ttpa","ph","pco2","lactato","exceso_base")

d$unidad <- factor(d$unidad)
d$sexo   <- factor(d$sexo)

# Validacion por enmascaramiento. Se ocultan valores efectivamente
# observados, se imputan, y se comparan las estimaciones con el dato real.
# Es la unica comprobacion directa de exactitud: la semejanza entre las
# distribuciones de valores observados e imputados indica consistencia, no
# que las estimaciones individuales sean correctas.
FRACCION <- 0.10

completo <- d[complete.cases(d[, vars]), ]
cat("Casos completos disponibles:", nrow(completo), "\n\n")

enmascarado <- completo
mascara <- list()
for (v in vars) {
  idx <- sample(nrow(completo), floor(FRACCION * nrow(completo)))
  mascara[[v]] <- idx
  enmascarado[[v]][idx] <- NA
}

datos <- enmascarado[, c(vars, "edad", "sexo", "unidad")]
imp <- mice(datos, m = 5, maxit = 10, method = "pmm",
            seed = 20260818, printFlag = FALSE)

evaluar <- function(imputado, etiqueta) {
  do.call(rbind, lapply(vars, function(v) {
    idx <- mascara[[v]]
    real <- completo[[v]][idx]
    est  <- imputado[[v]][idx]
    data.frame(metodo = etiqueta, variable = v,
               error_absoluto_mediano = round(median(abs(est - real)), 3),
               error_relativo_pct = round(100 * median(abs(est - real) /
                                     pmax(abs(real), 1e-6)), 2),
               correlacion = round(cor(est, real), 3),
               row.names = NULL)
  }))
}

r_mice <- evaluar(complete(imp, 1), "mice con unidad")

# Referencia: imputacion por la mediana de cada variable. Constituye el
# procedimiento mas simple posible y sirve para establecer si el metodo
# multivariante aporta exactitud sobre una regla trivial.
med <- enmascarado
for (v in vars) med[[v]][is.na(med[[v]])] <- median(completo[[v]])
r_med <- evaluar(med, "mediana")

# Referencia: mice sin la unidad en el modelo. Permite verificar si la
# inclusion de esa variable, defendida por su relacion con el mecanismo de
# ausencia, modifica de forma apreciable los valores estimados.
imp_su <- mice(datos[, c(vars, "edad", "sexo")], m = 5, maxit = 10,
               method = "pmm", seed = 20260818, printFlag = FALSE)
r_su <- evaluar(complete(imp_su, 1), "mice sin unidad")

cat("=== ERROR DE IMPUTACION POR METODO ===\n")
comp <- rbind(r_mice, r_med, r_su)
for (v in vars) {
  s <- comp[comp$variable == v, ]
  cat(sprintf("%-14s mice %.3f | mediana %.3f | sin unidad %.3f  (corr %.3f)\n",
      v, s$error_absoluto_mediano[1], s$error_absoluto_mediano[2],
      s$error_absoluto_mediano[3], s$correlacion[1]))
}

cat("\n=== RESUMEN ===\n")
res <- aggregate(cbind(error_relativo_pct, correlacion) ~ metodo, comp, mean)
print(res, row.names = FALSE)

mejora_med <- mean(r_med$error_absoluto_mediano > r_mice$error_absoluto_mediano)
mejora_uni <- mean(r_su$error_absoluto_mediano > r_mice$error_absoluto_mediano)
cat("\nVariables donde mice supera a la mediana:",
    sum(r_med$error_absoluto_mediano > r_mice$error_absoluto_mediano),
    "de", length(vars), "\n")
cat("Variables donde incluir la unidad reduce el error:",
    sum(r_su$error_absoluto_mediano > r_mice$error_absoluto_mediano),
    "de", length(vars), "\n")

dir.create("outputs/fase6", showWarnings = FALSE, recursive = TRUE)
write.csv(comp, "outputs/fase6/validacion_enmascaramiento.csv",
          row.names = FALSE)
