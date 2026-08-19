library(mice)

imp <- readRDS("outputs/fase6/objeto_mice.rds")
le  <- imp$loggedEvents

cat("\n=== TIPOS DE EVENTO REGISTRADOS ===\n")
print(table(le$meth))

cat("\n=== VARIABLES ELIMINADAS ===\n")
print(sort(table(le$out), decreasing = TRUE))

cat("\n=== EN QUE ITERACIONES OCURRE ===\n")
print(table(le$it))

d <- read.csv("outputs/fase5/matriz_particionada.csv", stringsAsFactors = FALSE)
d <- d[d$grupo == "entrenamiento", ]

cat("\n=== NIVELES DE UNIDAD EN ENTRENAMIENTO ===\n")
print(table(d$unidad))

vars <- c("leucocitos","hemoglobina","plaquetas","rdw","creatinina","urea",
          "brecha_anion","sodio","potasio","cloro","bicarbonato","inr",
          "ttpa","ph","pco2","lactato","exceso_base")

cat("\n=== RANGO DEL DISENO CON UNIDAD COMO FACTOR ===\n")
sub <- d[complete.cases(d[, vars]), ]
X <- model.matrix(~ factor(unidad) + edad + factor(sexo) + lactato_medido,
                  data = sub)
cat("Columnas de la matriz:", ncol(X), "\n")
cat("Rango efectivo:", qr(X)$rank, "\n")
if (qr(X)$rank < ncol(X)) {
  cat("HAY DEPENDENCIA LINEAL. Columnas redundantes:",
      ncol(X) - qr(X)$rank, "\n")
} else {
  cat("Sin dependencia lineal en este subconjunto.\n")
}

cat("\n=== RELACION ENTRE UNIDAD Y lactato_medido ===\n")
print(table(sub$unidad, sub$lactato_medido))
