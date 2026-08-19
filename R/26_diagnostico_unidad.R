d <- read.csv("outputs/fase5/matriz_particionada.csv", stringsAsFactors = FALSE)
d <- d[d$grupo != "excluido", ]
d$unidad <- factor(d$unidad)

cat("\n=== NIVELES DEL FACTOR ===\n")
print(levels(d$unidad))

cat("\n=== FILAS POR NIVEL Y GRUPO ===\n")
print(table(d$unidad, d$grupo))

ent <- d[d$grupo == "entrenamiento", ]

cat("\n=== NIVELES SIN OBSERVACIONES EN ENTRENAMIENTO ===\n")
vacios <- levels(d$unidad)[table(ent$unidad) == 0]
if (length(vacios) == 0) {
  cat("Ninguno.\n")
} else {
  print(vacios)
}

cat("\n=== RANGO CON EL FACTOR COMPLETO, SOLO ENTRENAMIENTO ===\n")
X1 <- model.matrix(~ unidad + edad + factor(sexo), data = ent)
cat("Columnas:", ncol(X1), " Rango:", qr(X1)$rank, "\n")
cat("Columnas con suma cero:",
    paste(colnames(X1)[colSums(X1) == 0], collapse = ", "), "\n")

cat("\n=== RANGO CON NIVELES DEPURADOS ===\n")
ent2 <- ent
ent2$unidad <- droplevels(ent2$unidad)
X2 <- model.matrix(~ unidad + edad + factor(sexo), data = ent2)
cat("Columnas:", ncol(X2), " Rango:", qr(X2)$rank, "\n")
if (qr(X2)$rank == ncol(X2)) {
  cat("Sin dependencia lineal al depurar niveles vacios.\n")
} else {
  cat("Persiste dependencia. La causa es otra.\n")
}
