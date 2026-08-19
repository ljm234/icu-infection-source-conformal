library(glmnet)

pru <- read.csv("outputs/fase7/prob_prueba.csv", stringsAsFactors = FALSE)
clases <- c("sin_crecimiento","urinario","respiratorio","sangre")

cat("\n=== RANGO DE PROBABILIDADES PREDICHAS POR CLASE ===\n")
# Un rango estrecho indica que el modelo asigna practicamente la misma
# probabilidad a todos los pacientes, es decir que no discrimina.
res <- do.call(rbind, lapply(clases, function(k) {
  p <- pru[[k]]
  data.frame(clase = k,
             minimo = round(min(p), 4),
             p25 = round(quantile(p, 0.25), 4),
             mediana = round(median(p), 4),
             p75 = round(quantile(p, 0.75), 4),
             maximo = round(max(p), 4),
             rango = round(max(p) - min(p), 4),
             row.names = NULL)
}))
print(res, row.names = FALSE)

cat("\n=== PREVALENCIA OBSERVADA CONTRA PROBABILIDAD MEDIA ===\n")
comp <- do.call(rbind, lapply(clases, function(k) {
  data.frame(clase = k,
             prevalencia = round(mean(pru$clase == k), 4),
             prob_media = round(mean(pru[[k]]), 4),
             row.names = NULL)
}))
print(comp, row.names = FALSE)

cat("\n=== DISCRIMINACION: AUC UNO CONTRA EL RESTO ===\n")
auc <- function(p, y) {
  r <- rank(p)
  n1 <- sum(y); n0 <- length(y) - n1
  if (n1 == 0 || n0 == 0) return(NA)
  (sum(r[y == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0)
}
disc <- do.call(rbind, lapply(clases, function(k) {
  data.frame(clase = k,
             n_casos = sum(pru$clase == k),
             auc = round(auc(pru[[k]], as.integer(pru$clase == k)), 4),
             row.names = NULL)
}))
print(disc, row.names = FALSE)

cat("\n=== CLASE PREDICHA CONTRA OBSERVADA ===\n")
pred <- clases[apply(pru[, clases], 1, which.max)]
print(table(observada = pru$clase, predicha = pred))
