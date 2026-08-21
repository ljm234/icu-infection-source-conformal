pru <- read.csv("outputs/fase8/prueba_con_conjuntos.csv",
                stringsAsFactors = FALSE)
CLASES <- c("sin_crecimiento","urinario","respiratorio","sangre")

# Analisis de curvas de decision, preespecificado en el protocolo. El
# beneficio neto a un umbral de probabilidad pt cuenta los verdaderos
# positivos y descuenta los falsos positivos con peso pt/(1-pt), que
# expresa la razon entre el dano de actuar sin necesidad y el beneficio de
# actuar a tiempo. Un modelo aporta utilidad clinica solo en los umbrales
# donde su beneficio neto supera simultaneamente al de tratar a todos y al
# de no tratar a nadie.

beneficio_neto <- function(p, y, pt) {
  pos <- p >= pt
  tp <- sum(pos & y == 1)
  fp <- sum(pos & y == 0)
  n <- length(y)
  tp / n - fp / n * pt / (1 - pt)
}

objetivos <- list(
  cualquier_foco = list(p = 1 - pru$sin_crecimiento,
                        y = as.integer(pru$clase != "sin_crecimiento")),
  urinario     = list(p = pru$urinario,
                      y = as.integer(pru$clase == "urinario")),
  respiratorio = list(p = pru$respiratorio,
                      y = as.integer(pru$clase == "respiratorio")),
  sangre       = list(p = pru$sangre,
                      y = as.integer(pru$clase == "sangre")))

UMBRALES <- seq(0.01, 0.30, by = 0.01)

res <- do.call(rbind, lapply(names(objetivos), function(nom) {
  o <- objetivos[[nom]]
  prev <- mean(o$y)
  do.call(rbind, lapply(UMBRALES, function(pt) {
    data.frame(objetivo = nom, umbral = pt,
               prevalencia = round(prev, 4),
               bn_modelo = round(beneficio_neto(o$p, o$y, pt), 5),
               bn_tratar_todos = round(prev - (1 - prev) * pt / (1 - pt), 5),
               bn_no_tratar = 0,
               row.names = NULL)
  }))
}))

res$util <- res$bn_modelo > pmax(res$bn_tratar_todos, 0)

cat("=== PREVALENCIA POR OBJETIVO EN EL CONJUNTO DE PRUEBA ===\n")
print(unique(res[, c("objetivo","prevalencia")]), row.names = FALSE)

cat("\n=== BENEFICIO NETO EN UMBRALES SELECCIONADOS ===\n")
sel <- res[res$umbral %in% c(0.02, 0.05, 0.10, 0.15, 0.20, 0.30), ]
print(sel[, c("objetivo","umbral","bn_modelo","bn_tratar_todos","util")],
      row.names = FALSE)

cat("\n=== VENTANA DE UTILIDAD POR OBJETIVO ===\n")
for (nom in names(objetivos)) {
  u <- res[res$objetivo == nom & res$util, "umbral"]
  if (length(u) == 0) {
    cat(sprintf("%-16s sin ventana util: nunca supera a ambas referencias\n",
        nom))
  } else {
    cat(sprintf("%-16s util entre %.2f y %.2f (%d umbrales de %d)\n",
        nom, min(u), max(u), length(u), length(UMBRALES)))
  }
}

dir.create("outputs/fase16", recursive = TRUE, showWarnings = FALSE)
write.csv(res, "outputs/fase16/curvas_decision.csv", row.names = FALSE)
cat("\nGuardado en outputs/fase16/curvas_decision.csv\n")
