m <- read.csv("outputs/fase4/matriz.csv")

# Limites fisiologicos compatibles con la vida. Los valores fuera de rango
# corresponden a error de registro o de unidad y se marcan como ausentes,
# no se recortan, porque recortar inventa un valor que nunca se midio.
limites <- list(
  leucocitos   = c(0,    500),
  hemoglobina  = c(1,    25),
  plaquetas    = c(1,    3000),
  rdw          = c(8,    50),
  creatinina   = c(0.05, 30),
  urea         = c(1,    300),
  brecha_anion = c(-10,  60),
  sodio        = c(90,   200),
  potasio      = c(1,    12),
  cloro        = c(50,   160),
  bicarbonato  = c(1,    60),
  inr          = c(0.5,  30),
  ttpa         = c(10,   200),
  ph           = c(6.5,  7.9),
  pco2         = c(5,    250),
  lactato      = c(0.1,  40),
  exceso_base  = c(-40,  40))

cat("\n=== VALORES FUERA DE RANGO ===\n")
for (v in names(limites)) {
  lo <- limites[[v]][1]; hi <- limites[[v]][2]
  fuera <- which(!is.na(m[[v]]) & (m[[v]] < lo | m[[v]] > hi))
  if (length(fuera) > 0) {
    cat(sprintf("%-14s %3d casos fuera de [%g, %g]: %s\n",
        v, length(fuera), lo, hi,
        paste(head(sort(m[[v]][fuera]), 5), collapse = ", ")))
    m[[v]][fuera] <- NA
  }
}

cat("\n=== FALTANTES TRAS LA LIMPIEZA ===\n")
vars <- names(limites)
print(data.frame(
  variable = vars,
  pct = round(100 * sapply(vars, function(v) mean(is.na(m[[v]]))), 1),
  row.names = NULL), row.names = FALSE)

completos <- sum(complete.cases(m[, vars]))
cat("\nCasos completos:", completos,
    sprintf("(%.1f%%)\n", 100 * completos / nrow(m)))

write.csv(m, "outputs/fase4/matriz_limpia.csv", row.names = FALSE)
cat("\nMatriz limpia escrita.\n")
