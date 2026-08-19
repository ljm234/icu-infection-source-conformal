m <- read.csv("outputs/fase4/matriz_limpia.csv")

vars <- c("leucocitos","hemoglobina","plaquetas","rdw","creatinina","urea",
          "brecha_anion","sodio","potasio","cloro","bicarbonato","inr",
          "ttpa","ph","pco2","lactato","exceso_base")

unidades <- names(sort(table(m$unidad), decreasing = TRUE))
unidades <- unidades[table(m$unidad)[unidades] >= 500]

cat("\n=== 1. DISTRIBUCION DE CLASES POR UNIDAD (%) ===\n")
tab <- table(m$unidad, m$clase)
tab <- tab[unidades, ]
print(round(100 * prop.table(tab, 1), 1))

cat("\n=== 2. PREVALENCIA DE CONFIRMACION POR UNIDAD ===\n")
prev <- data.frame(
  unidad = unidades,
  n = as.integer(table(m$unidad)[unidades]),
  pct_confirmado = round(100 * sapply(unidades, function(u)
    mean(m$clase[m$unidad == u] %in%
         c("sangre","respiratorio","urinario"))), 1))
print(prev[order(-prev$pct_confirmado), ], row.names = FALSE)

cat("\n=== 3. FALTANTES PROMEDIO POR UNIDAD (%) ===\n")
falt <- data.frame(
  unidad = unidades,
  pct_faltante = round(100 * sapply(unidades, function(u)
    mean(is.na(as.matrix(m[m$unidad == u, vars])))), 1))
print(falt[order(-falt$pct_faltante), ], row.names = FALSE)

cat("\n=== 4. DISTANCIA DE CADA UNIDAD AL RESTO ===\n")
# Para cada variable se calcula la diferencia estandarizada entre la unidad
# y el resto de la cohorte. Se promedia sobre variables. Valores altos
# indican una unidad cuyos pacientes difieren del conjunto.
dist <- sapply(unidades, function(u) {
  dentro <- m[m$unidad == u, vars]
  fuera  <- m[m$unidad != u, vars]
  d <- sapply(vars, function(v) {
    a <- dentro[[v]]; b <- fuera[[v]]
    s <- sqrt((var(a, na.rm = TRUE) + var(b, na.rm = TRUE)) / 2)
    if (is.na(s) || s == 0) return(NA)
    abs(mean(a, na.rm = TRUE) - mean(b, na.rm = TRUE)) / s
  })
  mean(d, na.rm = TRUE)
})
print(data.frame(unidad = unidades,
                 distancia = round(dist, 3),
                 row.names = NULL)[order(-dist), ], row.names = FALSE)

cat("\n=== 5. EDAD Y SEXO POR UNIDAD ===\n")
demo <- data.frame(
  unidad = unidades,
  edad_mediana = sapply(unidades, function(u) median(m$edad[m$unidad == u])),
  pct_mujer = round(100 * sapply(unidades, function(u)
    mean(m$sexo[m$unidad == u] == "F")), 1))
print(demo, row.names = FALSE)

dir.create("outputs/fase5", recursive = TRUE, showWarnings = FALSE)
write.csv(as.data.frame.matrix(round(100 * prop.table(tab, 1), 1)),
          "outputs/fase5/clases_por_unidad.csv")
write.csv(data.frame(unidad = unidades, distancia = round(dist, 3)),
          "outputs/fase5/distancia_unidades.csv", row.names = FALSE)
