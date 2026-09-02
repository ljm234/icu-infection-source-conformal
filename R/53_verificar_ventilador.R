v <- read.csv("data/derivados/vitales_limpios.csv", stringsAsFactors = FALSE)
g <- read.csv("data/derivados/glasgow.csv", stringsAsFactors = FALSE)

CL <- c("sin_crecimiento","urinario","respiratorio","sangre")
VITALES <- c("temperatura","frec_cardiaca","frec_respiratoria","saturacion")

d <- merge(v[, c("stay_id","clase","grupo","unidad", VITALES)],
           g[, c("stay_id","intubado")], by = "stay_id")
d <- d[d$grupo %in% c("entrenamiento","calibracion"), ]
d <- d[d$clase %in% CL, ]
d <- d[!is.na(d$intubado), ]

cat("Estancias examinadas:", nrow(d), "\n")
cat("Con tubo endotraqueal:", sum(d$intubado == 1),
    sprintf("(%.1f por ciento)\n", 100 * mean(d$intubado == 1)))

auc <- function(p, y) {
  ok <- !is.na(p) & !is.na(y)
  p <- p[ok]; y <- y[ok]
  n1 <- sum(y == 1); n0 <- sum(y == 0)
  if (n1 < 10 || n0 < 10) return(NA)
  r <- rank(p)
  round((sum(r[y == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0), 4)
}

# La ventilacion mecanica puede fijar total o parcialmente la frecuencia
# respiratoria del paciente, y la administracion de oxigeno suplementario
# eleva la saturacion con independencia del estado pulmonar. La sedacion que
# acompana al procedimiento deprime ademas la frecuencia cardiaca. Antes de
# incorporar estas constantes al modelo procede establecer que su capacidad
# discriminativa no procede del aparato.
#
# La estratificacion resuelve la cuestion: dentro de cada estrato la
# condicion de portar tubo permanece constante y no puede sostener asociacion
# alguna. Una senal que se conserve en ambos estratos sera fisiologica.

# Como en el deposito de la escala de conciencia: el estrato completo son las
# estancias de entrenamiento y calibracion en las categorias modeladas, no la
# cohorte.
estratos <- list(
  "entrenamiento y calibracion" = rep(TRUE, nrow(d)),
  "con tubo"         = d$intubado == 1,
  "sin tubo"         = d$intubado == 0)

res <- do.call(rbind, lapply(names(estratos), function(nom) {
  s <- d[estratos[[nom]], ]
  do.call(rbind, lapply(VITALES, function(vv) {
    fila <- data.frame(estrato = nom, variable = vv, n = nrow(s),
                       row.names = NULL)
    for (k in CL) fila[[k]] <- auc(s[[vv]], as.integer(s$clase == k))
    fila
  }))
}))

cat("\n=== DISCRIMINACION POR ESTRATO ===\n")
print(res, row.names = FALSE)

cat("\n=== CONSERVACION DE LA SENAL AL ESTRATIFICAR ===\n")
print(do.call(rbind, lapply(VITALES, function(vv) {
  a <- res[res$estrato == "entrenamiento y calibracion" &
           res$variable == vv, ]
  b <- res[res$estrato == "con tubo"         & res$variable == vv, ]
  cc <- res[res$estrato == "sin tubo"        & res$variable == vv, ]
  fc <- max(abs(unlist(a[CL]) - 0.5))
  ft <- max(abs(unlist(b[CL]) - 0.5))
  fs <- max(abs(unlist(cc[CL]) - 0.5))
  data.frame(variable = vv,
             fuerza_completa = round(fc, 4),
             fuerza_con_tubo = round(ft, 4),
             fuerza_sin_tubo = round(fs, 4),
             proporcion_conservada = round(mean(c(ft, fs)) / fc, 3),
             row.names = NULL)
})), row.names = FALSE)

# Una constante cuyo valor establezca un aparato presentara menor dispersion
# en el grupo sometido a el, dado que el ajuste sustituye a la variabilidad
# propia del paciente. El cociente entre ambas dispersiones cuantifica ese
# efecto. Se emplea tanto la desviacion tipica como el rango intercuartilico,
# cuya discrepancia revelaria la influencia de valores extremos.
cat("\n=== DISPERSION SEGUN PRESENCIA DE TUBO ===\n")
con <- d[d$intubado == 1, ]; sin_ <- d[d$intubado == 0, ]
print(do.call(rbind, lapply(VITALES, function(vv) {
  data.frame(variable = vv,
             mediana_con = round(median(con[[vv]], na.rm = TRUE), 1),
             mediana_sin = round(median(sin_[[vv]], na.rm = TRUE), 1),
             sd_con = round(sd(con[[vv]], na.rm = TRUE), 2),
             sd_sin = round(sd(sin_[[vv]], na.rm = TRUE), 2),
             cociente_sd = round(sd(con[[vv]], na.rm = TRUE) /
                                 sd(sin_[[vv]], na.rm = TRUE), 3),
             cociente_iqr = round(IQR(con[[vv]], na.rm = TRUE) /
                                  IQR(sin_[[vv]], na.rm = TRUE), 3),
             row.names = NULL)
})), row.names = FALSE)

cat("\n=== VALORES MAS FRECUENTES DE LA FRECUENCIA RESPIRATORIA ===\n")
cat("Con tubo endotraqueal\n")
print(head(sort(table(con$frec_respiratoria), decreasing = TRUE), 8))
cat("\nSin tubo endotraqueal\n")
print(head(sort(table(sin_$frec_respiratoria), decreasing = TRUE), 8))

dir.create("outputs/fase17", recursive = TRUE, showWarnings = FALSE)
write.csv(res, "outputs/fase17/vitales_por_estrato.csv", row.names = FALSE)
cat("\nGuardado en outputs/fase17/vitales_por_estrato.csv\n")
