m <- read.csv("data/derivados/glasgow.csv", stringsAsFactors = FALSE)

# El diagnostico de discriminacion se realiza sobre los conjuntos de
# desarrollo. La decision sobre que variables integran el modelo constituye
# una eleccion de diseno y no una evaluacion de resultados, de modo que no
# procede examinar el conjunto de prueba para adoptarla.
#
# La unidad reservada si se examina, al final de este procedimiento, y
# conviene declararlo en lugar de enunciar una regla que el propio
# procedimiento incumple. Alli se describe la distribucion de la escala de
# conciencia en esa unidad, porque una variable carente de variacion en la
# sede de validacion externa no puede sostener prediccion alguna y esa
# comprobacion precede a admitirla. Es un descriptivo de una variable
# candidata y no una medida de desempeno, ocurre con el modelo primario ya
# congelado y evaluado, y el modelo ampliado no llega a evaluarse en esa
# unidad. Aun asi, la unidad deja de estar intacta para la extension y el
# documento publico lo declara.
d <- m[m$grupo %in% c("entrenamiento","calibracion"), ]
CL <- c("sin_crecimiento","urinario","respiratorio","sangre")

cat("Estancias en entrenamiento y calibracion:", nrow(d), "\n")

auc <- function(p, y) {
  ok <- !is.na(p) & !is.na(y)
  p <- p[ok]; y <- y[ok]
  n1 <- sum(y == 1); n0 <- sum(y == 0)
  if (n1 < 10 || n0 < 10) return(NA)
  r <- rank(p)
  round((sum(r[y == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0), 4)
}

cat("\n=== DISTRIBUCION DE CATEGORIAS SEGUN PRESENCIA DE TUBO ===\n")
sub <- d[!is.na(d$intubado), ]
tab <- table(tubo = ifelse(sub$intubado == 1, "con tubo", "sin tubo"),
             clase = sub$clase)
cat("Recuentos\n"); print(tab)
cat("\nPorcentaje dentro de cada fila\n")
print(round(100 * prop.table(tab, 1), 2))

# Discriminacion atribuible al procedimiento por si solo. La cifra establece
# el techo de la asociacion espuria: cuanto predice cada categoria el mero
# hecho de portar tubo endotraqueal, con independencia de toda medida
# fisiologica.
cat("\n=== DISCRIMINACION DEL INDICADOR DE TUBO, AISLADO ===\n")
print(do.call(rbind, lapply(CL, function(k) {
  data.frame(clase = k,
             auc_solo_tubo = auc(d$intubado, as.integer(d$clase == k)),
             row.names = NULL)
})), row.names = FALSE)

# Comprobacion determinante. Dentro de cada estrato la condicion de portar
# tubo permanece constante y no puede sostener asociacion alguna. Si la
# discriminacion de la escala se conserva, esta reflejaria estado
# neurologico; si desciende hasta el azar, procederia por entero del
# procedimiento.
#
# Se emplea la puntuacion con signo invertido porque un valor menor
# corresponde a mayor probabilidad de la categoria, mientras que el area bajo
# la curva presupone lo contrario.
# El estrato completo se nombra por el conjunto que es. Se llamaba "cohorte
# completa", y la cohorte son 23.213 estancias: esto son las de entrenamiento
# y calibracion. La misma etiqueta nombraba ademas otra cantidad distinta en
# el deposito de constantes por estrato, de modo que una palabra falsa
# designaba dos conjuntos.
estratos <- list(
  "entrenamiento y calibracion" = rep(TRUE, nrow(d)),
  "con tubo endotraqueal" = !is.na(d$intubado) & d$intubado == 1,
  "sin tubo endotraqueal" = !is.na(d$intubado) & d$intubado == 0)

res <- do.call(rbind, lapply(names(estratos), function(nom) {
  s <- d[estratos[[nom]], ]
  do.call(rbind, lapply(CL, function(k) {
    y <- as.integer(s$clase == k)
    data.frame(estrato = nom, clase = k, n = nrow(s), casos = sum(y),
               auc_gcs_total = auc(-s$gcs_total, y),
               auc_gcs_em = auc(-s$gcs_em, y),
               row.names = NULL)
  }))
}))

cat("\n=== DISCRIMINACION DE LA ESCALA POR ESTRATO ===\n")
print(res, row.names = FALSE)

cat("\n=== PERDIDA DE DISCRIMINACION AL ESTRATIFICAR ===\n")
print(do.call(rbind, lapply(CL, function(k) {
  a <- res[res$estrato == "entrenamiento y calibracion" & res$clase == k, ]
  b <- res[res$estrato == "con tubo endotraqueal" & res$clase == k, ]
  cc <- res[res$estrato == "sin tubo endotraqueal" & res$clase == k, ]
  data.frame(clase = k,
             completa_em = a$auc_gcs_em,
             con_tubo_em = b$auc_gcs_em,
             sin_tubo_em = cc$auc_gcs_em,
             caida_media = round(a$auc_gcs_em -
                                 mean(c(b$auc_gcs_em, cc$auc_gcs_em),
                                      na.rm = TRUE), 4),
             row.names = NULL)
})), row.names = FALSE)

# Distribucion de la escala en la unidad reservada. Una variable carente de
# variacion en la sede de validacion externa no puede sostener alli
# prediccion alguna, con independencia de su comportamiento en las unidades
# de desarrollo.
cat("\n=== VARIACION DE LA ESCALA EN LA UNIDAD RESERVADA ===\n")
sel <- m[m$grupo == "sellado", ]
print(data.frame(
  version = c("glasgow total","ocular mas motora"),
  p25 = c(quantile(sel$gcs_total, .25, na.rm = TRUE),
          quantile(sel$gcs_em, .25, na.rm = TRUE)),
  mediana = c(median(sel$gcs_total, na.rm = TRUE),
              median(sel$gcs_em, na.rm = TRUE)),
  p75 = c(quantile(sel$gcs_total, .75, na.rm = TRUE),
          quantile(sel$gcs_em, .75, na.rm = TRUE)),
  desviacion = round(c(sd(sel$gcs_total, na.rm = TRUE),
                       sd(sel$gcs_em, na.rm = TRUE)), 2),
  row.names = NULL), row.names = FALSE)
cat("Desviacion en entrenamiento y calibracion, ocular mas motora:",
    round(sd(d$gcs_em, na.rm = TRUE), 2), "\n")

dir.create("outputs/fase17", recursive = TRUE, showWarnings = FALSE)
write.csv(res, "outputs/fase17/circularidad_glasgow.csv", row.names = FALSE)
cat("\nGuardado en outputs/fase17/circularidad_glasgow.csv\n")
