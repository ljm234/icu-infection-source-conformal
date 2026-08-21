set.seed(20260818)

sd_ <- read.csv("outputs/fase11/sellado_evaluado.csv", stringsAsFactors = FALSE)
CLASES <- c("sin_crecimiento","urinario","respiratorio","sangre")
ALFA <- 0.10

cuantil_conforme <- function(s, alpha) {
  n <- length(s)
  k <- ceiling((n + 1) * (1 - alpha))
  if (k > n) return(NA)
  sort(s)[k]
}

cat("=== CASOS DISPONIBLES EN LA SEDE ===\n")
print(table(sd_$clase))
cat("\nMinimo teorico por clase para alpha =", ALFA, ":",
    ceiling((1 - ALFA) / ALFA), "\n")

# La cohorte de la sede se divide en una porcion reservada para evaluacion y
# otra destinada a recalibracion. La porcion de evaluacion permanece fija a
# lo largo del ejercicio, de modo que las comparaciones entre tamanos de
# calibracion se realicen sobre el mismo conjunto de pacientes.
idx <- sample(nrow(sd_))
n_eval <- floor(0.40 * nrow(sd_))
eval_ <- sd_[idx[1:n_eval], ]
pool  <- sd_[idx[(n_eval + 1):nrow(sd_)], ]

cat("\nEvaluacion fija:", nrow(eval_), " Reserva para calibracion:",
    nrow(pool), "\n")

# Umbrales originales, procedentes del conjunto de calibracion de las
# unidades de desarrollo.
cal_orig <- read.csv("outputs/fase8/prob_calibracion.csv",
                     stringsAsFactors = FALSE)
um_orig <- sapply(CLASES, function(k)
  cuantil_conforme(1 - cal_orig[cal_orig$clase == k, k], ALFA))

medir <- function(datos, umbrales) {
  pert <- sapply(CLASES, function(k) {
    if (is.na(umbrales[k])) return(rep(NA, nrow(datos)))
    (1 - datos[[k]]) <= umbrales[k]
  })
  colnames(pert) <- CLASES
  tam <- rowSums(pert, na.rm = TRUE)
  cub <- sapply(seq_len(nrow(datos)), function(i)
    pert[i, as.character(datos$clase[i])])
  cob <- sapply(CLASES, function(k) {
    s <- datos$clase == k
    if (sum(s) < 5) return(NA)
    mean(cub[s], na.rm = TRUE)
  })
  list(marginal = mean(cub, na.rm = TRUE), por_clase = cob,
       tamano = mean(tam), resuelve = mean(tam == 1))
}

base_ <- medir(eval_, um_orig)

cat("\n=== SIN RECALIBRACION ===\n")
cat("Cobertura marginal:", round(base_$marginal, 4), "\n")
print(round(base_$por_clase, 4))
cat("Tamano medio:", round(base_$tamano, 3),
    " Resuelve:", round(100 * base_$resuelve, 1), "por ciento\n")

# Se examina como evoluciona la garantia al incorporar volumenes crecientes
# de casos locales. Cada tamano se repite veinte veces con muestras
# distintas, de modo que la variabilidad reportada refleje el efecto del
# muestreo y no una realizacion particular.
TAMANOS <- c(50, 100, 200, 400, 800, 1600, nrow(pool))
REPS <- 20

res <- do.call(rbind, lapply(TAMANOS, function(n) {
  m <- t(replicate(REPS, {
    s <- pool[sample(nrow(pool), min(n, nrow(pool))), ]
    um <- sapply(CLASES, function(k) {
      sc <- 1 - s[s$clase == k, k]
      if (length(sc) < ceiling((1 - ALFA) / ALFA)) return(NA)
      cuantil_conforme(sc, ALFA)
    })
    r <- medir(eval_, um)
    c(r$marginal, r$resuelve, r$tamano,
      sum(!is.na(um)))
  }))
  data.frame(n_local = min(n, nrow(pool)),
             cobertura = round(mean(m[,1]), 4),
             sd_cobertura = round(sd(m[,1]), 4),
             pct_resuelve = round(100 * mean(m[,2]), 1),
             tamano_medio = round(mean(m[,3]), 3),
             clases_calibrables = round(mean(m[,4]), 1),
             row.names = NULL)
}))

cat("\n=== EFECTO DEL VOLUMEN LOCAL DE CALIBRACION ===\n")
cat("Nominal:", 1 - ALFA, "\n\n")
print(res, row.names = FALSE)

# Alternativa marginal: un unico umbral calculado sobre el conjunto local
# completo, sin distinguir clases. Renuncia a la garantia condicional pero
# resulta aplicable cuando las categorias poco frecuentes no reunen el
# minimo requerido.
um_marg <- cuantil_conforme(
  sapply(seq_len(nrow(pool)), function(i)
    1 - pool[i, as.character(pool$clase[i])]), ALFA)
marg <- medir(eval_, setNames(rep(um_marg, 4), CLASES))

cat("\n=== RECALIBRACION MARGINAL, UMBRAL UNICO ===\n")
cat("Cobertura marginal:", round(marg$marginal, 4), "\n")
print(round(marg$por_clase, 4))
cat("Resuelve:", round(100 * marg$resuelve, 1), "por ciento\n")

dir.create("outputs/fase12", recursive = TRUE, showWarnings = FALSE)
write.csv(res, "outputs/fase12/recalibracion.csv", row.names = FALSE)
