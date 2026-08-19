cal <- read.csv("outputs/fase8/prob_calibracion.csv", stringsAsFactors = FALSE)
pru <- read.csv("outputs/fase8/prueba_con_conjuntos.csv", stringsAsFactors = FALSE)
CLASES <- c("sin_crecimiento","urinario","respiratorio","sangre")

umbral_conforme <- function(s, alpha) {
  n <- length(s)
  k <- ceiling((n + 1) * (1 - alpha))
  if (k > n) return(Inf)
  sort(s)[k]
}

# Se recorre el nivel de confianza nominal para describir el compromiso
# entre la garantia declarada, la proporcion de casos que el sistema
# resuelve y el error que comete entre ellos. Un solo nivel no informa
# sobre la forma de esa relacion.
ALFAS <- c(0.05, 0.10, 0.15, 0.20, 0.25, 0.30, 0.35, 0.40, 0.50)

res <- do.call(rbind, lapply(ALFAS, function(a) {
  um <- sapply(CLASES, function(k)
    umbral_conforme(1 - cal[cal$clase == k, k], a))

  pert <- sapply(CLASES, function(k) (1 - pru[[k]]) <= um[k])
  colnames(pert) <- CLASES
  tam <- rowSums(pert)
  cub <- sapply(seq_len(nrow(pru)), function(i)
    pert[i, as.character(pru$clase[i])])

  decide <- tam == 1
  if (sum(decide) > 0) {
    unica <- sapply(which(decide), function(i) CLASES[pert[i, ]][1])
    err <- 1 - mean(unica == as.character(pru$clase[decide]))
    n_min <- sum(unica != "sin_crecimiento")
    acierto_min <- if (n_min > 0)
      sum(unica != "sin_crecimiento" &
          unica == as.character(pru$clase[decide])) else 0
  } else {
    err <- NA; n_min <- 0; acierto_min <- 0
  }

  cob_cl <- sapply(CLASES, function(k) mean(cub[pru$clase == k]))

  data.frame(
    alfa = a,
    confianza = 1 - a,
    cobertura_marginal = round(mean(cub), 4),
    cobertura_minima_clase = round(min(cob_cl), 4),
    tamano_medio = round(mean(tam), 3),
    pct_conjunto_completo = round(100 * mean(tam == 4), 1),
    pct_resuelve = round(100 * mean(decide), 1),
    error_entre_resueltos = round(err, 4),
    conclusiones_minoritarias = n_min,
    aciertos_minoritarios = acierto_min,
    pct_vacio = round(100 * mean(tam == 0), 1),
    row.names = NULL)
}))

cat("\n=== COMPROMISO ENTRE CONFIANZA, RESOLUCION Y ERROR ===\n")
print(res, row.names = FALSE)

cat("\n=== COBERTURA POR CLASE EN CADA NIVEL ===\n")
cob_det <- do.call(rbind, lapply(ALFAS, function(a) {
  um <- sapply(CLASES, function(k)
    umbral_conforme(1 - cal[cal$clase == k, k], a))
  pert <- sapply(CLASES, function(k) (1 - pru[[k]]) <= um[k])
  colnames(pert) <- CLASES
  cub <- sapply(seq_len(nrow(pru)), function(i)
    pert[i, as.character(pru$clase[i])])
  d <- data.frame(alfa = a, nominal = 1 - a,
                  t(round(sapply(CLASES, function(k)
                    mean(cub[pru$clase == k])), 4)))
  names(d)[3:6] <- CLASES
  d
}))
print(cob_det, row.names = FALSE)

dir.create("outputs/fase9", recursive = TRUE, showWarnings = FALSE)
write.csv(res,     "outputs/fase9/curva_alfa.csv", row.names = FALSE)
write.csv(cob_det, "outputs/fase9/cobertura_por_alfa.csv", row.names = FALSE)
