library(glmnet)
library(splines)

ALFA <- 0.10
OUT <- "outputs/fase11"
dir.create(OUT, recursive = TRUE, showWarnings = FALSE)

esp <- readRDS("outputs/fase7/especificacion.rds")
mod <- readRDS("outputs/fase8/modelo_final.rds")
imps <- readRDS("outputs/fase6/imputaciones.rds")
M <- length(imps)
CLASES <- esp$clases

base_ent <- imps[[1]][imps[[1]]$grupo == "entrenamiento", ]
niveles <- sort(unique(base_ent$unidad))

imps <- lapply(imps, function(x) {
  x$unidad_mod <- factor(ifelse(x$unidad %in% niveles,
                                as.character(x$unidad), niveles[1]),
                         levels = niveles)
  x
})

construir <- function(datos) {
  bl <- list()
  for (v in esp$con_spline) {
    nu <- esp$nudos[[v]]
    b <- ns(datos[[v]], knots = nu[2:3], Boundary.knots = nu[c(1,4)])
    colnames(b) <- paste0(v, "_s", seq_len(ncol(b)))
    bl[[v]] <- as.matrix(b)
  }
  lin <- as.matrix(datos[, esp$lineales, drop = FALSE])
  otr <- data.frame(edad = datos$edad,
                    lactato_medido = datos$lactato_medido,
                    sexo_M = as.integer(datos$sexo == "M"))
  uni <- model.matrix(~ unidad_mod - 1, data = datos)[, -1, drop = FALSE]
  cbind(do.call(cbind, bl), lin, as.matrix(otr), uni)
}

# Los umbrales conformes provienen del conjunto de calibracion original y no
# se recalculan sobre la unidad sellada. Recalcularlos equivaldria a emplear
# datos de la sede de validacion, que es precisamente lo que el sellado
# impide.
cal <- read.csv("outputs/fase8/prob_calibracion.csv", stringsAsFactors = FALSE)
umbral_conforme <- function(s, alpha) {
  n <- length(s); k <- ceiling((n + 1) * (1 - alpha))
  if (k > n) return(Inf)
  sort(s)[k]
}
um <- sapply(CLASES, function(k)
  umbral_conforme(1 - cal[cal$clase == k, k], ALFA))

sel <- do.call(rbind, lapply(imps, function(x) x[x$grupo == "sellado", ]))
sel <- sel[sel$clase %in% CLASES, ]
sel$clase <- factor(as.character(sel$clase), levels = CLASES)

p <- predict(mod, newx = construir(sel), type = "response")[, , 1]
ag <- aggregate(p, by = list(stay_id = sel$stay_id), FUN = mean)
et <- unique(sel[, c("stay_id","clase")])
sd_ <- merge(ag, et, by = "stay_id")

cat("=== CONJUNTO SELLADO ===\n")
cat("Unidad: Cardiac Vascular Intensive Care Unit (CVICU)\n")
cat("Pacientes:", nrow(sd_), "\n\n")
print(table(sd_$clase))

auc <- function(pv, yb) {
  r <- rank(pv); n1 <- sum(yb); n0 <- length(yb) - n1
  if (n1 < 3 || n0 < 3) return(NA)
  (sum(r[yb == 1]) - n1*(n1+1)/2) / (n1*n0)
}

cat("\n=== DISCRIMINACION ===\n")
print(do.call(rbind, lapply(CLASES, function(k)
  data.frame(clase = k, n = sum(sd_$clase == k),
             auc = round(auc(sd_[[k]], as.integer(sd_$clase == k)), 4),
             row.names = NULL))), row.names = FALSE)

cat("\n=== CALIBRACION ===\n")
print(do.call(rbind, lapply(CLASES, function(k)
  data.frame(clase = k,
             prevalencia = round(mean(sd_$clase == k), 4),
             prob_media = round(mean(sd_[[k]]), 4),
             diferencia = round(mean(sd_[[k]]) - mean(sd_$clase == k), 4),
             row.names = NULL))), row.names = FALSE)

pert <- sapply(CLASES, function(k) (1 - sd_[[k]]) <= um[k])
colnames(pert) <- CLASES
tam <- rowSums(pert)
cub <- sapply(seq_len(nrow(sd_)), function(i)
  pert[i, as.character(sd_$clase[i])])

cat("\n=== COBERTURA CONFORME ===\n")
cob <- do.call(rbind, lapply(CLASES, function(k) {
  s <- sd_$clase == k; n <- sum(s)
  ic <- if (n >= 5) binom.test(sum(cub[s]), n)$conf.int else c(NA, NA)
  data.frame(clase = k, n = n,
             cobertura = round(mean(cub[s]), 4),
             ic_inf = round(ic[1], 4), ic_sup = round(ic[2], 4),
             nominal = 1 - ALFA, row.names = NULL)
}))
print(cob, row.names = FALSE)
cat("\nCobertura marginal:", round(mean(cub), 4), "\n")
cat("Nominal:", 1 - ALFA, "\n")

cat("\n=== ABSTENCION ===\n")
cat("Tamano medio del conjunto:", round(mean(tam), 3), "\n")
print(table(tamano = tam))
decide <- tam == 1
cat("\nCasos con conclusion:", sum(decide),
    sprintf("(%.1f%%)\n", 100 * mean(decide)))
if (sum(decide) > 0) {
  unica <- sapply(which(decide), function(i) CLASES[pert[i, ]][1])
  cat("Error entre resueltos:",
      round(1 - mean(unica == as.character(sd_$clase[decide])), 4), "\n")
  print(table(emitida = unica, real = sd_$clase[decide]))
}

sd_$tamano <- tam; sd_$cubierto <- cub
write.csv(sd_, file.path(OUT, "sellado_evaluado.csv"), row.names = FALSE)
write.csv(cob, file.path(OUT, "cobertura_sellado.csv"), row.names = FALSE)
