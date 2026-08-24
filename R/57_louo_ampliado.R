library(glmnet)
library(splines)
set.seed(20260818)

imps <- readRDS("data/derivados/imputaciones_ampliadas.rds")
amp  <- readRDS("data/derivados/modelo_ampliado_final.rds")
esp  <- readRDS("outputs/fase7/especificacion.rds")

M <- length(imps)
CL <- c("sin_crecimiento","urinario","respiratorio","sangre")
VITALES <- c("temperatura","frec_cardiaca","frec_respiratoria","saturacion")
ALFA <- 0.10

base <- do.call(rbind, lapply(imps, function(x)
  x[x$grupo %in% c("entrenamiento","calibracion","prueba"), ]))
base <- base[base$clase %in% CL, ]
base$clase <- factor(as.character(base$clase), levels = CL)

UNIDADES <- sort(unique(as.character(base$unidad)))
cat("Unidades de desarrollo:", length(UNIDADES), "\n")
print(UNIDADES)

base_spline <- function(x, nu, nombre) {
  b <- ns(x, knots = nu[2:3], Boundary.knots = nu[c(1,4)])
  colnames(b) <- paste0(nombre, "_s", seq_len(ncol(b)))
  as.matrix(b)
}

# Los nudos permanecen invariables a lo largo de las cinco iteraciones.
# Recalcularlos en cada una haria depender la forma funcional de que unidad
# se excluyo, con lo que la comparacion mezclaria dos efectos distintos.
construir <- function(d, niveles) {
  bl <- list()
  for (v in esp$con_spline) bl[[v]] <- base_spline(d[[v]], esp$nudos[[v]], v)
  for (v in amp$con_spline) bl[[v]] <- base_spline(d[[v]], amp$nudos_vit[[v]], v)
  lineales_vit <- setdiff(VITALES, amp$con_spline)
  lin <- as.matrix(d[, c(esp$lineales, lineales_vit), drop = FALSE])
  otr <- data.frame(edad = d$edad,
                    lactato_medido = d$lactato_medido,
                    sexo_M = as.integer(d$sexo == "M"))
  um <- factor(ifelse(as.character(d$unidad) %in% niveles,
                      as.character(d$unidad), niveles[1]),
               levels = niveles)
  uni <- model.matrix(~ um - 1)[, -1, drop = FALSE]
  cbind(do.call(cbind, bl), lin, as.matrix(otr), uni)
}

umbral_conforme <- function(s, alpha) {
  n <- length(s); k <- ceiling((n + 1) * (1 - alpha))
  if (k > n) return(Inf)
  sort(s)[k]
}

auc <- function(p, yb) {
  r <- rank(p); n1 <- sum(yb); n0 <- length(yb) - n1
  if (n1 < 3 || n0 < 3) return(NA)
  (sum(r[yb == 1]) - n1*(n1+1)/2) / (n1*n0)
}

resultados <- list()

for (u in UNIDADES) {
  cat("\nDejando fuera:", u, "\n")

  dentro <- base[as.character(base$unidad) != u, ]
  fuera  <- base[as.character(base$unidad) == u, ]

  # Las unidades restantes se reparten en un subconjunto de ajuste y otro de
  # calibracion conforme. La particion se realiza por paciente, de modo que
  # las copias imputadas de un mismo caso permanezcan juntas y la
  # calibracion conserve su validez.
  pac <- unique(dentro$stay_id)
  cal_pac <- sample(pac, floor(0.35 * length(pac)))
  d_cal <- dentro[dentro$stay_id %in% cal_pac, ]
  d_ent <- dentro[!dentro$stay_id %in% cal_pac, ]

  niveles <- sort(unique(as.character(d_ent$unidad)))

  X <- construir(d_ent, niveles)
  y <- d_ent$clase
  w <- rep(1/M, nrow(X))

  # Se conserva el valor de penalizacion del modelo ajustado sobre la cohorte
  # completa en lugar de reoptimizarlo en cada iteracion. La eleccion replica
  # el tratamiento aplicado en el analisis de transportabilidad del modelo
  # original, sin el cual la comparacion entre ambos careceria de sentido.
  mod <- glmnet(X, y, family = "multinomial", alpha = 1,
                weights = w, lambda = amp$lambda, standardize = TRUE)

  prob <- function(dat) {
    p <- predict(mod, newx = construir(dat, niveles), type = "response")[, , 1]
    ag <- aggregate(p, by = list(stay_id = dat$stay_id), FUN = mean)
    et <- unique(dat[, c("stay_id","clase")])
    merge(ag, et, by = "stay_id")
  }

  pc <- prob(d_cal)
  pf <- prob(fuera)

  um <- sapply(CL, function(k) {
    s <- 1 - pc[pc$clase == k, k]
    if (length(s) < 10) return(NA)
    umbral_conforme(s, ALFA)
  })

  pert <- sapply(CL, function(k)
    if (is.na(um[k])) rep(FALSE, nrow(pf)) else (1 - pf[[k]]) <= um[k])
  colnames(pert) <- CL
  tam <- rowSums(pert)
  cub <- sapply(seq_len(nrow(pf)), function(i)
    pert[i, as.character(pf$clase[i])])

  cob_cl <- sapply(CL, function(k) {
    s <- pf$clase == k
    if (sum(s) < 5) return(NA)
    mean(cub[s])
  })

  a_cl <- sapply(CL, function(k) auc(pf[[k]], as.integer(pf$clase == k)))

  resultados[[u]] <- list(unidad = u, n = nrow(pf),
                          cobertura = mean(cub), cob_clase = cob_cl,
                          auc = a_cl, tamano = mean(tam),
                          resuelve = mean(tam == 1))
}

cat("\n=== COBERTURA MARGINAL POR UNIDAD EXCLUIDA ===\n")
tab <- do.call(rbind, lapply(resultados, function(r)
  data.frame(unidad = substr(r$unidad, 1, 34), n = r$n,
             cobertura = round(r$cobertura, 4),
             tamano_medio = round(r$tamano, 3),
             pct_resuelve = round(100 * r$resuelve, 1),
             row.names = NULL)))
print(tab, row.names = FALSE)
cat("\nNominal:", 1 - ALFA, "\n")
cat("Media:", round(mean(tab$cobertura), 4),
    " Desviacion:", round(sd(tab$cobertura), 4), "\n")
cat("Rango:", round(min(tab$cobertura), 4), "a",
    round(max(tab$cobertura), 4), "\n")

cat("\n=== COBERTURA POR CATEGORIA Y UNIDAD ===\n")
cc <- do.call(rbind, lapply(resultados, function(r)
  data.frame(unidad = substr(r$unidad, 1, 30),
             t(round(r$cob_clase, 4)), row.names = NULL)))
names(cc)[2:5] <- CL
print(cc, row.names = FALSE)

cat("\n=== DISCRIMINACION POR CATEGORIA Y UNIDAD ===\n")
aa <- do.call(rbind, lapply(resultados, function(r)
  data.frame(unidad = substr(r$unidad, 1, 30),
             t(round(r$auc, 4)), row.names = NULL)))
names(aa)[2:5] <- CL
print(aa, row.names = FALSE)

cat("\n=== HETEROGENEIDAD DEL DESEMPENO ===\n")
for (k in CL) {
  v <- aa[[k]]
  cat(sprintf("%-16s media %.4f  sd %.4f  rango %.4f a %.4f\n",
      k, mean(v, na.rm = TRUE), sd(v, na.rm = TRUE),
      min(v, na.rm = TRUE), max(v, na.rm = TRUE)))
}

# Contraste directo con el analisis de transportabilidad del modelo original,
# cuyos resultados se recuperan del deposito de la fase correspondiente.
cat("\n=== CONTRASTE CON EL MODELO ORIGINAL ===\n")
ori_cob <- read.csv("outputs/fase10/cobertura_louo.csv", stringsAsFactors = FALSE)
ori_auc <- read.csv("outputs/fase10/auc_louo.csv", stringsAsFactors = FALSE)

print(data.frame(
  indicador = c("cobertura media", "desviacion de la cobertura",
                "cobertura minima", "cobertura maxima",
                "proporcion resuelta media"),
  original = round(c(mean(ori_cob$cobertura), sd(ori_cob$cobertura),
                     min(ori_cob$cobertura), max(ori_cob$cobertura),
                     mean(ori_cob$pct_resuelve)), 4),
  ampliado = round(c(mean(tab$cobertura), sd(tab$cobertura),
                     min(tab$cobertura), max(tab$cobertura),
                     mean(tab$pct_resuelve)), 4),
  row.names = NULL), row.names = FALSE)

cat("\nDiscriminacion media por categoria\n")
print(data.frame(
  clase = CL,
  original = round(sapply(CL, function(k) mean(ori_auc[[k]], na.rm = TRUE)), 4),
  ampliado = round(sapply(CL, function(k) mean(aa[[k]], na.rm = TRUE)), 4),
  diferencia = round(sapply(CL, function(k)
    mean(aa[[k]], na.rm = TRUE) - mean(ori_auc[[k]], na.rm = TRUE)), 4),
  row.names = NULL), row.names = FALSE)

dir.create("outputs/fase19", recursive = TRUE, showWarnings = FALSE)
write.csv(tab, "outputs/fase19/cobertura_louo_ampliado.csv", row.names = FALSE)
write.csv(cc,  "outputs/fase19/cobertura_clase_louo_ampliado.csv", row.names = FALSE)
write.csv(aa,  "outputs/fase19/auc_louo_ampliado.csv", row.names = FALSE)
cat("\nGuardado en outputs/fase19\n")
