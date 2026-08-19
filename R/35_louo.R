library(glmnet)
library(splines)
set.seed(20260818)

ALFA <- 0.10
OUT <- "outputs/fase10"
dir.create(OUT, recursive = TRUE, showWarnings = FALSE)

esp  <- readRDS("outputs/fase7/especificacion.rds")
imps <- readRDS("outputs/fase6/imputaciones.rds")
M <- length(imps)
CLASES <- esp$clases

base <- do.call(rbind, lapply(imps, function(x)
  x[x$grupo %in% c("entrenamiento","calibracion","prueba"), ]))
base <- base[base$clase %in% CLASES, ]
base$clase <- factor(as.character(base$clase), levels = CLASES)

UNIDADES <- sort(unique(base$unidad))
cat("Unidades:", length(UNIDADES), "\n\n")

construir <- function(datos, niveles) {
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
  um <- factor(ifelse(datos$unidad %in% niveles,
                      as.character(datos$unidad), niveles[1]),
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
  cat("Dejando fuera:", u, "\n")

  dentro <- base[base$unidad != u, ]
  fuera  <- base[base$unidad == u, ]

  # Dentro de cada iteracion, las unidades restantes se reparten en un
  # subconjunto de ajuste y otro de calibracion conforme. La particion se
  # realiza por paciente, de modo que las copias imputadas de un mismo caso
  # permanezcan juntas.
  pac <- unique(dentro$stay_id)
  cal_pac <- sample(pac, floor(0.35 * length(pac)))
  d_cal <- dentro[dentro$stay_id %in% cal_pac, ]
  d_ent <- dentro[!dentro$stay_id %in% cal_pac, ]

  niveles <- sort(unique(d_ent$unidad))

  X <- construir(d_ent, niveles)
  y <- d_ent$clase
  w <- rep(1/M, nrow(X))

  mod <- glmnet(X, y, family = "multinomial", alpha = 1,
                weights = w, lambda = 0.000191, standardize = TRUE)

  prob <- function(dat) {
    p <- predict(mod, newx = construir(dat, niveles), type = "response")[, , 1]
    ag <- aggregate(p, by = list(stay_id = dat$stay_id), FUN = mean)
    et <- unique(dat[, c("stay_id","clase")])
    merge(ag, et, by = "stay_id")
  }

  pc <- prob(d_cal)
  pf <- prob(fuera)

  um <- sapply(CLASES, function(k) {
    s <- 1 - pc[pc$clase == k, k]
    if (length(s) < 10) return(NA)
    umbral_conforme(s, ALFA)
  })

  pert <- sapply(CLASES, function(k)
    if (is.na(um[k])) rep(FALSE, nrow(pf)) else (1 - pf[[k]]) <= um[k])
  colnames(pert) <- CLASES
  tam <- rowSums(pert)
  cub <- sapply(seq_len(nrow(pf)), function(i)
    pert[i, as.character(pf$clase[i])])

  cob_cl <- sapply(CLASES, function(k) {
    s <- pf$clase == k
    if (sum(s) < 5) return(NA)
    mean(cub[s])
  })

  a_cl <- sapply(CLASES, function(k)
    auc(pf[[k]], as.integer(pf$clase == k)))

  resultados[[u]] <- list(
    unidad = u, n = nrow(pf),
    cobertura = mean(cub),
    cob_clase = cob_cl,
    auc = a_cl,
    tamano = mean(tam),
    resuelve = mean(tam == 1))
}

cat("\n=== COBERTURA MARGINAL POR UNIDAD EXCLUIDA ===\n")
tab <- do.call(rbind, lapply(resultados, function(r)
  data.frame(unidad = substr(r$unidad, 1, 40), n = r$n,
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

cat("\n=== COBERTURA POR CLASE Y UNIDAD ===\n")
cc <- do.call(rbind, lapply(resultados, function(r)
  data.frame(unidad = substr(r$unidad, 1, 30),
             t(round(r$cob_clase, 4)), row.names = NULL)))
names(cc)[2:5] <- CLASES
print(cc, row.names = FALSE)

cat("\n=== AUC POR CLASE Y UNIDAD ===\n")
aa <- do.call(rbind, lapply(resultados, function(r)
  data.frame(unidad = substr(r$unidad, 1, 30),
             t(round(r$auc, 4)), row.names = NULL)))
names(aa)[2:5] <- CLASES
print(aa, row.names = FALSE)

cat("\n=== HETEROGENEIDAD DEL DESEMPENO ===\n")
for (k in CLASES) {
  v <- aa[[k]]
  cat(sprintf("%-16s media %.4f  sd %.4f  rango %.4f a %.4f\n",
      k, mean(v, na.rm = TRUE), sd(v, na.rm = TRUE),
      min(v, na.rm = TRUE), max(v, na.rm = TRUE)))
}

write.csv(tab, file.path(OUT, "cobertura_louo.csv"), row.names = FALSE)
write.csv(cc,  file.path(OUT, "cobertura_clase_louo.csv"), row.names = FALSE)
write.csv(aa,  file.path(OUT, "auc_louo.csv"), row.names = FALSE)
