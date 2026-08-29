library(glmnet)
library(splines)
library(DBI)
library(duckdb)
set.seed(20260818)

esp  <- readRDS("outputs/fase7/especificacion.rds")
imps <- readRDS("outputs/fase6/imputaciones.rds")
M <- length(imps)
CLASES <- esp$clases

# La penalizacion se lee de la tabla que la fija, no de un literal. El literal
# con el que se ejecuto esta fase se conserva como guardia: si lo leido no lo
# reproduce con exactitud, los artefactos depositados no serian los que este
# codigo produce y procede detenerse.
hip <- read.csv("outputs/fase7/busqueda_hiperparametros.csv",
                stringsAsFactors = FALSE)
fila_lambda <- which(abs(hip$alpha - 1) < 1e-9)
if (length(fila_lambda) != 1)
  stop("La busqueda de hiperparametros no contiene una fila unica para la ",
       "mezcla empleada.")
LAMBDA_FINAL <- hip$lambda_min[fila_lambda]
if (abs(LAMBDA_FINAL - 0.000191) > 0)
  stop("La penalizacion leida no reproduce el literal con el que se ejecuto ",
       "esta fase.")

base_ent <- imps[[1]][imps[[1]]$grupo == "entrenamiento", ]
niveles <- sort(unique(base_ent$unidad))
imps <- lapply(imps, function(x) {
  x$unidad_mod <- factor(ifelse(x$unidad %in% niveles,
                                as.character(x$unidad), niveles[1]),
                         levels = niveles)
  x
})

apilar <- function(g) {
  p <- do.call(rbind, lapply(imps, function(x) x[x$grupo == g, ]))
  p <- p[p$clase %in% CLASES, ]
  p$clase <- factor(as.character(p$clase), levels = CLASES)
  p
}
ent <- apilar("entrenamiento"); pru <- apilar("prueba")
w <- rep(1/M, nrow(ent))

auc <- function(p, yb) {
  r <- rank(p); n1 <- sum(yb); n0 <- length(yb) - n1
  if (n1 < 3 || n0 < 3) return(NA)
  (sum(r[yb == 1]) - n1*(n1+1)/2) / (n1*n0)
}

construir <- function(d, con_spline, lineales, con_unidad = TRUE,
                      con_indicador = TRUE) {
  bl <- list()
  for (v in con_spline) {
    nu <- esp$nudos[[v]]
    b <- ns(d[[v]], knots = nu[2:3], Boundary.knots = nu[c(1,4)])
    colnames(b) <- paste0(v, "_s", seq_len(ncol(b)))
    bl[[v]] <- as.matrix(b)
  }
  partes <- list()
  if (length(bl) > 0) partes$spl <- do.call(cbind, bl)
  if (length(lineales) > 0) partes$lin <- as.matrix(d[, lineales, drop = FALSE])
  otr <- data.frame(edad = d$edad, sexo_M = as.integer(d$sexo == "M"))
  if (con_indicador) otr$lactato_medido <- d$lactato_medido
  partes$otr <- as.matrix(otr)
  if (con_unidad)
    partes$uni <- model.matrix(~ unidad_mod - 1, data = d)[, -1, drop = FALSE]
  do.call(cbind, partes)
}

ajustar_evaluar <- function(cs, li, cu, ci, etiqueta) {
  Xe <- construir(ent, cs, li, cu, ci)
  Xp <- construir(pru, cs, li, cu, ci)
  m <- glmnet(Xe, ent$clase, family = "multinomial", alpha = 1,
              lambda = LAMBDA_FINAL, weights = w, standardize = TRUE)
  p <- predict(m, newx = Xp, type = "response")[, , 1]
  ag <- aggregate(p, by = list(stay_id = pru$stay_id), FUN = mean)
  et <- unique(pru[, c("stay_id","clase")])
  mm <- merge(ag, et, by = "stay_id")
  a <- sapply(CLASES, function(k) auc(mm[[k]], as.integer(mm$clase == k)))
  data.frame(especificacion = etiqueta,
             parametros = sum(sapply(coef(m), function(z) sum(z[-1] != 0))),
             t(round(a, 4)),
             promedio_minoritarias = round(mean(a[-1]), 4),
             row.names = NULL)
}

# Analisis de sensibilidad preespecificado. Se compara la especificacion
# adoptada frente a su version sin lactato ni indicador de determinacion. El
# proposito es cuantificar que parte del desempeno procede del valor medido y
# que parte de la decision clinica implicita en haber solicitado el examen.
sin_lac_s <- setdiff(esp$con_spline, "lactato")
sin_lac_l <- setdiff(esp$lineales, "lactato")

r_full <- ajustar_evaluar(esp$con_spline, esp$lineales, TRUE, TRUE,
                          "completa")
r_nolac <- ajustar_evaluar(sin_lac_s, sin_lac_l, TRUE, FALSE,
                           "sin lactato ni indicador")
r_noind <- ajustar_evaluar(esp$con_spline, esp$lineales, TRUE, FALSE,
                           "lactato sin indicador")

comp <- rbind(r_full, r_noind, r_nolac)
names(comp)[3:6] <- CLASES

cat("\n=== SENSIBILIDAD AL LACTATO ===\n")
print(comp, row.names = FALSE)

cat("\nContribucion del indicador de determinacion:",
    round(r_full$promedio_minoritarias - r_noind$promedio_minoritarias, 4), "\n")
cat("Contribucion del valor de lactato:",
    round(r_noind$promedio_minoritarias - r_nolac$promedio_minoritarias, 4), "\n")

dir.create("outputs/fase15", recursive = TRUE, showWarnings = FALSE)
write.csv(comp, "outputs/fase15/sensibilidad_lactato.csv", row.names = FALSE)
