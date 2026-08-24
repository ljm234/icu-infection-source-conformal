library(glmnet)
library(splines)
set.seed(20260818)

imps <- readRDS("data/derivados/imputaciones_ampliadas.rds")
M <- length(imps)
CL <- c("sin_crecimiento","urinario","respiratorio","sangre")
VITALES <- c("temperatura","frec_cardiaca","frec_respiratoria","saturacion")

esp <- readRDS("outputs/fase7/especificacion.rds")

apilar <- function(g) {
  p <- do.call(rbind, lapply(imps, function(x) x[x$grupo == g, ]))
  p <- p[p$clase %in% CL, ]
  p$clase <- factor(as.character(p$clase), levels = CL)
  p
}
ent <- apilar("entrenamiento")
pru <- apilar("prueba")
cat("Filas apiladas en entrenamiento:", nrow(ent), "\n")
cat("Pacientes distintos:", length(unique(ent$stay_id)), "\n")

# Los nudos se fijan sobre el conjunto de entrenamiento de la primera
# imputacion y se mantienen invariables en lo sucesivo. Recalcularlos en cada
# conjunto haria depender la forma funcional del relleno de los datos
# ausentes, y recalcularlos sobre el conjunto de prueba constituiria
# anticipacion de informacion.
nudos_vit <- lapply(VITALES, function(v)
  as.numeric(quantile(ent[[v]][seq_len(nrow(ent)/M)],
                      c(0.05, 0.35, 0.65, 0.95), na.rm = TRUE)))
names(nudos_vit) <- VITALES

cat("\n=== NUDOS DE LAS CONSTANTES ===\n")
print(do.call(rbind, lapply(VITALES, function(v)
  data.frame(variable = v, t(round(nudos_vit[[v]], 2)), row.names = NULL))),
  row.names = FALSE)

base_spline <- function(x, nu, nombre) {
  b <- ns(x, knots = nu[2:3], Boundary.knots = nu[c(1,4)])
  colnames(b) <- paste0(nombre, "_s", seq_len(ncol(b)))
  as.matrix(b)
}

construir <- function(d, vit_spline) {
  bl <- list()
  for (v in esp$con_spline) {
    nu <- esp$nudos[[v]]
    bl[[v]] <- base_spline(d[[v]], nu, v)
  }
  for (v in VITALES) {
    if (v %in% vit_spline) bl[[v]] <- base_spline(d[[v]], nudos_vit[[v]], v)
  }
  lineales_vit <- setdiff(VITALES, vit_spline)
  lin <- as.matrix(d[, c(esp$lineales, lineales_vit), drop = FALSE])
  otr <- data.frame(edad = d$edad,
                    lactato_medido = d$lactato_medido,
                    sexo_M = as.integer(d$sexo == "M"))
  uni <- model.matrix(~ unidad - 1, data = d)[, -1, drop = FALSE]
  cbind(do.call(cbind, bl), lin, as.matrix(otr), uni)
}

w <- rep(1/M, nrow(ent))
y <- ent$clase

pac <- unique(ent$stay_id)
asign <- sample(rep(1:10, length.out = length(pac)))
names(asign) <- pac
foldid <- asign[as.character(ent$stay_id)]

devianza_cv <- function(vit_spline, yy) {
  X <- construir(ent, vit_spline)
  cv <- cv.glmnet(X, yy, family = "multinomial", alpha = 1, weights = w,
                  foldid = foldid, type.measure = "deviance")
  min(cv$cvm) * nrow(X)
}

# El umbral que separa una mejora real del azar se deriva de una
# distribucion nula. Al permutar el desenlace se destruye toda relacion con
# los predictores, de modo que la ganancia observada con desenlace permutado
# corresponde por entero al ruido. El percentil noventa y cinco de esa
# distribucion establece el limite: por debajo de el la mejora resulta
# indistinguible del azar.
cat("\nConstruyendo la distribucion nula. Esto toma varios minutos.\n")
NPERM <- 20
nulas <- sapply(seq_len(NPERM), function(i) {
  pac_p <- unique(ent$stay_id)
  cl_p <- sample(tapply(as.character(ent$clase), ent$stay_id,
                        function(z) z[1])[as.character(pac_p)])
  names(cl_p) <- pac_p
  yp <- factor(cl_p[as.character(ent$stay_id)], levels = CL)
  d0 <- devianza_cv(character(0), yp)
  d1 <- devianza_cv(VITALES, yp)
  d0 - d1
})

umbral <- as.numeric(quantile(nulas, 0.95))
cat("\n=== DISTRIBUCION NULA DE LA GANANCIA ===\n")
cat("Mediana:", round(median(nulas), 2),
    " Percentil 95:", round(umbral, 2), "\n")

cat("\nEvaluando la forma funcional de cada constante.\n")
d_base <- devianza_cv(character(0), y)

gan <- do.call(rbind, lapply(VITALES, function(v) {
  d_v <- devianza_cv(v, y)
  data.frame(variable = v,
             ganancia = round(d_base - d_v, 2),
             supera_umbral = (d_base - d_v) > umbral,
             row.names = NULL)
}))
cat("\n=== GANANCIA DEL SPLINE POR CONSTANTE ===\n")
print(gan, row.names = FALSE)
cat("Umbral de ruido:", round(umbral, 2), "\n")

CON_SPLINE <- gan$variable[gan$supera_umbral]
cat("\nConstantes con forma flexible:",
    if (length(CON_SPLINE) == 0) "ninguna" else paste(CON_SPLINE, collapse = ", "),
    "\n")

X <- construir(ent, CON_SPLINE)
cat("Columnas de la matriz de diseno:", ncol(X), "\n")

cv <- cv.glmnet(X, y, family = "multinomial", alpha = 1, weights = w,
                foldid = foldid, type.measure = "deviance")
LAMBDA <- cv$lambda.min
modelo <- glmnet(X, y, family = "multinomial", alpha = 1,
                 weights = w, lambda = LAMBDA, standardize = TRUE)

cat("\nLambda seleccionado:", signif(LAMBDA, 4), "\n")
cat("Coeficientes no nulos:",
    sum(sapply(coef(modelo), function(z) sum(z[-1] != 0))), "\n")

auc <- function(p, yb) {
  r <- rank(p); n1 <- sum(yb); n0 <- length(yb) - n1
  if (n1 < 3 || n0 < 3) return(NA)
  (sum(r[yb == 1]) - n1*(n1+1)/2) / (n1*n0)
}

Xp <- construir(pru, CON_SPLINE)
p <- predict(modelo, newx = Xp, type = "response")[, , 1]
ag <- aggregate(p, by = list(stay_id = pru$stay_id), FUN = mean)
et <- unique(pru[, c("stay_id","clase")])
mm <- merge(ag, et, by = "stay_id")

a_amp <- sapply(CL, function(k) auc(mm[[k]], as.integer(mm$clase == k)))
a_ori <- c(sin_crecimiento = 0.6521, urinario = 0.6518,
           respiratorio = 0.6597, sangre = 0.7090)

comp <- data.frame(clase = CL,
                   auc_original = as.numeric(a_ori[CL]),
                   auc_ampliado = round(a_amp, 4),
                   diferencia = round(a_amp - a_ori[CL], 4),
                   row.names = NULL)

cat("\n=== DISCRIMINACION EN PRUEBA ===\n")
print(comp, row.names = FALSE)
cat("\nPromedio minoritarias, modelo original:", round(mean(a_ori[-1]), 4), "\n")
cat("Promedio minoritarias, modelo ampliado:", round(mean(a_amp[-1]), 4), "\n")
cat("Diferencia:", round(mean(a_amp[-1]) - mean(a_ori[-1]), 4), "\n")

argm <- CL[apply(mm[, CL], 1, which.max)]
cat("\nPacientes con clase minoritaria como argmax:",
    sum(argm != "sin_crecimiento"), "de", nrow(mm), "\n")

saveRDS(list(modelo = modelo, lambda = LAMBDA, con_spline = CON_SPLINE,
             nudos_vit = nudos_vit, umbral = umbral),
        "data/derivados/modelo_ampliado.rds")
dir.create("outputs/fase18", recursive = TRUE, showWarnings = FALSE)
write.csv(comp, "outputs/fase18/comparacion_ampliado.csv", row.names = FALSE)
write.csv(gan, "outputs/fase18/ganancia_splines.csv", row.names = FALSE)
cat("\nGuardado en outputs/fase18\n")
