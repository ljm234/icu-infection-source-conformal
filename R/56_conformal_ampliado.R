library(glmnet)
library(splines)
set.seed(20260818)

imps <- readRDS("data/derivados/imputaciones_ampliadas.rds")
amp  <- readRDS("data/derivados/modelo_ampliado.rds")
esp  <- readRDS("outputs/fase7/especificacion.rds")

M <- length(imps)
CL <- c("sin_crecimiento","urinario","respiratorio","sangre")
VITALES <- c("temperatura","frec_cardiaca","frec_respiratoria","saturacion")
ALFA <- 0.10

apilar <- function(g) {
  p <- do.call(rbind, lapply(imps, function(x) x[x$grupo == g, ]))
  p <- p[p$clase %in% CL, ]
  p$clase <- factor(as.character(p$clase), levels = CL)
  p
}
ent <- apilar("entrenamiento")
cal <- apilar("calibracion")
pru <- apilar("prueba")

base_spline <- function(x, nu, nombre) {
  b <- ns(x, knots = nu[2:3], Boundary.knots = nu[c(1,4)])
  colnames(b) <- paste0(nombre, "_s", seq_len(ncol(b)))
  as.matrix(b)
}

construir <- function(d) {
  bl <- list()
  for (v in esp$con_spline) bl[[v]] <- base_spline(d[[v]], esp$nudos[[v]], v)
  for (v in amp$con_spline) bl[[v]] <- base_spline(d[[v]], amp$nudos_vit[[v]], v)
  lineales_vit <- setdiff(VITALES, amp$con_spline)
  lin <- as.matrix(d[, c(esp$lineales, lineales_vit), drop = FALSE])
  otr <- data.frame(edad = d$edad,
                    lactato_medido = d$lactato_medido,
                    sexo_M = as.integer(d$sexo == "M"))
  uni <- model.matrix(~ unidad - 1, data = d)[, -1, drop = FALSE]
  cbind(do.call(cbind, bl), lin, as.matrix(otr), uni)
}

X <- construir(ent); y <- ent$clase; w <- rep(1/M, nrow(X))
pac <- unique(ent$stay_id)
asign <- sample(rep(1:10, length.out = length(pac)))
names(asign) <- pac
foldid <- asign[as.character(ent$stay_id)]

auc <- function(p, yb) {
  r <- rank(p); n1 <- sum(yb); n0 <- length(yb) - n1
  if (n1 < 3 || n0 < 3) return(NA)
  (sum(r[yb == 1]) - n1*(n1+1)/2) / (n1*n0)
}

evaluar <- function(mod, datos) {
  p <- predict(mod, newx = construir(datos), type = "response")[, , 1]
  ag <- aggregate(p, by = list(stay_id = datos$stay_id), FUN = mean)
  et <- unique(datos[, c("stay_id","clase","unidad")])
  merge(ag, et, by = "stay_id")
}

# La regla de penalizacion no se hereda de la especificacion anterior. La
# matriz de diseno ha variado al incorporar cuatro constantes, de modo que el
# valor optimo del parametro puede diferir. Se repite por tanto la
# comparacion preespecificada con el mismo criterio fijado en su momento:
# se adopta el valor minimo unicamente si mejora en mas de dos centesimas el
# area promediada sobre las categorias minoritarias, sin que ninguna
# categoria empeore en esa misma cuantia.
cat("Ajustando la ruta completa de penalizacion.\n")
cv <- cv.glmnet(X, y, family = "multinomial", alpha = 1, weights = w,
                foldid = foldid, type.measure = "deviance")

comparar <- function(lam, etiqueta) {
  m <- glmnet(X, y, family = "multinomial", alpha = 1,
              weights = w, lambda = lam, standardize = TRUE)
  mm <- evaluar(m, pru)
  a <- sapply(CL, function(k) auc(mm[[k]], as.integer(mm$clase == k)))
  list(modelo = m, auc = a,
       no_nulos = sum(sapply(coef(m), function(z) sum(z[-1] != 0))),
       etiqueta = etiqueta)
}

r1 <- comparar(cv$lambda.1se, "1se")
r2 <- comparar(cv$lambda.min, "min")

cat("\n=== COMPARACION DE REGLAS DE PENALIZACION ===\n")
print(data.frame(clase = CL,
                 auc_1se = round(r1$auc, 4),
                 auc_min = round(r2$auc, 4),
                 diferencia = round(r2$auc - r1$auc, 4),
                 row.names = NULL), row.names = FALSE)

mn <- CL[-1]
prom1 <- mean(r1$auc[mn]); prom2 <- mean(r2$auc[mn])
peor <- min(r2$auc[mn] - r1$auc[mn])
cat("\nPromedio minoritarias, regla 1se:", round(prom1, 4), "\n")
cat("Promedio minoritarias, regla min:", round(prom2, 4), "\n")
cat("Coeficientes no nulos: 1se =", r1$no_nulos, " min =", r2$no_nulos, "\n")

cat("\n=== DECISION SEGUN CRITERIO PREESPECIFICADO ===\n")
if (prom2 - prom1 > 0.02 && peor > -0.02) {
  cat("Se adopta la regla min\n"); elegido <- r2; LAMBDA <- cv$lambda.min
} else {
  cat("Se conserva la regla 1se\n"); elegido <- r1; LAMBDA <- cv$lambda.1se
}
modelo <- elegido$modelo
cat("Lambda:", signif(LAMBDA, 4), " Coeficientes:", elegido$no_nulos, "\n")

# La puntuacion de no conformidad de un caso respecto de una categoria es el
# complemento de la probabilidad que el modelo le asigna. El umbral se
# calcula por separado dentro de cada categoria sobre el conjunto de
# calibracion, lo que confiere garantia condicional. La correccion de muestra
# finita emplea el cuantil de orden ceil((n+1)(1-alfa))/n, que se deriva de
# que la puntuacion del caso nuevo ocupa posicion uniforme entre las n+1
# puntuaciones intercambiables.
umbral_conforme <- function(s, alpha) {
  n <- length(s)
  k <- ceiling((n + 1) * (1 - alpha))
  if (k > n) return(Inf)
  sort(s)[k]
}

pc <- evaluar(modelo, cal)
pp <- evaluar(modelo, pru)

um <- sapply(CL, function(k) umbral_conforme(1 - pc[pc$clase == k, k], ALFA))

cat("\n=== UMBRALES DE CALIBRACION ===\n")
print(data.frame(clase = CL,
                 n_calibracion = sapply(CL, function(k) sum(pc$clase == k)),
                 prob_minima_para_entrar = round(1 - um, 4),
                 row.names = NULL), row.names = FALSE)

pert <- sapply(CL, function(k) (1 - pp[[k]]) <= um[k])
colnames(pert) <- CL
pp$tamano <- rowSums(pert)
pp$cubierto <- sapply(seq_len(nrow(pp)), function(i)
  pert[i, as.character(pp$clase[i])])

cat("\n=== COBERTURA POR CATEGORIA ===\n")
print(do.call(rbind, lapply(CL, function(k) {
  s <- pp[pp$clase == k, ]
  n <- nrow(s); c1 <- sum(s$cubierto)
  ic <- binom.test(c1, n)$conf.int
  data.frame(clase = k, n = n, cobertura = round(c1/n, 4),
             ic_inferior = round(ic[1], 4), ic_superior = round(ic[2], 4),
             nominal = 1 - ALFA, tamano_medio = round(mean(s$tamano), 3),
             row.names = NULL)
})), row.names = FALSE)
cat("\nCobertura marginal:", round(mean(pp$cubierto), 4), "\n")

cat("\n=== DISTRIBUCION DEL TAMANO DEL CONJUNTO ===\n")
print(table(tamano = pp$tamano))

decide <- pp$tamano == 1
cat("\n=== COMPORTAMIENTO DE LA ABSTENCION ===\n")
cat("Casos con conclusion:", sum(decide),
    sprintf("(%.1f por ciento)\n", 100 * mean(decide)))
if (sum(decide) > 0) {
  unica <- sapply(which(decide), function(i) CL[pert[i, ]][1])
  cat("Error entre los resueltos:",
      round(1 - mean(unica == as.character(pp$clase[decide])), 4), "\n")
  print(table(emitida = unica, real = pp$clase[decide]))
}

# Los cuatro indicadores del modelo original se leen de la curva por nivel de
# confianza, que es donde se calcularon, y en la fila del nivel que esta fase
# emplea. Transcritos a mano quedarian sin seguimiento de su fuente aunque
# solo se impriman.
RUTA_ALFA <- "outputs/fase9/curva_alfa.csv"
if (!file.exists(RUTA_ALFA)) {
  cat("\nFuente ausente:", RUTA_ALFA, "\n")
  cat("El procedimiento se detiene.\n"); quit(status = 1)
}
ca <- read.csv(RUTA_ALFA, stringsAsFactors = FALSE)
fa <- which(abs(ca$alfa - ALFA) < 1e-9)
if (length(fa) != 1) {
  cat("\nLa curva por nivel no contiene una fila para el nivel empleado.\n")
  cat("El procedimiento se detiene.\n"); quit(status = 1)
}

cat("\n=== COMPARACION CON EL MODELO ORIGINAL ===\n")
print(data.frame(
  indicador = c("proporcion resuelta", "error entre resueltos",
                "cobertura marginal", "tamano medio del conjunto"),
  original = c(ca$pct_resuelve[fa] / 100, ca$error_entre_resueltos[fa],
               ca$cobertura_marginal[fa], ca$tamano_medio[fa]),
  ampliado = c(round(mean(decide), 4),
               if (sum(decide) > 0)
                 round(1 - mean(sapply(which(decide), function(i)
                   CL[pert[i, ]][1]) == as.character(pp$clase[decide])), 4)
               else NA,
               round(mean(pp$cubierto), 4),
               round(mean(pp$tamano), 3)),
  row.names = NULL), row.names = FALSE)

saveRDS(list(modelo = modelo, lambda = LAMBDA, umbrales = um,
             con_spline = amp$con_spline, nudos_vit = amp$nudos_vit),
        "data/derivados/modelo_ampliado_final.rds")
dir.create("outputs/fase18", recursive = TRUE, showWarnings = FALSE)
write.csv(data.frame(clase = CL, umbral = round(um, 6)),
          "outputs/fase18/umbrales_ampliado.csv", row.names = FALSE)
cat("\nGuardado en outputs/fase18\n")
