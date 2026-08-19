library(glmnet)
library(splines)
library(jsonlite)

SEMILLA <- 20260818
set.seed(SEMILLA)

OUT <- "outputs/fase7"
dir.create(OUT, recursive = TRUE, showWarnings = FALSE)

imps <- readRDS("outputs/fase6/imputaciones.rds")
M <- length(imps)

# ---------------------------------------------------------------------------
# Especificacion
#
# Las variables reciben forma no lineal cuando la ganancia en validacion
# cruzada supera diez unidades de devianza y la ganancia se reproduce en las
# veinte imputaciones. El criterio se derivo de una distribucion nula
# construida por permutacion del desenlace, que situa el ruido en 6.5, y de
# una evaluacion fuera de muestra que distingue el ajuste de la capacidad
# predictiva. Plaquetas y pH quedan en forma lineal porque su spline predice
# peor que la recta.
# ---------------------------------------------------------------------------

CON_SPLINE <- c("urea","creatinina","cloro","bicarbonato",
                "leucocitos","sodio","exceso_base","rdw")

LINEALES <- c("hemoglobina","plaquetas","inr","ttpa","ph","pco2",
              "lactato","potasio","brecha_anion")

PERCENTILES <- c(0.05, 0.35, 0.65, 0.95)

CLASES <- c("sin_crecimiento","urinario","respiratorio","sangre")

# Los nudos se fijan sobre el conjunto de entrenamiento de la primera
# imputacion y se conservan para todas las demas y para los conjuntos de
# calibracion, prueba y sellado. Recalcularlos en cada conjunto produciria
# bases distintas y los coeficientes dejarian de ser comparables.
base_ent <- imps[[1]][imps[[1]]$grupo == "entrenamiento", ]

nudos <- lapply(CON_SPLINE, function(v)
  quantile(base_ent[[v]], PERCENTILES, na.rm = TRUE))
names(nudos) <- CON_SPLINE

construir_diseno <- function(datos) {
  bloques <- list()
  for (v in CON_SPLINE) {
    nu <- nudos[[v]]
    b <- ns(datos[[v]], knots = nu[2:3], Boundary.knots = nu[c(1,4)])
    colnames(b) <- paste0(v, "_s", seq_len(ncol(b)))
    bloques[[v]] <- as.matrix(b)
  }
  lin <- as.matrix(datos[, LINEALES, drop = FALSE])
  otras <- data.frame(
    edad           = datos$edad,
    lactato_medido = datos$lactato_medido,
    sexo_M         = as.integer(datos$sexo == "M"))
  uni <- model.matrix(~ unidad_mod - 1, data = datos)
  uni <- uni[, -1, drop = FALSE]
  cbind(do.call(cbind, bloques), lin, as.matrix(otras), uni)
}

# La unidad se recodifica igual que en la imputacion: los niveles se
# restringen a los observados en entrenamiento y la unidad sellada se asigna
# al nivel de referencia, que es la condicion de una sede no representada en
# el desarrollo.
niveles_ent <- sort(unique(base_ent$unidad))
recodificar <- function(x) {
  x$unidad_mod <- factor(ifelse(x$unidad %in% niveles_ent,
                                as.character(x$unidad), niveles_ent[1]),
                         levels = niveles_ent)
  x
}

imps <- lapply(imps, recodificar)

# ---------------------------------------------------------------------------
# Apilado
#
# Las veinte imputaciones se apilan en una unica matriz con peso 1/M por
# fila. Frente al promedio de coeficientes, el apilado produce un modelo
# unico y coherente: con penalizacion l1 cada imputacion puede anular
# coeficientes distintos, y promediar ceros con valores no nulos generaria
# una combinacion que no corresponde a ninguna de ellas.
# ---------------------------------------------------------------------------

apilar <- function(grupo) {
  partes <- lapply(imps, function(x) x[x$grupo == grupo, ])
  do.call(rbind, partes)
}

ent <- apilar("entrenamiento")
ent <- ent[ent$clase %in% CLASES, ]
ent$clase <- factor(as.character(ent$clase), levels = CLASES)

X <- construir_diseno(ent)
y <- ent$clase
w <- rep(1/M, nrow(X))

cat("Matriz de entrenamiento:", nrow(X), "filas,", ncol(X), "columnas\n")
cat("Equivale a", nrow(X)/M, "pacientes\n")
cat("Distribucion de clases:\n")
print(table(y))

# Los pliegues de validacion cruzada se asignan por paciente y no por fila.
# Un mismo paciente aparece M veces en la matriz apilada; si sus copias se
# repartieran entre pliegues, el modelo evaluaria pacientes que ya vio y la
# estimacion del error seria optimista.
pacientes <- unique(ent$stay_id)
asign <- sample(rep(1:10, length.out = length(pacientes)))
names(asign) <- pacientes
foldid <- asign[as.character(ent$stay_id)]

# ---------------------------------------------------------------------------
# Busqueda conjunta de alfa y lambda
#
# El parametro alfa reparte la penalizacion entre las normas l1 y l2. La
# presencia de pares con correlacion superior a 0.69, como bicarbonato con
# exceso de base o sodio con cloro, aconseja conservar componente cuadratica:
# la norma l1 pura escogeria uno de cada par de forma inestable entre
# muestras, mientras que la cuadratica reparte el coeficiente.
# ---------------------------------------------------------------------------

ALFAS <- c(0.1, 0.25, 0.5, 0.75, 1)

cat("\nBuscando alfa y lambda. Esto toma varios minutos.\n")
inicio <- Sys.time()

resultados <- lapply(ALFAS, function(a) {
  cat("  alfa =", a, "\n")
  cv <- cv.glmnet(X, y, family = "multinomial", alpha = a,
                  weights = w, foldid = foldid,
                  type.measure = "deviance", standardize = TRUE)
  list(alpha = a, cv = cv,
       dev_min = min(cv$cvm),
       lambda_min = cv$lambda.min,
       lambda_1se = cv$lambda.1se)
})

cat("Tiempo:", round(difftime(Sys.time(), inicio, units = "mins"), 1), "min\n")

tabla <- do.call(rbind, lapply(resultados, function(r)
  data.frame(alpha = r$alpha,
             devianza_cv = round(r$dev_min, 4),
             lambda_min = signif(r$lambda_min, 4),
             lambda_1se = signif(r$lambda_1se, 4),
             row.names = NULL)))

cat("\n=== BUSQUEDA DE HIPERPARAMETROS ===\n")
print(tabla, row.names = FALSE)

mejor <- resultados[[which.min(sapply(resultados, function(r) r$dev_min))]]
cat("\nAlfa seleccionado:", mejor$alpha, "\n")

# Se adopta lambda.1se en lugar de lambda.min. El primero corresponde a la
# penalizacion mas fuerte cuyo error permanece dentro de un error estandar
# del minimo, y produce modelos mas parsimoniosos y estables entre muestras.
# La eleccion se fija por anticipado y no depende del desempeno observado.
LAMBDA <- mejor$lambda_1se

modelo <- glmnet(X, y, family = "multinomial", alpha = mejor$alpha,
                 weights = w, lambda = LAMBDA, standardize = TRUE)

cat("\n=== COEFICIENTES NO NULOS POR CLASE ===\n")
co <- coef(modelo)
resumen <- data.frame(
  clase = names(co),
  no_nulos = sapply(co, function(m) sum(m[-1] != 0)),
  total = ncol(X),
  row.names = NULL)
print(resumen, row.names = FALSE)

cat("\n=== VARIABLES RETENIDAS EN ALGUNA CLASE ===\n")
retenidas <- unique(unlist(lapply(co, function(m) {
  nz <- which(m[-1] != 0)
  rownames(m)[-1][nz]
})))
cat(length(retenidas), "de", ncol(X), "columnas\n")
print(sort(retenidas))

# ---------------------------------------------------------------------------
# Probabilidades predichas sobre calibracion y prueba
# ---------------------------------------------------------------------------

predecir <- function(grupo) {
  g <- apilar(grupo)
  g <- g[g$clase %in% CLASES, ]
  g$clase <- factor(as.character(g$clase), levels = CLASES)
  Xg <- construir_diseno(g)
  p <- predict(modelo, newx = Xg, type = "response")[, , 1]
  # Las M copias de cada paciente se promedian, lo que aplica la regla de
  # combinacion de imputacion multiple sobre la escala de probabilidad.
  agg <- aggregate(p, by = list(stay_id = g$stay_id), FUN = mean)
  eti <- unique(g[, c("stay_id","clase","unidad","grupo")])
  merge(agg, eti, by = "stay_id")
}

cal <- predecir("calibracion")
pru <- predecir("prueba")

cat("\n=== PACIENTES CON PROBABILIDAD PREDICHA ===\n")
cat("Calibracion:", nrow(cal), "\n")
cat("Prueba:", nrow(pru), "\n")

saveRDS(modelo, file.path(OUT, "modelo.rds"))
saveRDS(list(nudos = nudos, con_spline = CON_SPLINE, lineales = LINEALES,
             niveles = niveles_ent, clases = CLASES),
        file.path(OUT, "especificacion.rds"))
write.csv(cal, file.path(OUT, "prob_calibracion.csv"), row.names = FALSE)
write.csv(pru, file.path(OUT, "prob_prueba.csv"), row.names = FALSE)
write.csv(tabla, file.path(OUT, "busqueda_hiperparametros.csv"),
          row.names = FALSE)

writeLines(toJSON(list(
  fase = "7",
  ejecutado_en = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
  semilla = SEMILLA,
  glmnet_version = as.character(packageVersion("glmnet")),
  alpha = mejor$alpha,
  lambda = LAMBDA,
  regla_lambda = "1se",
  con_spline = CON_SPLINE,
  lineales = LINEALES,
  percentiles_nudos = PERCENTILES,
  columnas_diseno = ncol(X),
  columnas_retenidas = length(retenidas),
  imputaciones_apiladas = M,
  pliegues_por_paciente = TRUE), auto_unbox = TRUE, pretty = TRUE),
  file.path(OUT, "manifiesto.json"))

cat("\nGuardado en", OUT, "\n")
