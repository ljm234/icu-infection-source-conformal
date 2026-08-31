library(jsonlite)

options(scipen = 999)

# Curva de calibracion por categoria en el conjunto de prueba.
#
# La fase trigesimo cuarta deposita la calibracion en media dentro de cada
# categoria. Esa cantidad puede cuadrar sin que el modelo haya distinguido
# nada: si la probabilidad predicha para una categoria poco frecuente se
# mantiene siempre cerca de su prevalencia, la media reproduce la prevalencia
# por construccion. Lo que la media no puede mostrar es si el modelo llega
# alguna vez a asignar probabilidad alta a un caso de esa categoria.
#
# Esa distincion es la misma que el trabajo ya midio tres veces en otras
# cantidades: la exactitud promediada frente al recuento de identificaciones,
# la cobertura marginal frente a la condicional por clase, y la cobertura al
# recalibrar frente al tamano de los conjuntos. Aqui se mide la cuarta.
#
# No se reajusta ni se vuelve a predecir. Se leen las probabilidades por
# estancia que R/33_conformal.R dejo escritas al calibrar el modelo final.
#
# ESTE PROCEDIMIENTO NO INTERPRETA. Deposita recuentos, cuantiles y los dos
# coeficientes que la guia de reporte pide, y no escribe lectura alguna de
# ellos. La lectura se decide despues y sobre las cifras, no aqui.

OUT <- "outputs/fase35"

RUTA_ESP <- "outputs/fase7/especificacion.rds"
RUTA_PRU <- "outputs/fase8/prueba_con_conjuntos.csv"
RUTA_COB <- "outputs/fase8/cobertura.csv"

# ---------------------------------------------------------------------------
# Numero de grupos.
#
# Un solo numero rige para las cuatro categorias. Emplear mas grupos en la
# mayoritaria y menos en las escasas produciria dos varas de medir dentro de
# una misma tabla, que es el defecto que este desarrollo ya corrigio dos
# veces, y ademas haria incomparables las cuatro curvas.
#
# El numero lo fija la categoria menos frecuente, que es la que antes se
# queda sin casos. Se exige que en ella el promedio de casos por grupo no
# baje de EVENTOS_MINIMOS. Con ese promedio, una frecuencia observada de
# grupo tiene un error tipico que no desborda la escala en que se lee.
#
# El promedio es un promedio: los casos de una categoria no se reparten por
# igual entre los grupos, sino que se concentran donde la probabilidad
# predicha es mayor. Por eso el deposito lleva el recuento efectivo de cada
# grupo, y no solo su tamano, de modo que quien lea la curva vea donde queda
# fina sin tener que suponerlo.
# ---------------------------------------------------------------------------

EVENTOS_MINIMOS <- 20
GRUPOS_MINIMO   <- 3
GRUPOS_MAXIMO   <- 10
CONFIANZA       <- 0.95

detener <- function(...) {
  cat("\n", ..., "\n", sep = "")
  cat("El procedimiento se detiene. No procede interpretar el resultado.\n")
  quit(status = 1)
}

for (r in c(RUTA_ESP, RUTA_PRU, RUTA_COB))
  if (!file.exists(r)) detener("Fuente ausente: ", r)

esp <- readRDS(RUTA_ESP)
if (is.null(esp$clases)) detener("La especificacion no declara categorias.")
CLASES <- esp$clases

pru <- read.csv(RUTA_PRU, stringsAsFactors = FALSE)
cob <- read.csv(RUTA_COB, stringsAsFactors = FALSE)

faltan <- setdiff(c(CLASES, "clase"), names(pru))
if (length(faltan) > 0)
  detener("La evaluacion carece de columnas: ", paste(faltan, collapse = ", "))
if (!all(pru$clase %in% CLASES))
  detener("La evaluacion contiene categorias ajenas a la especificacion.")
if (any(is.na(pru[, CLASES])) || any(is.na(pru$clase)))
  detener("La evaluacion contiene valores ausentes.")
if (max(abs(rowSums(pru[, CLASES]) - 1)) > 1e-6)
  detener("Alguna fila no suma la unidad sobre las categorias modeladas.")

# Los recuentos han de reproducir la cobertura publicada. Si difirieran, esta
# tabla y la que el trabajo reporta describirian evaluaciones distintas.
n_eva <- as.integer(table(factor(pru$clase, levels = CLASES)))
n_cob <- as.integer(cob$n[match(CLASES, cob$clase)])
if (any(is.na(n_cob)) || !identical(n_eva, n_cob))
  detener("Los recuentos por categoria no coinciden con la cobertura ",
          "publicada.")

# La transformacion logit exige probabilidades estrictamente interiores. Un
# cero o un uno exactos indicarian saturacion y harian infinito el predictor
# lineal, de modo que procede detenerse en lugar de recortar en silencio.
p_todas <- as.matrix(pru[, CLASES])
if (min(p_todas) <= 0 || max(p_todas) >= 1)
  detener("Alguna probabilidad predicha alcanza los extremos del intervalo.")

names(n_eva) <- CLASES
G <- max(GRUPOS_MINIMO, min(GRUPOS_MAXIMO,
                            floor(min(n_eva) / EVENTOS_MINIMOS)))

cat("=== NUMERO DE GRUPOS ===\n")
cat("Estancias evaluadas:", nrow(pru), "\n")
cat("Casos por categoria:", paste(sprintf("%s=%d", CLASES, n_eva),
                                  collapse = "  "), "\n")
cat("Categoria menos frecuente:", CLASES[which.min(n_eva)],
    "con", min(n_eva), "casos\n")
cat("Casos por grupo exigidos en ella:", EVENTOS_MINIMOS, "\n")
cat("Grupos adoptados para las cuatro categorias:", G, "\n")
cat("Promedio de casos por grupo en la menos frecuente:",
    round(min(n_eva) / G, 1), "\n")

# ---------------------------------------------------------------------------
# Curva. Grupos de igual tamano por cuantiles de la probabilidad predicha,
# sobre las estancias evaluadas y no sobre las de la categoria: la pregunta
# es como se comporta la probabilidad que el modelo emite, cualquiera que sea
# la categoria observada del caso.
# ---------------------------------------------------------------------------

curva <- do.call(rbind, lapply(CLASES, function(k) {
  p <- pru[[k]]
  y <- as.integer(pru$clase == k)
  brk <- quantile(p, probs = seq(0, 1, length.out = G + 1), names = FALSE)
  if (length(unique(brk)) != G + 1)
    detener("Los cuantiles de ", k, " no separan ", G, " grupos: la ",
            "probabilidad predicha repite valores en los cortes.")
  g <- cut(p, breaks = brk, include.lowest = TRUE, labels = FALSE)
  if (length(unique(g)) != G)
    detener("El agrupamiento de ", k, " deja algun grupo vacio.")
  do.call(rbind, lapply(seq_len(G), function(i) {
    s <- g == i
    data.frame(clase = k, grupo = i, n = sum(s),
               prob_minima = round(min(p[s]), 4),
               prob_maxima = round(max(p[s]), 4),
               probabilidad_media = round(mean(p[s]), 4),
               observados = sum(y[s]),
               frecuencia_observada = round(mean(y[s]), 4),
               row.names = NULL)
  }))
}))

# Todo caso ha de caer en un grupo. Si la suma por categoria no reprodujera
# su recuento, la curva estaria describiendo un subconjunto sin declararlo.
for (k in CLASES) {
  s <- curva[curva$clase == k, ]
  if (sum(s$n) != nrow(pru))
    detener("Los grupos de ", k, " no cubren las estancias evaluadas.")
  if (sum(s$observados) != n_eva[[k]])
    detener("Los grupos de ", k, " no cubren sus casos observados.")
}

cat("\n=== CURVA DE CALIBRACION ===\n")
print(curva, row.names = FALSE)

# ---------------------------------------------------------------------------
# Recorrido de la probabilidad predicha. Se informa en dos estratos: sobre
# todas las estancias evaluadas y sobre las de la propia categoria. El
# segundo es el que responde que probabilidad llega a recibir un caso que si
# pertenece a la categoria.
# ---------------------------------------------------------------------------

rango <- do.call(rbind, lapply(CLASES, function(k) {
  p <- pru[[k]]
  do.call(rbind, lapply(c("todas", "de la categoria"), function(e) {
    v <- if (e == "todas") p else p[pru$clase == k]
    q <- quantile(v, c(0, 0.25, 0.5, 0.75, 1), names = FALSE)
    data.frame(clase = k, estrato = e, n = length(v),
               minimo = round(q[1], 4), q1 = round(q[2], 4),
               mediana = round(q[3], 4), q3 = round(q[4], 4),
               maximo = round(q[5], 4), row.names = NULL)
  }))
}))

cat("\n=== RECORRIDO DE LA PROBABILIDAD PREDICHA ===\n")
print(rango, row.names = FALSE)

# ---------------------------------------------------------------------------
# Pendiente e intercepto.
#
# La pendiente es el coeficiente del predictor lineal en la regresion
# logistica del desenlace sobre el logit de la probabilidad predicha. Vale
# uno cuando la prediccion no esta ni comprimida ni exagerada.
#
# Se depositan dos interceptos porque el termino se emplea con dos sentidos.
# El del propio ajuste acompana a la pendiente y no se lee por separado. El
# de calibracion en conjunto se obtiene con la pendiente fijada en uno,
# dejando el logit como desplazamiento, y vale cero cuando la prediccion no
# esta desplazada en bloque. La guia de reporte pide este ultimo junto a la
# pendiente.
#
# Los intervalos son de Wald sobre la escala del coeficiente.
# ---------------------------------------------------------------------------

z <- qnorm(1 - (1 - CONFIANZA) / 2)

pend <- do.call(rbind, lapply(CLASES, function(k) {
  p <- pru[[k]]
  y <- as.integer(pru$clase == k)
  lp <- log(p / (1 - p))
  f1 <- glm(y ~ lp, family = binomial)
  f0 <- glm(y ~ 1, family = binomial, offset = lp)
  if (!f1$converged || !f0$converged)
    detener("El ajuste de calibracion de ", k, " no converge.")
  s1 <- summary(f1)$coefficients
  s0 <- summary(f0)$coefficients
  data.frame(
    clase = k,
    pendiente = round(s1["lp", "Estimate"], 4),
    pendiente_ic_inferior = round(s1["lp", "Estimate"] -
                                  z * s1["lp", "Std. Error"], 4),
    pendiente_ic_superior = round(s1["lp", "Estimate"] +
                                  z * s1["lp", "Std. Error"], 4),
    intercepto_del_ajuste = round(s1["(Intercept)", "Estimate"], 4),
    calibracion_en_conjunto = round(s0["(Intercept)", "Estimate"], 4),
    conjunto_ic_inferior = round(s0["(Intercept)", "Estimate"] -
                                 z * s0["(Intercept)", "Std. Error"], 4),
    conjunto_ic_superior = round(s0["(Intercept)", "Estimate"] +
                                 z * s0["(Intercept)", "Std. Error"], 4),
    row.names = NULL)
}))

cat("\n=== PENDIENTE E INTERCEPTO ===\n")
print(pend, row.names = FALSE)

cat("\nLa pendiente vale uno y la calibracion en conjunto vale cero cuando",
    "la\nprediccion no esta comprimida ni desplazada. No se deposita lectura",
    "alguna\nde estas cifras.\n")

dir.create(OUT, recursive = TRUE, showWarnings = FALSE)
write.csv(curva, file.path(OUT, "curva_calibracion.csv"), row.names = FALSE)
write.csv(rango, file.path(OUT, "rango_probabilidad.csv"), row.names = FALSE)
write.csv(pend, file.path(OUT, "pendiente_calibracion.csv"), row.names = FALSE)

writeLines(toJSON(list(
  fase = "35",
  ejecutado_en = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
  r_version = R.version.string,
  proposito = paste("calibracion por tramos de probabilidad predicha en el",
                    "conjunto de prueba, que es lo que la calibracion en",
                    "media de la fase trigesimo cuarta no puede mostrar"),
  fuente = RUTA_PRU,
  origen = paste("probabilidades por estancia escritas por R/33_conformal.R",
                 "al calibrar el modelo final; no se reajusta ni se vuelve a",
                 "predecir"),
  grupos = G,
  regla_de_los_grupos = paste("un solo numero para las cuatro categorias,",
                              "fijado por la menos frecuente, exigiendo en",
                              "ella un promedio de", EVENTOS_MINIMOS,
                              "casos por grupo, acotado entre",
                              GRUPOS_MINIMO, "y", GRUPOS_MAXIMO),
  agrupamiento = paste("cuantiles de la probabilidad predicha sobre las",
                       "estancias evaluadas, grupos de igual tamano"),
  advertencia_de_los_grupos = paste("el promedio de casos por grupo es un",
                                    "promedio; los casos se concentran donde",
                                    "la probabilidad predicha es mayor, y por",
                                    "eso se deposita el recuento efectivo de",
                                    "cada grupo"),
  recorrido = paste("cuartiles de la probabilidad predicha en dos estratos:",
                    "todas las estancias evaluadas, y las de la propia",
                    "categoria"),
  pendiente = paste("coeficiente del logit de la probabilidad predicha en la",
                    "regresion logistica del desenlace; vale uno sin",
                    "compresion"),
  calibracion_en_conjunto = paste("intercepto con el logit como",
                                  "desplazamiento y la pendiente fijada en",
                                  "uno; vale cero sin desplazamiento"),
  intervalos = "de Wald, sobre la escala del coeficiente",
  confianza = CONFIANZA,
  lectura = paste("no se deposita ninguna. Este procedimiento entrega",
                  "recuentos, cuantiles y coeficientes sin interpretarlos"),
  estancias = nrow(pru),
  categorias = length(CLASES)), auto_unbox = TRUE, pretty = TRUE),
  file.path(OUT, "manifiesto.json"))

cat("\nDepositado en", OUT, "\n")
