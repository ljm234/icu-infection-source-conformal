library(jsonlite)

options(scipen = 999)

# Curva de calibracion, recorrido de la probabilidad predicha y separacion
# entre la categoria mayoritaria y las demas.
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
# estancia que R/33_conformal.R dejo escritas al calibrar el modelo final, y
# las que R/36_sellado.R dejo escritas en su unica evaluacion de la unidad
# reservada. Esta es la cuarta lectura de aquel archivo, tras la del propio
# R/36 que lo escribio, la de R/38_recalibracion.R y la de
# R/71_calibracion_sellado.R; ninguna de las cuatro reajusta ni vuelve a
# predecir, y conviene que el recuento conste porque el documento lo enumera.
#
# ESTE PROCEDIMIENTO NO INTERPRETA. Deposita recuentos, cuantiles, los dos
# coeficientes que la guia de reporte pide y la cota que se deriva de la
# probabilidad minima de la mayoritaria, y no escribe lectura alguna de
# ellos. La lectura se decide despues y sobre las cifras, no aqui.

OUT <- "outputs/fase35"

RUTA_ESP  <- "outputs/fase7/especificacion.rds"
RUTA_PRU  <- "outputs/fase8/prueba_con_conjuntos.csv"
RUTA_COB  <- "outputs/fase8/cobertura.csv"
RUTA_SEL  <- "outputs/fase11/sellado_evaluado.csv"
RUTA_COBS <- "outputs/fase11/cobertura_sellado.csv"

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
# baje de EVENTOS_MINIMOS.
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

for (r in c(RUTA_ESP, RUTA_PRU, RUTA_COB, RUTA_SEL, RUTA_COBS))
  if (!file.exists(r)) detener("Fuente ausente: ", r)

esp <- readRDS(RUTA_ESP)
if (is.null(esp$clases)) detener("La especificacion no declara categorias.")
CLASES <- esp$clases

# La misma comprobacion se aplica a los dos conjuntos evaluados. Escribirla
# una vez evita que uno de ellos quede con menos guardas que el otro.
validar <- function(d, etiqueta, agregado) {
  faltan <- setdiff(c(CLASES, "clase"), names(d))
  if (length(faltan) > 0)
    detener(etiqueta, ": faltan columnas ", paste(faltan, collapse = ", "))
  if (!all(d$clase %in% CLASES))
    detener(etiqueta, ": contiene categorias ajenas a la especificacion.")
  if (any(is.na(d[, CLASES])) || any(is.na(d$clase)))
    detener(etiqueta, ": contiene valores ausentes.")
  if (max(abs(rowSums(d[, CLASES]) - 1)) > 1e-6)
    detener(etiqueta, ": alguna fila no suma la unidad.")
  # La transformacion logit exige probabilidades estrictamente interiores. Un
  # cero o un uno exactos indicarian saturacion y harian infinito el predictor
  # lineal, de modo que procede detenerse en lugar de recortar en silencio.
  p <- as.matrix(d[, CLASES])
  if (min(p) <= 0 || max(p) >= 1)
    detener(etiqueta, ": alguna probabilidad predicha alcanza los extremos.")
  # Los recuentos han de reproducir los publicados. Si difirieran, esta tabla
  # y la que el trabajo reporta describirian evaluaciones distintas.
  n_eva <- as.integer(table(factor(d$clase, levels = CLASES)))
  n_pub <- as.integer(agregado$n[match(CLASES, agregado$clase)])
  if (any(is.na(n_pub)) || !identical(n_eva, n_pub))
    detener(etiqueta, ": los recuentos por categoria no coinciden con los ",
            "publicados.")
  invisible(TRUE)
}

pru <- read.csv(RUTA_PRU, stringsAsFactors = FALSE)
sel <- read.csv(RUTA_SEL, stringsAsFactors = FALSE)
validar(pru, "conjunto de prueba", read.csv(RUTA_COB, stringsAsFactors = FALSE))
validar(sel, "unidad reservada", read.csv(RUTA_COBS, stringsAsFactors = FALSE))

CONJUNTOS <- list("prueba" = pru, "unidad reservada" = sel)

n_eva <- table(factor(pru$clase, levels = CLASES))
G <- max(GRUPOS_MINIMO, min(GRUPOS_MAXIMO,
                            floor(min(n_eva) / EVENTOS_MINIMOS)))

cat("=== NUMERO DE GRUPOS ===\n")
cat("Estancias evaluadas en prueba:", nrow(pru), "\n")
cat("Casos por categoria:", paste(sprintf("%s=%d", CLASES, as.integer(n_eva)),
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
#
# La curva y la pendiente se calculan sobre el conjunto de prueba y no sobre
# la unidad reservada. Alli la categoria respiratoria reune trece casos, y
# repartirlos en grupos produciria una tabla cuyas frecuencias no
# significarian nada. El recorrido y la cota si se calculan en los dos: son
# extremos y no requieren reparto.
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
  if (sum(s$observados) != as.integer(n_eva[[k]]))
    detener("Los grupos de ", k, " no cubren sus casos observados.")
}

cat("\n=== CURVA DE CALIBRACION, CONJUNTO DE PRUEBA ===\n")
print(curva, row.names = FALSE)

# ---------------------------------------------------------------------------
# Recorrido de la probabilidad predicha, en los dos conjuntos evaluados. Se
# informa en dos estratos: sobre todas las estancias y sobre las de la propia
# categoria. El segundo es el que responde que probabilidad llega a recibir
# un caso que si pertenece a la categoria.
# ---------------------------------------------------------------------------

rango <- do.call(rbind, lapply(names(CONJUNTOS), function(nm) {
  d <- CONJUNTOS[[nm]]
  do.call(rbind, lapply(CLASES, function(k) {
    p <- d[[k]]
    do.call(rbind, lapply(c("todas", "de la categoria"), function(e) {
      v <- if (e == "todas") p else p[d$clase == k]
      if (length(v) == 0) return(NULL)
      q <- quantile(v, c(0, 0.25, 0.5, 0.75, 1), names = FALSE)
      data.frame(conjunto = nm, clase = k, estrato = e, n = length(v),
                 minimo = round(q[1], 4), q1 = round(q[2], 4),
                 mediana = round(q[3], 4), q3 = round(q[4], 4),
                 maximo = round(q[5], 4), row.names = NULL)
    }))
  }))
}))

cat("\n=== RECORRIDO DE LA PROBABILIDAD PREDICHA ===\n")
print(rango, row.names = FALSE)

# ---------------------------------------------------------------------------
# Cota sobre la regla del maximo.
#
# Las cuatro probabilidades suman la unidad en cada estancia. Si la de la
# categoria mayoritaria no baja nunca de un valor m, las tres restantes no
# pueden sumar mas de 1 - m, y por tanto ninguna de ellas puede alcanzar
# individualmente 1 - m. Cuando m supera la mitad, 1 - m queda por debajo de
# m y la categoria mayoritaria es el maximo en toda estancia.
#
# La cota descansa en una sola cifra por conjunto, la probabilidad minima de
# la mayoritaria, y no en el maximo observado de las minoritarias, que se
# deposita al lado como respaldo. Una cota derivada de un minimo es mas
# dificil de rebatir que una coincidencia de dos extremos observados.
#
# Se exige que la cifra supere la mitad tambien despues de redondear: quien
# rehaga la resta con el valor publicado ha de obtener el mismo signo que el
# procedimiento obtuvo con el valor entero.
# ---------------------------------------------------------------------------

separacion <- do.call(rbind, lapply(names(CONJUNTOS), function(nm) {
  d <- CONJUNTOS[[nm]]
  nk <- table(factor(d$clase, levels = CLASES))
  may <- CLASES[which.max(nk)]
  pmin <- min(d[[may]])
  pmin_r <- round(pmin, 4)
  cota <- round(1 - pmin_r, 4)
  data.frame(
    conjunto = nm,
    estancias = nrow(d),
    clase_mayoritaria = may,
    prob_minima_mayoritaria = pmin_r,
    cota_de_cada_minoritaria = cota,
    separacion = round(pmin_r - cota, 4),
    maximo_observado_minoritarias =
      round(max(as.matrix(d[, setdiff(CLASES, may), drop = FALSE])), 4),
    sostiene_la_imposibilidad = pmin > 0.5 && pmin_r > 0.5,
    row.names = NULL)
}))

cat("\n=== COTA SOBRE LA REGLA DEL MAXIMO ===\n")
print(separacion, row.names = FALSE)

# El conjunto de prueba es el que sostiene lo que el documento publica. Si su
# minimo dejara de superar la mitad, la cota se cae y procede detenerse antes
# de depositar, para que lo advierta el procedimiento y no un lector.
f_pru <- separacion[separacion$conjunto == "prueba", ]
if (!f_pru$sostiene_la_imposibilidad)
  detener("La probabilidad minima de la mayoritaria en el conjunto de ",
          "prueba no supera la mitad. La cota sobre la regla del maximo ",
          "deja de sostenerse y no procede depositarla como tal.")

# ---------------------------------------------------------------------------
# Pendiente e intercepto, sobre el conjunto de prueba.
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
    contiene_la_unidad = s1["lp", "Estimate"] - z * s1["lp", "Std. Error"] <= 1 &
                         s1["lp", "Estimate"] + z * s1["lp", "Std. Error"] >= 1,
    row.names = NULL)
}))

cat("\n=== PENDIENTE E INTERCEPTO, CONJUNTO DE PRUEBA ===\n")
print(pend, row.names = FALSE)

cat("\nLa pendiente vale uno y la calibracion en conjunto vale cero cuando",
    "la\nprediccion no esta comprimida ni desplazada. No se deposita lectura",
    "alguna\nde estas cifras.\n")

dir.create(OUT, recursive = TRUE, showWarnings = FALSE)
write.csv(curva, file.path(OUT, "curva_calibracion.csv"), row.names = FALSE)
write.csv(rango, file.path(OUT, "rango_probabilidad.csv"), row.names = FALSE)
write.csv(separacion, file.path(OUT, "separacion_argmax.csv"), row.names = FALSE)
write.csv(pend, file.path(OUT, "pendiente_calibracion.csv"), row.names = FALSE)

writeLines(toJSON(list(
  fase = "35",
  ejecutado_en = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
  r_version = R.version.string,
  proposito = paste("calibracion por tramos de probabilidad predicha y cota",
                    "sobre la regla del maximo, que es lo que la calibracion",
                    "en media de la fase trigesimo cuarta no puede mostrar"),
  fuentes = list(prueba = RUTA_PRU, unidad_reservada = RUTA_SEL),
  origen = paste("probabilidades por estancia escritas por R/33_conformal.R",
                 "y por R/36_sellado.R; no se reajusta ni se vuelve a",
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
  alcance = paste("la curva y la pendiente se calculan sobre el conjunto de",
                  "prueba; el recorrido y la cota, sobre los dos conjuntos",
                  "evaluados. La unidad reservada reune trece casos",
                  "respiratorios y no admite un reparto en grupos"),
  cota = paste("las cuatro probabilidades suman la unidad, de modo que si la",
               "de la mayoritaria no baja de m, ninguna otra alcanza 1-m;",
               "cuando m supera la mitad, la mayoritaria es el maximo en",
               "toda estancia"),
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
                  "recuentos, cuantiles, coeficientes y una cota, sin",
                  "interpretarlos"),
  categorias = length(CLASES)), auto_unbox = TRUE, pretty = TRUE),
  file.path(OUT, "manifiesto.json"))

cat("\nDepositado en", OUT, "\n")
