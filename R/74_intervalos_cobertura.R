library(glmnet)
library(splines)
library(jsonlite)

# Intervalos exactos de la cobertura condicional, y un solo criterio.
#
# El deposito emplea dos criterios distintos para la misma pregunta. En el
# conjunto de prueba se declara que la cobertura alcanza el nivel nominal
# cuando el intervalo lo contiene. En la validacion dejando una sede fuera y
# en la unidad reservada se compara la estimacion puntual contra el nominal,
# sin intervalo. La segunda comparacion sostiene el titular de que la
# garantia condicional falla en todas las sedes.
#
# Los dos criterios no son equivalentes y no pueden convivir. Una proporcion
# estimada sobre trece casos queda por debajo del nominal con facilidad sin
# que ello acredite incumplimiento alguno: el intervalo lo dira. Este
# procedimiento deposita los intervalos exactos por clase y sede, con sus
# denominadores, y aplica a las dos secciones el criterio que el conjunto de
# prueba ya usaba.
#
# La validacion dejando una sede fuera se recomputa porque los recuentos por
# clase no constan en ningun archivo: solo las proporciones. La replica
# reproduce el procedimiento original sin variacion, misma semilla y mismo
# orden de extracciones aleatorias, y se detiene si las proporciones
# recompuestas no coinciden con las publicadas.
#
# La unidad reservada no se vuelve a abrir. Su recuento de cubiertos se
# deduce del archivo ya depositado y se verifica por partida doble: la
# proporcion ha de reproducirse y el intervalo exacto ha de coincidir con el
# que ese mismo archivo consigna.

SEMILLA <- 20260818
set.seed(SEMILLA)

OUT <- "outputs/fase26"

RUTA_ESP  <- "outputs/fase7/especificacion.rds"
RUTA_IMP  <- "outputs/fase6/imputaciones.rds"
RUTA_MAN  <- "outputs/fase7/manifiesto.json"
RUTA_HIP  <- "outputs/fase7/busqueda_hiperparametros.csv"
RUTA_COB8 <- "outputs/fase8/cobertura.csv"
RUTA_LOUO <- "outputs/fase10/cobertura_louo.csv"
RUTA_CLC  <- "outputs/fase10/cobertura_clase_louo.csv"
RUTA_SELL <- "outputs/fase11/cobertura_sellado.csv"
RUTA_UMBR <- "outputs/fase8/umbrales.csv"

detener <- function(...) {
  cat("\n", ..., "\n", sep = "")
  cat("El procedimiento se detiene. No procede interpretar el resultado.\n")
  quit(status = 1)
}

tol4 <- 10^(-4) / 2

for (r in c(RUTA_ESP, RUTA_IMP, RUTA_MAN, RUTA_HIP, RUTA_COB8, RUTA_LOUO,
            RUTA_CLC, RUTA_SELL, RUTA_UMBR))
  if (!file.exists(r)) detener("Fuente ausente: ", r)

esp <- readRDS(RUTA_ESP)
if (is.null(esp$clases)) detener("La especificacion no declara categorias.")
CLASES <- esp$clases

cob8 <- read.csv(RUTA_COB8, stringsAsFactors = FALSE)
NOMINAL <- cob8$nominal[1]
ALFA <- 1 - NOMINAL

# El nivel y la penalizacion se leen de los archivos que los fijaron. R/35
# repetia la penalizacion escrita a mano; aqui se lee, de modo que la replica
# sigue a su fuente y no a una transcripcion.
man <- fromJSON(RUTA_MAN)
if (is.null(man$alpha)) detener("El manifiesto de la fase 7 no declara alfa.")
hip <- read.csv(RUTA_HIP, stringsAsFactors = FALSE)
fa <- which(abs(hip$alpha - as.numeric(man$alpha)) < 1e-9)
if (length(fa) != 1) detener("La busqueda no contiene una fila para alfa.")
LAMBDA <- hip$lambda_min[fa]
ALFA_NET <- as.numeric(man$alpha)

cat("=== PARAMETROS LEIDOS ===\n")
cat("Nivel nominal:", NOMINAL, "\n")
cat("Mezcla:", ALFA_NET, "  penalizacion:", signif(LAMBDA, 4), "\n")

# Intervalo exacto de Clopper y Pearson. Se obtiene por inversion de la
# prueba binomial, que es lo que binom.test devuelve, y no admite
# aproximacion normal: con trece casos la aproximacion carece de validez y es
# precisamente ese tamano el que motiva esta revision.
CONFIANZA <- 0.95

# Nivel de la familia de contrastes. Veinte celdas contrastadas al cinco por
# ciento dan, bajo la hipotesis global de que toda cobertura alcanza el
# nominal, una falsa positiva esperada por puro azar. Declarar cinco fallos
# sin corregir seria por tanto declarar uno de mas en promedio, y por eso se
# corrige. La correccion se aplica dentro de la validacion dejando una sede
# fuera, que es donde se contrastan las veinte; la unidad reservada se evaluo
# una sola vez y por separado, y sus cuatro celdas quedan fuera de la familia.
# Se contrastan dos niveles. El documento emplea intervalos bilaterales al
# noventa y cinco por ciento, y contrastar su extremo superior contra el
# nominal equivale a una prueba unilateral a la mitad de ese nivel. Los
# valores p, en cambio, son unilaterales. Entre una convencion y otra hay un
# factor dos, y alguna celda puede caer en esa brecha. Se declaran los
# recuentos bajo ambos niveles en lugar de elegir el que convenga.
NIVELES <- c(0.05, 0.025)
NIVEL_FAMILIA <- NIVELES[1]

intervalo <- function(k, n) {
  if (n == 0) return(c(NA_real_, NA_real_))
  as.numeric(binom.test(k, n, conf.level = CONFIANZA)$conf.int)
}

# Valor exacto unilateral. Contrasta que la cobertura alcanza el nominal
# frente a que queda por debajo, que es la unica direccion en que la garantia
# puede incumplirse. Se obtiene de la distribucion binomial acumulada, sin
# aproximacion normal: con cincuenta y cinco casos esa aproximacion decide
# junto al umbral y no procede fiarle la conclusion.
valor_p <- function(k, n, p0) {
  if (n == 0) return(NA_real_)
  binom.test(k, n, p = p0, alternative = "less")$p.value
}

# Valor exacto que reconoce la incertidumbre de la calibracion.
#
# El contraste binomial trata el nivel nominal como una probabilidad conocida.
# No lo es. El umbral conforme es un estadistico de orden de un conjunto de
# calibracion finito, y R/35 lo reestima dentro de cada pliegue: cinco
# calibraciones distintas para cinco sedes. Condicionada a una de ellas, la
# cobertura que el procedimiento alcanza sobre casos intercambiables es una
# Beta cuyos parametros dependen del tamano de esa calibracion y del orden
# escogido. El recuento cubierto es entonces Beta-Binomial en el margen, con
# mas varianza que la Binomial que el contraste anterior supone, de modo que
# aquel resulta anticonservador.
#
# Con calibracion de tamano n y orden k, la cobertura sigue Beta(k, n+1-k).
# Ambos parametros son enteros, de modo que la suma admite calculo exacto en
# escala logaritmica sin recurrir a aproximacion alguna.
# Cola de la Beta-Binomial, compuesta en escala logaritmica y anclada al
# termino mayor. Sumar exponenciales sin anclar pierde precision cuando la
# cola abarca miles de terminos y produce acumulados que rebasan la unidad.
cola_beta <- function(i, mm, a, b) {
  l <- lchoose(mm, i) + lbeta(i + a, mm - i + b) - lbeta(a, b)
  mx <- max(l)
  exp(mx + log(sum(exp(l - mx))))
}

# Intervalo que reconoce la incertidumbre de la calibracion.
#
# El de Clopper y Pearson se obtiene invirtiendo la binomial, y por tanto
# queda condicionado al umbral: supone conocida la cobertura que el umbral
# alcanza. Aqui se invierte la Beta-Binomial con la sobredispersion fijada por
# el tamano de la calibracion, que no se estima porque se conoce. El unico
# parametro libre es la localizacion, y el intervalo se obtiene igual que
# aquel, buscando los valores del parametro que dejan en cada cola la mitad
# del complemento del nivel. Cuando la calibracion crece, el intervalo
# converge al de Clopper y Pearson.
intervalo_beta <- function(x, mm, tam, conf) {
  if (is.na(tam) || mm == 0) return(c(NA_real_, NA_real_))
  al <- (1 - conf) / 2
  f_le <- function(q) cola_beta(0:x, mm, q * tam, (1 - q) * tam) - al
  f_ge <- function(q) cola_beta(x:mm, mm, q * tam, (1 - q) * tam) - al
  lo <- if (x == 0) 0 else uniroot(f_ge, c(1e-9, 1 - 1e-9), tol = 1e-10)$root
  hi <- if (x == mm) 1 else uniroot(f_le, c(1e-9, 1 - 1e-9), tol = 1e-10)$root
  c(lo, hi)
}

valor_p_beta <- function(x, mm, a, b) {
  if (is.na(a) || is.na(b) || mm == 0) return(NA_real_)
  cola_beta(0:x, mm, a, b)
}

# ---------------------------------------------------------------------------
# Replica de la validacion dejando una sede fuera
# ---------------------------------------------------------------------------

imps <- readRDS(RUTA_IMP)
M <- length(imps)

base <- do.call(rbind, lapply(imps, function(x)
  x[x$grupo %in% c("entrenamiento","calibracion","prueba"), ]))
base <- base[base$clase %in% CLASES, ]
base$clase <- factor(as.character(base$clase), levels = CLASES)

UNIDADES <- sort(unique(base$unidad))

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

cat("\nRecomponiendo la validacion dejando una sede fuera.\n")
inicio <- Sys.time()
filas <- list()
marginal <- list()

for (u in UNIDADES) {
  cat("  ", substr(u, 1, 40), "\n")
  dentro <- base[base$unidad != u, ]
  fuera  <- base[base$unidad == u, ]

  pac <- unique(dentro$stay_id)
  cal_pac <- sample(pac, floor(0.35 * length(pac)))
  d_cal <- dentro[dentro$stay_id %in% cal_pac, ]
  d_ent <- dentro[!dentro$stay_id %in% cal_pac, ]

  niveles <- sort(unique(d_ent$unidad))
  X <- construir(d_ent, niveles)
  w <- rep(1/M, nrow(X))
  mod <- glmnet(X, d_ent$clase, family = "multinomial", alpha = ALFA_NET,
                weights = w, lambda = LAMBDA, standardize = TRUE)

  prob <- function(dat) {
    p <- predict(mod, newx = construir(dat, niveles), type = "response")[, , 1]
    ag <- aggregate(p, by = list(stay_id = dat$stay_id), FUN = mean)
    et <- unique(dat[, c("stay_id","clase")])
    merge(ag, et, by = "stay_id")
  }

  pc <- prob(d_cal)
  pf <- prob(fuera)

  # Tamano de la calibracion por clase dentro de este pliegue, y orden del
  # estadistico escogido. De ambos depende la distribucion de la cobertura.
  n_cal <- sapply(CLASES, function(k) sum(pc$clase == k))
  k_orden <- ceiling((n_cal + 1) * (1 - ALFA))

  um <- sapply(CLASES, function(k) {
    s <- 1 - pc[pc$clase == k, k]
    if (length(s) < 10) return(NA)
    umbral_conforme(s, ALFA)
  })

  pert <- sapply(CLASES, function(k)
    if (is.na(um[k])) rep(FALSE, nrow(pf)) else (1 - pf[[k]]) <= um[k])
  colnames(pert) <- CLASES
  cub <- sapply(seq_len(nrow(pf)), function(i)
    pert[i, as.character(pf$clase[i])])

  marginal[[u]] <- data.frame(unidad = u, n = nrow(pf),
                              cobertura = mean(cub), row.names = NULL)

  for (k in CLASES) {
    s <- pf$clase == k
    nk <- sum(s)
    if (nk < 5) next
    kk <- sum(cub[s])
    ic <- intervalo(kk, nk)
    filas[[length(filas)+1]] <- data.frame(
      seccion = "dejando una sede fuera", sede = u, clase = k,
      n = nk, cubiertos = kk, cobertura = kk / nk,
      ic_inferior = ic[1], ic_superior = ic[2],
      n_calibracion = n_cal[[k]], orden_umbral = k_orden[[k]],
      row.names = NULL)
  }
}
cat("Tiempo:", round(difftime(Sys.time(), inicio, units = "mins"), 1), "min\n")

louo_cl <- do.call(rbind, filas)
marg <- do.call(rbind, marginal)

# --- contraste contra lo publicado -----------------------------------------

pub_cl <- read.csv(RUTA_CLC, stringsAsFactors = FALSE)
pub_m  <- read.csv(RUTA_LOUO, stringsAsFactors = FALSE)

# Los archivos publicados truncan el nombre de la sede. Se casan por el
# prefijo comun, y se comprueba que la correspondencia sea uno a uno.
casar <- function(nombres, largos) {
  sapply(nombres, function(n) {
    i <- which(substr(largos, 1, nchar(n)) == n)
    if (length(i) != 1) detener("La sede no se identifica sin ambiguedad: ", n)
    largos[i]
  })
}
pub_cl$sede <- casar(pub_cl$unidad, UNIDADES)
pub_m$sede  <- casar(pub_m$unidad, UNIDADES)

d_max <- 0
for (i in seq_len(nrow(louo_cl))) {
  f <- pub_cl[pub_cl$sede == louo_cl$sede[i], louo_cl$clase[i]]
  if (length(f) != 1) detener("Falta la sede o la clase en lo publicado.")
  d_max <- max(d_max, abs(louo_cl$cobertura[i] - f))
}
dm_max <- max(abs(marg$cobertura -
                  pub_m$cobertura[match(marg$unidad, pub_m$sede)]))
dn_max <- max(abs(marg$n - pub_m$n[match(marg$unidad, pub_m$sede)]))

cat("\n=== CONTRASTE CON LA VALIDACION PUBLICADA ===\n")
cat("Discrepancia maxima por clase:", signif(d_max, 3), "\n")
cat("Discrepancia maxima marginal:", signif(dm_max, 3), "\n")
cat("Discrepancia maxima en los recuentos:", dn_max, "\n")
if (d_max >= tol4 || dm_max >= tol4 || dn_max > 0)
  detener("La replica no reproduce la validacion publicada.")
cat("La replica reproduce la validacion publicada.\n")

# ---------------------------------------------------------------------------
# Unidad reservada. No se reabre: el recuento se deduce del archivo.
# ---------------------------------------------------------------------------

sell <- read.csv(RUTA_SELL, stringsAsFactors = FALSE)
for (cl in c("clase","n","cobertura","ic_inf","ic_sup"))
  if (is.null(sell[[cl]])) detener("La cobertura sellada carece de: ", cl)

# La unidad reservada no recalibra: emplea los umbrales del conjunto de
# calibracion original, cuyo tamano por clase consta en la fase octava. Ese
# umbral tampoco es una probabilidad conocida, de modo que la misma correccion
# le corresponde. Dejarla en el contraste binomial mientras la validacion usa
# el otro recrearia la asimetria de criterios que este procedimiento corrige.
umbr <- read.csv(RUTA_UMBR, stringsAsFactors = FALSE)
if (is.null(umbr$n_calibracion))
  detener("Los umbrales no declaran el tamano de la calibracion.")
ncal_sell <- setNames(umbr$n_calibracion, umbr$clase)
if (!all(sell$clase %in% names(ncal_sell)))
  detener("Los umbrales no cubren las categorias de la unidad reservada.")

sfilas <- list()
for (i in seq_len(nrow(sell))) {
  nk <- sell$n[i]
  kk <- round(sell$cobertura[i] * nk)
  # Doble verificacion. La proporcion deducida ha de reproducir la publicada,
  # y el intervalo exacto que de ella se obtiene ha de coincidir con el que el
  # propio archivo consigna. Si ambas cosas se cumplen, el recuento es el que
  # produjo aquella evaluacion y no una reconstruccion plausible.
  if (abs(round(kk / nk, 4) - sell$cobertura[i]) > 1e-9)
    detener("El recuento deducido no reproduce la cobertura de ",
            sell$clase[i], ".")
  ic <- intervalo(kk, nk)
  if (abs(round(ic[1], 4) - sell$ic_inf[i]) > 1e-9 ||
      abs(round(ic[2], 4) - sell$ic_sup[i]) > 1e-9)
    detener("El intervalo recompuesto no coincide con el publicado en ",
            sell$clase[i], ".")
  sfilas[[i]] <- data.frame(
    seccion = "unidad reservada", sede = "Cardiac Vascular",
    clase = sell$clase[i], n = nk, cubiertos = kk, cobertura = kk / nk,
    ic_inferior = ic[1], ic_superior = ic[2],
    n_calibracion = as.integer(ncal_sell[[sell$clase[i]]]),
    orden_umbral = as.integer(ceiling(
      (ncal_sell[[sell$clase[i]]] + 1) * (1 - ALFA))),
    row.names = NULL)
}
sell_cl <- do.call(rbind, sfilas)
cat("\nLos recuentos deducidos de la unidad reservada reproducen su",
    "cobertura\ny sus intervalos publicados.\n")

# ---------------------------------------------------------------------------
# Un solo criterio
# ---------------------------------------------------------------------------

tab <- rbind(louo_cl, sell_cl)
tab$nominal <- NOMINAL
tab$confianza <- CONFIANZA

# ---------------------------------------------------------------------------
# Multiplicidad
#
# Se emplean dos correcciones. La de Bonferroni reparte el nivel entre los
# contrastes por igual. La de Holm los ordena y va relajando el umbral, de
# modo que controla la misma tasa de error por familia y rechaza al menos
# tanto como la primera. Lo que cae bajo Holm cae de verdad.
# ---------------------------------------------------------------------------

tab$p_unilateral <- mapply(valor_p, tab$cubiertos, tab$n, MoreArgs =
                           list(p0 = NOMINAL))
tab$p_beta <- mapply(valor_p_beta, tab$cubiertos, tab$n,
                     tab$orden_umbral,
                     tab$n_calibracion + 1 - tab$orden_umbral)

# Las dos distribuciones no son comparables sin mas. La Beta-Binomial tiene
# mas varianza, lo que eleva el valor, pero su media es el orden dividido
# entre el tamano de calibracion mas uno, y el techo del cuantil situa esa
# media por encima del nivel nominal, lo que lo rebaja. Cual de los dos
# efectos domina depende de cada celda, de modo que no cabe exigir que el
# valor suba siempre. Se comprueba lo que si tiene que cumplirse: parametros
# positivos, valores dentro del intervalo unidad, y media nula no inferior al
# nominal, que es lo que el techo garantiza.
tab$media_nula <- tab$orden_umbral / (tab$n_calibracion + 1)

# Intervalos que incorporan la incertidumbre de la calibracion, para que los
# dos instrumentos del documento descansen sobre el mismo modelo. Los de
# Clopper y Pearson se conservan al lado: la diferencia entre ambos es cuanta
# precision se atribuia de mas.
ib <- t(mapply(intervalo_beta, tab$cubiertos, tab$n,
               tab$n_calibracion + 1, MoreArgs = list(conf = CONFIANZA)))
tab$ic_beta_inferior <- ib[, 1]
tab$ic_beta_superior <- ib[, 2]

if (any(tab$ic_beta_inferior > tab$cobertura + 1e-6, na.rm = TRUE) ||
    any(tab$ic_beta_superior < tab$cobertura - 1e-6, na.rm = TRUE))
  detener("Algun intervalo no contiene su propia estimacion puntual.")
if (any((tab$ic_beta_superior - tab$ic_beta_inferior) <
        (tab$ic_superior - tab$ic_inferior) - 1e-9, na.rm = TRUE))
  detener("El intervalo que reconoce la calibracion resulta mas estrecho ",
          "que el condicionado al umbral.")

tab$beta_por_debajo <- tab$ic_beta_superior < NOMINAL
tab$beta_por_encima <- tab$ic_beta_inferior > NOMINAL
tab$beta_excluye_nominal <- tab$beta_por_debajo | tab$beta_por_encima
en_f <- !is.na(tab$media_nula)
if (any(tab$orden_umbral[en_f] < 1) ||
    any(tab$n_calibracion[en_f] + 1 - tab$orden_umbral[en_f] < 1))
  detener("Los parametros de la Beta no son positivos.")
# El acumulado puede rebasar la unidad por redondeo cuando la cola abarca
# miles de terminos. Se admite ese margen y se acota; un exceso mayor
# indicaria un error en los parametros y no en la aritmetica.
exceso <- max(abs(pmin(pmax(tab$p_beta[en_f], 0), 1) - tab$p_beta[en_f]))
cat("Exceso maximo por redondeo en la suma Beta-Binomial:",
    signif(exceso, 3), "\n")
if (exceso > 1e-6)
  detener("El valor que reconoce la calibracion cae fuera del intervalo ",
          "por un margen que el redondeo no explica.")
tab$p_beta <- pmin(pmax(tab$p_beta, 0), 1)
if (any(tab$media_nula[en_f] < NOMINAL - 1e-12))
  detener("La media nula queda por debajo del nominal. El techo del ",
          "cuantil no se esta aplicando.")
familia <- tab$seccion == "dejando una sede fuera"
tab$en_familia <- familia
m <- sum(familia)

tab$p_bonferroni <- NA_real_
tab$p_holm <- NA_real_
tab$p_bonferroni[familia] <- p.adjust(tab$p_unilateral[familia], "bonferroni")
tab$p_holm[familia] <- p.adjust(tab$p_unilateral[familia], "holm")
# El valor ajustado no depende del nivel: la eleccion de nivel es una
# comparacion posterior. Se deposita el ajuste una vez y la decision para
# cada uno de los dos niveles por separado.
tab$p_beta_holm <- NA_real_
tab$p_beta_holm[familia] <- p.adjust(tab$p_beta[familia], "holm")
tab$p_beta_bonferroni <- NA_real_
tab$p_beta_bonferroni[familia] <- p.adjust(tab$p_beta[familia], "bonferroni")

etq <- function(a) sub("[.]", "", format(a, nsmall = 3))
for (a in NIVELES) {
  tab[[paste0("resiste_bonferroni_", etq(a))]] <- tab$p_bonferroni <= a
  tab[[paste0("resiste_holm_", etq(a))]] <- tab$p_holm <= a
  tab[[paste0("resiste_beta_holm_", etq(a))]] <- tab$p_beta_holm <= a
  tab[[paste0("resiste_beta_bonferroni_", etq(a))]] <-
    tab$p_beta_bonferroni <= a
}
tab$resiste_bonferroni <- tab[[paste0("resiste_bonferroni_",
                                      etq(NIVEL_FAMILIA))]]
tab$resiste_holm <- tab[[paste0("resiste_holm_", etq(NIVEL_FAMILIA))]]

# El umbral escalonado de Holm se consigna junto a cada celda para que la
# decision pueda seguirse a mano. Cada analisis ordena sus propios valores, de
# modo que cada uno lleva su umbral: compararlos cruzados seria un error, y
# que las dos ordenaciones coincidan aqui no autoriza a suponerlo.
escalonar <- function(pref, valores) {
  o <- order(valores[familia])
  for (a in NIVELES) {
    col <- paste0(pref, etq(a))
    tab[[col]] <<- NA_real_
    v <- rep(NA_real_, m)
    v[o] <- a / (m - seq_len(m) + 1)
    tab[[col]][familia] <<- v
  }
}
escalonar("umbral_holm_", tab$p_unilateral)
escalonar("umbral_holm_beta_", tab$p_beta)
tab$umbral_holm <- tab[[paste0("umbral_holm_", etq(NIVEL_FAMILIA))]]

# Comprobacion de coherencia entre las dos formulaciones de Holm. Rechazar por
# valor ajustado frente al nivel y rechazar por valor bruto frente al umbral
# escalonado son la misma decision; si difirieran, una de las dos estaria mal.
for (a in NIVELES) {
  por_ajustado <- tab$p_beta_holm[familia] <= a
  por_umbral <- tab$p_beta[familia] <=
    tab[[paste0("umbral_holm_beta_", etq(a))]][familia]
  if (!identical(sort(which(por_ajustado)), sort(which(por_umbral))))
    detener("Las dos formulaciones de Holm no coinciden al nivel ", a, ".")
}

# Comprobacion del sentido de las dos correcciones. Holm no puede rechazar
# menos que Bonferroni. Si lo hiciera, una de las dos estaria mal aplicada.
if (sum(tab$resiste_holm, na.rm = TRUE) <
    sum(tab$resiste_bonferroni, na.rm = TRUE))
  detener("Holm rechaza menos que Bonferroni. La correccion esta mal ",
          "aplicada.")
tab$excluye_nominal <- tab$ic_inferior > NOMINAL | tab$ic_superior < NOMINAL
tab$por_debajo <- tab$ic_superior < NOMINAL
tab$por_encima <- tab$ic_inferior > NOMINAL
tab$punto_bajo_nominal <- tab$cobertura < NOMINAL
for (v in c("cobertura","ic_inferior","ic_superior",
            "ic_beta_inferior","ic_beta_superior"))
  tab[[v]] <- round(tab[[v]], 4)
for (v in grep("^p_|^umbral_holm", names(tab), value = TRUE))
  tab[[v]] <- signif(tab[[v]], 6)

cat("\n=== INTERVALOS EXACTOS POR CLASE Y SEDE ===\n")
print(tab[, c("seccion","sede","clase","n","cubiertos","cobertura",
              "ic_inferior","ic_superior","por_debajo","punto_bajo_nominal")],
      row.names = FALSE)

cat("\n=== CELDAS QUE FALLAN, CON MULTIPLICIDAD ===\n")
cat("Celdas contrastadas en la familia:", m, "\n")
cat("Falsas positivas esperadas bajo la hipotesis global:",
    m * NIVEL_FAMILIA, "\n")
cat("Umbral de Bonferroni:", signif(NIVEL_FAMILIA / m, 3), "\n\n")
fa_ord <- tab[familia, ]
fa_ord <- fa_ord[order(fa_ord$p_unilateral), ]
print(fa_ord[fa_ord$por_debajo | fa_ord$p_unilateral <= NIVEL_FAMILIA,
             c("sede","clase","n","cubiertos","cobertura","p_unilateral",
               "umbral_holm","resiste_bonferroni","resiste_holm")],
      row.names = FALSE)

cat("\nValores exactos de la unidad reservada, fuera de la familia\n")
print(tab[!familia, c("clase","n","cubiertos","cobertura","n_calibracion",
                      "p_unilateral","p_beta")], row.names = FALSE)

# --- recuentos bajo cada correccion y cada nivel ---------------------------
sedes_f <- unique(tab$sede[familia])
rec <- do.call(rbind, lapply(NIVELES, function(a)
  do.call(rbind, lapply(c("bonferroni","holm","beta_bonferroni",
                          "beta_holm"), function(cr) {
    col <- paste0("resiste_", cr, "_", etq(a))
    r <- tab[[col]] & familia
    data.frame(nivel = a, correccion = cr,
               umbral_bonferroni = a / m,
               celdas_resisten = sum(r, na.rm = TRUE),
               sedes_fallan = sum(sapply(sedes_f, function(u)
                 any(r[tab$sede == u], na.rm = TRUE))),
               row.names = NULL)
  }))))

cat("\n=== RECUENTOS BAJO CADA CORRECCION Y CADA NIVEL ===\n")
print(rec, row.names = FALSE)

est_holm <- length(unique(rec$celdas_resisten[rec$correccion == "holm"])) == 1
est_bonf <- length(unique(rec$celdas_resisten[
  rec$correccion == "bonferroni"])) == 1
cat("\nEl recuento bajo Holm", if (est_holm) "no varia" else "varia",
    "entre los dos niveles.\n")
cat("El recuento bajo Bonferroni",
    if (est_bonf) "tampoco varia" else "si varia", "entre ellos.\n")

# --- causas del fallo, por sede -------------------------------------------
# El conjunto de clases que resiste la correccion en cada sede constituye la
# causa de su fallo. Se agrupan las sedes por causa, en lugar de contar
# patrones: una sede sin fallo demostrable tiene conjunto vacio, y contarlo
# como un patron mas sugeriria que tambien falla.
ADOPTADO <- paste0("resiste_beta_holm_", etq(0.025))
cat("\nAnalisis adoptado:", ADOPTADO, "\n")

causa <- sapply(sedes_f, function(u) {
  k <- tab$clase[tab$sede == u & familia & tab[[ADOPTADO]]]
  if (length(k) == 0) "" else paste(sort(k), collapse = "+")
})
cau <- as.data.frame(table(causa[causa != ""]), stringsAsFactors = FALSE)
names(cau) <- c("clases", "sedes")
cau <- cau[order(-cau$sedes, cau$clases), ]
cau$sin_fallo_demostrable <- sum(causa == "")

cat("\n=== CAUSA DEL FALLO POR SEDE ===\n")
print(cau, row.names = FALSE)
cat("Sedes sin fallo que resista la correccion:", sum(causa == ""), "\n")

por_sede <- function(s) {
  x <- tab[tab$seccion == s, ]
  sedes <- unique(x$sede)
  data.frame(
    seccion = s,
    sedes = length(sedes),
    sedes_punto = sum(sapply(sedes, function(u)
      any(x$punto_bajo_nominal[x$sede == u]))),
    sedes_intervalo_debajo = sum(sapply(sedes, function(u)
      any(x$por_debajo[x$sede == u]))),
    sedes_intervalo_excluye = sum(sapply(sedes, function(u)
      any(x$excluye_nominal[x$sede == u]))),
    clases = nrow(x),
    clases_punto = sum(x$punto_bajo_nominal),
    clases_intervalo_debajo = sum(x$por_debajo),
    clases_intervalo_excluye = sum(x$excluye_nominal),
    sedes_beta_intervalo_debajo = sum(sapply(sedes, function(u)
      any(x$beta_por_debajo[x$sede == u]))),
    clases_beta_intervalo_debajo = sum(x$beta_por_debajo),
    clases_beta_intervalo_excluye = sum(x$beta_excluye_nominal),
    sedes_bonferroni = sum(sapply(sedes, function(u)
      any(x$resiste_bonferroni[x$sede == u], na.rm = TRUE))),
    sedes_holm = sum(sapply(sedes, function(u)
      any(x$resiste_holm[x$sede == u], na.rm = TRUE))),
    clases_bonferroni = sum(x$resiste_bonferroni, na.rm = TRUE),
    clases_holm = sum(x$resiste_holm, na.rm = TRUE),
    row.names = NULL)
}
resumen <- rbind(por_sede("dejando una sede fuera"),
                 por_sede("unidad reservada"))

cat("\n=== RECUENTO BAJO CADA CRITERIO ===\n")
print(resumen, row.names = FALSE)

cat("\nCriterio puntual: la estimacion queda por debajo del nominal.\n")
cat("Criterio de intervalo: el intervalo exacto queda entero por debajo.\n")
cat("Exclusion a dos colas: el intervalo no contiene el nominal, sea por\n")
cat("defecto o por exceso. La garantia conforme es unilateral, de modo que\n")
cat("una cobertura superior al nominal no la incumple.\n")

dir.create(OUT, recursive = TRUE, showWarnings = FALSE)
write.csv(tab, file.path(OUT, "intervalos_cobertura.csv"), row.names = FALSE)
write.csv(resumen, file.path(OUT, "criterios_cobertura.csv"),
          row.names = FALSE)
write.csv(rec, file.path(OUT, "recuentos_multiplicidad.csv"),
          row.names = FALSE)
write.csv(cau, file.path(OUT, "causas_fallo.csv"), row.names = FALSE)

writeLines(toJSON(list(
  fase = "26",
  ejecutado_en = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
  semilla = SEMILLA,
  r_version = R.version.string,
  glmnet_version = as.character(packageVersion("glmnet")),
  proposito = paste("intervalos exactos de la cobertura condicional y",
                    "aplicacion de un solo criterio"),
  intervalo = "Clopper y Pearson, por inversion de la prueba binomial",
  confianza = CONFIANZA,
  lateralidad = paste("el intervalo es a dos colas; contrastar su extremo",
                      "superior contra el nominal equivale a una prueba",
                      "unilateral a la mitad de ese nivel"),
  nivel_nominal = NOMINAL,
  criterio_puntual = "la estimacion queda por debajo del nivel nominal",
  criterio_intervalo = paste("el intervalo exacto queda entero por",
                             "debajo del nivel nominal"),
  criterio_dos_colas = "el intervalo no contiene el nivel nominal",
  niveles_contrastados = NIVELES,
  nivel_principal = NIVEL_FAMILIA,
  celdas_en_la_familia = m,
  familia = paste("las celdas de clase por sede de la validacion dejando una",
                  "sede fuera; la unidad reservada se evaluo una sola vez y",
                  "por separado, y queda fuera"),
  falsas_positivas_esperadas = paste("bajo la hipotesis global de que toda",
                                     "cobertura alcanza el nominal, el",
                                     "numero de celdas por el nivel"),
  valor_p = paste("exacto y unilateral, de la binomial acumulada; contrasta",
                  "que la cobertura alcanza el nominal frente a que queda",
                  "por debajo"),
  correcciones = "Bonferroni y Holm, ambas sobre la tasa de error por familia",
  garantia = paste("unilateral: la cobertura por encima del nominal no",
                   "incumple la garantia"),
  origen_sede_fuera = paste("replica de R/35_louo.R, contrastada contra las",
                            "proporciones publicadas"),
  origen_unidad_reservada = paste("recuento deducido de la cobertura",
                                  "publicada y verificado contra sus",
                                  "intervalos; la unidad no se reabre")),
  auto_unbox = TRUE, pretty = TRUE, digits = 15),
  file.path(OUT, "manifiesto.json"))

cat("\nDepositado en", OUT, "\n")
