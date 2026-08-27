library(glmnet)
library(jsonlite)

# Reparacion de los manifiestos de las fases septima y octava.
#
# El serializador redondea a cuatro decimales por omision, y esos manifiestos
# registran cantidades que exigen mas. La penalizacion de la fase octava
# consta como dos diezmilesimas cuando vale 0.000191, cifra con la que nadie
# reproduciria el modelo. Un umbral consta ademas en desacuerdo con el
# archivo que lo publica, por caer su valor junto a la frontera de redondeo.
#
# No se reejecutan R/30 ni R/33. Reescribirian cuatro archivos excluidos del
# versionado, entre ellos aquel del que R/36 toma los umbrales de la unidad
# reservada: una reproduccion imperfecta obligaria a reabrirla, y no hay copia
# de la que restaurar.
#
# Se procede al reves. Reejecutar produciria el registro de una ejecucion
# nueva; leer de las fuentes produce el valor que dejo la original, porque
# esas fuentes son su producto y estan versionadas. Cada campo numerico
# procede de una fuente nombrada o queda declarado como conservado, y el
# procedimiento se detiene si alguno no encaja en ninguna de las dos
# categorias.
#
# Sobre la segunda guardia. El enunciado natural seria exigir que el valor
# exacto redondee al que el manifiesto tenia. No sirve: el defecto que se
# repara es precisamente un redondeo mal hecho, y el umbral de la categoria
# urinaria queda tan cerca de la frontera que el serializador lo dejo en una
# diezmilesima por debajo de lo que el archivo de umbrales publica. Se exige
# en su lugar que la diferencia no exceda una unidad del ultimo decimal que el
# manifiesto conservaba. Por encima de eso ya no seria redondeo sino
# discrepancia, y procede detenerse.

TOL <- 1e-4

detener <- function(...) {
  cat("\n", ..., "\n", sep = "")
  cat("El procedimiento se detiene. No se reescribe manifiesto alguno.\n")
  quit(status = 1)
}

leer_json <- function(r) {
  if (!file.exists(r)) detener("Manifiesto ausente: ", r)
  fromJSON(r, simplifyVector = TRUE)
}

# Registro de procedencia. Cada campo numerico se acompana de su origen, y el
# resumen final lo consigna para que un lector no tenga que reconstruirlo.
proc <- list()
derivados <- list("7" = character(0), "8" = character(0))
anotar <- function(fase, campo, valor, fuente) {
  proc[[length(proc)+1]] <<- data.frame(
    fase = fase, campo = campo, fuente = fuente,
    valor = paste(format(valor, digits = 15), collapse = " "),
    row.names = NULL)
  derivados[[fase]] <<- c(derivados[[fase]], sub("[.].*", "", campo))
  valor
}

RUTA <- c(esp  = "outputs/fase7/especificacion.rds",
          m7   = "outputs/fase7/modelo.rds",
          hip  = "outputs/fase7/busqueda_hiperparametros.csv",
          man7 = "outputs/fase7/manifiesto.json",
          m8   = "outputs/fase8/modelo_final.rds",
          umb  = "outputs/fase8/umbrales.csv",
          cob  = "outputs/fase8/cobertura.csv",
          man8 = "outputs/fase8/manifiesto.json",
          imp  = "outputs/fase6/imputaciones.rds")
for (r in RUTA) if (!file.exists(r)) detener("Fuente ausente: ", r)

esp <- readRDS(RUTA[["esp"]])
m7  <- readRDS(RUTA[["m7"]])
m8  <- readRDS(RUTA[["m8"]])
hip <- read.csv(RUTA[["hip"]], stringsAsFactors = FALSE)
umb <- read.csv(RUTA[["umb"]], stringsAsFactors = FALSE)
cob <- read.csv(RUTA[["cob"]], stringsAsFactors = FALSE)
a7  <- leer_json(RUTA[["man7"]])
a8  <- leer_json(RUTA[["man8"]])

cat("=== FUENTES LEIDAS ===\n")
for (n in names(RUTA)) cat(sprintf("  %-5s %s\n", n, RUTA[[n]]))

# ---------------------------------------------------------------------------
# Fase septima
# ---------------------------------------------------------------------------

lam7 <- anotar("7", "lambda", m7$lambda, "outputs/fase7/modelo.rds")
fa <- which(abs(hip$alpha - a7$alpha) < 1e-9)
if (length(fa) != 1)
  detener("La busqueda de hiperparametros no contiene la mezcla del ",
          "manifiesto.")
if (abs(lam7 - hip$lambda_1se[fa]) > TOL)
  detener("La penalizacion del modelo no concuerda con la registrada en la ",
          "busqueda.")

pct <- as.numeric(sub("%", "", names(esp$nudos[[1]]))) / 100
if (length(pct) != 4 || any(is.na(pct)))
  detener("Los percentiles de los nudos no se recuperan de la ",
          "especificacion.")
pct <- anotar("7", "percentiles_nudos", pct,
              "nombres de los cuantiles en especificacion.rds")

ncol7 <- anotar("7", "columnas_diseno", nrow(coef(m7)[[1]]) - 1,
                "outputs/fase7/modelo.rds")
ret <- unique(unlist(lapply(coef(m7), function(z) {
  nz <- which(z[-1] != 0); rownames(z)[-1][nz] })))
nret <- anotar("7", "columnas_retenidas", length(ret),
               "outputs/fase7/modelo.rds")
nimp <- anotar("7", "imputaciones_apiladas", length(readRDS(RUTA[["imp"]])),
               "outputs/fase6/imputaciones.rds")

man7 <- list(
  fase = a7$fase,
  ejecutado_en = a7$ejecutado_en,
  semilla = a7$semilla,
  glmnet_version = a7$glmnet_version,
  alpha = a7$alpha,
  lambda = lam7,
  regla_lambda = a7$regla_lambda,
  con_spline = as.character(esp$con_spline),
  lineales = as.character(esp$lineales),
  percentiles_nudos = pct,
  columnas_diseno = ncol7,
  columnas_retenidas = nret,
  imputaciones_apiladas = nimp,
  pliegues_por_paciente = a7$pliegues_por_paciente,
  reparado_por = "R/75_manifiestos_exactos.R",
  procedencia = "outputs/fase27/procedencia_manifiestos.csv")

# ---------------------------------------------------------------------------
# Fase octava
# ---------------------------------------------------------------------------

lam8 <- anotar("8", "lambda", m8$lambda, "outputs/fase8/modelo_final.rds")
nom <- anotar("8", "cobertura_nominal", cob$nominal[1],
              "outputs/fase8/cobertura.csv")

# El nivel alfa es un parametro de diseno y no un resultado, y el
# serializador no lo daño. Se conserva en lugar de recalcularlo: obtenerlo por
# diferencia introduce en el registro una cifra que ninguna fuente contiene,
# porque el complemento de nueve decimos no es exacto en base dos. Se
# comprueba en cambio que ambos sigan siendo complementarios.
alf <- a8$alfa
if (abs((1 - alf) - nom) > 1e-10)
  detener("El nivel alfa y la cobertura nominal no son complementarios.")
um <- setNames(as.list(umb$umbral), umb$clase)
for (k in names(um)) anotar("8", paste0("umbrales.", k), um[[k]],
                            "outputs/fase8/umbrales.csv")

man8 <- list(
  fase = a8$fase,
  ejecutado_en = a8$ejecutado_en,
  semilla = a8$semilla,
  alfa = alf,
  cobertura_nominal = nom,
  lambda = lam8,
  regla_lambda = a8$regla_lambda,
  justificacion_lambda = a8$justificacion_lambda,
  puntuacion = a8$puntuacion,
  calibracion = a8$calibracion,
  umbrales = um,
  cobertura_marginal = a8$cobertura_marginal,
  proporcion_con_conclusion = a8$proporcion_con_conclusion,
  reparado_por = "R/75_manifiestos_exactos.R",
  procedencia = "outputs/fase27/procedencia_manifiestos.csv")

# ---------------------------------------------------------------------------
# Guardias
# ---------------------------------------------------------------------------

# Campos conservados del manifiesto anterior. Son los que el serializador no
# pudo danar, por carecer de decimales o de naturaleza numerica, y los que
# describen la ejecucion y no su resultado. Se declaran uno a uno: sin esta
# relacion, un campo nuevo pasaria sin fuente y sin aviso.
CONSERVADOS <- list(
  "7" = c("fase","ejecutado_en","semilla","glmnet_version","alpha",
          "regla_lambda","con_spline","lineales","pliegues_por_paciente"),
  "8" = c("fase","ejecutado_en","semilla","alfa","regla_lambda",
          "justificacion_lambda","puntuacion","calibracion",
          "cobertura_marginal","proporcion_con_conclusion"))

# La marca de reparacion no procede del manifiesto anterior ni de fuente
# alguna: la anade este procedimiento, y por eso se declara aparte. Un
# registro de ejecucion reescrito despues sin decirlo es lo que un lector
# atento señala, aunque cada valor que contenga sea rastreable.
ANADIDOS <- c("reparado_por", "procedencia")

derivados <- lapply(derivados, unique)

cat("\n=== PROCEDENCIA DE CADA CAMPO ===\n")
for (f in c("7","8")) {
  nuevo <- if (f == "7") man7 else man8
  for (campo in names(nuevo)) {
    origen <- if (campo %in% derivados[[f]]) "leido de su fuente"
              else if (campo %in% CONSERVADOS[[f]]) "conservado"
              else if (campo %in% ANADIDOS) "anadido por la reparacion"
              else NA_character_
    if (is.na(origen))
      detener("El campo ", campo, " de la fase ", f,
              " carece de fuente rastreable y no figura entre los ",
              "conservados.")
    cat(sprintf("  fase %s  %-26s %s\n", f, campo, origen))
  }
}

cat("\n=== CONTRASTE CON EL MANIFIESTO ANTERIOR ===\n")
comparar <- function(f, nuevo, viejo) {
  for (campo in names(nuevo)) {
    if (campo %in% ANADIDOS) next
    a <- nuevo[[campo]]; b <- viejo[[campo]]
    if (is.null(b)) detener("El campo ", campo, " no figuraba en el ",
                            "manifiesto de la fase ", f, ".")
    if (is.list(a)) { a <- unlist(a); b <- unlist(b) }
    if (is.numeric(a)) {
      if (length(a) != length(b))
        detener("El campo ", campo, " cambia de longitud.")
      d <- max(abs(as.numeric(a) - as.numeric(b)))
      cat(sprintf("  fase %s  %-26s desviacion %.3g%s\n", f, campo, d,
                  if (d > 0) "  (redondeo reparado)" else ""))
      if (d > TOL)
        detener("El campo ", campo, " de la fase ", f, " difiere en ",
                signif(d, 3), ", por encima de lo que el redondeo explica. ",
                "Es discrepancia real y procede investigarla.")
    } else if (!identical(as.character(a), as.character(b))) {
      detener("El campo no numerico ", campo, " de la fase ", f,
              " no coincide.")
    }
  }
}
comparar("7", man7, a7)
comparar("8", man8, a8)

# El argumento entero descansa en que el serializador conserve ahora los
# decimales que importan. Se comprueba sobre el valor que peor lo pasaba.
#
# No se exige identidad de bits. Conservarla obliga a quince decimales en toda
# cifra redonda, de modo que el nivel nominal se escribiria como
# 0.90000000000000002, y un registro destinado a leerse empeora con eso. Se
# exige recuperar el valor con error relativo por debajo de una milbillonesima,
# que agota la precision util del tipo y deja las cifras redondas intactas.
DIGITOS <- 15
TOL_REL <- 1e-15
prueba <- fromJSON(toJSON(list(x = lam8), auto_unbox = TRUE,
                          digits = DIGITOS))$x
err <- abs(prueba - lam8) / abs(lam8)
cat("\nError relativo del serializador sobre la penalizacion:",
    signif(err, 3), "\n")
if (err > TOL_REL)
  detener("El serializador sigue perdiendo decimales que importan.")

redondo <- fromJSON(toJSON(list(x = nom), auto_unbox = TRUE,
                           digits = DIGITOS))$x
if (!identical(redondo, nom))
  detener("El serializador altera una cifra redonda.")
cat("Las cifras redondas se conservan sin alteracion.\n")

writeLines(toJSON(man7, auto_unbox = TRUE, pretty = TRUE, digits = DIGITOS),
           RUTA[["man7"]])
writeLines(toJSON(man8, auto_unbox = TRUE, pretty = TRUE, digits = DIGITOS),
           RUTA[["man8"]])

pr <- do.call(rbind, proc)
dir.create("outputs/fase27", recursive = TRUE, showWarnings = FALSE)
write.csv(pr, "outputs/fase27/procedencia_manifiestos.csv", row.names = FALSE)

cat("\nManifiestos reescritos desde sus fuentes.\n")
cat("Procedencia depositada en outputs/fase27\n")
