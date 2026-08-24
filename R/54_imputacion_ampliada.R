library(mice)
set.seed(20260818)

VITALES <- c("temperatura","frec_cardiaca","frec_respiratoria","saturacion")
LABS <- c("leucocitos","hemoglobina","plaquetas","rdw","creatinina","urea",
          "brecha_anion","sodio","potasio","cloro","bicarbonato","inr",
          "ttpa","ph","pco2","lactato","exceso_base")

lab <- read.csv("outputs/fase5/matriz_particionada.csv", stringsAsFactors = FALSE)
vit <- read.csv("data/derivados/vitales_limpios.csv", stringsAsFactors = FALSE)

d <- merge(lab, vit[, c("stay_id", VITALES)], by = "stay_id", all.x = TRUE)
cat("Estancias:", nrow(d), "\n")

d$unidad <- factor(d$unidad)
d$sexo   <- factor(d$sexo)

# La unidad reservada carece de representacion en el conjunto de
# entrenamiento. Conservarla como nivel del factor produciria una columna
# constante en la matriz de diseno, con la consiguiente deficiencia de rango
# que llevaria al procedimiento a suprimir variables sin advertencia. Se
# eliminan por tanto los niveles vacios y se fija como referencia el primero
# de los presentes.
niveles_ent <- sort(unique(as.character(d$unidad[d$grupo == "entrenamiento"])))
d$unidad <- factor(ifelse(as.character(d$unidad) %in% niveles_ent,
                          as.character(d$unidad), niveles_ent[1]),
                   levels = niveles_ent)

cat("\n=== AUSENTES ANTES DE IMPUTAR ===\n")
TODAS <- c(LABS, VITALES)
print(data.frame(
  variable = TODAS,
  pct_ausente = round(100 * sapply(TODAS, function(x) mean(is.na(d[[x]]))), 2),
  row.names = NULL), row.names = FALSE)

# El modelo de imputacion se estima exclusivamente sobre el conjunto de
# entrenamiento. Las restantes filas reciben valores imputados sin contribuir
# a la estimacion, de modo que ninguna informacion procedente de la
# evaluacion influya en el tratamiento de los datos ausentes.
#
# El desenlace queda fuera del modelo: el sistema debe operar sobre pacientes
# cuya categoria se desconoce, y su inclusion produciria imputaciones
# inalcanzables en el momento de la aplicacion.
datos <- d[, c(LABS, VITALES, "edad", "sexo", "unidad")]
ignorar <- d$grupo != "entrenamiento"

cat("\nFilas que contribuyen a la estimacion:", sum(!ignorar), "\n")
cat("Filas que solo reciben imputacion:", sum(ignorar), "\n")
cat("\nImputando. Veinte conjuntos, diez iteraciones. Esto toma varios minutos.\n")

imp <- mice(datos, m = 20, maxit = 10, method = "pmm",
            seed = 20260818, printFlag = FALSE, ignore = ignorar)

cat("Completado.\n")

cat("\n=== VARIABLES SUPRIMIDAS POR EL PROCEDIMIENTO ===\n")
ev <- imp$loggedEvents
if (is.null(ev)) cat("Ninguna. El procedimiento conservo todas las variables.\n") else {
  cat("Se registraron", nrow(ev), "eventos.\n"); print(head(ev, 20))
}

imps <- lapply(1:20, function(i) {
  x <- complete(imp, i)
  x$stay_id <- d$stay_id
  x$clase   <- d$clase
  x$grupo   <- d$grupo
  x$lactato_medido <- d$lactato_medido
  x
})

cat("\n=== COMPROBACION DE AUSENTES TRAS IMPUTAR ===\n")
cat("Total de valores ausentes en el primer conjunto:",
    sum(is.na(imps[[1]][, TODAS])), "\n")

cat("\n=== MEDIANA DE LAS CONSTANTES, OBSERVADO FRENTE A IMPUTADO ===\n")
print(do.call(rbind, lapply(VITALES, function(vv) {
  obs <- d[[vv]][!is.na(d[[vv]])]
  falt <- is.na(d[[vv]])
  impv <- unlist(lapply(imps, function(x) x[[vv]][falt]))
  data.frame(variable = vv,
             n_ausentes = sum(falt),
             mediana_observada = round(median(obs), 2),
             mediana_imputada = round(median(impv), 2),
             row.names = NULL)
})), row.names = FALSE)

# Validacion por enmascaramiento. Se ocultan valores efectivamente observados
# y se comparan las estimaciones con el dato real. Constituye la unica
# comprobacion directa de exactitud: la semejanza entre las distribuciones de
# valores observados e imputados indica consistencia, no que las
# estimaciones individuales sean correctas.
cat("\n=== VALIDACION POR ENMASCARAMIENTO ===\n")
ent <- d[d$grupo == "entrenamiento", ]
completo <- ent[complete.cases(ent[, c(LABS, VITALES)]), ]
cat("Casos completos disponibles:", nrow(completo), "\n")

FRACCION <- 0.10
enmascarado <- completo
mascara <- list()
for (vv in VITALES) {
  idx <- sample(nrow(completo), floor(FRACCION * nrow(completo)))
  mascara[[vv]] <- idx
  enmascarado[[vv]][idx] <- NA
}

imp_v <- mice(enmascarado[, c(LABS, VITALES, "edad", "sexo", "unidad")],
              m = 5, maxit = 10, method = "pmm",
              seed = 20260818, printFlag = FALSE)
est <- complete(imp_v, 1)

print(do.call(rbind, lapply(VITALES, function(vv) {
  idx <- mascara[[vv]]
  real <- completo[[vv]][idx]
  esti <- est[[vv]][idx]
  med  <- median(completo[[vv]])
  data.frame(variable = vv,
             error_mediano_mice = round(median(abs(esti - real)), 3),
             error_mediano_mediana = round(median(abs(med - real)), 3),
             correlacion = round(cor(esti, real), 3),
             row.names = NULL)
})), row.names = FALSE)

saveRDS(imps, "data/derivados/imputaciones_ampliadas.rds")
cat("\nGuardado en data/derivados/imputaciones_ampliadas.rds\n")
