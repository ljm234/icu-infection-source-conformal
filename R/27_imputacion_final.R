library(mice)
library(jsonlite)

SEMILLA <- 20260818
set.seed(SEMILLA)

M_IMPUTACIONES <- 20
ITERACIONES    <- 10
OUT <- "outputs/fase6"
dir.create(OUT, recursive = TRUE, showWarnings = FALSE)

d <- read.csv("outputs/fase5/matriz_particionada.csv", stringsAsFactors = FALSE)
d <- d[d$grupo != "excluido", ]

vars <- c("leucocitos","hemoglobina","plaquetas","rdw","creatinina","urea",
          "brecha_anion","sodio","potasio","cloro","bicarbonato","inr",
          "ttpa","ph","pco2","lactato","exceso_base")

# La unidad sellada no aporta observaciones al conjunto de estimacion, de
# modo que su indicador constituye una columna nula dentro de la matriz de
# diseno y genera dependencia lineal. Los niveles se restringen a los
# presentes en entrenamiento y las filas de la unidad sellada se asignan al
# nivel de referencia. La imputacion de esa unidad emplea entonces las
# relaciones aprendidas en las demas, que es la condicion real de una sede
# no representada en el desarrollo del modelo.
niveles_ent <- sort(unique(d$unidad[d$grupo == "entrenamiento"]))
d$unidad_mod <- factor(ifelse(d$unidad %in% niveles_ent,
                              d$unidad, niveles_ent[1]),
                       levels = niveles_ent)
d$sexo <- factor(d$sexo)

predictoras <- c(vars, "edad", "sexo", "unidad_mod")
datos <- d[, predictoras]

ignorar <- d$grupo != "entrenamiento"

cat("Filas totales:", nrow(datos), "\n")
cat("Estiman el modelo:", sum(!ignorar), "\n")
cat("Niveles de unidad en el modelo:", nlevels(datos$unidad_mod), "\n\n")

metodo <- make.method(datos)
metodo[names(metodo) %in% vars] <- "pmm"
metodo[!(names(metodo) %in% vars)] <- ""

predm <- make.predictorMatrix(datos)
diag(predm) <- 0

inicio <- Sys.time()
imp <- mice(datos, m = M_IMPUTACIONES, maxit = ITERACIONES,
            method = metodo, predictorMatrix = predm,
            ignore = ignorar, seed = SEMILLA, printFlag = FALSE)
cat("Tiempo:", round(difftime(Sys.time(), inicio, units = "mins"), 1), "min\n")

cat("\n=== EVENTOS REGISTRADOS ===\n")
if (is.null(imp$loggedEvents)) {
  cat("Ninguno. Todas las predictoras participaron.\n")
  ok <- TRUE
} else {
  print(sort(table(imp$loggedEvents$out), decreasing = TRUE))
  ok <- !any(grepl("unidad", imp$loggedEvents$out))
}

cat("\n=== MEDIANA DE LACTATO IMPUTADO POR UNIDAD REAL ===\n")
p1 <- complete(imp, 1)
p1$unidad <- d$unidad
p1$falta  <- is.na(datos$lactato)
si <- p1[p1$falta, ]
tab <- aggregate(lactato ~ unidad, data = si,
                 FUN = function(x) round(median(x), 2))
names(tab) <- c("unidad", "mediana_imputada")
print(tab, row.names = FALSE)

cat("\n=== ESTABILIDAD ===\n")
estab <- do.call(rbind, lapply(vars, function(v) {
  u <- imp$chainMean[v, , ][(ITERACIONES-4):ITERACIONES, ]
  data.frame(variable = v,
             media = round(mean(u, na.rm = TRUE), 3),
             variacion = round(sd(apply(u, 1, mean, na.rm = TRUE)), 4),
             row.names = NULL)
}))
print(estab, row.names = FALSE)

completas <- lapply(1:M_IMPUTACIONES, function(i) {
  x <- complete(imp, i)
  x$stay_id <- d$stay_id; x$clase <- d$clase; x$grupo <- d$grupo
  x$unidad <- d$unidad;   x$lactato_medido <- d$lactato_medido; x$imp <- i
  x
})

saveRDS(imp, file.path(OUT, "objeto_mice.rds"))
saveRDS(completas, file.path(OUT, "imputaciones.rds"))
write.csv(estab, file.path(OUT, "estabilidad.csv"), row.names = FALSE)

writeLines(toJSON(list(
  fase = "6", ejecutado_en = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
  semilla = SEMILLA, m = M_IMPUTACIONES, maxit = ITERACIONES, metodo = "pmm",
  mice_version = as.character(packageVersion("mice")),
  niveles_unidad = as.character(niveles_ent),
  unidad_sellada_al_nivel_referencia = TRUE,
  unidad_en_todos_los_modelos = ok,
  n_entrenamiento = sum(!ignorar)), auto_unbox = TRUE, pretty = TRUE),
  file.path(OUT, "manifiesto.json"))

cat("\nGuardado en", OUT, "\n")
