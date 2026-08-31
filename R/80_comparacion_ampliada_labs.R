library(DBI)
library(duckdb)
library(glmnet)
library(jsonlite)

# Comparacion de la especificacion retenida contra una ampliada.
#
# ANALISIS POSTERIOR. Se escribe el 2026-08-31, trece dias despues de que la
# seleccion de determinaciones quedara fijada en R/19_matriz.R el 2026-08-18.
# Responde a una pregunta que la seleccion original no documento, y no
# reconstruye el criterio con que se hizo: ese criterio no consta.
#
# La pregunta no es si setenta y tres determinaciones rinden mas que
# diecisiete. Esa no admite respuesta por casos completos: ninguna estancia de
# la cohorte tiene las setenta y tres dentro de la ventana, y el maximo
# observado se queda en sesenta y nueve. La pregunta que si admite respuesta,
# y que es la que un lector hara, es por que quedaron fuera determinaciones
# mas frecuentes que varias de las incluidas. Se comparan por tanto las
# diecisiete retenidas contra ellas mismas mas las descartadas cuya cobertura
# supera a la de la retenida menos frecuente.
#
# EL SESGO, DECLARADO. Restringir a casos completos no produce una muestra
# aleatoria de la cohorte. Las estancias con analitica mas completa son las
# mas monitorizadas, y la intensidad de monitorizacion se asocia tanto a la
# gravedad del paciente como a la unidad que lo atiende. Lo que sigue
# responde si las determinaciones anadidas aportan ENTRE PACIENTES CON
# ANALITICA COMPLETA. No responde si aportarian en la cohorte, y el resultado
# no debe leerse asi.
#
# Ambas especificaciones entran en forma lineal. Las anadidas no tienen nudos
# definidos, y dar forma flexible a unas y no a otras confundiria el conjunto
# de variables con la forma funcional, que es justo lo que la comparacion
# quiere aislar. La penalizacion se valida por separado dentro de cada
# especificacion, de modo que ninguna compite con un valor ajustado para la
# otra.
#
# La unidad reservada no interviene: el ajuste emplea el conjunto de
# entrenamiento y la evaluacion el de prueba.

SEMILLA <- 20260818
set.seed(SEMILLA)

BASE <- path.expand("~/mimic-data/physionet.org/files/mimiciv/3.1")
LAB  <- file.path(BASE, "hosp", "labevents.csv.gz")
ICU  <- file.path(BASE, "icu",  "icustays.csv.gz")

OUT <- "outputs/fase32"
RUTA_CAND <- "outputs/fase30/determinaciones_candidatas.csv"
RUTA_PART <- "outputs/fase5/matriz_particionada.csv"
RUTA_ESP  <- "outputs/fase7/especificacion.rds"
RUTA_MAN  <- "outputs/fase7/manifiesto.json"
RUTA_FLU  <- "outputs/fase2/flujo.csv"
PLIEGUES <- 10

detener <- function(...) {
  cat("\n", ..., "\n", sep = "")
  cat("El procedimiento se detiene. No procede concluir sobre esta base.\n")
  quit(status = 1)
}

for (r in c(LAB, ICU, RUTA_CAND, RUTA_PART, RUTA_ESP, RUTA_MAN, RUTA_FLU))
  if (!file.exists(r)) detener("Fuente ausente: ", r)

esp <- readRDS(RUTA_ESP)
CLASES <- esp$clases
man <- fromJSON(RUTA_MAN)
if (is.null(man$alpha)) detener("El manifiesto de la fase 7 no declara alfa.")
ALFA_NET <- as.numeric(man$alpha)

cand <- read.csv(RUTA_CAND, stringsAsFactors = FALSE)
cand$itemid <- as.character(cand$itemid)
minima <- min(cand$cobertura_pct[cand$retenida])
cand$en_subconjunto <- cand$retenida | cand$cobertura_pct > minima
anadidas <- cand[cand$en_subconjunto & !cand$retenida, ]
if (nrow(anadidas) == 0) detener("No hay descartadas por encima del listado.")

cat("=== ESPECIFICACIONES ===\n")
cat("Retenidas:", sum(cand$retenida), "  anadidas:", nrow(anadidas),
    "  ampliada:", sum(cand$en_subconjunto), "\n")
cat("Las anadidas y su cobertura:\n")
print(anadidas[, c("etiqueta", "panel", "cobertura_pct")], row.names = FALSE)

part <- read.csv(RUTA_PART, stringsAsFactors = FALSE)
vars17 <- c(esp$con_spline, esp$lineales)
if (length(vars17) != 17) detener("La especificacion no declara diecisiete.")
falta <- setdiff(c("stay_id","clase","grupo","unidad","edad","sexo", vars17),
                 names(part))
if (length(falta) > 0)
  detener("La particion carece de: ", paste(falta, collapse = ", "))

# ---------------------------------------------------------------------------
# Extraccion de las anadidas, con la regla de R/19: primera medicion dentro de
# la ventana, ordenando por hora de registro.
# ---------------------------------------------------------------------------

con <- dbConnect(duckdb::duckdb())
dbExecute(con, "PRAGMA threads=8")
dbExecute(con, "PRAGMA memory_limit='40GB'")
for (v in list(c("lab", LAB), c("icustays", ICU)))
  dbExecute(con, sprintf(
    "CREATE VIEW %s AS SELECT * FROM read_csv('%s', all_varchar=true)",
    v[1], v[2]))
dbWriteTable(con, "cohorte",
             data.frame(stay_id = as.character(unique(part$stay_id))))

dbExecute(con, "
  CREATE VIEW estancia AS
  SELECT e.subject_id, e.hadm_id, e.stay_id, e.intime FROM (
    SELECT subject_id, hadm_id, stay_id,
           CAST(intime AS TIMESTAMP) AS intime,
           ROW_NUMBER() OVER (PARTITION BY subject_id
                              ORDER BY CAST(intime AS TIMESTAMP)) AS orden
    FROM icustays) e
  JOIN cohorte c ON c.stay_id = e.stay_id
  WHERE e.orden = 1")

ids <- paste0("'", paste(anadidas$itemid, collapse = "','"), "'")
cat("\nExtrayendo las determinaciones anadidas.\n")
inicio <- Sys.time()
ext <- dbGetQuery(con, sprintf("
  SELECT stay_id, itemid, valuenum FROM (
    SELECT e.stay_id, l.itemid, CAST(l.valuenum AS DOUBLE) AS valuenum,
           ROW_NUMBER() OVER (PARTITION BY e.stay_id, l.itemid
                              ORDER BY CAST(l.storetime AS TIMESTAMP),
                                       CAST(l.charttime AS TIMESTAMP),
                                       l.labevent_id) AS r
    FROM estancia e
    JOIN lab l ON l.hadm_id = e.hadm_id
    WHERE l.valuenum IS NOT NULL
      AND l.itemid IN (%s)
      AND CAST(l.storetime AS TIMESTAMP) >= e.intime
      AND CAST(l.storetime AS TIMESTAMP) <= e.intime + INTERVAL 6 HOUR)
  WHERE r = 1", ids))
dbDisconnect(con, shutdown = TRUE)
cat("Tiempo:", round(difftime(Sys.time(), inicio, units = "mins"), 1), "min\n")

ancho <- reshape(ext, idvar = "stay_id", timevar = "itemid",
                 direction = "wide")
names(ancho) <- sub("^valuenum\\.", "x", names(ancho))
ancho$stay_id <- as.numeric(ancho$stay_id)
nuevas <- setdiff(names(ancho), "stay_id")
if (length(nuevas) != nrow(anadidas))
  detener("La extraccion no devuelve todas las determinaciones anadidas.")

d <- merge(part, ancho, by = "stay_id", all.x = TRUE)
if (nrow(d) != nrow(part)) detener("La union altera el numero de filas.")

# ---------------------------------------------------------------------------
# Casos completos y conjuntos
# ---------------------------------------------------------------------------

d <- d[d$clase %in% CLASES & d$grupo %in% c("entrenamiento", "prueba"), ]
d$clase <- factor(as.character(d$clase), levels = CLASES)
completo <- complete.cases(d[, c(vars17, nuevas)])
n_antes <- nrow(d)
d <- d[completo, ]

flu <- read.csv(RUTA_FLU, stringsAsFactors = FALSE)
N_COHORTE <- flu$n[flu$paso == "p5_cohorte_final"]

cat("\n=== CASOS COMPLETOS ===\n")
cat("Estancias de entrenamiento y prueba en las categorias modeladas:",
    n_antes, "\n")
cat("De ellas, completas en las", length(vars17) + length(nuevas), ":",
    nrow(d), sprintf("(%.2f por ciento)\n", 100 * nrow(d) / n_antes))
cat("Sobre la cohorte de", N_COHORTE, ":",
    sprintf("%.2f por ciento\n", 100 * nrow(d) / N_COHORTE))
print(table(grupo = d$grupo, clase = d$clase))

ent <- d[d$grupo == "entrenamiento", ]
pru <- d[d$grupo == "prueba", ]
if (nrow(ent) < 200 || nrow(pru) < 100)
  detener("Los conjuntos completos son demasiado pequenos para comparar.")
for (k in CLASES) if (sum(pru$clase == k) < 10)
  detener("La categoria ", k, " no reune diez casos en prueba.")

# ---------------------------------------------------------------------------
# Ajuste y evaluacion
# ---------------------------------------------------------------------------

matriz <- function(datos, vs) {
  uni <- model.matrix(~ unidad - 1, data = datos)[, -1, drop = FALSE]
  as.matrix(cbind(datos[, vs, drop = FALSE],
                  edad = datos$edad,
                  sexo_M = as.integer(datos$sexo == "M"),
                  uni))
}

auc <- function(p, yb) {
  r <- rank(p); n1 <- sum(yb); n0 <- length(yb) - n1
  if (n1 < 3 || n0 < 3) return(NA)
  (sum(r[yb == 1]) - n1*(n1+1)/2) / (n1*n0)
}

pac <- unique(ent$stay_id)
asign <- sample(rep(seq_len(PLIEGUES), length.out = length(pac)))
names(asign) <- pac
foldid <- asign[as.character(ent$stay_id)]

evaluar <- function(vs, etiqueta) {
  X <- matriz(ent, vs); Xp <- matriz(pru, vs)
  if (!identical(colnames(X), colnames(Xp)))
    detener(etiqueta, ": las dos matrices no comparten columnas.")
  cv <- cv.glmnet(X, ent$clase, family = "multinomial", alpha = ALFA_NET,
                  foldid = foldid, type.measure = "deviance",
                  standardize = TRUE)
  mod <- glmnet(X, ent$clase, family = "multinomial", alpha = ALFA_NET,
                lambda = cv$lambda.min, standardize = TRUE)
  p <- predict(mod, newx = Xp, type = "response")[, , 1]
  a <- sapply(CLASES, function(k) auc(p[, k], as.integer(pru$clase == k)))
  if (any(is.na(a))) detener(etiqueta, ": alguna area no se estima.")
  list(etiqueta = etiqueta, auc = a, lambda = cv$lambda.min,
       columnas = ncol(X),
       no_nulos = sum(sapply(coef(mod), function(z) sum(z[-1] != 0))))
}

cat("\nAjustando ambas especificaciones.\n")
r17 <- evaluar(vars17, "retenidas")
r29 <- evaluar(c(vars17, nuevas), "ampliada")

MIN <- CLASES[-1]
tab <- data.frame(
  clase = CLASES,
  auc_retenidas = round(r17$auc, 4),
  auc_ampliada = round(r29$auc, 4),
  diferencia = round(r29$auc - r17$auc, 4),
  row.names = NULL)

cat("\n=== DISCRIMINACION EN PRUEBA, CASOS COMPLETOS ===\n")
print(tab, row.names = FALSE)

pr17 <- mean(r17$auc[MIN]); pr29 <- mean(r29$auc[MIN])
cat("\nPromedio en minoritarias, retenidas:", round(pr17, 4), "\n")
cat("Promedio en minoritarias, ampliada:  ", round(pr29, 4), "\n")
cat("Diferencia:", round(pr29 - pr17, 4), "\n")

resumen <- data.frame(
  determinaciones_retenidas = length(vars17),
  determinaciones_anadidas = length(nuevas),
  columnas_retenidas = r17$columnas,
  columnas_ampliada = r29$columnas,
  no_nulos_retenidas = r17$no_nulos,
  no_nulos_ampliada = r29$no_nulos,
  estancias_ajuste = nrow(ent),
  estancias_prueba = nrow(pru),
  estancias_completas = nrow(d),
  denominador_cohorte = N_COHORTE,
  pct_de_la_cohorte = round(100 * nrow(d) / N_COHORTE, 2),
  promedio_minoritarias_retenidas = round(pr17, 4),
  promedio_minoritarias_ampliada = round(pr29, 4),
  diferencia_minoritarias = round(pr29 - pr17, 4),
  peor_cambio_por_clase = round(min(r29$auc[MIN] - r17$auc[MIN]), 4),
  row.names = NULL)

dir.create(OUT, recursive = TRUE, showWarnings = FALSE)
write.csv(tab, file.path(OUT, "comparacion_por_clase.csv"), row.names = FALSE)
write.csv(resumen, file.path(OUT, "comparacion_resumen.csv"),
          row.names = FALSE)
write.csv(anadidas[, c("itemid","etiqueta","panel","fluido","cobertura_pct")],
          file.path(OUT, "determinaciones_anadidas.csv"), row.names = FALSE)

writeLines(toJSON(list(
  fase = "32",
  ejecutado_en = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
  semilla = SEMILLA,
  r_version = R.version.string,
  glmnet_version = as.character(packageVersion("glmnet")),
  naturaleza = paste("analisis posterior escrito el 2026-08-31; la seleccion",
                     "se fijo el 2026-08-18 en R/19_matriz.R"),
  pregunta = paste("aportan las determinaciones descartadas de cobertura",
                   "superior a la de la retenida menos frecuente"),
  sesgo = paste("la restriccion a casos completos no produce una muestra",
                "aleatoria: las estancias con analitica mas completa son las",
                "mas monitorizadas, y la monitorizacion se asocia a gravedad",
                "y a unidad. El resultado responde si aportan entre pacientes",
                "con analitica completa, no si aportarian en la cohorte"),
  forma_funcional = paste("ambas especificaciones en forma lineal, para no",
                          "confundir el conjunto de variables con la forma"),
  penalizacion = paste("validada por separado dentro de cada especificacion,",
                       "regla del minimo"),
  unidad_reservada = "no interviene",
  alpha = ALFA_NET), auto_unbox = TRUE, pretty = TRUE, digits = 15),
  file.path(OUT, "manifiesto.json"))

cat("\nDepositado en", OUT, "\n")
