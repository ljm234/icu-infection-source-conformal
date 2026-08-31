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
# LO QUE SE COMPARA NO ES EL MODELO PUBLICADO, y las areas que siguen no son
# comparables con las del articulo. Difieren en cinco cosas, todas comunes a
# las dos ramas y por tanto inocuas para la comparacion, pero decisivas para
# quien intente cotejar cifras: aqui no hay imputacion, porque se trabaja
# sobre casos completos; no hay splines, por la razon dicha; no entra el
# indicador de solicitud de lactato; el conjunto de ajuste es mucho menor, al
# quedar restringido a esos casos completos; y la penalizacion se valida
# dentro de cada rama en vez de heredarse. La cifra de una rama solo significa
# algo frente a la de la otra.
#
# La direccion de la diferencia no se afirma sin contraste. Con cuarenta y
# cinco, sesenta y tres y sesenta casos en las categorias poco frecuentes, el
# error tipico de un area ronda las cuatro centesimas. Ahora bien, lo que
# interesa no es la precision de cada area sino la de su diferencia, y ambas
# se calculan sobre las mismas estancias: la diferencia emparejada elimina la
# variacion comun y puede ser bastante mas precisa que cualquiera de los dos
# valores por separado. Se estima por remuestreo emparejado del conjunto de
# evaluacion, con los modelos ajustados una sola vez, que es lo que aisla la
# incertidumbre de la comparacion y no la del procedimiento entero.
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
REPLICAS <- 10000
CONFIANZA <- 0.95

# Niveles de la familia. El trabajo corrige por multiplicidad en la seccion de
# transportabilidad y no puede dejar de hacerlo aqui: serian dos varas de
# medir en el mismo documento, que es el defecto que ya se corrigio entre el
# conjunto de prueba y la unidad reservada. La familia son las cuatro
# diferencias por categoria. El promedio sobre las minoritarias no entra en
# ella: es un unico resumen declarado de antemano y no una de cuatro
# comparaciones exploradas, y se reporta con su intervalo por separado.
NIVELES <- c(0.05, 0.025)

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
#
# La ordenacion incorpora el valor y el identificador del registro ademas de
# las dos horas. Ordenar solo por ellas no determina que fila se retiene
# cuando varias comparten instante, y una primera version de este
# procedimiento devolvia areas que diferian en la cuarta cifra entre
# ejecuciones por esa causa. Es el mismo defecto que la fase vigesima
# documenta para las constantes vitales. Con el valor en la clave, dos filas
# que empaten en las anteriores presentan por definicion el mismo valor y da
# igual cual se retenga.
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
                                       CAST(l.valuenum AS DOUBLE),
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
# El orden de las columnas se fija. reshape las devuelve en el orden en que
# encuentra los identificadores, que depende del orden de filas que la
# consulta entregue, y el descenso por coordenadas de glmnet recorre los
# predictores en el orden de la matriz: dos ordenes distintos convergen a
# soluciones que difieren en la ultima cifra. Ordenarlas hace el ajuste
# reproducible sin alterar el modelo.
nuevas <- sort(setdiff(names(ancho), "stay_id"))
if (length(nuevas) != nrow(anadidas))
  detener("La extraccion no devuelve todas las determinaciones anadidas.")

d <- merge(part, ancho, by = "stay_id", all.x = TRUE)
if (nrow(d) != nrow(part)) detener("La union altera el numero de filas.")

# ---------------------------------------------------------------------------
# Casos completos y conjuntos
#
# La cifra final es el resultado de tres restricciones encadenadas, y se
# deposita la cascada entera y no solo su ultimo peldano. Sin ella nadie
# puede reconstruirla sin leer este archivo, y aplicar proporciones sobre el
# total no la reproduce: la unidad reservada y las unidades por debajo del
# umbral no entran en la particion, de modo que no se reparten como las
# demas y su peso entre las estancias completas no tiene por que ser el que
# tienen en la cohorte.
#
# La completitud se evalua sobre la matriz particionada, que ya paso por los
# limites de plausibilidad. Un valor anulado por implausible cuenta aqui como
# ausente y contaba como presente en el recuento de la fase trigesima
# primera, que interroga la tabla de origen. Las dos cifras miden cosas
# distintas y el deposito las enfrenta en lugar de dejarlas sueltas.
# ---------------------------------------------------------------------------

flu <- read.csv(RUTA_FLU, stringsAsFactors = FALSE)
N_COHORTE <- flu$n[flu$paso == "p5_cohorte_final"]
if (nrow(d) != N_COHORTE)
  detener("La matriz particionada no cubre la cohorte final del embudo.")

completo   <- complete.cases(d[, c(vars17, nuevas)])
en_conj    <- completo & d$grupo %in% c("entrenamiento", "prueba")
en_clases  <- en_conj & d$clase %in% CLASES
n_antes    <- sum(d$clase %in% CLASES & d$grupo %in% c("entrenamiento", "prueba"))

cascada <- data.frame(
  paso = seq_len(4),
  descripcion = c(
    "estancias de la cohorte en la matriz particionada",
    "completas en las determinaciones comparadas",
    "y ademas en entrenamiento o prueba",
    "y ademas en una de las categorias modeladas"),
  estancias = c(nrow(d), sum(completo), sum(en_conj), sum(en_clases)),
  row.names = NULL)
cascada$pct_del_paso_anterior <- round(
  100 * cascada$estancias / c(NA, cascada$estancias[-nrow(cascada)]), 2)

# Reparto de las estancias completas entre los conjuntos de la particion. Es
# lo que dice a quien describe la comparacion: si un grupo aporta completas
# en proporcion distinta a su tamano, la restriccion no es neutral respecto
# de el.
grupos <- sort(unique(d$grupo))
por_grupo <- do.call(rbind, lapply(grupos, function(g) {
  s <- d$grupo == g
  data.frame(grupo = g, estancias = sum(s), completas = sum(s & completo),
             pct_del_grupo = round(100 * sum(s & completo) / sum(s), 2),
             pct_de_las_completas = round(100 * sum(s & completo) /
                                          sum(completo), 2),
             row.names = NULL)
}))

por_grupo_clase <- do.call(rbind, lapply(
  sort(unique(as.character(d$grupo[en_clases]))), function(g)
    do.call(rbind, lapply(CLASES, function(k)
      data.frame(grupo = g, clase = k,
                 estancias = sum(en_clases & d$grupo == g & d$clase == k),
                 row.names = NULL)))))

d <- d[en_clases, ]
d$clase <- factor(as.character(d$clase), levels = CLASES)

# La cascada ha de cerrar contra si misma y contra el desglose. Si dejara de
# hacerlo, alguno de los dos describiria un conjunto distinto del ajustado.
if (cascada$estancias[nrow(cascada)] != nrow(d))
  detener("La cascada no termina en el conjunto que se ajusta.")
if (sum(por_grupo$completas) != sum(completo))
  detener("El reparto por grupo no suma las estancias completas.")
if (sum(por_grupo_clase$estancias) != nrow(d))
  detener("El desglose por grupo y categoria no suma el conjunto ajustado.")

cat("\n=== CASCADA DE FILTROS ===\n")
print(cascada, row.names = FALSE)
cat("\n=== ESTANCIAS COMPLETAS POR CONJUNTO DE LA PARTICION ===\n")
print(por_grupo, row.names = FALSE)
cat("\n=== DESGLOSE DEL CONJUNTO AJUSTADO ===\n")
print(por_grupo_clase, row.names = FALSE)
cat("\nEstancias de entrenamiento y prueba en las categorias modeladas:",
    n_antes, "\n")
cat("De ellas, completas en las", length(vars17) + length(nuevas), ":",
    nrow(d), sprintf("(%.2f por ciento)\n", 100 * nrow(d) / n_antes))

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
  list(etiqueta = etiqueta, auc = a, prob = p, lambda = cv$lambda.min,
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

# ---------------------------------------------------------------------------
# Incertidumbre de la diferencia, por remuestreo emparejado
#
# Se remuestrean con reposicion las estancias de evaluacion, y en cada replica
# se recalculan las dos areas sobre las mismas estancias. Los modelos no se
# reajustan: la pregunta es cuanta incertidumbre tiene la diferencia observada
# entre dos predicciones dadas, no cuanta tendria el procedimiento completo si
# se repitiera de principio a fin.
# ---------------------------------------------------------------------------

cat("\nRemuestreando la diferencia,", REPLICAS, "replicas.\n")
y <- as.character(pru$clase)
n <- nrow(pru)
al <- (1 - CONFIANZA) / 2

# Diez mil replicas y no mil. El valor p mas pequeno que mil replicas puede
# expresar es de dos milesimas, y el umbral escalonado de Holm sobre cuatro
# comparaciones desciende hasta seis milesimas: con esa resolucion la decision
# sobre una categoria situada junto al umbral dependeria del sorteo.
difs <- matrix(NA_real_, nrow = REPLICAS, ncol = length(CLASES) + 1)
colnames(difs) <- c(CLASES, "promedio_minoritarias")
for (b in seq_len(REPLICAS)) {
  i <- sample.int(n, n, replace = TRUE)
  yb <- y[i]
  # El nombre importa: d ya designa el marco de casos completos, y
  # reutilizarlo aqui lo destruiria en silencio para todo lo que sigue.
  dif_b <- sapply(CLASES, function(k) {
    o <- as.integer(yb == k)
    auc(r29$prob[i, k], o) - auc(r17$prob[i, k], o)
  })
  difs[b, seq_along(CLASES)] <- dif_b
  difs[b, length(CLASES) + 1] <- mean(dif_b[MIN])
}
usables <- sum(complete.cases(difs))
cat("Replicas utilizables:", usables, "de", REPLICAS, "\n")
if (usables < 0.95 * REPLICAS)
  detener("Demasiadas replicas sin area estimable. El remuestreo no ",
          "sostiene un intervalo.")

ic <- do.call(rbind, lapply(colnames(difs), function(cl) {
  v <- difs[, cl]; v <- v[!is.na(v)]
  q <- quantile(v, c(al, 1 - al), names = FALSE)
  data.frame(cantidad = cl,
             diferencia = round(if (cl == "promedio_minoritarias")
               mean(r29$auc[MIN]) - mean(r17$auc[MIN]) else
               r29$auc[cl] - r17$auc[cl], 4),
             ic_inferior = round(q[1], 4), ic_superior = round(q[2], 4),
             excluye_cero = q[1] > 0 | q[2] < 0,
             row.names = NULL)
}))

# Valor bilateral por remuestreo, obtenido invirtiendo el intervalo de
# percentiles. La correccion de una unidad en numerador y denominador impide
# que el valor resulte nulo, que ninguna cantidad finita de replicas acredita.
valor_p <- function(v) {
  v <- v[!is.na(v)]
  b <- length(v)
  min(1, 2 * min((1 + sum(v <= 0)) / (b + 1), (1 + sum(v >= 0)) / (b + 1)))
}

fam <- ic$cantidad %in% CLASES
ic$p_bilateral <- NA_real_
for (i in which(fam)) ic$p_bilateral[i] <- valor_p(difs[, ic$cantidad[i]])
ic$p_holm <- NA_real_
ic$p_holm[fam] <- p.adjust(ic$p_bilateral[fam], "holm")
etq <- function(a) sub("[.]", "", format(a, nsmall = 3))
for (a in NIVELES) ic[[paste0("resiste_holm_", etq(a))]] <- ic$p_holm <= a
ic$p_bilateral <- signif(ic$p_bilateral, 4)
ic$p_holm <- signif(ic$p_holm, 4)

if (sum(fam) != length(CLASES))
  detener("La familia no reune las cuatro diferencias por categoria.")

cat("\n=== INTERVALO DE LA DIFERENCIA, REMUESTREO EMPAREJADO ===\n")
print(ic, row.names = FALSE)

cat("\n=== MULTIPLICIDAD SOBRE LAS", length(CLASES), "DIFERENCIAS ===\n")
resisten <- sapply(NIVELES, function(a)
  sum(ic[[paste0("resiste_holm_", etq(a))]][fam], na.rm = TRUE))
mult <- data.frame(nivel = NIVELES, correccion = "holm",
                   comparaciones = sum(fam),
                   resisten = resisten, row.names = NULL)
print(mult, row.names = FALSE)

sin_corregir <- sum(ic$excluye_cero[fam])
cat("\nCategorias cuyo intervalo excluye el cero sin corregir:",
    sin_corregir, "\n")
cat("Categorias que resisten la correccion en cualquiera de los niveles:",
    max(resisten), "\n")

fila <- ic[ic$cantidad == "promedio_minoritarias", ]
cat("\n=== LO QUE ESTO AUTORIZA A DECIR ===\n")
if (fila$excluye_cero) {
  cat("El intervalo de la diferencia en el promedio de minoritarias excluye\n")
  cat("el cero, de modo que la direccion se sostiene: la especificacion\n")
  cat(if (fila$diferencia < 0) "ampliada empeora.\n" else
      "ampliada mejora.\n")
} else {
  cat("El intervalo de la diferencia en el promedio de minoritarias\n")
  cat("contiene el cero. La direccion no se sostiene, y lo unico que cabe\n")
  cat("afirmar es que la especificacion ampliada no mejora.\n")
}

resumen <- data.frame(
  determinaciones_retenidas = length(vars17),
  determinaciones_anadidas = length(nuevas),
  columnas_retenidas = r17$columnas,
  columnas_ampliada = r29$columnas,
  no_nulos_retenidas = r17$no_nulos,
  no_nulos_ampliada = r29$no_nulos,
  estancias_ajuste = nrow(ent),
  estancias_prueba = nrow(pru),
  estancias_analizables = n_antes,
  estancias_completas = nrow(d),
  pct_de_las_analizables = round(100 * nrow(d) / n_antes, 2),
  promedio_minoritarias_retenidas = round(pr17, 4),
  promedio_minoritarias_ampliada = round(pr29, 4),
  diferencia_minoritarias = round(pr29 - pr17, 4),
  peor_cambio_por_clase = round(min(r29$auc[MIN] - r17$auc[MIN]), 4),
  replicas = REPLICAS,
  ic_inferior_minoritarias = fila$ic_inferior,
  ic_superior_minoritarias = fila$ic_superior,
  direccion_sostenida = fila$excluye_cero,
  clases_que_excluyen_el_cero = sin_corregir,
  clases_que_resisten_holm = max(resisten),
  row.names = NULL)

dir.create(OUT, recursive = TRUE, showWarnings = FALSE)
write.csv(tab, file.path(OUT, "comparacion_por_clase.csv"), row.names = FALSE)
write.csv(resumen, file.path(OUT, "comparacion_resumen.csv"),
          row.names = FALSE)
write.csv(anadidas[, c("itemid","etiqueta","panel","fluido","cobertura_pct")],
          file.path(OUT, "determinaciones_anadidas.csv"), row.names = FALSE)
write.csv(ic, file.path(OUT, "intervalo_diferencia.csv"), row.names = FALSE)
write.csv(cascada, file.path(OUT, "cascada_casos_completos.csv"),
          row.names = FALSE)
write.csv(por_grupo, file.path(OUT, "completos_por_grupo.csv"),
          row.names = FALSE)
write.csv(por_grupo_clase, file.path(OUT, "conjunto_por_grupo_y_clase.csv"),
          row.names = FALSE)
write.csv(mult, file.path(OUT, "multiplicidad_diferencias.csv"),
          row.names = FALSE)

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
  no_comparable_con_el_modelo_publicado = paste(
    "las areas de esta fase no se comparan con las del articulo. Difieren en",
    "que aqui no hay imputacion, por trabajarse sobre casos completos; no hay",
    "splines; no entra el indicador de solicitud de lactato; el conjunto de",
    "ajuste es mucho menor; y la penalizacion se valida dentro de cada rama.",
    "Las cinco diferencias son comunes a las dos ramas y no sesgan la",
    "comparacion, pero impiden cotejar cualquiera de estas cifras con las",
    "publicadas"),
  cascada = paste("la cifra de casos completos es el ultimo peldano de tres",
                  "restricciones encadenadas; la cascada entera y el reparto",
                  "por conjunto de la particion se depositan al lado, porque",
                  "aplicar proporciones sobre el total no la reproduce"),
  completitud = paste("evaluada sobre la matriz particionada, que ya paso por",
                      "los limites de plausibilidad; un valor anulado por",
                      "implausible cuenta aqui como ausente y contaba como",
                      "presente en el recuento de la fase trigesima primera,",
                      "que interroga la tabla de origen"),
  incertidumbre = paste("intervalo de la diferencia por remuestreo emparejado",
                        "del conjunto de evaluacion, con los modelos",
                        "ajustados una sola vez"),
  replicas = REPLICAS,
  confianza = CONFIANZA,
  multiplicidad = paste("Holm sobre las cuatro diferencias por categoria, en",
                        "los dos niveles que el trabajo emplea. El promedio",
                        "sobre las minoritarias queda fuera de la familia por",
                        "ser un unico resumen declarado y no una de cuatro",
                        "comparaciones exploradas"),
  niveles = NIVELES,
  alpha = ALFA_NET), auto_unbox = TRUE, pretty = TRUE, digits = 15),
  file.path(OUT, "manifiesto.json"))

cat("\nDepositado en", OUT, "\n")
