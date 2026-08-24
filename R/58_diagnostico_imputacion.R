library(mice)
set.seed(20260818)

VITALES <- c("temperatura","frec_cardiaca","frec_respiratoria","saturacion")
LABS <- c("leucocitos","hemoglobina","plaquetas","rdw","creatinina","urea",
          "brecha_anion","sodio","potasio","cloro","bicarbonato","inr",
          "ttpa","ph","pco2","lactato","exceso_base")
TODAS <- c(LABS, VITALES)
CL <- c("sin_crecimiento","urinario","respiratorio","sangre")

imps <- readRDS("data/derivados/imputaciones_ampliadas.rds")
M <- length(imps)

lab <- read.csv("outputs/fase5/matriz_particionada.csv", stringsAsFactors = FALSE)
vit <- read.csv("data/derivados/vitales_limpios.csv", stringsAsFactors = FALSE)
d <- merge(lab, vit[, c("stay_id", VITALES)], by = "stay_id", all.x = TRUE)

# Los diagnosticos comparan cada valor imputado con la posicion que ocupaba
# el dato ausente. Un desajuste de una sola fila invalidaria la totalidad de
# las comparaciones, de modo que la correspondencia se establece de forma
# explicita en lugar de confiar en que dos operaciones de cruce hayan
# producido el mismo orden.
ord <- match(imps[[1]]$stay_id, d$stay_id)
stopifnot(!any(is.na(ord)), length(ord) == nrow(imps[[1]]))
d <- d[ord, ]
cat("Filas alineadas:", nrow(d), "\n")

# Primer diagnostico. Si las veinte imputaciones de un mismo valor apenas
# difieren entre si, el procedimiento no estaria transmitiendo incertidumbre
# alguna, y la ventaja que se le atribuye frente a la sustitucion por un
# valor unico careceria de fundamento. La comparacion con la dispersion de
# la variable en el conjunto de la cohorte proporciona la escala.
cat("\n=== DISPERSION ENTRE IMPUTACIONES ===\n")
print(do.call(rbind, lapply(VITALES, function(v) {
  falt <- is.na(d[[v]])
  if (sum(falt) < 10) return(NULL)
  vals <- sapply(imps, function(x) x[[v]][falt])
  sd_entre <- apply(vals, 1, sd)
  sd_total <- sd(d[[v]], na.rm = TRUE)
  data.frame(variable = v,
             n_imputados = sum(falt),
             sd_entre_mediana = round(median(sd_entre), 3),
             sd_de_la_variable = round(sd_total, 3),
             cociente = round(median(sd_entre) / sd_total, 3),
             pct_sin_variacion = round(100 * mean(sd_entre < 0.001), 1),
             row.names = NULL)
})), row.names = FALSE)

# Segundo diagnostico. La fraccion de informacion faltante cuantifica que
# proporcion de la incertidumbre total de una estimacion procede de los datos
# ausentes. Se obtiene mediante las reglas de Rubin, que descomponen la
# varianza total en un componente interno a cada imputacion y otro entre
# ellas. Un valor nulo indicaria que el procedimiento no aporta nada sobre la
# sustitucion por un valor unico.
cat("\n=== FRACCION DE INFORMACION FALTANTE SOBRE LA MEDIA ===\n")
print(do.call(rbind, lapply(TODAS, function(v) {
  medias <- sapply(imps, function(x) mean(x[[v]]))
  vars_int <- sapply(imps, function(x) var(x[[v]]) / nrow(x))
  W <- mean(vars_int); B <- var(medias)
  Tot <- W + (1 + 1/M) * B
  data.frame(variable = v,
             pct_ausente = round(100 * mean(is.na(d[[v]])), 2),
             varianza_interna = signif(W, 3),
             varianza_entre = signif(B, 3),
             fmi = round((1 + 1/M) * B / Tot, 4),
             row.names = NULL)
})), row.names = FALSE)

# Tercer diagnostico. El algoritmo encadenado requiere un numero suficiente
# de iteraciones para estabilizarse. Se reejecuta con identica semilla y
# parametros, lo que reproduce exactamente las imputaciones empleadas, y se
# conserva el objeto completo para examinar la evolucion de las medias a lo
# largo del proceso.
cat("\nReejecutando el procedimiento para examinar la convergencia.\n")

d$unidad <- factor(d$unidad); d$sexo <- factor(d$sexo)
niveles_ent <- sort(unique(as.character(d$unidad[d$grupo == "entrenamiento"])))
d$unidad <- factor(ifelse(as.character(d$unidad) %in% niveles_ent,
                          as.character(d$unidad), niveles_ent[1]),
                   levels = niveles_ent)

imp <- mice(d[, c(TODAS, "edad", "sexo", "unidad")],
            m = M, maxit = 10, method = "pmm", seed = 20260818,
            printFlag = FALSE, ignore = d$grupo != "entrenamiento")
saveRDS(imp, "data/derivados/objeto_mice_ampliado.rds")

cat("\n=== ESTABILIDAD DE LAS CADENAS ===\n")
cm <- imp$chainMean
print(do.call(rbind, lapply(VITALES, function(v) {
  ini <- mean(cm[v, 1:3, ], na.rm = TRUE)
  fin <- mean(cm[v, 8:10, ], na.rm = TRUE)
  data.frame(variable = v,
             media_iteraciones_1_a_3 = round(ini, 3),
             media_iteraciones_8_a_10 = round(fin, 3),
             deriva_relativa_pct = round(100 * abs(fin - ini) / abs(ini), 3),
             row.names = NULL)
})), row.names = FALSE)

# Cuarto diagnostico. En imputacion multiple lo relevante no consiste en
# acertar valores aislados sino en conservar la estructura de asociacion
# entre variables, dado que es esa estructura la que el modelo posterior
# aprovecha. Se contrasta la matriz de correlaciones obtenida sobre casos
# completos con la que resulta de los datos imputados.
cat("\n=== CONSERVACION DE LA ESTRUCTURA DE ASOCIACION ===\n")
cc <- d[complete.cases(d[, TODAS]), TODAS]
c_obs <- cor(cc)
c_imp <- cor(imps[[1]][, TODAS])
dif <- abs(c_obs - c_imp)
diag(dif) <- 0
cat("Casos completos empleados:", nrow(cc), "\n")
cat("Discrepancia maxima entre matrices:", round(max(dif), 4), "\n")
cat("Discrepancia mediana:", round(median(dif[upper.tri(dif)]), 4), "\n")
pos <- which(dif == max(dif), arr.ind = TRUE)[1, ]
cat("Par de mayor discrepancia:", rownames(dif)[pos[1]], "y",
    colnames(dif)[pos[2]], "\n")
cat("  observada:", round(c_obs[pos[1], pos[2]], 4),
    " imputada:", round(c_imp[pos[1], pos[2]], 4), "\n")

# Quinto diagnostico. Si los valores imputados carecieran de la asociacion
# con el desenlace que presentan los observados, el procedimiento estaria
# diluyendo la senal en lugar de conservarla. La comparacion se realiza por
# separado en ambos subgrupos.
auc <- function(p, y) {
  ok <- !is.na(p) & !is.na(y)
  p <- p[ok]; y <- y[ok]
  n1 <- sum(y == 1); n0 <- sum(y == 0)
  if (n1 < 10 || n0 < 10) return(NA)
  r <- rank(p)
  round((sum(r[y == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0), 4)
}

cat("\n=== ASOCIACION CON EL DESENLACE, OBSERVADO FRENTE A IMPUTADO ===\n")
x1 <- imps[[1]]
dev <- x1$grupo %in% c("entrenamiento","calibracion") & x1$clase %in% CL
print(do.call(rbind, lapply(VITALES, function(v) {
  obs <- dev & !is.na(d[[v]])
  imv <- dev &  is.na(d[[v]])
  do.call(rbind, lapply(CL, function(k) {
    data.frame(variable = v, clase = k,
               n_observado = sum(obs), n_imputado = sum(imv),
               auc_observado = auc(x1[[v]][obs],
                                   as.integer(x1$clase[obs] == k)),
               auc_imputado = auc(x1[[v]][imv],
                                  as.integer(x1$clase[imv] == k)),
               row.names = NULL)
  }))
})), row.names = FALSE)

dir.create("outputs/fase20", recursive = TRUE, showWarnings = FALSE)
cat("\nObjeto de imputacion guardado para verificaciones ulteriores.\n")
