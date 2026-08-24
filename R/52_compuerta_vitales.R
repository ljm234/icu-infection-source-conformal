v   <- read.csv("data/derivados/vitales_limpios.csv", stringsAsFactors = FALSE)
lab <- read.csv("outputs/fase5/matriz_particionada.csv", stringsAsFactors = FALSE)

CL <- c("sin_crecimiento","urinario","respiratorio","sangre")
VITALES <- c("temperatura","frec_cardiaca","frec_respiratoria","saturacion")

# El examen se realiza sobre los conjuntos de desarrollo y se restringe a las
# categorias que el modelo contempla. La decision sobre que variables lo
# integran constituye una eleccion de diseno, de modo que no procede examinar
# el conjunto de prueba ni la unidad reservada para adoptarla.
d <- v[v$grupo %in% c("entrenamiento","calibracion"), ]
d <- d[d$clase %in% CL, ]
cat("Estancias examinadas:", nrow(d), "\n")

auc <- function(p, y) {
  ok <- !is.na(p) & !is.na(y)
  p <- p[ok]; y <- y[ok]
  n1 <- sum(y == 1); n0 <- sum(y == 0)
  if (n1 < 10 || n0 < 10) return(NA)
  r <- rank(p)
  round((sum(r[y == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0), 4)
}

# Discriminacion de cada constante por separado. Un valor inferior a la mitad
# indica relacion inversa y discrimina con la misma intensidad que su
# complemento, de modo que la distancia respecto de la mitad expresa la
# fuerza de la senal con independencia de su sentido.
cat("\n=== DISCRIMINACION INDIVIDUAL DE CADA CONSTANTE ===\n")
print(do.call(rbind, lapply(VITALES, function(vv) {
  fila <- data.frame(variable = vv, row.names = NULL)
  for (k in CL) {
    a <- auc(d[[vv]], as.integer(d$clase == k))
    fila[[k]] <- a
  }
  fila$fuerza_maxima <- round(max(abs(unlist(fila[CL]) - 0.5)), 4)
  fila
})), row.names = FALSE)

# La relacion entre temperatura e infeccion no es monotona: tanto la
# elevacion termica como la hipotermia acompanan a los cuadros graves. Un
# termino lineal buscaria una tendencia unica y no hallaria senal alguna
# donde la asociacion adopta forma de u. La particion en tramos revela la
# forma efectiva de la relacion antes de fijar la especificacion.
cat("\n=== TASA DE EVENTOS POR TRAMO DE CADA CONSTANTE ===\n")
for (vv in VITALES) {
  x <- d[[vv]]
  brk <- unique(quantile(x, seq(0, 1, 0.1), na.rm = TRUE))
  if (length(brk) < 4) { cat("\n---", vv, ": tramos insuficientes ---\n"); next }
  dec <- cut(x, breaks = brk, include.lowest = TRUE, labels = FALSE)
  cat("\n---", vv, "-", length(brk) - 1, "tramos ---\n")
  print(do.call(rbind, lapply(sort(unique(dec[!is.na(dec)])), function(k) {
    s <- d[!is.na(dec) & dec == k, ]
    data.frame(tramo = k, n = nrow(s),
               valor_mediano = round(median(s[[vv]], na.rm = TRUE), 1),
               pct_algun_foco = round(100 * mean(s$clase != "sin_crecimiento"), 2),
               pct_respiratorio = round(100 * mean(s$clase == "respiratorio"), 2),
               pct_urinario = round(100 * mean(s$clase == "urinario"), 2),
               row.names = NULL)
  })), row.names = FALSE)
}

# Una constante cuya mediana difiera de forma apreciable entre unidades
# incorporaria practica de medicion y no fisiologia, circunstancia ya
# constatada en el lactato, en la regla de agregacion, en el metodo de
# determinacion de la presion y en la escala de conciencia.
cat("\n=== MEDIANA DE CADA CONSTANTE POR UNIDAD ===\n")
print(do.call(rbind, lapply(sort(unique(d$unidad)), function(un) {
  s <- d[d$unidad == un, ]
  if (nrow(s) < 500) return(NULL)
  data.frame(unidad = substr(un, 1, 34), n = nrow(s),
             t(round(sapply(VITALES, function(vv)
               median(s[[vv]], na.rm = TRUE)), 1)),
             row.names = NULL)
})), row.names = FALSE)

# Una constante fuertemente asociada a un predictor ya presente en el modelo
# aportaria parametros sin informacion nueva. La comprobacion establece si
# las constantes describen un dominio distinto del que cubren las
# determinaciones bioquimicas.
labvars <- c("leucocitos","hemoglobina","plaquetas","rdw","creatinina","urea",
             "brecha_anion","sodio","potasio","cloro","bicarbonato","inr",
             "ttpa","ph","pco2","lactato","exceso_base","edad")

dl <- merge(d[, c("stay_id", VITALES)], lab[, c("stay_id", labvars)],
            by = "stay_id")
cm <- cor(dl[, VITALES], dl[, labvars], use = "pairwise.complete.obs")

cat("\n=== ASOCIACION MAXIMA CON LOS PREDICTORES YA PRESENTES ===\n")
print(do.call(rbind, lapply(seq_len(nrow(cm)), function(i) {
  j <- which.max(abs(cm[i, ]))
  data.frame(constante = rownames(cm)[i],
             predictor_mas_asociado = colnames(cm)[j],
             correlacion = round(cm[i, j], 4),
             row.names = NULL)
})), row.names = FALSE)

cat("\nCorrelacion maxima absoluta observada:",
    round(max(abs(cm), na.rm = TRUE), 4), "\n")

cat("\n=== CORRELACION ENTRE LAS PROPIAS CONSTANTES ===\n")
print(round(cor(d[, VITALES], use = "pairwise.complete.obs"), 3))

dir.create("outputs/fase17", recursive = TRUE, showWarnings = FALSE)
write.csv(round(cm, 4), "outputs/fase17/correlacion_vitales_labs.csv")
cat("\nGuardado en outputs/fase17/correlacion_vitales_labs.csv\n")
