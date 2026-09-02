# Deposito de la discriminacion atribuible al indicador de intubacion
# considerado aisladamente. La cifra sostiene uno de los hallazgos centrales
# del bloque de extension y se calculo en su momento sin depositarse, de modo
# que la documentacion no podia respaldarla.
#
# El calculo replica exactamente el original. En particular conserva la
# categoria de abstencion dentro del conjunto examinado, dado que asi se
# efectuo en su momento: excluirla alteraria el denominador y produciria una
# cifra distinta de la reportada.

m <- read.csv("data/derivados/glasgow.csv", stringsAsFactors = FALSE)
CL <- c("sin_crecimiento","urinario","respiratorio","sangre")

d <- m[m$grupo %in% c("entrenamiento","calibracion"), ]
cat("Estancias en entrenamiento y calibracion:", nrow(d), "\n")
cat("Categorias presentes:", length(unique(d$clase)), "\n")

auc <- function(p, y) {
  ok <- !is.na(p) & !is.na(y)
  p <- p[ok]; y <- y[ok]
  n1 <- sum(y == 1); n0 <- sum(y == 0)
  if (n1 < 10 || n0 < 10) return(NA)
  r <- rank(p)
  round((sum(r[y == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0), 4)
}

# La proporcion de intubados se calcula sobre la cohorte entera, conforme
# al procedimiento que la establecio, mientras que la discriminacion se
# obtiene sobre entrenamiento y calibracion.
res <- do.call(rbind, lapply(CL, function(k) {
  s <- m[m$clase == k, ]
  data.frame(clase = k,
             casos_cohorte = nrow(s),
             pct_intubado = round(100 * mean(s$intubado, na.rm = TRUE), 1),
             # La columna se llamaba casos_desarrollo, y desarrollo nombra
             # 18.054 estancias en todas las demas fases. Estas son 14.442.
             casos_entrenamiento_calibracion = sum(d$clase == k),
             auc_solo_tubo = auc(d$intubado, as.integer(d$clase == k)),
             row.names = NULL)
}))

cat("\n=== DISCRIMINACION DEL INDICADOR DE TUBO, AISLADO ===\n")
print(res, row.names = FALSE)

cat("\nDiscriminacion sobre la categoria respiratoria:",
    res$auc_solo_tubo[res$clase == "respiratorio"], "\n")

dir.create("outputs/fase17", recursive = TRUE, showWarnings = FALSE)
write.csv(res, "outputs/fase17/indicador_tubo.csv", row.names = FALSE)
cat("Depositado en outputs/fase17/indicador_tubo.csv\n")
