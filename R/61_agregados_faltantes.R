# Reconstruccion de los resultados agregados que sostienen dos decisiones del
# desarrollo y que hasta ahora solo constaban en la salida por pantalla. Una
# decision cuyo fundamento no figura en archivo alguno no admite verificacion
# por terceros, con independencia de que sea correcta.
#
# Ambos conjuntos se reconstruyen a partir de material ya depositado en
# disco, sin recalculo alguno. Las cifras deben coincidir con las reportadas;
# cualquier discrepancia indicaria alteracion del material de partida.

VITALES <- c("temperatura","frec_cardiaca","frec_respiratoria","saturacion")
LABS <- c("leucocitos","hemoglobina","plaquetas","rdw","creatinina","urea",
          "brecha_anion","sodio","potasio","cloro","bicarbonato","inr",
          "ttpa","ph","pco2","lactato","exceso_base")
TODAS <- c(LABS, VITALES)

# ---------------------------------------------------------------------------
# Concordancia entre metodos de determinacion de la presion arterial
# ---------------------------------------------------------------------------

f <- "data/derivados/concordancia_presion.csv"
if (!file.exists(f)) {
  cat("No consta el material de partida:", f, "\n")
  quit(status = 1)
}

d <- read.csv(f, stringsAsFactors = FALSE)
v <- read.csv("data/derivados/vitales_limpios.csv", stringsAsFactors = FALSE)

cat("=== CONCORDANCIA ENTRE METODOS DE PRESION ARTERIAL ===\n")
cat("Estancias con ambas determinaciones:", nrow(d), "\n\n")

tramos <- list("menos de 15 min" = c(0, 15), "de 15 a 60 min" = c(15, 60),
               "de 1 a 3 horas"  = c(60, 180), "mas de 3 horas" = c(180, Inf))

conc <- do.call(rbind, lapply(names(tramos), function(nom) {
  tr <- tramos[[nom]]
  s <- d[abs(d$desfase_min) >= tr[1] & abs(d$desfase_min) < tr[2], ]
  if (nrow(s) < 30)
    return(data.frame(tramo = nom, n = nrow(s), correlacion = NA,
                      dif_mediana = NA, dif_absoluta_mediana = NA,
                      row.names = NULL))
  data.frame(tramo = nom, n = nrow(s),
             correlacion = round(cor(s$pam_inv, s$pam_ni), 4),
             dif_mediana = round(median(s$pam_inv - s$pam_ni), 2),
             dif_absoluta_mediana = round(median(abs(s$pam_inv - s$pam_ni)), 2),
             row.names = NULL)
}))
print(conc, row.names = FALSE)

disp <- data.frame(
  serie = c("no invasiva en el subgrupo", "no invasiva en la cohorte",
            "invasiva en el subgrupo"),
  desviacion = round(c(sd(d$pam_ni), sd(v$presion_media, na.rm = TRUE),
                       sd(d$pam_inv)), 2),
  row.names = NULL)
cat("\nDispersion\n"); print(disp, row.names = FALSE)
cat("Cociente subgrupo sobre cohorte:",
    round(sd(d$pam_ni) / sd(v$presion_media, na.rm = TRUE), 3), "\n")

cat("\nCorrelacion global:", round(cor(d$pam_inv, d$pam_ni), 4), "\n")

dir.create("outputs/fase17", recursive = TRUE, showWarnings = FALSE)
write.csv(conc, "outputs/fase17/concordancia_presion.csv", row.names = FALSE)
write.csv(disp, "outputs/fase17/dispersion_presion.csv", row.names = FALSE)

# ---------------------------------------------------------------------------
# Diagnostico de la imputacion ampliada
# ---------------------------------------------------------------------------

imps <- readRDS("data/derivados/imputaciones_ampliadas.rds")
M <- length(imps)

lab <- read.csv("outputs/fase5/matriz_particionada.csv", stringsAsFactors = FALSE)
orig <- merge(lab, v[, c("stay_id", VITALES)], by = "stay_id", all.x = TRUE)

# La correspondencia entre cada valor imputado y la posicion que ocupaba el
# dato ausente se establece de forma explicita. Un desajuste de una sola fila
# invalidaria la totalidad del diagnostico.
ord <- match(imps[[1]]$stay_id, orig$stay_id)
stopifnot(!any(is.na(ord)), length(ord) == nrow(imps[[1]]))
orig <- orig[ord, ]

cat("\n=== DISPERSION ENTRE IMPUTACIONES ===\n")
dispimp <- do.call(rbind, lapply(VITALES, function(vv) {
  falt <- is.na(orig[[vv]])
  if (sum(falt) < 10) return(NULL)
  vals <- sapply(imps, function(x) x[[vv]][falt])
  sd_entre <- apply(vals, 1, sd)
  sd_total <- sd(orig[[vv]], na.rm = TRUE)
  data.frame(variable = vv, n_imputados = sum(falt),
             sd_entre_mediana = round(median(sd_entre), 3),
             sd_de_la_variable = round(sd_total, 3),
             cociente = round(median(sd_entre) / sd_total, 3),
             pct_sin_variacion = round(100 * mean(sd_entre < 0.001), 1),
             row.names = NULL)
}))
print(dispimp, row.names = FALSE)

# La fraccion de informacion faltante cuantifica que proporcion de la
# incertidumbre total de una estimacion procede de los datos ausentes. Se
# obtiene mediante las reglas de Rubin, que descomponen la varianza total en
# un componente interno a cada imputacion y otro entre ellas.
cat("\n=== FRACCION DE INFORMACION FALTANTE ===\n")
fmi <- do.call(rbind, lapply(TODAS, function(vv) {
  medias <- sapply(imps, function(x) mean(x[[vv]]))
  vars_int <- sapply(imps, function(x) var(x[[vv]]) / nrow(x))
  W <- mean(vars_int); B <- var(medias)
  Tot <- W + (1 + 1/M) * B
  data.frame(variable = vv,
             pct_ausente = round(100 * mean(is.na(orig[[vv]])), 2),
             varianza_interna = signif(W, 3),
             varianza_entre = signif(B, 3),
             fmi = round((1 + 1/M) * B / Tot, 4),
             row.names = NULL)
}))
print(fmi, row.names = FALSE)

cat("\nFraccion maxima en determinaciones bioquimicas:",
    round(max(fmi$fmi[fmi$variable %in% LABS]), 4), "\n")
cat("Fraccion maxima en constantes vitales:",
    round(max(fmi$fmi[fmi$variable %in% VITALES]), 4), "\n")

cat("\n=== CONSERVACION DE LA ESTRUCTURA DE ASOCIACION ===\n")
cc <- orig[complete.cases(orig[, TODAS]), TODAS]
c_obs <- cor(cc); c_imp <- cor(imps[[1]][, TODAS])
dif <- abs(c_obs - c_imp); diag(dif) <- 0
estr <- data.frame(
  indicador = c("casos completos empleados", "discrepancia maxima",
                "discrepancia mediana"),
  valor = c(nrow(cc), round(max(dif), 4),
            round(median(dif[upper.tri(dif)]), 4)),
  row.names = NULL)
print(estr, row.names = FALSE)

dir.create("outputs/fase20", recursive = TRUE, showWarnings = FALSE)
write.csv(dispimp, "outputs/fase20/dispersion_imputaciones.csv", row.names = FALSE)
write.csv(fmi, "outputs/fase20/fraccion_informacion_faltante.csv", row.names = FALSE)
write.csv(estr, "outputs/fase20/estructura_asociacion.csv", row.names = FALSE)

cat("\nArchivos agregados depositados.\n")
