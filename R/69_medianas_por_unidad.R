# Deposito de las medianas de las constantes vitales por unidad. La
# afirmacion de que su valor no depende de la sede se apoyaba en un calculo
# que solo se imprimia por pantalla, de modo que nadie podia reproducirlo.
#
# El calculo original se restringia ademas a los conjuntos de desarrollo, con
# lo que excluia la unidad reservada, precisamente aquella cuya practica de
# medicion resulta mas extrema. Aqui se incluyen todas.

v <- read.csv("data/derivados/vitales_limpios.csv", stringsAsFactors = FALSE)
VIT <- c("temperatura","frec_cardiaca","frec_respiratoria","saturacion")

v <- v[!is.na(v$unidad), ]
tab <- table(v$unidad)
grandes <- names(tab)[tab >= 500]
v <- v[v$unidad %in% grandes, ]

cat("Unidades con quinientas estancias o mas:", length(grandes), "\n")
cat("Estancias examinadas:", nrow(v), "\n\n")

agg <- aggregate(v[, VIT], by = list(unidad = v$unidad),
                 FUN = function(x) median(x, na.rm = TRUE))
agg$n <- as.numeric(tab[agg$unidad])

cob <- aggregate(v[, VIT], by = list(unidad = v$unidad),
                 FUN = function(x) round(100 * mean(!is.na(x)), 1))

cat("=== MEDIANA POR UNIDAD ===\n")
print(agg[, c("unidad","n", VIT)], row.names = FALSE)

cat("\n=== DISPONIBILIDAD POR UNIDAD, PORCENTAJE ===\n")
print(cob, row.names = FALSE)

# El recorrido de la mediana entre unidades cuantifica la dependencia de sede
# del valor registrado; el de la disponibilidad, la del hecho de registrarlo.
rng_med <- apply(agg[, VIT], 2, function(x) max(x) - min(x))
rng_cob <- apply(cob[, VIT], 2, function(x) max(x) - min(x))

res <- data.frame(variable = VIT,
                  mediana_minima = apply(agg[, VIT], 2, min),
                  mediana_maxima = apply(agg[, VIT], 2, max),
                  recorrido_mediana = round(rng_med, 2),
                  cobertura_minima = apply(cob[, VIT], 2, min),
                  cobertura_maxima = apply(cob[, VIT], 2, max),
                  recorrido_cobertura = round(rng_cob, 1),
                  row.names = NULL)

cat("\n=== RECORRIDO ENTRE UNIDADES ===\n")
print(res, row.names = FALSE)

dir.create("outputs/fase17", recursive = TRUE, showWarnings = FALSE)
write.csv(agg, "outputs/fase17/medianas_por_unidad.csv", row.names = FALSE)
write.csv(res, "outputs/fase17/recorrido_por_unidad.csv", row.names = FALSE)
cat("\nDepositado en outputs/fase17\n")
