# Composicion de la documentacion publica. Ninguna cifra se transcribe: todas
# proceden de un archivo de resultados versionado.
#
# La garantia es estructural. Toda linea atraviesa una de dos puertas. La
# primera admite unicamente prosa y se detiene ante cualquier digito. La
# segunda admite un patron de formato acompanado de valores leidos de
# archivos, y se detiene si el patron contiene digitos ajenos a sus codigos de
# sustitucion o si el resultado no constituye exactamente una linea.
#
# Las guardias no alcanzan a la prosa cualitativa. Una auditoria externa
# hallo diez afirmaciones que los archivos contradecian o no sostenian, sin
# una sola discrepancia numerica en tres rondas. De ahi que las afirmaciones
# interpretativas se hayan suprimido en lugar de reescribirse: un documento no
# las necesita, y cada una constituye un pasivo.

# La relacion admite unicamente identificadores literales: rutas y versiones
# que el texto nombra. Se retira cada uno antes de buscar digitos, de modo que
# una cifra escrita junto a ellos se sigue detectando.
EXENTAS <- c("MIMIC-IV version 3.1", "R 4.6.1", "seed is 20260818",
             "SHA-256", "R/59_verificar_cifras.R",
             "R/64_auditoria_publicacion.R", "R/65_generar_readme.R",
             "R/67_diagnostico_dependencias.R", "R/68_traduccion_yachay.R",
             "R/19_matriz.R", "R/21_perfil_unidades.R", "R/22_particion.R",
             "R/32_comparar_lambda.R",
             "R/36_sellado.R", "R/38_recalibracion.R",
             "R/51_circularidad_glasgow.R",
             "outputs/fase5/distancia_unidades.csv",
             "outputs/fase5/clases_por_unidad.csv",
             "outputs/fase22/decision_lambda.csv",
             "outputs/fase25/divergencia_etiquetado.csv",
             "outputs/fase26/intervalos_cobertura.csv",
             "outputs/fase28/cobertura_vitales_por_etapa.csv",
             "outputs/fase30/determinaciones_candidatas.csv",
             "outputs/fase35/curva_calibracion.csv",
             "outputs/fase35/pendiente_calibracion.csv",
             "~/mimic-data/physionet.org/files/mimiciv/3.1",
             "outputs/fase20/TRACEABILITY.md",
             "outputs/fase20/REPRODUCIBILITY.md",
             "outputs/fase21/PROTOCOLO.md",
             "R/82_decisiones_vivas.R")

despojar <- function(s) {
  for (e in EXENTAS) s <- gsub(e, "", s, fixed = TRUE)
  s
}

tiene_digito <- function(s) grepl("[0-9]", despojar(s))

patron_con_digito <- function(fmt)
  grepl("[0-9]", despojar(gsub("%[-+0-9.]*[sdfe]", "", fmt)))

compone_una_linea <- function(fmt, ...) length(sprintf(fmt, ...)) == 1

prosa <- function(...) {
  x <- c(...)
  for (s in x)
    if (tiene_digito(s)) {
      cat("Cifra literal en la prosa:\n  ", s, "\n"); quit(status = 1)
    }
  x
}

cifra <- function(fmt, ...) {
  if (!grepl("%[-+0-9.]*[sdfe]", fmt)) {
    cat("Patron sin codigo de sustitucion. Corresponde a prosa:\n  ", fmt, "\n")
    quit(status = 1)
  }
  if (patron_con_digito(fmt)) {
    cat("Cifra literal en un patron:\n  ", fmt, "\n"); quit(status = 1)
  }
  if (!compone_una_linea(fmt, ...)) {
    cat("La composicion no produjo una linea:\n  ", fmt, "\n"); quit(status = 1)
  }
  sprintf(fmt, ...)
}

leer <- function(ruta) {
  if (!file.exists(ruta)) {
    cat("Fuente ausente:", ruta, "\n"); quit(status = 1)
  }
  read.csv(ruta, stringsAsFactors = FALSE)
}

val <- function(tabla, columna, condicion) {
  x <- tabla[condicion, columna]
  if (length(x) != 1 || is.na(x)) {
    cat("Extraccion fallida en la columna", columna, "\n"); quit(status = 1)
  }
  x
}

flujo <- leer("outputs/fase2/flujo.csv")
clases<- leer("outputs/fase2/clases.csv")
val6  <- leer("outputs/fase6/validacion_enmascaramiento.csv")
cob8  <- leer("outputs/fase8/cobertura.csv")
alfa  <- leer("outputs/fase9/curva_alfa.csv")
louo  <- leer("outputs/fase10/cobertura_louo.csv")
louoc <- leer("outputs/fase10/cobertura_clase_louo.csv")
sell  <- leer("outputs/fase11/cobertura_sellado.csv")
recal <- leer("outputs/fase12/recalibracion.csv")
refs  <- leer("outputs/fase13/referencias.csv")
lac   <- leer("outputs/fase15/sensibilidad_lactato.csv")
agr   <- leer("outputs/fase15/comparacion_agregacion.csv")
dec   <- leer("outputs/fase16/curvas_decision.csv")
gbm   <- leer("outputs/fase16/gbm_comparacion.csv")
cove  <- leer("outputs/fase28/cobertura_vitales_por_etapa.csv")
coh   <- leer("outputs/fase29/cohorte_analizada.csv")
via   <- leer("outputs/fase30/viabilidad_comparacion.csv")
comp  <- leer("outputs/fase31/casos_completos.csv")
cmp29 <- leer("outputs/fase32/comparacion_resumen.csv")
icd   <- leer("outputs/fase32/intervalo_diferencia.csv")
mlt   <- leer("outputs/fase32/multiplicidad_diferencias.csv")
casc  <- leer("outputs/fase32/cascada_casos_completos.csv")
cgrp  <- leer("outputs/fase32/completos_por_grupo.csv")
cmpl  <- leer("outputs/fase36/completitud_por_conjunto.csv")
rec   <- leer("outputs/fase17/recorrido_por_unidad.csv")
conc  <- leer("outputs/fase17/concordancia_presion.csv")
cpres <- leer("outputs/fase17/cobertura_presion.csv")
gcs   <- leer("outputs/fase17/circularidad_glasgow.csv")
tubo  <- leer("outputs/fase17/indicador_tubo.csv")
amp   <- leer("outputs/fase18/comparacion_ampliado.csv")
spl   <- leer("outputs/fase18/ganancia_splines.csv")
louoa <- leer("outputs/fase19/cobertura_louo_ampliado.csv")
fmi   <- leer("outputs/fase20/fraccion_informacion_faltante.csv")
empc  <- leer("outputs/fase20/empates_constantes.csv")
empl  <- leer("outputs/fase20/empates_laboratorio.csv")
divg  <- leer("outputs/fase20/divergencia_extraccion.csv")
det   <- leer("outputs/fase20/determinismo_extraccion.csv")
cobx  <- leer("outputs/fase20/cobertura_extraccion.csv")
lamb  <- leer("outputs/fase22/decision_lambda.csv")
calsl <- leer("outputs/fase23/calibracion_sellado.csv")
calpr <- leer("outputs/fase34/calibracion_prueba.csv")
curv  <- leer("outputs/fase35/curva_calibracion.csv")
rngp  <- leer("outputs/fase35/rango_probabilidad.csv")
sepa  <- leer("outputs/fase35/separacion_argmax.csv")
pcal  <- leer("outputs/fase35/pendiente_calibracion.csv")
iv    <- leer("outputs/fase26/intervalos_cobertura.csv")
mult  <- leer("outputs/fase26/recuentos_multiplicidad.csv")
causa <- leer("outputs/fase26/causas_fallo.csv")

# La cota sobre la regla del maximo se enuncia como imposibilidad, y esa
# forma solo es licita mientras la probabilidad minima de la mayoritaria
# supere la mitad en los dos conjuntos evaluados. Si dejara de superarla, la
# frase pasaria a ser falsa, de modo que procede detenerse antes de
# escribirla y no despues de publicarla.
if (!all(sepa$sostiene_la_imposibilidad)) {
  cat("La cota sobre la regla del maximo no se sostiene en algun conjunto.\n")
  cat("La seccion que la enuncia no puede componerse.\n")
  quit(status = 1)
}
sp_pru <- sepa[sepa$conjunto == "prueba", ]
sp_sel <- sepa[sepa$conjunto == "unidad reservada", ]
i_ancho <- which.max(pcal$pendiente_ic_superior - pcal$pendiente_ic_inferior)
ur_pru <- rngp[rngp$conjunto == "prueba" &
               rngp$estrato == "de la categoria" &
               rngp$clase == "urinario", ]

MIN <- c("urinario","respiratorio","sangre")
CLC <- c("sin_crecimiento","urinario","respiratorio","sangre")
VIT <- c("temperatura","frec_cardiaca","frec_respiratoria","saturacion")
NOMINAL <- cob8$nominal[1]

# Transportabilidad. La garantia declarada es la condicional por clase; la
# marginal se reporta por separado, dado que una unidad puede alcanzar el
# nivel nominal en el promedio de sus casos sin alcanzarlo en alguna clase.
bajo_marg  <- sum(louo$cobertura < NOMINAL)
falla_cond <- sum(apply(louoc[, CLC], 1, function(r) any(r < NOMINAL, na.rm = TRUE)))
n_sedes    <- nrow(louo)

# Las clases que fallan no son las mismas en todas las sedes, pero tampoco
# difieren una a una: dos pares de sedes comparten el mismo conjunto. Se
# cuentan los conjuntos distintos en lugar de calificar el patron con una
# palabra que el archivo no sostiene.
# Evaluacion de la garantia condicional. Se abandona la comparacion de la
# estimacion puntual contra el nominal, que el conjunto de prueba nunca uso, y
# se adopta en las dos secciones el criterio del intervalo. Las celdas se
# corrigen ademas por multiplicidad, y el contraste reconoce que el umbral
# conforme se reestima en cada pliegue.
fam <- iv$seccion == "dejando una sede fuera"
sel <- iv$seccion == "unidad reservada"
ADOPT <- "resiste_beta_holm_0025"

n_celdas  <- sum(fam)
sedes_iv  <- length(unique(iv$sede[fam]))
bajo_iv   <- fam & iv$beta_por_debajo
resiste   <- fam & iv[[ADOPT]]
n_bajo    <- sum(bajo_iv)
n_resiste <- sum(resiste)
sedes_dem <- length(unique(iv$sede[resiste]))

nivel_conv <- max(mult$nivel)
nivel_adop <- min(mult$nivel)
falsas <- n_celdas * nivel_conv

# Las dos sedes sin fallo demostrable. Se ordenan por cobertura para que la
# frase no dependa del orden en que el archivo las deposito.
sin_dem <- iv[bajo_iv & !iv[[ADOPT]], ]
sin_dem <- sin_dem[order(-sin_dem$cobertura), ]

# Recuento bajo el contraste que trata el umbral como conocido, para declarar
# cuanto se debe a esa suposicion. Bajo Holm el recuento es el mismo en los
# dos niveles; bajo Bonferroni no lo es, y la frase que sigue solo afirma lo
# que ambos niveles sostienen.
n_binom_niveles <- mult$celdas_resisten[mult$correccion == "holm"]
if (length(unique(n_binom_niveles)) != 1) {
  cat("El recuento bajo Holm varia entre los dos niveles.\n")
  quit(status = 1)
}
n_binom <- n_binom_niveles[1]

# Cuanto se estrechan los intervalos condicionados al umbral, frente a los que
# reconocen la calibracion.
anch <- function(f) (iv$ic_superior[f] - iv$ic_inferior[f]) /
                    (iv$ic_beta_superior[f] - iv$ic_beta_inferior[f])
estrecha_min <- 100 * (1 - max(anch(bajo_iv)))
estrecha_max <- 100 * (1 - min(anch(bajo_iv)))

# Las sedes se nombran por su sigla. El nombre completo no cabe en la tabla y
# truncarlo parte palabras por la mitad.
iv$sigla <- sub(".*\\(([^)]+)\\).*", "\\1", iv$sede)

sll <- iv[sel, ]
sll <- sll[match(CLC, sll$clase), ]
mayor_sll <- sll$clase[which.max(sll$n)]
cal_may <- val(calsl, "probabilidad_media", calsl$clase == mayor_sll)
frec_may <- val(calsl, "frecuencia_observada", calsl$clase == mayor_sll)

# Causas del fallo. La frase que sigue supone dos causas distintas y una sola
# sede sin fallo demostrable por cada una; si el archivo dejara de tener esa
# forma, la frase describiria algo que ya no ocurre y procede detenerse.
if (nrow(causa) != 2 || nrow(sin_dem) != causa$sin_fallo_demostrable[1]) {
  cat("La estructura de las causas no admite la redaccion prevista.\n")
  quit(status = 1)
}
n_sin_dem <- causa$sin_fallo_demostrable[1]
mb <- mult[grepl("^beta_", mult$correccion), ]
mb$correccion <- sub("^beta_", "", mb$correccion)
bajo_sell  <- sum(sell$cobertura < NOMINAL)

# Comparacion de imputacion. Los recuentos se derivan del archivo en lugar de
# resumirse con un juicio: la relacion entre metodos es estrecha y calificarla
# de derrota excedia lo que los datos muestran.
m6 <- val6[val6$metodo == "mice con unidad", ]
d6 <- val6[val6$metodo == "mediana", ]
o6 <- match(m6$variable, d6$variable)
gana_med  <- sum(m6$error_absoluto_mediano > d6$error_absoluto_mediano[o6])
gana_mice <- sum(m6$error_absoluto_mediano < d6$error_absoluto_mediano[o6])

d10 <- alfa$alfa == 0.10
cm  <- alfa$conclusiones_minoritarias > 0
i_pmax <- which.max(alfa$aciertos_minoritarios[cm] /
                    alfa$conclusiones_minoritarias[cm])
pmin_alfa <- (alfa$aciertos_minoritarios[cm] /
              alfa$conclusiones_minoritarias[cm])[i_pmax]
n_pmax <- alfa$conclusiones_minoritarias[cm][i_pmax]

sdec <- dec[dec$objetivo == "cualquier_foco", ]
gan_dec <- max(sdec$bn_modelo - pmax(sdec$bn_tratar_todos, 0))
nmin <- min(recal$n_local)

L <- character(0)
add <- function(...) L <<- c(L, ...)

add(prosa(
"# Conformal prediction of infection source at ICU admission",
"",
"A methodological testbed built on MIMIC-IV for multiclass classification of",
"culture site in adults with suspected infection, using class-conditional",
"conformal prediction sets and an abstention mechanism.",
"",
"Principal Investigator: Luis Jordan Montenegro-Calla",
""))

add(cifra("Generated on %s by `R/65_generar_readme.R`.",
          format(Sys.Date(), "%Y-%m-%d")))

add(prosa(
"Every numeric figure below is read from a versioned results file. The",
"generator rejects prose lines containing a digit, format patterns carrying",
"digits outside their substitution codes, and any line that fails to compose.",
"",
"## Question",
"",
"An adult is admitted to intensive care with suspected infection, established",
"by a culture drawn alongside systemic antibiotics. Using only information",
"available in the first six hours, can the source of infection be identified?",
"The system returns a set of compatible categories rather than a single",
"label, and abstains when the available information does not permit",
"discrimination.",
"",
"## Data",
"",
"MIMIC-IV version 3.1, obtained from PhysioNet under individual credentialing.",
"The source files are not redistributed and form no part of this repository.",
"Reproducing the analysis requires separate credentialing and approved access.",
"",
"No patient-level derived data are versioned. `R/64_auditoria_publicacion.R`",
"inspects the header of every tracked CSV, lists the tracked binary objects",
"with their sizes, and checks that none of the protected paths appears in the",
"commit history.",
"",
"## Cohort",
""))

for (i in seq_len(nrow(flujo)))
  add(cifra("    %-46s %12s", flujo$paso[i],
            format(flujo$n[i], big.mark = ",")))

add(prosa(
"",
"The step labels are Spanish and read, in order: unique stays, adults, stays",
"with a culture drawn, suspected infection and the final cohort.",
""))

add(prosa(
"Of the stays with suspected infection, those in units contributing at"))
add(cifra("least %d make up the analysed cohort: %s stays across %d units.",
          as.integer(coh$umbral),
          format(coh$estancias_retenidas, big.mark = ","),
          as.integer(coh$unidades_retenidas)))
add(cifra("The model was developed on the %s stays of %d of them and",
          format(coh$estancias_de_desarrollo, big.mark = ","),
          as.integer(coh$unidades_de_desarrollo)))
add(cifra("evaluated once on the %s of the unit held back.",
          format(coh$estancias_reservadas, big.mark = ",")))

add(prosa(
"",
"## Categories",
""))

for (i in seq_len(nrow(clases)))
  add(cifra("    %-18s %9s   %5.1f percent", clases$clase[i],
            format(clases$n[i], big.mark = ","), clases$pct[i]))

add(prosa(
"",
"The category names are Spanish: sin_crecimiento is no growth on culture,",
"otro_sitio another site, urinario urinary, respiratorio respiratory, sangre",
"bloodstream, herida wound and intraabdominal intra-abdominal.",
"",
"The abstention category groups wound, intra-abdominal, cerebrospinal fluid",
"and other sites whose frequency does not support conditional calibration. It",
"is not modelled.",
"",
"## Design decisions",
"",
"**Time origin.** Admission to the first intensive care stay, adopted after",
"comparing against hospital admission.",
"",
"**Predictor window.** Six hours. The temporal filter uses the time a result",
"was recorded rather than the time the specimen was drawn, since a result",
"recorded afterwards was not available at the moment of decision.",
"",
"**Laboratory variables.** Seventeen, drawn from the candidates present in"))
add(cifra("at least %s stays within the window, of which there are %d. The",
          format(via$umbral_de_estancias, big.mark = ","),
          as.integer(via$candidatas)))
add(prosa(
"rule that narrowed the candidates to the seventeen is not recorded, and",
"nothing in the deposit reproduces it: neither the sample type, nor the",
"panel the source dictionary assigns, nor the coverage separates the",
"retained from the rest. The candidate with the highest coverage of all is",
"among the discarded. `outputs/fase30/determinaciones_candidatas.csv`",
"describes every one of them.",
""))

add(prosa(
"A comparison on stays complete in all the candidates cannot be made."))
add(cifra("%d of the %s stays have all of them within the window, and the most",
          as.integer(comp$estancias_completas[comp$conjunto == "candidatas"]),
          format(comp$denominador[1], big.mark = ",")))
add(cifra("any stay reaches is %d. What can be compared is the seventeen",
          as.integer(comp$maximo_presentes[comp$conjunto == "candidatas"])))
add(cifra("against those plus the %d discarded whose coverage exceeds the",
          as.integer(cmp29$determinaciones_anadidas)))
add(prosa("least frequent retained one."))
add(cifra("Of that cohort, %s stays are complete in those %d; %s of them fall",
          format(casc$estancias[2], big.mark = ","),
          as.integer(cmp29$determinaciones_retenidas +
                     cmp29$determinaciones_anadidas),
          format(casc$estancias[3], big.mark = ",")))
add(cifra("in the training or test partition, and %s of those carry one of the",
          format(casc$estancias[4], big.mark = ",")))
add(prosa(
"four modelled classes. That last set carries the comparison, and on it the",
"wider specification does not improve on the narrower: the"))
add(cifra("mean area across minority classes moves by %.4f, with a paired",
          cmp29$diferencia_minoritarias))
add(cifra("bootstrap interval of %.4f to %.4f that contains zero, so the",
          cmp29$ic_inferior_minoritarias, cmp29$ic_superior_minoritarias))

add(prosa("direction is not established."))
add(cifra("%d of the %d classes does show an interval that excludes zero. The",
          as.integer(cmp29$clases_que_excluyen_el_cero),
          as.integer(nrow(icd) - 1)))
add(prosa(
"four are corrected jointly by Holm's method, as the cells of the"))
add(cifra("transportability section are, and neither at %s nor at %s does any",
          format(mlt$nivel[1]), format(mlt$nivel[2])))
add(cifra("of them survive: %d resist. Reporting the one that excludes",
          as.integer(cmp29$clases_que_resisten_holm)))
add(prosa(
"zero without saying that it does not survive correction would apply one",
"standard here and another there.",
"",
"Three things bound that comparison. Stays complete in that many",
"determinations are not a random sample: they are the more heavily monitored",
"ones, and monitoring intensity tracks both severity and unit, so the answer",
"holds among patients with complete laboratory work rather than in the",
"cohort. How far completeness tracks the unit is measurable in the sealed"))
add(cifra("one, which this comparison leaves out anyway: %.2f percent of its",
          val(cgrp, "pct_del_grupo", cgrp$grupo == "sellado")))
add(cifra("stays are complete in these determinations, against %.2f percent in",
          val(cgrp, "pct_del_grupo", cgrp$grupo == "entrenamiento")))
add(cifra("the training partition. Over the %d the published model uses the",
          as.integer(cmpl[["determinaciones"]][1])))
add(cifra("ordering reverses, at %.2f against %.2f percent, so this figure",
          val(cmpl, "pct_completas", cmpl$conjunto == "unidad reservada"),
          val(cmpl, "pct_completas", cmpl$conjunto == "desarrollo")))
add(prosa(
"belongs to the determinations compared here and not to the model.",
"Both specifications were fitted linearly, without splines, without",
"imputation and without the lactate ordering indicator, so that the variable",
"set is the only thing that differs between them; neither figure is",
"comparable with the areas reported elsewhere in this document. And the",
"interval is for the difference, which is paired on the same stays and",
"therefore tighter than the interval of either area alone.",
"",
"**Aggregation.** First recorded value per variable. The first value is the",
"only one computable without knowing how many measurements follow, which",
"matters because monitoring intensity differs across units."))

add(cifra("Correlation with worst-value aggregation never falls below %.4f",
          min(agr$correlacion)))

add(prosa(
"across the seventeen laboratory variables; the comparison does not cover",
"vital signs.",
"",
"**Imputation.** Chained equations with predictive mean matching, twenty",
"datasets, ten iterations, estimated on the training set alone. The outcome",
"is excluded, since the system must operate on patients whose class is",
"unknown.",
"",
"**Model.** Penalized multinomial logistic regression. The elastic-net mixing",
"parameter was selected by cross-validation over a grid and settled on the",
"lasso limit, so the penalty actually fitted is lasso rather than a mixture.",
"Folds assigned by patient. Restricted cubic splines where the cross-validated",
"gain exceeded a threshold derived from a permutation null.",
"",
"The penalty is the cross-validated minimum and not the one-standard-error",
"rule. The two were compared under a stated criterion: adopt the minimum if",
"the mean area across minority classes improves by more than a declared",
"margin and no minority class loses more than that same margin. The",
"criterion is written into `R/32_comparar_lambda.R`, which entered the",
"repository in the same commit as its result, so no artefact establishes",
"that it preceded the comparison. The minimum"))

add(cifra("won, gaining %.4f against a margin of %.2f.",
          val(lamb, "mejora_original", TRUE),
          val(lamb, "umbral_mejora", TRUE)))

add(prosa(
"",
"That comparison was made on the test set, which is the set that later",
"reports discrimination and coverage. It is therefore a design decision",
"taken on the evaluation data, and it was checked again without it: the",
"training set alone was split by patient, the whole comparison repeated",
"inside it, and the same criterion applied."))

add(cifra("On %d stays held out from the same training set, against the",
          as.integer(val(lamb, "pacientes_validacion", TRUE))))
add(cifra("%d used for fitting, the answer is the same, with a gain of",
          as.integer(val(lamb, "pacientes_ajuste", TRUE))))
add(cifra("%.4f. The reduced fit favours the",
          val(lamb, "mejora_interna", TRUE)))

add(prosa(
"one-standard-error rule, since a smaller sample calls for a heavier",
"penalty, so the minimum wins there against the odds. The check is in",
"`outputs/fase22/decision_lambda.csv`.",
"",
"**Partition.** The cardiovascular unit was sealed in full. The choice was a",
"judgement informed by the unit profiles that `R/21_perfil_unidades.R`",
"computes, of which two are versioned: `outputs/fase5/distancia_unidades.csv`",
"and `outputs/fase5/clases_por_unidad.csv`. It was not the application of a",
"rule. `R/22_particion.R` names the unit as a constant and computes no",
"selection criterion.",
"",
"The sealed unit is touched three times, and it is worth listing them.",
"`R/36_sellado.R` evaluates the primary model there once, with the thresholds",
"of the original calibration set and with that model already frozen.",
"`R/38_recalibracion.R` then reuses those same predictions for a"))

add(cifra("recalibration exercise, resampling the unit at each of %d local",
          as.integer(nrow(recal))))

add(prosa(
"sizes; it recomputes thresholds only and never refits the model. And",
"`R/51_circularidad_glasgow.R` describes the distribution of the",
"consciousness scale in that unit, to decide whether the variable could",
"enter the extension at all. The first is an evaluation, the second an",
"exercise on the same predictions, the third a descriptive check on a",
"candidate variable. The extended model was never evaluated there.",
"",
"## Principal results",
"",
"### Discrimination and conformal coverage",
"",
"    class              AUC    coverage",
""))

for (k in CLC)
  add(cifra("    %-16s %7.4f    %7.4f", k,
            val(amp, "auc_original", amp$clase == k),
            val(cob8, "cobertura", cob8$clase == k)))

add(prosa(""))
add(cifra("Coverage ranges from %.4f to %.4f against a nominal level of %.2f,",
          min(cob8$cobertura), max(cob8$cobertura), NOMINAL))

add(prosa(
"with confidence intervals containing the nominal value in all four",
"categories.",
"",
"Those intervals are conditional on the conformal threshold, which is itself",
"estimated from a finite calibration set. Incorporating that uncertainty",
"would widen them, as it does in the transportability section below, where",
"the thresholds are re-estimated within each fold. It cannot change the",
"conclusion here: an interval that already contains a point still contains",
"it once widened.",
"",
"### Calibration",
"",
"    class                 n   observed   predicted",
""))

for (i in seq_len(nrow(calpr)))
  add(cifra("    %-16s %5d     %.4f      %.4f", calpr$clase[i],
            as.integer(calpr$n[i]), calpr$frecuencia_observada[i],
            calpr$probabilidad_media[i]))

add(prosa(""))
add(cifra("Mean predicted probability tracks observed frequency to within %.4f",
          max(abs(calpr$diferencia))))

add(prosa(
"in every class on the test set. That set comes from the same random",
"partition as the training data, so agreement there is what a correctly",
"fitted model should produce and is not evidence that it would hold",
"elsewhere.",
""))

add(cifra("Within each class the test set was split into %d equal bins of",
          as.integer(max(curv$grupo))))
add(prosa(
"predicted probability, and observed frequency tracks predicted probability",
"across the range and not only in the mean. The calibration slopes run"))
add(cifra("from %.4f to %.4f and the calibration-in-the-large terms from",
          min(pcal$pendiente), max(pcal$pendiente)))
add(cifra("%.4f to %.4f, every interval covering the value that means no",
          min(pcal$calibracion_en_conjunto),
          max(pcal$calibracion_en_conjunto)))
add(prosa("compression and no shift.", ""))

add(prosa(
"That is absence of evidence of miscalibration, not evidence of good",
"calibration. The intervals are wide at these class sizes: the widest"))
add(cifra("slope runs from %.4f to %.4f, so a moderate compression would not",
          pcal$pendiente_ic_inferior[i_ancho],
          pcal$pendiente_ic_superior[i_ancho]))

add(prosa(
"be detected. The bins and the coefficients are in",
"`outputs/fase35/curva_calibracion.csv` and",
"`outputs/fase35/pendiente_calibracion.csv`.",
"",
"### The argmax rule cannot name a source",
"",
"The four class probabilities sum to one in every stay, so if the majority",
"class probability never falls below a value, no other class can reach one",
"minus that value. On the test set the majority class never falls below"))

add(cifra("%.4f, which caps every minority class at %.4f. On the sealed unit",
          sp_pru$prob_minima_mayoritaria, sp_pru$cota_de_cada_minoritaria))
add(cifra("it never falls below %.4f, capping them at %.4f.",
          sp_sel$prob_minima_mayoritaria, sp_sel$cota_de_cada_minoritaria))

add(prosa(
"In both sets the cap lies below the floor, so the majority class is the",
"maximum in every stay and the argmax rule cannot name a minority source.",
"That is a bound and not an observation: it holds whatever the minority",
"probabilities turn out to be, and it rests on one figure per set.",
"",
"The observed maxima are further below still. No minority probability"))

add(cifra("exceeds %.4f on the test set or %.4f on the sealed unit. The margin",
          sp_pru$maximo_observado_minoritarias,
          sp_sel$maximo_observado_minoritarias))
add(cifra("the bound leaves is %.4f on the test set and %.4f on the sealed",
          sp_pru$separacion, sp_sel$separacion))

add(prosa(
"unit, so on the test set the model comes close to admitting a different",
"maximum without ever producing one.",
"",
"The model attains high apparent accuracy while identifying no source at",
"all. The same behaviour was observed under both penalty rules, under",
"gradient boosting and under the extended model; the bound above is",
"measured for the final model on the two sets it was evaluated on.",
"",
"What a genuine case receives is the clinical form of the same fact. Among",
"the test-set stays whose source is urinary, the median probability the"))

add(cifra("model assigns to the urinary class is %.4f, against a prevalence",
          ur_pru$mediana))
add(cifra("of %.4f, and the highest any of them receives is %.4f.",
          val(calpr, "frecuencia_observada", calpr$clase == "urinario"),
          ur_pru$maximo))

add(prosa(
"",
"The model is calibrated and discriminates weakly: its probabilities are",
"honest and almost flat. A real urinary case is told it is barely more",
"likely to be urinary than the base rate. That is what a correctly fitted",
"model on insufficient information looks like, and recalibration does not",
"repair it.",
"",
"### Confidence, resolution and error",
""))

add(cifra("At the ninety percent level the system resolves %.1f percent of",
          val(alfa, "pct_resuelve", d10)))
add(cifra("cases with an error of %.4f among them. Loosening the level raises",
          val(alfa, "error_entre_resueltos", d10)))
add(cifra("resolution to %.1f percent and error to %.4f.",
          max(alfa$pct_resuelve), max(alfa$error_entre_resueltos)))

add(prosa(""))
add(cifra("Precision on minority-class conclusions peaks at %.4f, from %d such",
          pmin_alfa, as.integer(n_pmax)))

add(prosa(
"conclusions in total. There is no confidence level at which the system",
"usefully names the source.",
"",
"### Clinical utility",
""))

add(cifra("Decision curve analysis gives a maximum net benefit of %.4f over the",
          gan_dec))

add(prosa(
"better of the two trivial policies, for the question of whether any source",
"is present. The benefit concentrates at low thresholds and converges to zero",
"above them.",
"",
"### Transportability",
""))

add(prosa("Under leave-one-unit-out validation, marginal coverage ranges from"))
add(cifra("%.4f to %.4f with a mean of %.4f, below the nominal level. %d of the",
          min(louo$cobertura), max(louo$cobertura), mean(louo$cobertura),
          as.integer(bajo_marg)))
add(cifra("%d units fall below nominal on that measure.", as.integer(n_sedes)))

add(prosa(""))
add(prosa(
"The conditional guarantee, which is the one this work claims, does not"))
add(cifra("hold: it fails in %d of the %d units. The classes that fail are not",
          as.integer(sedes_dem), as.integer(sedes_iv)))
add(cifra("the same everywhere. Of the units that fail, %d fail on %s and %d",
          as.integer(causa$sedes[1]), causa$clases[1],
          as.integer(causa$sedes[2])))
add(cifra("on %s. The remaining %d show point coverage of %.4f and %.4f on",
          causa$clases[2], as.integer(n_sin_dem),
          sin_dem$cobertura[1], sin_dem$cobertura[2]))
add(cifra("%d and %d cases, too few to establish the shortfall. Absence of",
          as.integer(sin_dem$n[1]), as.integer(sin_dem$n[2])))

add(prosa(
"demonstration is not evidence of compliance.",
""))
add(cifra("The cells whose interval falls entirely below nominal, the %d that",
          as.integer(n_resiste)))
add(prosa(
"survive correction among them:",
"",
"    unit         class                 n   cover   interval",
""))

for (i in which(bajo_iv))
  add(cifra("    %-12s %-16s %5d  %.4f  %.4f to %.4f",
            iv$sigla[i], iv$clase[i], as.integer(iv$n[i]),
            iv$cobertura[i], iv$ic_beta_inferior[i], iv$ic_beta_superior[i]))

add(prosa(
"",
"How that count was reached. The guarantee is assessed class by class"))
add(cifra("within each unit, which gives %d cells, and two things shape how",
          as.integer(n_celdas)))
add(prosa(
"they are read. The cells are corrected jointly for multiplicity, since at"))
add(cifra("the conventional level of %.2f the expected number of false",
          nivel_conv))
add(cifra("positives is %.1f under the hypothesis that every cell meets",
          falsas))
add(prosa(
"nominal. And the conformal threshold is not a known quantity: it is",
"re-estimated inside each fold from a finite calibration set, which makes",
"the covered count beta-binomial rather than binomial. Treating it as",
"binomial credits the evidence with a precision it does not have.",
""))
add(cifra("Of the %d cells, %d have their whole interval below nominal and",
          as.integer(n_celdas), as.integer(n_bajo)))
add(cifra("%d survive the correction. The count does not depend on the",
          as.integer(n_resiste)))
add(prosa(
"choice of correction or level, under the test that recognises the",
"calibration:",
"",
"    correction                 level   cells",
""))

# El nombre de la correccion se compone con la familia delante. El archivo
# deposita las cuatro variantes, y `mb` retiene solo las que reconocen la
# calibracion: imprimirlas con el nombre a secas publicaba un recuento que no
# es el que ese nombre tiene en la fuente, y el parrafo siguiente, que da el
# recuento condicionado al umbral, quedaba sin nada que lo distinguiera.
for (i in seq_len(nrow(mb)))
  add(cifra("    beta-binomial %-12s %5.3f  %6d", mb$correccion[i],
            mb$nivel[i], as.integer(mb$celdas_resisten[i])))

add(prosa(""))
add(cifra("Under the test that treats the threshold as known, %d cells would",
          as.integer(n_binom)))
add(prosa(
"survive under Holm's correction at either level. The intervals above",
"incorporate the calibration uncertainty; conditioning on the threshold",
"instead narrows"))
add(cifra("them by between %.0f and %.0f percent. All the cells of this",
          estrecha_min, estrecha_max))
add(prosa(
"section and of the sealed-unit section below are in",
"`outputs/fase26/intervalos_cobertura.csv`.",
"",
"The interval and the test are anchored slightly differently. The interval",
"is inverted against the nominal level exactly; the test is taken against",
"the mean coverage the procedure targets, which the ceiling in the conformal",
"quantile places marginally above nominal. Both are reported, and here they",
"agree on every cell.",
"",
"### The sealed unit",
"",
"Evaluated once, without recalibration, and read by the same criterion:",
"",
"    class                 n   cover   interval",
""))

for (i in seq_len(nrow(sll)))
  add(cifra("    %-16s %5d  %.4f  %.4f to %.4f", sll$clase[i],
            as.integer(sll$n[i]), sll$cobertura[i],
            sll$ic_beta_inferior[i], sll$ic_beta_superior[i]))

add(prosa(
"",
"The majority class is covered above nominal. No minority class has an",
"interval falling below it, so this section reports no conclusion about the",
"conditional guarantee here: the cases are too few to establish a shortfall",
"in either direction. The over-coverage of the majority class is consistent",
"with a prevalence shift: its mean predicted probability is"))

add(cifra("%.4f against an observed frequency of %.4f. On the test set the", cal_may, frec_may))
add(cifra("same model predicts that class to within %.4f of its observed",
          max(abs(calpr$diferencia))))
add(prosa(
"frequency, so the gap is specific to this unit rather than a property of",
"the model on the data it was fitted from.",
"",
"Prevalence shift is not the only mechanism that would produce over-coverage",
"there. A unit measured less completely would be predicted with more",
"imputation, and imputation pulls predictions toward the training mean,",
"which the majority class dominates. That mechanism is not available here:"))
add(cifra("over the %d determinations the model uses, %.2f percent of the",
          as.integer(cmpl[["determinaciones"]][1]),
          val(cmpl, "pct_completas", cmpl$conjunto == "unidad reservada")))
add(cifra("sealed unit's stays are complete against %.2f percent of the",
          val(cmpl, "pct_completas", cmpl$conjunto == "desarrollo")))
add(cifra("development set, and %.2f percent of its cells are observed against",
          val(cmpl, "pct_valores_presentes",
              cmpl$conjunto == "unidad reservada")))
add(cifra("%.2f. The unit is measured more completely, not less, so the",
          val(cmpl, "pct_valores_presentes", cmpl$conjunto == "desarrollo")))
add(prosa(
"asymmetry runs opposite to what that explanation would need. This work does",
"not separate the mechanisms further."))

add(prosa(
"",
"### The limit of local recalibration",
""))

add(cifra("With %d local cases an average of %.1f of the %d classes reaches",
          as.integer(nmin),
          val(recal, "clases_calibrables", recal$n_local == nmin),
          nrow(cob8)))
add(prosa("the minimum required for conditional calibration, and mean set size"))
add(cifra("falls to %.3f: sets are often empty and the system has collapsed to",
          val(recal, "tamano_medio", recal$n_local == nmin)))

add(prosa(
"a degenerate binary classifier. Apparent coverage near the nominal level",
"conceals this.",
"",
"### Model complexity is justified and insufficient",
"",
"Parameters are the non-zero coefficients, intercepts excluded, summed over",
"the class blocks.",
"",
"    specification         parameters   AUC on minority classes",
""))

for (i in seq_len(nrow(refs)))
  add(cifra("    %-20s %10d   %7.4f", refs$modelo[i],
            as.integer(refs$parametros[i]),
            refs$promedio_minoritarias[i]))

add(prosa(
"",
"The specification names are Spanish: demografia is demographics, tres",
"marcadores three markers, lineal sin unidad a linear model without the unit",
"and modelo completo the full model.",
""))
add(cifra("Gradient boosted trees on the same predictors gain %.4f over the",
          mean(gbm$auc_gbm[gbm$clase != "sin_crecimiento"]) -
          mean(gbm$auc_lineal[gbm$clase != "sin_crecimiento"])))

add(prosa(
"penalized linear model. The ceiling belongs to the information available,",
"not to the functional form.",
"",
"### The model reads clinical judgement",
""))

add(cifra("The indicator of whether lactate was ordered contributes %.4f to",
          val(lac, "promedio_minoritarias", lac$especificacion == "completa") -
          val(lac, "promedio_minoritarias",
              lac$especificacion == "lactato sin indicador")))
add(cifra("AUC; the measured value contributes %.4f. The ordering decision",
          val(lac, "promedio_minoritarias",
              lac$especificacion == "lactato sin indicador") -
          val(lac, "promedio_minoritarias",
              lac$especificacion == "sin lactato ni indicador")))

add(prosa(
"carries more signal than the physiology it measures.",
"",
"## Extension with vital signs",
"",
"Declared as a secondary study. The sealed model is not modified.",
"",
"### Two variables were excluded after measurement",
"",
"**Blood pressure.** Agreement between invasive and non-invasive measurement"))

add(cifra("reaches only %.4f even when restricted to readings less than fifteen",
          val(conc, "correlacion", conc$tramo == "menos de 15 min")))
add(cifra("minutes apart, with a median absolute discrepancy of %.0f mmHg. The",
          val(conc, "dif_absoluta_mediana", conc$tramo == "menos de 15 min")))
add(cifra("proportion measured by catheter ranges from %.1f to %.1f percent",
          min(cpres$pct_por_cateter), max(cpres$pct_por_cateter)))

add(prosa(
"across units.",
"",
"**Glasgow Coma Scale.** A binary indicator of endotracheal intubation alone"))

add(cifra("discriminates the respiratory class at %.4f. The scale scores %.4f",
          val(tubo, "auc_solo_tubo", tubo$clase == "respiratorio"),
          val(gcs, "auc_gcs_total", gcs$estrato == "cohorte completa" &
                                    gcs$clase == "respiratorio")))
add(cifra("on its full three-component form and %.4f on eye plus motor. The",
          val(gcs, "auc_gcs_em", gcs$estrato == "cohorte completa" &
                                 gcs$clase == "respiratorio")))

add(prosa(
"reduced form is the one reported here because the verbal component assigns",
"the minimum score to intubated patients, conflating absent response with",
"inability to speak. Within the intubated stratum the reduced scale falls to"))

add(cifra("%.4f, and the full form scores the same %.4f there. The two forms",
          val(gcs, "auc_gcs_em", gcs$estrato == "con tubo endotraqueal" &
                                 gcs$clase == "respiratorio"),
          val(gcs, "auc_gcs_total", gcs$estrato == "con tubo endotraqueal" &
                                    gcs$clase == "respiratorio")))

add(prosa(
"coincide within that stratum because the verbal component is constant there,",
"so the full scale is the reduced one plus a fixed offset, which leaves the",
"ranking and therefore the area unchanged. The scale acts as a proxy for the",
"procedure, and the procedure is strongly associated with whether the",
"respiratory site is cultured at all:"))

add(cifra("%.1f percent of the respiratory cases are intubated, against",
          val(tubo, "pct_intubado", tubo$clase == "respiratorio")))
add(cifra("%.1f percent of those without growth. It runs in the direction",
          val(tubo, "pct_intubado", tubo$clase == "sin_crecimiento")))

add(prosa(
"the argument needs, but it remains an association: the data do not",
"establish that the one determines the other.",
"",
"### Four vital signs were retained",
"",
"Coverage of the table left after implausible values are blanked, which is",
"the one the model is fitted on, counted over the final funnel cohort. The",
"raw extraction is given beside it, and the difference between the columns",
"is what the plausibility limits discard.",
"The figures used everywhere below are the cleaned ones. Both stages are in",
"`outputs/fase28/cobertura_vitales_por_etapa.csv`.",
"",
"    variable                 raw   cleaned",
""))

for (i in seq_len(nrow(cove)))
  if (cove$variable[i] %in% VIT)
    add(cifra("    %-20s %7.1f %9.1f", cove$variable[i],
              cove$pct_crudo[i], cove$pct_limpio[i]))

add(prosa(
"",
"The variable names are Spanish: temperatura temperature, frec_cardiaca heart",
"rate, frec_respiratoria respiratory rate and saturacion oxygen saturation.",
"",
"Their medians are not constant across units. Heart rate ranges by"))

add(cifra("%.0f beats per minute and respiratory rate by %.0f breaths across",
          val(rec, "recorrido_mediana", rec$variable == "frec_cardiaca"),
          val(rec, "recorrido_mediana", rec$variable == "frec_respiratoria")))

add(prosa(
"the six units. That variation confounds case mix with measurement practice",
"and this analysis cannot separate them: a cardiac surgical unit has genuinely",
"slower, sedated patients. It differs from lactate, where whether the test is",
"ordered cannot depend on its own result, so the variation in how often it is",
"ordered across units is practice.",
""))

add(cifra("Availability also varies: temperature is recorded in %.1f percent of",
          val(rec, "cobertura_minima", rec$variable == "temperatura")))
add(cifra("stays in one unit and %.1f percent in another, a range of %.1f",
          val(rec, "cobertura_maxima", rec$variable == "temperatura"),
          val(rec, "recorrido_cobertura", rec$variable == "temperatura")))

add(prosa(
"points. Whether a temperature is taken is less patient-dependent than what",
"it reads, so its missingness carries site information more clearly than its",
"value does.",
"",
"Only temperature required a flexible functional form, with a gain of"))

add(cifra("%.2f against a permutation-derived noise threshold.",
          val(spl, "ganancia", spl$variable == "temperatura")))

add(prosa(""))

add(cifra("The extended model gains %.4f over the original in mean AUC across",
          mean(amp$auc_ampliado[amp$clase %in% MIN]) -
          mean(amp$auc_original[amp$clase %in% MIN])))

add(prosa(
"minority classes. It does not change the clinical verdict.",
"",
"Adding the vital signs does not improve transportability:"))

add(cifra("coverage dispersion is %.4f against %.4f, the mean falls from %.4f",
          sd(louoa$cobertura), sd(louo$cobertura), mean(louo$cobertura)))
add(cifra("to %.4f, and the minimum from %.4f to %.4f. The extended model",
          mean(louoa$cobertura), min(louo$cobertura), min(louoa$cobertura)))

add(prosa(
"transports no better than the original. That is consistent with prevalence",
"shift rather than predictor contamination as the mechanism, but does not",
"establish it: the extended model adds the vital signs to the same laboratory",
"variables rather than replacing them.",
"",
"### Imputation propagates uncertainty",
""))

add(cifra("Fraction of missing information reaches %.4f for laboratory results",
          max(fmi$fmi[fmi$pct_ausente > 20])))
add(cifra("and stays at or below %.4f for vital signs.",
          max(fmi$fmi[fmi$pct_ausente < 10])))

add(prosa(
"",
"On masked observed values the two approaches are close:"))

add(cifra("median substitution attains lower absolute error on %d variables,",
          as.integer(gana_med)))
add(cifra("multivariate imputation on %d, and the remaining variable ties. The",
          as.integer(gana_mice)))

add(prosa(
"multivariate approach wins where physiological correlation is high and loses",
"where it is absent. It is retained because its purpose is to propagate the",
"uncertainty of the fill, which a single substituted value cannot do.",
"",
"## A reproducibility finding",
""))

add(cifra("Nursing observations are validated in batches, so between %.2f and",
          min(empc$pct)))
add(cifra("%.2f percent of stays carry several measurements of the same",
          max(empc$pct)))
add(cifra("variable with an identical storetime, up to %d at once. That is the",
          as.integer(max(empc$maximo_coincidentes))))

add(prosa(
"time a result was recorded. Selecting the first measurement by ordering on",
"storetime alone leaves ties unresolved, and the row retained can differ",
"between runs of the same query.",
""))

add(cifra(
  "Laboratory results are affected in %.2f percent of stays. Analysers",
  empl$pct))

add(prosa(
"timestamp each result individually, which makes ties far rarer there but not",
"absent. The laboratory extraction in `R/19_matriz.R` orders by storetime",
"alone, the same pattern corrected here, and the re-extraction covered the",
"vital signs only. No versioned file bounds the divergence this leaves in the",
"core phases.",
"",
"Ordering now uses four keys: storetime, charttime (the time the",
"measurement was made), the value after unit conversion, and itemid (the",
"identifier of the measured item). The converted value earns its place:"))

add(cifra("%d groups of measurements agree on stay_id, storetime and charttime",
          as.integer(det$grupos_ambiguos)))

add(prosa(
"while disagreeing on the converted value, so the first two keys alone leave",
"the retained row undetermined."))

add(cifra("The query was run %d times and all %d results are identical.",
          as.integer(det$ejecuciones), as.integer(det$identicas_a_la_primera)))

add(prosa(""))

add(cifra("Material divergence from the earlier extraction reaches %.3f percent of",
          max(divg$pct_material)))
add(cifra("stays per variable, and no coverage figure moves by more than %.1f points.",
          max(abs(cobx$diferencia))))

add(prosa(
"",
"## Requirements",
"",
"R 4.6.1. Exact library versions are recorded in `renv.lock`:",
"",
"    R -e 'renv::restore()'",
"",
"Running any of this requires credentialed access to MIMIC-IV through",
"PhysioNet and a local copy of the release, which the procedures expect",
"under `~/mimic-data/physionet.org/files/mimiciv/3.1`. They are numbered in",
"the order they run: schema exploration and cohort first, then the analysis",
"matrix and the partition, the imputation, the model and its conformal",
"calibration, the validations, the extension with vital signs, and last the",
"verification and the documents.",
"",
"The seed is 20260818 across all phases involving randomization. The cohort",
"extraction manifest records a different seed, which is inert: that step",
"draws no random numbers. The extraction phases record software versions and",
"SHA-256 hashes of the source files in their manifests; later phases record",
"the seed and the analytical choices but not hashes.",
"",
"The manifest of the eighth phase records the penalty to full precision,",
"while the procedures that reuse it read the shortened value the",
"hyperparameter table publishes. The replication of the transportability",
"validation used the shorter value and reproduced the published result, so",
"no figure reported here is affected.",
"",
"## Limitations",
"",
"Etiological classification depends on which tests were ordered. Culture",
"positivity varies by an order of magnitude across specimen types, reflecting",
"that confirmation depends on prior clinical suspicion.",
"",
"Four quantities measured here carry the practice of the site rather than the",
"state of the patient. Whether lactate was ordered outweighs its value. A",
"binary indicator of intubation discriminates the respiratory class about as",
"well as the consciousness scale, and intubation is associated with whether",
"that site is cultured at all. Temperature is recorded in a quarter more of",
"the stays of one unit than of another. And laboratory completeness itself",
"differs by unit: over the determinations the model uses, the sealed unit is",
"complete in twice the proportion of stays that the development set is. Each",
"is a route by which a model fitted in one place reads where the patient is",
"rather than what is wrong with them.",
"",
"The respiratory category may include airway colonization in ventilated",
"patients, which the available fields cannot distinguish from infection.",
"",
"All units belong to a single tertiary academic centre, so the observed",
"heterogeneity is a lower bound on what separate institutions would show.",
"",
"The phase that built the analysis matrix rewrote the outcome label with a",
"shorter tie-break ladder than the phase that sealed the definitions, and the",
"shorter ladder promotes the urinary site above the intra-abdominal one. The",
"two can differ",
"only for a stay positive at both those sites and at neither blood nor",
"respiratory. No stay in the cohort is, so no label changes; the full",
"crosswalk is in `outputs/fase25/divergencia_etiquetado.csv`.",
""))

add(cifra("The sealed unit contributes %d respiratory cases, too few to",
          as.integer(val(sell, "n", sell$clase == "respiratorio"))))

add(prosa(
"estimate conditional coverage with useful precision.",
"",
"The core phases were extracted before the tie-breaking correction. They draw",
"only on laboratory results, where ties are far rarer than in nursing",
"observations but not absent, and where the ordering carries the same defect.",
"The correction re-extracted the vital signs only, so the divergence the core",
"phases could carry is not bounded by any file. The cohort was not rebuilt",
"because doing so after the sealed set had been opened would void the",
"external validation, and the limitation therefore stands unquantified.",
"",
"The study does not compare system performance against a clinician working",
"from the same information.",
"",
"## Repository",
"",
"File and line counts are omitted here: they are self-referential, so any",
"later commit makes them stale, and `git ls-files` reports them directly.",
"The self-checking procedures are:",
"",
"    R/59_verificar_cifras.R",
"        checks reported figures against their source files",
"    R/64_auditoria_publicacion.R",
"        checks the repository is safe to publish",
"    R/65_generar_readme.R",
"        generates this document",
"    R/67_diagnostico_dependencias.R",
"        checks the lockfile covers every library the procedures load",
"    R/68_traduccion_yachay.R",
"        generates the protocol document",
"    R/82_decisiones_vivas.R",
"        generates the record of standing decisions",
"",
"The other documents are [TRACEABILITY](outputs/fase20/TRACEABILITY.md),",
"which records every published figure against its source file,",
"[REPRODUCIBILITY](outputs/fase20/REPRODUCIBILITY.md), on the determinism of",
"the extraction, [PROTOCOLO](outputs/fase21/PROTOCOLO.md), which carries the",
"findings into decisions for a separate study, and",
"[DECISIONES](DECISIONES.md), the record of standing decisions consulted",
"before writing: what is declared as posterior analysis, which biases",
"accompany which figure, which claims the files do not support, and which",
"figures are not comparable with which. The last two are written in Spanish.",
"",
"## Language-model assistance",
"",
"Language models assisted in drafting the code and the documentation. No",
"patient-level data were transmitted to those tools: the PhysioNet data use",
"agreement prohibits it. Every procedure was executed in the project",
"environment under the author's direction. The published figures are",
"checked automatically against their source files by",
"`R/59_verificar_cifras.R`, and the repository is audited by",
"`R/64_auditoria_publicacion.R`.",
"",
"## License",
"",
"Code under the MIT license. Data are governed by the PhysioNet data use",
"agreement for MIMIC-IV."))

writeLines(L, "README.md")

cat("=== DOCUMENTACION GENERADA ===\n")
cat("Lineas escritas:", length(L), "\n")
cat("\nComprobaciones derivadas de archivo\n")
cat("  Sedes bajo el nominal en cobertura marginal:", bajo_marg, "\n")
cat("  Sedes con alguna clase puntualmente bajo el nominal:", falla_cond, "\n")
cat("  Clases bajo el nominal en la unidad sellada:", bajo_sell, "\n")
cat("  Imputacion, gana mediana:", gana_med, " gana mice:", gana_mice, "\n")
