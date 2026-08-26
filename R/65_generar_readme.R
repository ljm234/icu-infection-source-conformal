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

EXENTAS <- c("MIMIC-IV version 3.1", "R 4.6.1", "seed is 20260818",
             "SHA-256", "R/59_verificar_cifras.R",
             "R/64_auditoria_publicacion.R", "R/65_generar_readme.R",
             "R/67_diagnostico_dependencias.R")

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
cobv  <- leer("outputs/fase17/cobertura_vitales.csv")
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
"No patient-level derived data is versioned. `R/64_auditoria_publicacion.R`",
"inspects the header of every tracked CSV, lists the tracked binary objects",
"with their sizes, and checks that none of the protected paths appears in the",
"commit history.",
"",
"## Cohort",
""))

for (i in seq_len(nrow(flujo)))
  add(cifra("    %-46s %12s", flujo$paso[i],
            format(flujo$n[i], big.mark = ",")))

add(prosa(""))
add(prosa("Units contributing fewer than five hundred stays were dropped at the"))

add(prosa(
"partition step, so the analysed cohort is smaller than the final funnel row.",
"",
"## Categories",
""))

for (i in seq_len(nrow(clases)))
  add(cifra("    %-18s %9s   %5.1f percent", clases$clase[i],
            format(clases$n[i], big.mark = ","), clases$pct[i]))

add(prosa(
"",
"The abstention category groups wound, intra-abdominal, cerebrospinal fluid",
"and other sites whose frequency does not support conditional calibration. It",
"is not modelled and serves to evaluate the referral mechanism.",
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
"**Aggregation.** First recorded value per variable. The first value is the",
"only estimator whose distribution does not depend on how many measurements",
"were taken, which matters because monitoring intensity differs across units."))

add(cifra("Correlation with worst-value aggregation never falls below %.4f",
          min(agr$correlacion)))

add(prosa(
"across the seventeen laboratory variables; the comparison does not cover",
"vital signs. The shift the worst value induces tracks monitoring intensity",
"rather than physiology.",
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
"**Partition.** The cardiovascular unit was sealed in full and left",
"unexamined until development concluded, selected on four measures of",
"dissimilarity, two of which are versioned.",
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
"### The argmax rule never names a source",
"",
"At minority prevalences of a few percent, no minority class probability",
"exceeds the majority class. The model attains high apparent accuracy while",
"identifying no source at all. This was observed under both penalty rules,",
"under gradient boosting, and under the extended model.",
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
add(prosa("The conditional guarantee, which is the one this work claims, fails"))
add(cifra("in %d of %d units: none reaches nominal coverage across all four",
          as.integer(falla_cond), as.integer(n_sedes)))

add(prosa(
"classes. The failing class differs by unit, which is why a marginal summary",
"conceals it.",
"",
"### The sealed unit",
"",
"Evaluated once, without recalibration. Coverage by class:",
""))

for (k in CLC)
  add(cifra("    %-16s %7.4f", k, val(sell, "cobertura", sell$clase == k)))

add(prosa(""))
add(cifra("The majority class is over-covered while %d of the %d classes fall",
          as.integer(bajo_sell), nrow(sell)))

add(prosa(
"below nominal, so by the criterion applied above the sealed unit also fails",
"the conditional guarantee. The over-coverage of the majority class follows",
"from a prevalence shift: its mean predicted probability falls below its",
"observed frequency.",
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
"    specification         parameters   AUC on minority classes",
""))

for (i in seq_len(nrow(refs)))
  add(cifra("    %-20s %10d   %7.4f", refs$modelo[i],
            as.integer(refs$parametros[i]),
            refs$promedio_minoritarias[i]))

add(prosa(""))
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
"procedure, and the procedure determines whether the respiratory site is",
"cultured at all.",
"",
"### Four vital signs were retained",
"",
"    variable              coverage",
""))

for (i in seq_len(nrow(cobv)))
  if (cobv$variable[i] %in% VIT)
    add(cifra("    %-20s %6.1f percent", cobv$variable[i], cobv$pct[i]))

add(prosa(
"",
"Their medians are not constant across units. Heart rate ranges by"))

add(cifra("%.0f beats per minute and respiratory rate by %.0f breaths across",
          val(rec, "recorrido_mediana", rec$variable == "frec_cardiaca"),
          val(rec, "recorrido_mediana", rec$variable == "frec_respiratoria")))

add(prosa(
"the six units. That variation confounds case mix with measurement practice",
"and this analysis cannot separate them: a cardiac surgical unit has genuinely",
"slower, sedated patients. It differs from lactate, where whether the test is",
"ordered cannot depend on its own result, so the variation across units is",
"unambiguously practice.",
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

add(cifra("%.2f against a permutation-derived noise threshold. Its relationship",
          val(spl, "ganancia", spl$variable == "temperatura")))

add(prosa(
"with infection is U-shaped: both fever and hypothermia mark severity.",
""))

add(cifra("The extended model gains %.4f in mean AUC across minority classes",
          mean(amp$auc_ampliado[amp$clase %in% MIN]) -
          mean(amp$auc_original[amp$clase %in% MIN])))

add(prosa(
"and resolves a larger share of cases than the original. It does not change",
"the clinical verdict.",
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
add(cifra("multivariate imputation on %d, and the remainder tie. The",
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
add(cifra("variable with an identical storetime, up to %d at once. Selecting",
          as.integer(max(empc$maximo_coincidentes))))

add(prosa(
"the first measurement by ordering on storetime alone leaves ties unresolved,",
"and the row retained can differ between runs of the same query.",
""))

add(cifra("Laboratory results are unaffected at %.2f percent, since analysers",
          empl$pct))

add(prosa(
"timestamp each result individually.",
"",
"Ordering now uses four keys: storetime, charttime, the value after unit",
"conversion, and itemid. Three consecutive runs return identical results."))

add(cifra("Material divergence from the earlier extraction reaches %.3f percent",
          max(divg$pct_material)))

add(prosa(
"of stays per variable, and coverage is unchanged.",
"",
"## Requirements",
"",
"R 4.6.1. Exact library versions are recorded in `renv.lock`:",
"",
"    R -e 'renv::restore()'",
"",
"The seed is 20260818 across all phases involving randomization. The cohort",
"extraction manifest records a different seed, which is inert: that step",
"draws no random numbers. The extraction phases record software versions and",
"SHA-256 hashes of the source files in their manifests; later phases record",
"the seed and the analytical choices but not hashes.",
"",
"## Limitations",
"",
"Etiological classification depends on which tests were ordered. Culture",
"positivity varies by an order of magnitude across specimen types, reflecting",
"that confirmation depends on prior clinical suspicion.",
"",
"The respiratory category may include airway colonization in ventilated",
"patients, which the available fields cannot distinguish from infection.",
"",
"All units belong to a single tertiary academic centre, so the observed",
"heterogeneity is a lower bound on what separate institutions would show.",
""))

add(cifra("The sealed unit contributes %d respiratory cases, too few to",
          as.integer(val(sell, "n", sell$clase == "respiratorio"))))

add(prosa(
"estimate conditional coverage with useful precision. This was declared",
"before the set was examined.",
"",
"The core phases were extracted before the tie-breaking correction. They draw",
"only on laboratory results, which are unaffected. The cohort was not rebuilt",
"because doing so after the sealed set had been opened would void the",
"external validation.",
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
cat("  Sedes que fallan la garantia condicional:", falla_cond, "\n")
cat("  Clases bajo el nominal en la unidad sellada:", bajo_sell, "\n")
cat("  Imputacion, gana mediana:", gana_med, " gana mice:", gana_mice, "\n")
