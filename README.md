# Conformal prediction of infection source at ICU admission

A methodological testbed built on MIMIC-IV for multiclass classification of
culture site in adults with suspected infection, using class-conditional
conformal prediction sets and an abstention mechanism.

Principal Investigator: Luis Jordan Montenegro-Calla

Generated on 2026-09-01 by `R/65_generar_readme.R`. Every numeric figure below
is read from a versioned results file. The generator rejects prose lines
containing a digit, format patterns carrying digits outside their substitution
codes, and any line that fails to compose.

## Question

An adult is admitted to intensive care with suspected infection, established
by a culture drawn alongside systemic antibiotics. Using only information
available in the first six hours, can the source of infection be identified?
The system returns a set of compatible categories rather than a single label,
and abstains when the available information does not permit discrimination.

## Data

MIMIC-IV version 3.1, obtained from PhysioNet under individual credentialing.
The source files are not redistributed and form no part of this repository.
Reproducing the analysis requires separate credentialing and approved access.

No patient-level derived data are versioned. `R/64_auditoria_publicacion.R`
reads every tracked CSV in full, not only its header, since an identifier can
arrive as a value and not only as a column name; it also flags any tracked
table with as many rows as the cohort has stays, which is a patient-level
table by shape whatever its columns are called. It lists the tracked binary
objects with their sizes and checks that none of the protected paths appears
in the commit history.

## Cohort

    p1_estancias_unicas                                  65,366
    p2_adultos                                           65,366
    p3_con_cultivo                                       33,045
    p4_sospecha_infeccion                                23,213
    p5_cohorte_final                                     23,213

The step labels are Spanish and read, in order: unique stays, adults, stays
with a culture drawn, suspected infection and the final cohort.

Of the stays with suspected infection, those in units contributing at least
500 make up the analysed cohort: 22,778 stays across 6 units. The model was
developed on the 18,054 stays of 5 of them and evaluated once on the 4,724 of
the unit held back.

## Categories

    sin_crecimiento       19,688    84.8 percent
    otro_sitio             1,070     4.6 percent
    urinario                 805     3.5 percent
    respiratorio             671     2.9 percent
    sangre                   573     2.5 percent
    herida                   340     1.5 percent
    intraabdominal            66     0.3 percent

The category names are Spanish: sin_crecimiento is no growth on culture,
otro_sitio another site, urinario urinary, respiratorio respiratory, sangre
bloodstream, herida wound and intraabdominal intra-abdominal.

Abstention is not one of those labels. In the modelled table it is what
remains once the four modelled classes are taken out, and in the cohort table
above it corresponds to the labels that are not modelled: otro_sitio, herida,
intraabdominal. Those are 3 of the 7 labels above. Cerebrospinal fluid has no
label of its own anywhere in this work and falls inside otro_sitio. None of
them is frequent enough to support conditional calibration, and none is
modelled.

## Design decisions

**Time origin.** Admission to the first intensive care stay, adopted after
comparing against hospital admission.

**Predictor window.** Six hours. The temporal filter uses the time a result
was recorded rather than the time the specimen was drawn, since a result
recorded afterwards was not available at the moment of decision.

**Laboratory variables.** Seventeen, drawn from the candidates present in at
least 3,000 of the 65,366 first stays within the window, of which there are
73. That denominator is the one every coverage figure in this paragraph is
taken over, and it is not the analysed cohort, which is smaller. The candidate
with the highest coverage of all, at 68.5 percent of those stays, is among the
discarded, so the seventeen are not the seventeen most frequent.

The rule that narrowed the candidates to the seventeen is not recorded, and
nothing in the deposit reproduces it. No value of the sample type, of the
panel the source dictionary assigns, or of the coverage is exclusive to the
retained. The sample type does carry information in one direction: every
retained determination is a blood assay, and every urine assay among the
candidates was discarded, but none of those 6 exceeds 9.9 percent in coverage
against the 45.2 percent of the least covered retained one, so the sample type
adds nothing to the coverage ordering. Coverage alone does not account for the
split either: 12 blood assays with higher coverage than that were discarded.
`outputs/fase30/determinaciones_candidatas.csv` describes every one of them.

A comparison on stays complete in all the candidates cannot be made. Of the
23,213 stays, 0 have all of them within the window, and the most any stay
reaches is 69. What can be compared is the seventeen against those same
seventeen plus the 12 discarded whose coverage exceeds that of the least
frequent retained one. Of that cohort, 5,866 stays are complete in those 29;
3,774 of them fall in the training or test partition, and 3,498 of those carry
one of the four modelled classes. That last set carries the comparison, and on
it the wider specification does not improve on the narrower: the mean area
across minority classes moves by -0.0113, with a paired bootstrap interval of
-0.0249 to 0.0027 that contains zero, so the direction is not established.
Only 1 of the 4 classes shows an interval that excludes zero. The four are
corrected jointly by Holm's method, the same correction the transportability
section applies, though there it runs over a beta-binomial test and here over
a paired bootstrap. At neither 0.05 nor 0.025 does any of them survive: 0 do.
Reporting the one that excludes zero without saying that it does not survive
correction would apply one standard here and another there.

Two things bound that comparison and two work in its favour. Stays complete in
that many determinations are not a random sample: they are the more heavily
monitored ones, and monitoring intensity tracks both severity and unit, so the
answer holds among patients with complete laboratory work rather than in the
cohort. How far completeness tracks the unit is measurable in the sealed one,
which this comparison leaves out anyway: 7.87 percent of its stays are
complete in these determinations, against 29.78 percent in the training
partition. Over the 17 the published model uses the ordering reverses, at
60.69 against 30.34 percent, so this figure belongs to the determinations
compared here and not to the model. Both specifications were fitted linearly,
without splines, without imputation and without the lactate ordering
indicator, so that the variable set is the only thing that differs between
them; neither figure is comparable with the areas reported elsewhere in this
document. And the interval is for the difference, which is paired on the same
stays and therefore tighter than the interval of either area alone.

The fit also left power unused, and by a margin worth stating. The calibration
partition exists to set the conformal thresholds, and this comparison sets
none, so nothing in it required leaving that partition out. It was left out
all the same: both specifications were fitted on 2,487 stays where 4,010 were
available, so 1,523 went unused, 61.24 percent more than entered. That does
not bias the comparison, since the restriction falls on both branches alike,
but it costs power, and the conclusion here is a negative one, so the absence
of improvement is worse established than it could have been. It is not redone.
Refitting with more stays after seeing the result, in the direction that could
reverse it, is the pattern this document objects to when describing how the
penalty was fixed, and an objection that holds only against inconvenient
findings is not an objection. `outputs/fase32/potencia_no_usada.csv` carries
the count.

**Aggregation.** First recorded value per variable. The first value is the
only one computable without knowing how many measurements follow, and that
computability matters because monitoring intensity differs across units.
Correlation with worst-value aggregation never falls below 0.8722 across the
seventeen laboratory variables; the comparison does not cover vital signs.

**Imputation.** Chained equations with predictive mean matching, twenty
datasets, ten iterations, estimated on the training set alone. The outcome is
excluded, since the system must operate on patients whose class is unknown.

**Model.** Penalized multinomial logistic regression. The elastic-net mixing
parameter was selected by cross-validation over a grid and settled on the
lasso limit, so the penalty actually fitted is lasso rather than a mixture.
Folds assigned by patient. Restricted cubic splines where the cross-validated
gain exceeded a threshold derived from a permutation null.

The penalty is the cross-validated minimum and not the one-standard-error
rule. The two were compared under a stated criterion: adopt the minimum if the
mean area across minority classes improves by more than a declared margin and
no minority class loses more than that same margin. The criterion is written
into `R/32_comparar_lambda.R`, which entered the repository in the same commit
as its result, so no artefact establishes that it preceded the comparison. The
minimum won, gaining 0.0395 against a margin of 0.02.

That comparison was made on the test set, which is the set that later reports
discrimination and coverage. It is therefore a design decision taken on the
evaluation data, and it was checked again without it: the training set alone
was split by patient, the whole comparison repeated inside it, and the same
criterion applied. On 2791 stays held out from the same training set, against
the 5578 used for fitting, the answer is the same, with a gain of 0.0375. The
reduced fit favours the one-standard-error rule, since a smaller sample calls
for a heavier penalty, so the minimum wins there against the odds. The check
is in `outputs/fase22/decision_lambda.csv`.

**Partition.** The unit sealed in full is the one the deposit names `Cardiac
Vascular Intensive Care Unit (CVICU)`, and it goes by that name everywhere in
this repository. The choice was a judgement informed by the unit profiles that
`R/21_perfil_unidades.R` computes, of which two are versioned:
`outputs/fase5/distancia_unidades.csv` and
`outputs/fase5/clases_por_unidad.csv`. It was not the application of a rule.
`R/22_particion.R` names the unit as a constant and computes no selection
criterion.

That unit is also the most dissimilar of the 6, which is worth stating rather
than leaving for a reader to find. By the same statistic that informed the
judgement, the mean standardised difference between a unit and the rest of the
cohort averaged over the determinations, it stands at 0.402 against 0.220 for
the next, 1.83 times the highest of the other 5 and outside their range. It
also takes the extreme share in every one of the 5 columns of the class
profile, holding 95.2 percent of `sin_crecimiento` where no other unit exceeds
85.9. `outputs/fase42/distancia_de_la_reservada.csv` and
`outputs/fase42/extremos_por_categoria.csv` carry both comparisons.

What that establishes is that the external validation was not a favourable
draw. It was made on the unit furthest from the rest, so it is the worst case
among those available, and the sites that carry the leave-one-unit-out
analysis span a narrower range of dissimilarity than the sealed one
represents. What it does not establish is that the unit was chosen for that
reason. The distance was computed first, the judgement had it in view, and no
rule turns it into a criterion.

How often the unit is touched afterwards, and with what commitment, is derived
from the code by `R/89_contactos_con_el_sellado.R` rather than listed, because
a list goes stale every time someone reads that unit again, as it did when the
calibration curve was added. Two things count as a contact: reading the record
of the evaluation, `outputs/fase11/sellado_evaluado.csv`, and filtering by the
sealed group on a table held at patient level. Reading a file that merely
contains its rows among others does not, since almost every deposit does that
somewhere, and counting it would inflate the number until it meant nothing.

On that accounting the unit is touched 5 times, in 3 classes.

    procedure                    contact
    R/36_sellado.R               predice sobre la unidad
    R/38_recalibracion.R         reutiliza lo almacenado
    R/71_calibracion_sellado.R   reutiliza lo almacenado
    R/84_curva_calibracion.R     reutiliza lo almacenado
    R/51_circularidad_glasgow.R  la describe sin predecir

predice sobre la unidad predicts on it, reutiliza lo almacenado reuses the
stored predictions, la describe sin predecir describes it without predicting.
The single prediction was made with the thresholds of the original calibration
set and with that model already frozen. The three that reuse it never refit
and never predict again: one resamples the unit at each of 7 local sizes and
recomputes thresholds only, another deposits the per-class calibration this
document reports below, and the third the calibration curve. The last
describes the distribution of the consciousness scale in the unit, to decide
whether the variable could enter the extension at all. The extended model was
never evaluated there.

`R/22_particion.R` filters that same group and is not counted, for a reason
read off the code rather than asserted: it writes the file it filters. Whoever
writes the label defines it, and stands upstream of every evaluation; whoever
reads it afterwards uses it.

The partition also wrote the sealed rows to a file of their own, whose name
asks that it not be opened. That no procedure reads it is checkable rather
than promised: 2 procedures name the file and 0 read it, and the check is a
single command, recorded here with the output it gives:

    git grep -l -- 'outputs/fase5/SELLADO_NO_ABRIR.csv' -- 'R/*.R'
    R/22_particion.R
    R/89_contactos_con_el_sellado.R

## Principal results

### Discrimination and conformal coverage

    class              AUC    coverage

    sin_crecimiento   0.6521     0.9044
    urinario          0.6518     0.8881
    respiratorio      0.6597     0.8984
    sangre            0.7090     0.8679

Coverage ranges from 0.8679 to 0.9044 against a nominal level of 0.90, with
confidence intervals containing the nominal value in all four categories.

Those intervals are conditional on the conformal threshold, which is itself
estimated from a finite calibration set. Incorporating that uncertainty would
widen them, as it does in the transportability section below, where the
thresholds are re-estimated within each fold. Widening does not on its own
preserve a containment, since a wider interval can also be shifted, and one
cell of the sealed unit is shifted in exactly that way. So the property is
checked on these classes and not carried over from another section: the
deposit carries the Beta-Binomial interval for each of the 4, computed from
the same calibration sizes, and it is between 1.26 and 1.28 times as wide. All
4 contain the nominal level, as all 4 of the intervals conditioned on the
threshold do. The same equality holds across every cell the deposit carries:
12 of the 28 contain the nominal level under one interval and under the other
alike.


### Calibration

    class                 n   observed   predicted

    sin_crecimiento   2972     0.8874      0.8884
    urinario           143     0.0427      0.0419
    respiratorio       128     0.0382      0.0384
    sangre             106     0.0317      0.0314

Mean predicted probability tracks observed frequency to within 0.0010 in every
class on the test set. That set comes from the same random partition as the
training data, so agreement there is what a correctly fitted model should
produce and is not evidence that it would hold elsewhere.

Within each class the test set was split into 5 equal bins of predicted
probability, and observed frequency tracks predicted probability across the
range and not only in the mean. The calibration slopes run from 0.8169 to
1.0381 and the calibration-in-the-large terms from -0.0103 to 0.0208, every
interval covering the value that means no compression and no shift.

That is absence of evidence of miscalibration, not evidence of good
calibration. The intervals are wide at these class sizes: the widest slope
runs from 0.5666 to 1.1197, so a moderate compression would not be detected.
The bins and the coefficients are in `outputs/fase35/curva_calibracion.csv`
and `outputs/fase35/pendiente_calibracion.csv`.

### The argmax rule cannot name a source

The four class probabilities sum to one in every stay, so if the majority
class probability never falls below a value, no other class can reach one
minus that value. On the test set the majority class never falls below 0.5007,
which caps every minority class at 0.4993. On the sealed unit it never falls
below 0.5698, capping them at 0.4302. In both sets the cap lies below the
floor, so the majority class is the maximum in every stay and the argmax rule
cannot name a minority source. That is a bound and not an observation: it
holds whatever the minority probabilities turn out to be, and it rests on one
figure per set.

The observed maxima are further below still. No minority probability exceeds
0.3331 on the test set or 0.3804 on the sealed unit. The margin the bound
leaves is 0.0014 on the test set and 0.1396 on the sealed unit, so on the test
set the model comes close to admitting a different maximum without ever
producing one.

The model attains high apparent accuracy while identifying no source at all.
The same behaviour was observed under both penalty rules, under gradient
boosting and under the extended model; the bound above is measured for the
final model on the two sets it was evaluated on.

What a genuine case receives is the clinical form of the same fact. Among the
test-set stays whose source is urinary, the median probability the model
assigns to the urinary class is 0.0456, against a prevalence of 0.0427, and
the highest any of them receives is 0.1141.

The model is calibrated and discriminates weakly: its probabilities are honest
and almost flat. A real urinary case is told it is barely more likely to be
urinary than the base rate. That is what a correctly fitted model on
insufficient information looks like, and recalibration does not repair it.

### Confidence, resolution and error

At the ninety percent level the system resolves 9.6 percent of cases with an
error of 0.0280 among them. Loosening the level raises resolution to 69.1
percent and error to 0.3753.

Precision on minority-class conclusions peaks at 0.4000, from 5 such
conclusions in total. There is no confidence level at which the system
usefully names the source.

### Clinical utility

Decision curve analysis was computed for 4 questions and all 4 are reported.
The first is the binary one, whether any source is present. The other three
are the identification questions this work exists to answer, one for each
modelled source. Reporting only the first would report the most favourable of
the four.

    question         net benefit   at threshold    negative

    cualquier_foco        0.0222           0.11    11 of 30
    sangre                0.0087           0.03    19 of 30
    urinario              0.0071           0.04    21 of 30
    respiratorio          0.0067           0.04     4 of 30

The names are Spanish: cualquier_foco any source, urinario urinary,
respiratorio respiratory and sangre bloodstream. Net benefit is the maximum
gain over the better of the two trivial policies, treat all and treat none.
The last column counts the thresholds at which the model's own net benefit
falls below zero.

The binary question reaches 0.0222 at a threshold of 0.11. The three
identification questions reach between 0.0067 and 0.0087. That the questions
this work poses do worse than the one it does not is the result and not an
inconvenience: it is what the discrimination and the conformal sets already
show, arriving by a third route.

The benefit concentrates at low thresholds and does not converge to zero above
them: it crosses it. For the binary question the model's net benefit is below
zero at 11 of the 30 thresholds examined, from 0.20 upward, reaching -0.00231.
The deposit marks those rows as not useful, and it is
`outputs/fase16/curvas_decision.csv`.


### Transportability

Under leave-one-unit-out validation, marginal coverage ranges from 0.8036 to
0.9612 with a mean of 0.8944, below the nominal level. Of the 5 units, 2 fall
below nominal on that measure.

The conditional guarantee, which is the one this work claims, does not hold:
it fails in 3 of the 5 units. The classes that fail are not the same
everywhere. Of the units that fail, 2 fail on sin_crecimiento and 1 on
respiratorio. The remaining 2 show point coverage of 0.7818 and 0.7586 on 55
and 58 cases, too few to establish the shortfall. Absence of demonstration is
not evidence of compliance.

How a unit that the model has not seen is coded is a decision, not a detail.
The unit enters the model as a set of indicators, and a held-out unit has none
of its own, so its stays are scored with every unit indicator at zero. That is
the coding of whichever unit comes first alphabetically among those the fold
was fitted on. The alternative, dropping the unit term for this validation,
was not evaluated, so the coverage reported here is conditional on that
choice.

The 5 cells whose interval falls entirely below nominal are listed below, and
the 3 that survive the correction are marked in the last column:

    unit         class                 n   cover   interval          survives

    CCU          sin_crecimiento   1608  0.8004  0.7767 to 0.8227  yes
    MICU         sin_crecimiento   4686  0.7757  0.7570 to 0.7937  yes
    MICU/SICU    respiratorio       160  0.7500  0.6442 to 0.8398  yes
    SICU         respiratorio        58  0.7586  0.6119 to 0.8726  no
    TSICU        respiratorio        55  0.7818  0.6348 to 0.8917  no

How that count was reached. The guarantee is assessed class by class within
each unit, which gives 20 cells, and two things shape how they are read. The
cells are corrected jointly for multiplicity, since at the conventional level
of 0.05 the expected number of false positives is 1.0 under the hypothesis
that every cell meets nominal. And the conformal threshold is not a known
quantity: it is re-estimated inside each fold from a finite calibration set,
which makes the covered count beta-binomial rather than binomial. Treating it
as binomial credits the evidence with a precision it does not have.

Of the 20 cells, 5 have their whole interval below nominal and 3 survive the
correction. The count does not depend on the choice of correction or level,
under the test that recognises the calibration:

    correction                 level   cells

    beta-binomial bonferroni   0.050       3
    beta-binomial holm         0.050       3
    beta-binomial bonferroni   0.025       3
    beta-binomial holm         0.025       3

Under the test that treats the threshold as known, 4 cells would survive under
Holm's correction at either level. The intervals above incorporate the
calibration uncertainty; conditioning on the threshold instead narrows them by
between 9.69 and 34.33 percent. All the cells of this section and of the
sealed-unit section below are in `outputs/fase26/intervalos_cobertura.csv`.

The interval and the test are anchored slightly differently. The interval is
inverted against the nominal level exactly; the test is taken against the mean
coverage the procedure targets, which the ceiling in the conformal quantile
places marginally above nominal. Both are reported, and the deposit records
where they coincide: on all 20 cells of the family.

### The sealed unit

Evaluated once, without recalibration, and read by the criterion the
transportability section uses: the Beta-Binomial interval, which is not the
one the test-set table above reports.

    class                 n   cover   interval

    sin_crecimiento   4495  0.9911  0.9865 to 0.9946
    urinario            52  0.8654  0.7285 to 0.9508
    respiratorio        13  0.8462  0.5367 to 0.9826
    sangre              26  0.9615  0.7918 to 0.9990

The majority class is covered above nominal. No minority class has an interval
falling below it, so this section reports no conclusion about the conditional
guarantee here: the cases are too few to establish a shortfall in either
direction. The over-coverage of the majority class is consistent with a
prevalence shift: its mean predicted probability is 0.9054 against an observed
frequency of 0.9802 over the 4,586 stays of that unit which fall in one of the
modelled classes, which is fewer than the stays it contributes in total. On
the test set the same model predicts that class to within 0.0010 of its
observed frequency, so the gap is specific to this unit rather than a property
of the model on the data it was fitted from.

Prevalence shift is not the only mechanism that would produce over-coverage
there. A unit measured less completely would be predicted with more
imputation, and imputation pulls predictions toward the training mean, which
the majority class dominates. That mechanism is not available here: over the
17 determinations the model uses, 60.69 percent of the sealed unit's stays are
complete against 30.34 percent of the development set, and 88.08 percent of
its cells are observed against 66.00. The unit is measured more completely,
not less, so the asymmetry runs opposite to what that explanation would need.
This work does not separate the mechanisms further.

### The limit of local recalibration

With 50 local cases an average of 1.0 of the 4 classes reaches the minimum
required for conditional calibration, and mean set size falls to 0.888: sets
are often empty and the system has collapsed to a degenerate binary
classifier. Apparent coverage near the nominal level conceals this.

### Model complexity is justified and insufficient

Parameters are the non-zero coefficients, intercepts excluded, summed over the
class blocks.

    specification         parameters   AUC on minority classes

    demografia                    8    0.5421
    tres marcadores              12    0.5910
    lineal sin unidad            62    0.6343
    modelo completo             148    0.6735

The specification names are Spanish: demografia is demographics, tres
marcadores three markers, lineal sin unidad a linear model without the unit
and modelo completo the full model.

Gradient boosted trees on the same predictors gain 0.0002 in mean area over
the penalized linear model, averaged over the 3 minority classes. They are not
uniformly better, and the average hides the spread: they improve on 1 of the 4
classes and lose on the other 3, the largest loss being -0.0096 and the single
gain 0.0153. The per-class figures are in
`outputs/fase16/gbm_comparacion.csv`.

The ceiling belongs to the information available, not to the functional form.

### The model reads clinical judgement

The indicator of whether lactate was ordered contributes 0.0134 to AUC; the
measured value contributes 0.0038. The ordering decision carries more signal
than the physiology it measures.

## Extension with vital signs

Declared as a secondary study. The sealed model is not modified.

### Two variables were excluded after measurement

**Blood pressure.** Agreement between invasive and non-invasive measurement
reaches only 0.4969 even when restricted to readings less than fifteen minutes
apart, with a median absolute discrepancy of 12 mmHg. The proportion measured
by catheter ranges from 2.5 to 80.4 percent across units.

**Glasgow Coma Scale.** A binary indicator of endotracheal intubation alone
discriminates the respiratory class at 0.7111. The scale scores 0.7313 on its
full three-component form and 0.7086 on eye plus motor. The reduced form is
the one reported here because the verbal component assigns the minimum score
to intubated patients, conflating absent response with inability to speak.
Within the intubated stratum the reduced scale falls to 0.4865, and the full
form scores the same 0.4865 there. The two forms coincide within that stratum
because the verbal component is constant there, so the full scale is the
reduced one plus a fixed offset, which leaves the ranking and therefore the
area unchanged. The scale acts as a proxy for the procedure, and the procedure
is strongly associated with whether the respiratory site is cultured at all:
72.2 percent of the respiratory cases are intubated, against 39.3 percent of
those without growth. It runs in the direction the argument needs, but it
remains an association: the data do not establish that the one determines the
other.

### Four vital signs were retained

Coverage of the table left after implausible values are blanked, which is the
one the model is fitted on, counted over the final funnel cohort. The raw
extraction is given beside it, and the difference between the columns is what
the plausibility limits discard. The figures used everywhere below are the
cleaned ones. Both stages are in
`outputs/fase28/cobertura_vitales_por_etapa.csv`.

    variable                 raw   cleaned

    temperatura             93.0      92.7
    frec_cardiaca           99.2      99.2
    frec_respiratoria       98.7      98.3
    saturacion              98.9      98.8

The variable names are Spanish: temperatura temperature, frec_cardiaca heart
rate, frec_respiratoria respiratory rate and saturacion oxygen saturation.

Their medians are not constant across units. Heart rate ranges by 16 beats per
minute and respiratory rate by 7 breaths across the six units. That variation
confounds case mix with measurement practice and this analysis cannot separate
them: a cardiac surgical unit has genuinely slower, sedated patients. It
differs from lactate, where whether the test is ordered cannot depend on its
own result, so the variation in how often it is ordered across units is
practice.

Availability also varies: temperature is recorded in 76.5 percent of stays in
one unit and 97.9 percent in another, a range of 21.4 points. Whether a
temperature is taken is less patient-dependent than what it reads, so its
missingness carries site information more clearly than its value does.

Only temperature required a flexible functional form, with a gain of 283.00
against a permutation-derived noise threshold.

The extended model gains 0.0146 over the original in mean AUC across minority
classes. It does not change the clinical verdict.

Adding the vital signs does not improve transportability. Coverage dispersion
is essentially unchanged, 0.0736 against 0.0744, and the claim rests on the
other two: the mean falls from 0.8944 to 0.8838 and the minimum from 0.8036 to
0.7757. The extended model transports no better than the original. That is
consistent with prevalence shift rather than predictor contamination as the
mechanism, but does not establish it: the extended model adds the vital signs
to the same laboratory variables rather than replacing them.

### Imputation propagates uncertainty

Fraction of missing information reaches 0.6214 for laboratory results and
stays at or below 0.0648 for vital signs.

On masked observed values the two approaches are close: median substitution
attains lower absolute error on 9 variables, multivariate imputation on 7, and
the remaining variable ties. The multivariate approach wins where
physiological correlation is high and loses where it is absent. It is retained
because its purpose is to propagate the uncertainty of the fill, which a
single substituted value cannot do.

## A reproducibility finding

Nursing observations are validated in batches, so between 25.46 and 40.05
percent of stays carry several measurements of the same variable with an
identical storetime, up to 38 at once. Each of those percentages is taken over
the stays in which that variable is recorded, which is not the same
denominator for all of them: it runs from 4,645 to 63,888 stays. Storetime is
the time a result was recorded. Selecting the first measurement by ordering on
storetime alone leaves ties unresolved, and the row retained can differ
between runs of the same query.

Laboratory results are affected in 0.10 percent of stays. Analysers timestamp
each result individually, which makes ties far rarer there but not absent. The
laboratory extraction in `R/19_matriz.R` orders by storetime alone, the same
pattern corrected here, and the re-extraction covered the vital signs only. No
versioned file bounds the divergence this leaves in the core phases.

Ordering now uses four keys: storetime, charttime (the time the measurement
was made), the value after unit conversion, and itemid (the identifier of the
measured item). The converted value earns its place: 82 groups of measurements
agree on stay_id, storetime and charttime while disagreeing on the converted
value, so the first two keys alone leave the retained row undetermined. The
query was run 3 times and all 3 results are identical.

Material divergence from the earlier extraction reaches 0.110 percent of stays
per variable, and no coverage figure moves by more than 0.1 points.

### A fixed seed does not fix a partition

The query that built the analysis matrix did not fix its row order, and the
partition assigns by row position rather than by stay. The seed therefore
permuted positions and not patients: two runs of the same code, with the same
seed, sent different stays to training, to calibration and to the test set.

The pre-fix code was recovered from the history and run twice. The content is
identical: 0 of 580,325 cells differ between the two runs and both hold the
same 23,213 stays, while 98.77 percent of the row positions change. Against
the published matrix the figures are 0 cells and 99.83 percent. With the
ordering fixed, the same comparison gives 0.00 percent.

This is a failure mode a fixed seed appears to cover and does not, which is
why a careful analyst does not look for it. It is of the same family as the
tie-breaking above, with one difference that matters: the ties moved figures,
and this moved a design decision.

No static analysis found it. It was found by running the code twice and
comparing the deposits, and the same method then found insufficient orderings
in nine further queries, four earlier audits having passed over all of them. A
guard now stops any procedure whose deposit is not totally ordered by its own
key. The evidence that such a guard is needed rather than merely tidy came
from the guard on manifests: a routine re-run left a deposit recomputed and
its manifest stating a condition that no longer held, and only a declaration
at the level of the manifest caught it.

The finding does not weaken the choice of a beta-binomial for the coverage
intervals; it supplies the mechanism that choice assumes. That model was
adopted because the conformal threshold is estimated from a finite calibration
set and is therefore not a known quantity. The calibration set is now known to
be one draw among many that a re-run would have produced differently.

What reproduces and what does not. The funnel reproduces. The analysis matrix
reproduces cell for cell. The sealed unit reproduces, because it is selected
by unit name and not by position. The partition does not reproduce under the
code as it stood, and the model therefore does not either. The chain is
reproducible from the partitioned matrix onward, and the published partition
predates the fix in `R/22_particion.R`: it is kept as a versioned artefact and
is not regenerated, since rebuilding it would produce a model whose design
decisions were taken with the sealed unit already seen.


## Requirements

R 4.6.1. Exact library versions are recorded in `renv.lock`:

    R -e 'renv::restore()'

Running any of this requires credentialed access to MIMIC-IV through PhysioNet
and a local copy of the release, which the procedures expect under
`~/mimic-data/physionet.org/files/mimiciv/3.1`. They are numbered in the order
they run: schema exploration and cohort first, then the analysis matrix and
the partition, the imputation, the model and its conformal calibration, the
validations, the extension with vital signs, and last the verification and the
documents.

The seed is 20260818 across all phases involving randomization. The cohort
extraction manifest records a different seed, which is inert: that step draws
no random numbers. The extraction phases record software versions and SHA-256
hashes of the source files in their manifests; later phases record the seed
and the analytical choices but not hashes.

The manifest of the eighth phase records the penalty to full precision, while
the procedures that reuse it read the shortened value the hyperparameter table
publishes. The replication of the transportability validation used the shorter
value and reproduced the published result, so no figure reported here is
affected.

## Limitations

Etiological classification depends on which tests were ordered, and culture
positivity varies widely across specimen types. The variation is stated over a
declared denominator rather than over the source table, where types with a
handful of cultures run from none positive to all of them: across the 7 types
that each account for at least 1 percent of the 65,317 cultures in the window,
and which together cover 93.62 percent of them, positivity runs from 5.54 to
55.54 percent, a ratio of 10.03. The retained types are in
`outputs/fase43/positividad_retenidos.csv`, and confirmation depends on prior
clinical suspicion.

Four quantities measured here carry the practice of the site rather than the
state of the patient. Whether lactate was ordered outweighs its value. A
binary indicator of intubation discriminates the respiratory class about as
well as the consciousness scale, and intubation is associated with whether
that site is cultured at all. Temperature is recorded in a quarter more of the
stays of one unit than of another. And laboratory completeness itself differs
by unit: over the determinations the model uses, the sealed unit is complete
in twice the proportion of stays that the development set is. Each is a route
by which a model fitted in one place reads where the patient is rather than
what is wrong with them.

The respiratory category may include airway colonization in ventilated
patients, which the available fields cannot distinguish from infection.

All units belong to a single tertiary academic centre, so the observed
heterogeneity is a lower bound on what separate institutions would show.

The phase that built the analysis matrix rewrote the outcome label with a
shorter tie-break ladder than the phase that sealed the definitions, and the
shorter ladder promotes the urinary site above the intra-abdominal one. The
two can differ only for a stay positive at both those sites and at neither
blood nor respiratory. No stay in the cohort is, so no label changes; the full
crosswalk is in `outputs/fase25/divergencia_etiquetado.csv`.

The sealed unit contributes 13 respiratory cases, too few to estimate
conditional coverage with useful precision.

The core phases were extracted before the tie-breaking correction. They draw
only on laboratory results, where ties are far rarer than in nursing
observations but not absent, and where the ordering carries the same defect.
The correction re-extracted the vital signs only, so the divergence the core
phases could carry is not bounded by any file. The cohort was not rebuilt
because doing so after the sealed set had been opened would void the external
validation, and the limitation therefore stands unquantified.

The study does not compare system performance against a clinician working from
the same information.

## Repository

File and line counts are omitted here: they are self-referential, so any later
commit makes them stale, and `git ls-files` reports them directly. The
self-checking procedures are:

    R/59_verificar_cifras.R
        checks reported figures against their source files
    R/64_auditoria_publicacion.R
        checks the repository is safe to publish
    R/65_generar_readme.R
        generates this document
    R/67_diagnostico_dependencias.R
        checks the lockfile covers every library the procedures load
    R/68_traduccion_yachay.R
        generates the protocol document
    R/82_decisiones_vivas.R
        generates the record of standing decisions

The other documents are [TRACEABILITY](outputs/fase20/TRACEABILITY.md), which
records every published figure against its source file,
[REPRODUCIBILITY](outputs/fase20/REPRODUCIBILITY.md), on the determinism of
the extraction, [PROTOCOLO](outputs/fase21/PROTOCOLO.md), which carries the
findings into decisions for a separate study, and [DECISIONES](DECISIONES.md),
the record of standing decisions consulted before writing: what is declared as
posterior analysis, which biases accompany which figure, which claims the
files do not support, and which figures are not comparable with which. The
last two are written in Spanish.

## Language-model assistance

Language models assisted in drafting the code and the documentation. No
patient-level data were transmitted to those tools: the PhysioNet data use
agreement prohibits it. Every procedure was executed in the project
environment under the author's direction. The published figures are checked
automatically against their source files by `R/59_verificar_cifras.R`, and the
repository is audited by `R/64_auditoria_publicacion.R`.

## License

Code under the MIT license. Data are governed by the PhysioNet data use
agreement for MIMIC-IV.
