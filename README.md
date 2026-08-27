# Conformal prediction of infection source at ICU admission

A methodological testbed built on MIMIC-IV for multiclass classification of
culture site in adults with suspected infection, using class-conditional
conformal prediction sets and an abstention mechanism.

Principal Investigator: Luis Jordan Montenegro-Calla

Generated on 2026-08-26 by `R/65_generar_readme.R`.
Every numeric figure below is read from a versioned results file. The
generator rejects prose lines containing a digit, format patterns carrying
digits outside their substitution codes, and any line that fails to compose.

## Question

An adult is admitted to intensive care with suspected infection, established
by a culture drawn alongside systemic antibiotics. Using only information
available in the first six hours, can the source of infection be identified?
The system returns a set of compatible categories rather than a single
label, and abstains when the available information does not permit
discrimination.

## Data

MIMIC-IV version 3.1, obtained from PhysioNet under individual credentialing.
The source files are not redistributed and form no part of this repository.
Reproducing the analysis requires separate credentialing and approved access.

No patient-level derived data is versioned. `R/64_auditoria_publicacion.R`
inspects the header of every tracked CSV, lists the tracked binary objects
with their sizes, and checks that none of the protected paths appears in the
commit history.

## Cohort

    p1_estancias_unicas                                  65,366
    p2_adultos                                           65,366
    p3_con_cultivo                                       33,045
    p4_sospecha_infeccion                                23,213
    p5_cohorte_final                                     23,213

Units contributing fewer than five hundred stays were dropped at the
partition step, so the analysed cohort is smaller than the final funnel row.

## Categories

    sin_crecimiento       19,688    84.8 percent
    otro_sitio             1,070     4.6 percent
    urinario                 805     3.5 percent
    respiratorio             671     2.9 percent
    sangre                   573     2.5 percent
    herida                   340     1.5 percent
    intraabdominal            66     0.3 percent

The abstention category groups wound, intra-abdominal, cerebrospinal fluid
and other sites whose frequency does not support conditional calibration. It
is not modelled and serves to evaluate the referral mechanism.

## Design decisions

**Time origin.** Admission to the first intensive care stay, adopted after
comparing against hospital admission.

**Predictor window.** Six hours. The temporal filter uses the time a result
was recorded rather than the time the specimen was drawn, since a result
recorded afterwards was not available at the moment of decision.

**Aggregation.** First recorded value per variable. The first value is the
only estimator whose distribution does not depend on how many measurements
were taken, which matters because monitoring intensity differs across units.
Correlation with worst-value aggregation never falls below 0.8722
across the seventeen laboratory variables; the comparison does not cover
vital signs. The shift the worst value induces tracks monitoring intensity
rather than physiology.

**Imputation.** Chained equations with predictive mean matching, twenty
datasets, ten iterations, estimated on the training set alone. The outcome
is excluded, since the system must operate on patients whose class is
unknown.

**Model.** Penalized multinomial logistic regression. The elastic-net mixing
parameter was selected by cross-validation over a grid and settled on the
lasso limit, so the penalty actually fitted is lasso rather than a mixture.
Folds assigned by patient. Restricted cubic splines where the cross-validated
gain exceeded a threshold derived from a permutation null.

**Partition.** The cardiovascular unit was sealed in full. The choice was a
judgement informed by the unit profiles that `R/21_perfil_unidades.R`
computes, of which two are versioned: `outputs/fase5/distancia_unidades.csv`
and `outputs/fase5/clases_por_unidad.csv`. It was not the application of a
rule. `R/22_particion.R` names the unit as a constant and computes no
selection criterion.

The primary model was frozen before that unit was opened, and it was
evaluated there once. The extension proceeded differently: it examined
descriptive summaries of the sealed unit, among them the distribution of the
consciousness scale, in order to decide which variables to admit. The
extended model was never evaluated there.

## Principal results

### Discrimination and conformal coverage

    class              AUC    coverage

    sin_crecimiento   0.6521     0.9044
    urinario          0.6518     0.8881
    respiratorio      0.6597     0.8984
    sangre            0.7090     0.8679

Coverage ranges from 0.8679 to 0.9044 against a nominal level of 0.90,
with confidence intervals containing the nominal value in all four
categories.

### The argmax rule never names a source

At minority prevalences of a few percent, no minority class probability
exceeds the majority class. The model attains high apparent accuracy while
identifying no source at all. This was observed under both penalty rules,
under gradient boosting, and under the extended model.

### Confidence, resolution and error

At the ninety percent level the system resolves 9.6 percent of
cases with an error of 0.0280 among them. Loosening the level raises
resolution to 69.1 percent and error to 0.3753.

Precision on minority-class conclusions peaks at 0.4000, from 5 such
conclusions in total. There is no confidence level at which the system
usefully names the source.

### Clinical utility

Decision curve analysis gives a maximum net benefit of 0.0222 over the
better of the two trivial policies, for the question of whether any source
is present. The benefit concentrates at low thresholds and converges to zero
above them.

### Transportability

Under leave-one-unit-out validation, marginal coverage ranges from
0.8036 to 0.9612 with a mean of 0.8944, below the nominal level. 2 of the
5 units fall below nominal on that measure.

The conditional guarantee, which is the one this work claims, fails
in 5 of 5 units: none reaches nominal coverage across all four
classes. The classes that fail are not the same everywhere: they form
3 distinct patterns across the 5 units, so a marginal summary conceals
which classes are left uncovered where.

### The sealed unit

Evaluated once, without recalibration. Coverage by class:

    sin_crecimiento   0.9911
    urinario          0.8654
    respiratorio      0.8462
    sangre            0.9615

The majority class is over-covered while 2 of the 4 classes fall
below nominal, so by the criterion applied above the sealed unit also fails
the conditional guarantee. The over-coverage of the majority class follows
from a prevalence shift: its mean predicted probability falls below its
observed frequency.

### The limit of local recalibration

With 50 local cases an average of 1.0 of the 4 classes reaches
the minimum required for conditional calibration, and mean set size
falls to 0.888: sets are often empty and the system has collapsed to
a degenerate binary classifier. Apparent coverage near the nominal level
conceals this.

### Model complexity is justified and insufficient

Parameters are the non-zero coefficients, intercepts excluded, summed over
the class blocks.

    specification         parameters   AUC on minority classes

    demografia                    8    0.5421
    tres marcadores              12    0.5910
    lineal sin unidad            62    0.6343
    modelo completo             148    0.6735

Gradient boosted trees on the same predictors gain 0.0002 over the
penalized linear model. The ceiling belongs to the information available,
not to the functional form.

### The model reads clinical judgement

The indicator of whether lactate was ordered contributes 0.0134 to
AUC; the measured value contributes 0.0038. The ordering decision
carries more signal than the physiology it measures.

## Extension with vital signs

Declared as a secondary study. The sealed model is not modified.

### Two variables were excluded after measurement

**Blood pressure.** Agreement between invasive and non-invasive measurement
reaches only 0.4969 even when restricted to readings less than fifteen
minutes apart, with a median absolute discrepancy of 12 mmHg. The
proportion measured by catheter ranges from 2.5 to 80.4 percent
across units.

**Glasgow Coma Scale.** A binary indicator of endotracheal intubation alone
discriminates the respiratory class at 0.7111. The scale scores 0.7313
on its full three-component form and 0.7086 on eye plus motor. The
reduced form is the one reported here because the verbal component assigns
the minimum score to intubated patients, conflating absent response with
inability to speak. Within the intubated stratum the reduced scale falls to
0.4865, and the full form scores the same 0.4865 there. The two forms
coincide within that stratum because the verbal component is constant there,
so the full scale is the reduced one plus a fixed offset, which leaves the
ranking and therefore the area unchanged. The scale acts as a proxy for the
procedure, and the procedure determines whether the respiratory site is
cultured at all.

### Four vital signs were retained

    variable              coverage

    temperatura            93.0 percent
    frec_cardiaca          99.2 percent
    frec_respiratoria      98.7 percent
    saturacion             98.9 percent

Their medians are not constant across units. Heart rate ranges by
16 beats per minute and respiratory rate by 7 breaths across
the six units. That variation confounds case mix with measurement practice
and this analysis cannot separate them: a cardiac surgical unit has genuinely
slower, sedated patients. It differs from lactate, where whether the test is
ordered cannot depend on its own result, so the variation across units is
unambiguously practice.

Availability also varies: temperature is recorded in 76.5 percent of
stays in one unit and 97.9 percent in another, a range of 21.4
points. Whether a temperature is taken is less patient-dependent than what
it reads, so its missingness carries site information more clearly than its
value does.

Only temperature required a flexible functional form, with a gain of
283.00 against a permutation-derived noise threshold. Its relationship
with infection is U-shaped: both fever and hypothermia mark severity.

The extended model gains 0.0146 in mean AUC across minority classes
and resolves a larger share of cases than the original. It does not change
the clinical verdict.

Adding the vital signs does not improve transportability:
coverage dispersion is 0.0736 against 0.0744, the mean falls from 0.8944
to 0.8838, and the minimum from 0.8036 to 0.7757. The extended model
transports no better than the original. That is consistent with prevalence
shift rather than predictor contamination as the mechanism, but does not
establish it: the extended model adds the vital signs to the same laboratory
variables rather than replacing them.

### Imputation propagates uncertainty

Fraction of missing information reaches 0.6214 for laboratory results
and stays at or below 0.0648 for vital signs.

On masked observed values the two approaches are close:
median substitution attains lower absolute error on 9 variables,
multivariate imputation on 7, and the remainder tie. The
multivariate approach wins where physiological correlation is high and loses
where it is absent. It is retained because its purpose is to propagate the
uncertainty of the fill, which a single substituted value cannot do.

## A reproducibility finding

Nursing observations are validated in batches, so between 25.46 and
40.05 percent of stays carry several measurements of the same
variable with an identical storetime, up to 38 at once. Selecting
the first measurement by ordering on storetime alone leaves ties unresolved,
and the row retained can differ between runs of the same query.

Laboratory results are affected in 0.10 percent of stays. Analysers
timestamp each result individually, which makes ties far rarer there but not
absent. The laboratory extraction in `R/19_matriz.R` orders by storetime
alone, the same pattern corrected here, and the re-extraction covered the
vital signs only. No versioned file bounds the divergence that leaves in the
core phases.

Ordering now uses four keys: storetime, charttime, the value after unit
conversion, and itemid. The converted value earns its place:
82 groups of measurements agree on stay_id, storetime and charttime
while disagreeing on the converted value, so the first two keys alone leave
the retained row undetermined.
The query was run 3 times and all 3 results are identical.

Material divergence from the earlier extraction reaches 0.110 percent of
stays per variable, and no coverage figure moves by more than 0.1 points.

## Requirements

R 4.6.1. Exact library versions are recorded in `renv.lock`:

    R -e 'renv::restore()'

The seed is 20260818 across all phases involving randomization. The cohort
extraction manifest records a different seed, which is inert: that step
draws no random numbers. The extraction phases record software versions and
SHA-256 hashes of the source files in their manifests; later phases record
the seed and the analytical choices but not hashes.

## Limitations

Etiological classification depends on which tests were ordered. Culture
positivity varies by an order of magnitude across specimen types, reflecting
that confirmation depends on prior clinical suspicion.

The respiratory category may include airway colonization in ventilated
patients, which the available fields cannot distinguish from infection.

All units belong to a single tertiary academic centre, so the observed
heterogeneity is a lower bound on what separate institutions would show.

The sealed unit contributes 13 respiratory cases, too few to
estimate conditional coverage with useful precision.

The core phases were extracted before the tie-breaking correction. They draw
only on laboratory results, where ties are far rarer than in nursing
observations but not absent, and where the ordering carries the same defect.
The correction re-extracted the vital signs only, so the divergence the core
phases could carry is not bounded by any file. The cohort was not rebuilt
because doing so after the sealed set had been opened would void the
external validation, and the limitation therefore stands unquantified.

The study does not compare system performance against a clinician working
from the same information.

## Repository

File and line counts are omitted here: they are self-referential, so any
later commit makes them stale, and `git ls-files` reports them directly.
The self-checking procedures are:

    R/59_verificar_cifras.R
        checks reported figures against their source files
    R/64_auditoria_publicacion.R
        checks the repository is safe to publish
    R/65_generar_readme.R
        generates this document
    R/67_diagnostico_dependencias.R
        checks the lockfile covers every library the procedures load

## License

Code under the MIT license. Data are governed by the PhysioNet data use
agreement for MIMIC-IV.
