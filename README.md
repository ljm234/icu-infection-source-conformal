# Conformal prediction of infection source at ICU admission

A methodological testbed built on MIMIC-IV for multiclass classification of
culture site in adults with suspected infection, using class-conditional
conformal prediction sets and an abstention mechanism.

Principal Investigator: Luis Jordan Montenegro-Calla

Generated on 2026-08-25 by `R/65_generar_readme.R`.
Every numeric figure below is read from a versioned results file. The
generator refuses to write a figure typed by hand: prose lines are rejected
if they contain a digit, formatted lines are rejected if the pattern carries
digits outside its substitution codes, and any line that fails to compose
halts the run rather than vanishing.

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
inspects the header of every tracked file and confirms this, and checks that
no such file appears anywhere in the commit history.

## Cohort

    p1_estancias_unicas                                  65,366
    p2_adultos                                           65,366
    p3_con_cultivo                                       33,045
    p4_sospecha_infeccion                                23,213
    p5_cohorte_final                                     23,213

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
Correlation with worst-value aggregation stays above 0.8722 for every
variable, but the shift the worst value induces tracks monitoring intensity
rather than physiology.

**Imputation.** Chained equations with predictive mean matching, twenty
datasets, ten iterations, estimated on the training set alone. The outcome
is excluded, since the system must operate on patients whose class is
unknown.

**Model.** Penalized multinomial logistic regression with an elastic net
penalty. Folds assigned by patient. Restricted cubic splines where the
cross-validated gain exceeded a threshold derived from a permutation null.

**Partition.** The cardiovascular unit was sealed in full and left
unexamined until development concluded, selected on four independent
measures of dissimilarity.

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
identifying no source at all. This holds under both penalty rules, under
gradient boosting, and under the extended model: four independent
configurations, the same result.

### Confidence, resolution and error

At the ninety percent level the system resolves 9.6 percent of cases
with an error of 0.0280 among them. Loosening the level raises
resolution to 69.1 percent and error to 0.3753.

Precision on minority-class conclusions never exceeds 0.4000 at any
level. There is no confidence level at which the system usefully names the
source.

### Clinical utility

Decision curve analysis gives a maximum net benefit of 0.0222 over the
better of the two trivial policies, for the question of whether any source
is present. The benefit concentrates at low thresholds and converges to zero
above them.

### Transportability

Under leave-one-unit-out validation, coverage ranges from 0.8036 to
0.9612, with a standard deviation of 0.0744 around a mean of 0.8944. The
guarantee holds on average and in no individual unit.

On the sealed unit, without recalibration, coverage reaches 0.9911 for
the majority class against a nominal 0.90. The excess follows from a
prevalence shift, not from a broken guarantee. Prediction sets grow and the
proportion of resolved cases falls.

### The limit of local recalibration

With 50 local cases only 1.0 class reaches the minimum required for
conditional calibration, and mean set size falls to 0.888: sets are
often empty and the system has collapsed to a degenerate binary classifier.
Apparent coverage near the nominal level conceals this.

### Model complexity is justified and insufficient

    specification         parameters   AUC on minority classes

    demografia                    8    0.5421
    tres marcadores              16    0.5910
    lineal sin unidad            62    0.6343
    modelo completo             148    0.6735

Gradient boosted trees on the same predictors gain 0.0002 over the
penalized linear model. The ceiling belongs to the information available,
not to the functional form.

### The model reads clinical judgement

The indicator of whether lactate was ordered contributes 0.0134 to AUC;
the measured value contributes 0.0038. The ordering decision carries
more signal than the physiology it measures.

## Extension with vital signs

Declared as a secondary study. The sealed model is not modified.

### Two variables were excluded after measurement

**Blood pressure.** Agreement between invasive and non-invasive measurement
reaches only 0.4969 even when restricted to readings less than fifteen
minutes apart, with a median absolute discrepancy of 12 mmHg. The
proportion measured by catheter ranges from 2.5 to 80.4 percent
across units.

**Glasgow Coma Scale.** A binary indicator of endotracheal intubation alone
discriminates the respiratory class at 0.7111, above the 0.7086 achieved
by the scale itself. Within the intubated stratum the scale falls to
0.4865. The scale acts as a proxy for the procedure, and the procedure
determines whether the respiratory site is cultured at all.

### Four vital signs were retained

    variable              coverage

    temperatura            93.0 percent
    frec_cardiaca          99.2 percent
    frec_respiratoria      98.7 percent
    saturacion             98.9 percent

These are the only variables in the project free of site-dependent bias:
their medians are effectively constant across units, in contrast with
lactate ordering, pressure measurement method and intubation.

Only temperature required a flexible functional form, with a gain of
283.00 against a permutation-derived noise threshold. Its relationship
with infection is U-shaped: both fever and hypothermia mark severity.

The extended model gains 0.0146 in mean AUC across minority classes.
It resolves a larger share of cases than the original, at the cost of a
slightly higher error rate among them, and does not change the clinical
verdict.

Coverage dispersion across units is unchanged at 0.0736 against 0.0744.
Since the vital signs are the only unbiased variable family in the project,
this rules out contaminated predictors as the cause of the transportability
failure and supports prevalence shift as the mechanism.

### Imputation propagates uncertainty

Fraction of missing information reaches 0.6214 for laboratory results
and stays at or below 0.0648 for vital signs. Multivariate imputation
loses to median substitution on individual accuracy precisely because it
does not manufacture certainty where the information is absent.

## A reproducibility finding

Nursing observations are validated in batches, so between 25.46 and
40.05 percent of stays carry several measurements of the same variable
with an identical storetime, up to 38 at once. Selecting the first
measurement by ordering on storetime alone leaves ties unresolved, and the
row retained can differ between runs of the same query.

Laboratory results are unaffected at 0.10 percent, since analysers
timestamp each result individually.

Ordering now uses four keys: storetime, charttime, the value after unit
conversion, and itemid. Three consecutive runs return identical results.
Material divergence from the earlier extraction reaches 0.110 percent
of stays per variable, and coverage is unchanged.

## Requirements

R 4.6.1. Exact library versions are recorded in `renv.lock`:

    R -e 'renv::restore()'

The seed is 20260818 across all phases involving randomization. Manifests in
`outputs` record software versions and SHA-256 hashes of the source files.

## Limitations

Etiological classification depends on which tests were ordered. Culture
positivity varies by an order of magnitude across specimen types, reflecting
that confirmation depends on prior clinical suspicion.

The respiratory category may include airway colonization in ventilated
patients, which the available fields cannot distinguish from infection.

All units belong to a single tertiary academic centre, so the observed
heterogeneity is a lower bound on what separate institutions would show.

The sealed unit contributes 13 respiratory cases, too few to estimate
conditional coverage with useful precision. This was declared before the set
was examined.

The core phases were extracted before the tie-breaking correction. They draw
only on laboratory results, which are unaffected. The cohort was not rebuilt
because doing so after the sealed set had been opened would void the
external validation.

The study does not compare system performance against a clinician working
from the same information.

## Repository

    R procedures        63
    Lines of code       6,882
    Versioned files     149

    R/59_verificar_cifras.R      checks reported figures against sources
    R/64_auditoria_publicacion.R checks the repository is safe to publish
    R/65_generar_readme.R        generates this document

## License

Code under the MIT license. Data are governed by the PhysioNet data use
agreement for MIMIC-IV.
