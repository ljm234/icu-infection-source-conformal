# Conformal prediction of infection source at ICU admission

A methodological testbed built on MIMIC-IV for multiclass classification of
culture site in adults with suspected infection, using class-conditional
conformal prediction sets and an abstention mechanism.

Principal Investigator: Luis Jordan Montenegro-Calla

## Question

An adult is admitted to intensive care with suspected infection, established
by a culture drawn alongside systemic antibiotics. Using only laboratory
information available in the first six hours, can the source of infection be
identified? The system returns a set of compatible categories rather than a
single label, and abstains when the available information does not permit
discrimination.

## Data

MIMIC-IV version 3.1, obtained from PhysioNet under individual credentialing.
The source files are not redistributed and form no part of this repository.
Reproducing the analysis requires separate credentialing and approved access
to the project.

Files used from the hosp module: microbiologyevents, prescriptions, patients,
admissions, labevents, d_labitems. From the icu module: icustays.

SHA-256 hashes for every file are recorded in the per-phase manifests. The
d_micro table appears in the official hosp module documentation but is not
distributed with version 3.1. This does not affect the analysis, since
specimen and organism descriptions appear as text within microbiologyevents.

## Cohort

    Cultures on record                            1,924,289
    With an associated admission                    851,658
    From patients with a critical care stay         422,433
    First stay per patient                           65,366
    Adults                                           65,366
    Culture within the six hour window               33,045
    With associated systemic antibiotics             23,213

## Categories

    no_growth        19,688   84.8 percent
    abstention        1,476    6.4
    urinary             805    3.5
    respiratory         671    2.9
    blood               573    2.5

The abstention category groups wound, intra-abdominal, cerebrospinal fluid
and other sites whose frequency does not support conditional calibration. It
is not modelled and serves to evaluate the behaviour of the referral
mechanism.

## Design decisions

Time origin: admission to the first intensive care stay. This was adopted
after comparing performance against hospital admission, which yielded fewer
cultures within the window.

Predictor window: six hours from the origin. The temporal filter uses the
time a result was recorded rather than the time the specimen was drawn, since
a result drawn inside the window but recorded afterwards was not available at
the moment of decision.

Aggregation: first recorded value per variable. Between seventy-six and
ninety-three percent of stays carry a single determination per variable,
gasometry aside. The first value is the only estimator whose distribution
does not depend on how many determinations were made, which matters because
monitoring intensity differs across units.

Functional form: restricted cubic splines with four knots on eight variables,
selected where cross-validated gain exceeded ten deviance units and where the
gain reproduced across all twenty imputations. The threshold was derived from
a null distribution built by permuting the outcome, which places noise at 6.5
units.

Imputation: chained equations with predictive mean matching, twenty datasets
and ten iterations, estimated on the training set alone through the ignore
argument. Unit enters as a predictor because the proportion of missing
lactate ranges from thirteen to eighty-one percent across units. The outcome
is deliberately excluded from the imputation model, since the system must
operate on patients whose class is unknown.

Model: penalized multinomial logistic regression with an elastic net penalty
over forty design columns. Mixing and regularization parameters were selected
by ten-fold cross-validation with folds assigned by patient. The minimum
lambda rule was adopted following a preregistered comparison against the one
standard error rule. The final model retains one hundred and forty-eight
nonzero coefficients.

Conformal prediction: the nonconformity score is the complement of the
predicted probability, with a threshold computed separately within each class
on an independent calibration set. The finite sample correction uses the
quantile of order ceil((n+1)(1-alpha))/n.

Partition: the cardiovascular unit was sealed in full and left unexamined
until development concluded. Its selection rested on four independent
measures of dissimilarity from the rest of the network. The remaining stays
were split by class into fifty, thirty and twenty percent.

## Execution order

    R/01_verificar_esquema.R      actual schema of microbiologyevents
    R/02_contar.R                 table volume
    R/03_hadm_nulos.R             cultures without an admission
    R/04_con_uci.R                cultures from patients with a critical stay
    R/05_hora.R                   availability of specimen collection time
    R/06_verificar_hora.R         verification of missing values
    R/07_ventana.R                temporal distribution of cultures
    R/08_primera_uci.R            comparison of time origins
    R/09_tipos_muestra.R          specimen types within the window
    R/10_lista_completa.R         full catalogue of specimen types
    R/11_auditoria.R              true volume and multisite structure
    R/12_ver_prescripciones.R     prescriptions schema
    R/13_buscar_antibioticos.R    identification of systemic antibiotics
    R/14_cohorte.R                cohort construction
    R/15_ver_labs.R               laboratory dictionary
    R/16_cobertura_labs.R         availability by test
    R/17_patron_solicitud.R       lactate ordering pattern
    R/18_repeticion.R             repeat testing within the window
    R/19_matriz.R                 analysis matrix
    R/20_plausibilidad.R          screening for implausible values
    R/21_perfil_unidades.R        between-unit heterogeneity
    R/22_particion.R              partition with a sealed unit
    R/24_diagnostico_logs.R       imputation diagnostics
    R/26_diagnostico_unidad.R     cause of unit removal from the imputer
    R/27_imputacion_final.R       multiple imputation
    R/28_examen_previo.R          nonlinearity and collinearity
    R/29_umbral_splines.R         derivation of the spline threshold
    R/30_modelo.R                 initial fit
    R/31_diagnostico_modelo.R     discrimination and calibration
    R/32_comparar_lambda.R        comparison of penalty rules
    R/33_conformal.R              final model and conformal sets
    R/34_curva_alfa.R             confidence, resolution and error tradeoff
    R/35_louo.R                   cross-unit transportability
    R/36_sellado.R                evaluation on the sealed unit
    R/37_validar_imputacion.R     imputation accuracy

Scripts 23 and 25 correspond to superseded imputation specifications and were
removed. The diagnostics that led to their replacement are retained in 24 and
26.

## Requirements

R 4.6.1. Exact versions for all eighty-six libraries are recorded in
renv.lock and restored with:

    R -e 'renv::restore()'

Data paths are declared at the top of each script and default to
~/mimic-data/physionet.org/files/mimiciv/3.1.

## Reproducibility

Every reported figure comes from a versioned query. Manifests in outputs
record the random seed, software and library versions, and SHA-256 hashes of
the source files used.

The seed is 20260818 across all phases involving randomization.

## Principal results

Discrimination, area under the curve on the test set: 0.652 for no growth,
0.652 for urinary, 0.660 for respiratory and 0.709 for blood.

Class-conditional conformal coverage at a nominal ninety percent: between
0.868 and 0.904 depending on class, with confidence intervals containing the
nominal value in all four categories.

The argmax rule never assigns a minority category, regardless of penalty
rule. At three percent prevalence no minority class probability exceeds the
majority class, so accuracy reaches eighty-eight point seven percent while
the system identifies no source at all.

Varying the nominal level between 0.95 and 0.50 raises the proportion of
resolved cases from 3.6 to 69.1 percent and the error among them from 0.008
to 0.375. Precision for minority class conclusions never exceeds forty
percent at any level and falls as low as six percent.

Under leave-one-unit-out validation discrimination holds, with standard
deviations between 0.011 and 0.054, while coverage ranges from 0.804 to 0.961
with a standard deviation of 0.074. The guarantee holds on average and in no
individual unit.

On the sealed unit, without recalibration, coverage reaches 0.989 against a
nominal 0.90. The excess follows from a prevalence shift: mean predicted
probability for the majority category falls 7.5 points below its observed
frequency. Prediction sets grow accordingly and the proportion of resolved
cases drops to 3.2 percent.

Multivariate imputation improves on median substitution in seven of seventeen
variables. The advantage concentrates in variables with high physiological
correlation, between 0.83 and 0.96 for acid-base and electrolytes, and
disappears in haematological and coagulation variables, whose correlation
does not exceed 0.20.

## Limitations

Etiological classification depends on which tests were actually ordered. The
proportion of positive cultures ranges from seven percent for blood cultures
to ninety-one percent for abscess, reflecting that the probability of
confirmation depends on prior clinical suspicion.

The respiratory category may include airway colonization in mechanically
ventilated patients, which the available fields cannot distinguish from
infection.

All units belong to a single tertiary academic centre, so the observed
heterogeneity is a lower bound on what would be expected across separate
institutions.

The sealed unit contributes thirteen respiratory cases, too few to estimate
conditional coverage with useful precision. This limitation was declared
before the set was examined.

The study does not compare system performance against a clinician working
from the same information.

## License

Code under the MIT license. Data are governed by the PhysioNet data use
agreement for MIMIC-IV.
