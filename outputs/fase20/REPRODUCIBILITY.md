# Reproducibility of the extraction step

Generated on 2026-09-01 by R/64_auditoria_publicacion.R.

## Finding

Nursing observations in MIMIC-IV are validated in batches, so several
measurements of the same variable share an identical storetime, the time a
result was recorded. Selecting the first measurement by ordering on
storetime alone leaves ties unresolved, and the row retained can differ
between runs of the same query.

Ties affect between 25.46 and 40.05 percent of stays depending on the
variable, with up to 38 simultaneous measurements of a single
variable in one stay.

Laboratory results are affected in 0.10 percent of stays. Analysers
timestamp each result individually, which makes ties far rarer there, but
not absent.

## Correction

Ordering now uses four keys: storetime, then charttime (the time the
measurement was made), then the value after unit conversion, then itemid
(the identifier of the measured item). Ordering on the converted value
matters because temperature is recorded under two itemids in different
scales, so the raw figure places readings of very different temperatures
side by side.

Ties on the first two keys are real:
82 groups of measurements agree on stay_id, storetime and charttime while
disagreeing on the converted value, which is why that value enters the
ordering rather than closing it.
The query was run 3 times and all 3 results are identical.

## Impact

Material divergence from the earlier extraction reaches 0.110 percent
of stays per variable, and no coverage figure moves by more than 0.1
points. Coverage before and after is measured on the same stays.
The published figures come from the earlier extraction; the deterministic
version is provided alongside and the divergence is quantified above.

The core phases, up to and including the sealed model, draw only on
laboratory results. Their extraction in `R/19_matriz.R` orders by storetime
alone, which is the pattern corrected here, and this correction re-extracted
the vital signs only. No versioned file bounds the divergence the core
phases could carry, so that limitation stands unquantified. Their data are
not re-extracted, since rebuilding the cohort after the sealed set had been
opened would void the external validation.
