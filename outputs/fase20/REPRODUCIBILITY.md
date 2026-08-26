# Reproducibility of the extraction step

Generated on 2026-08-26 by R/64_auditoria_publicacion.R

## Finding

Nursing observations in MIMIC-IV are validated in batches, so several
measurements of the same variable share an identical storetime. Selecting
the first measurement by ordering on storetime alone leaves ties
unresolved, and the row retained can differ between runs of the same
query.

Ties affect between 25.46 and 40.05 percent of stays depending on the
variable, with up to 38 simultaneous measurements of a single
variable in one stay.

Laboratory results are largely unaffected at 0.10 percent, since
analysers timestamp each result individually.

## Correction

Ordering now uses four keys: storetime, then charttime, then the value
after unit conversion, then itemid. Ordering on the converted value
matters because temperature is recorded under two itemids in different
scales, so the raw figure places readings of very different temperatures
side by side.

Ties on the earlier keys carry equal converted values, so which row is
retained does not change the result.

## Impact

Material divergence from the earlier extraction reaches 0.110 percent
of stays per variable.
The published figures come from the earlier extraction; the deterministic
version is provided alongside and the divergence is quantified above.

The core phases, up to and including the sealed model, draw only on
laboratory results and are therefore unaffected. They are not re-extracted,
since rebuilding the cohort after the sealed set has been opened would void
the external validation.
