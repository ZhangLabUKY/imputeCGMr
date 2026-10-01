# Example dataset for CGMissingData

A small multi-subject CGM dataset intended for real missing-value
imputation examples. It contains 50 deterministic missing glucose
values.

## Usage

``` r
CGMExmplDat10Pct
```

## Format

A data frame with 500 rows and 5 variables:

- USUBJID:

  Numeric subject identifier.

- SEX:

  Synthetic sex of the subject.

- LBORRES:

  Laboratory Observed Result for Glucose (numeric), with deterministic
  missing values.

- Time:

  Raw timestamp in `yyyy:mm:dd:hh:nn` format.

- AGE:

  Synthetic age in years.

- hba1c:

  Synthetic HbA1c value.

## Examples

``` r
data("CGMExmplDat10Pct")
```
