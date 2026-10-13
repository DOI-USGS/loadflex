# The fraction of prediction that is due to a correction.

Computes the fraction of total concentration or flux prediction that is
attributable to a correction such as the residuals correction of
composite method models.

[`loadComp`](http://doi-usgs.github.io/loadflex/reference/loadComp.md)
models: The correction fraction for a composite method (`loadComp`)
model is defined here as:

\$\$CF = (\sum \|R\| \Delta t) / (\sum L \Delta t)\$\$

where \\\sum\\ indicates a sum over all predictions, \\L\\ is the vector
of composite predictions of flux or concentration, \\R\\ is the vector
of interpolated residuals in the same format (flux or concentration,
non-log space) as \\L\\, and \\\Delta t\\ is the vector of time periods
represented by the predictions.

## Usage

``` r
getCorrectionFraction(
  load.model,
  flux.or.conc = c("flux", "conc"),
  newdata,
  ...
)

# S3 method for class 'loadComp'
getCorrectionFraction(
  load.model,
  flux.or.conc = c("flux", "conc"),
  newdata,
  na.rm = FALSE,
  ...
)
```

## Arguments

- load.model:

  The load model from which to pull the correction fraction.

- flux.or.conc:

  character. Should the metric be computed with respect to predictions
  of flux rate or concentration?

- newdata:

  The data for which to compute the correction fraction, usually
  containing daily or instantaneous discharge.

- ...:

  Other arguments passed to class-specific implementations of
  getCorrectionFraction.

- na.rm:

  logical. Should predictions with NA values be excluded?

## Value

A value between 0 and 1 indicating the fraction of total load that is
attributed to a correction.

## See also

Other diagnostics:
[`estimateRho()`](http://doi-usgs.github.io/loadflex/reference/estimateRho.md),
[`residDurbinWatson()`](http://doi-usgs.github.io/loadflex/reference/residDurbinWatson.md)
