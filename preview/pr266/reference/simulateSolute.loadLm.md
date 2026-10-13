# Produce a set of predictions that reflect the coefficient uncertainty and natural variation.

This function resamples the coefficients from their joint distribution,
then makes predictions whose individual errors are sampled from a time
series with the same first-order autocorrelation as the original series
of errors.

## Usage

``` r
# S3 method for class 'loadLm'
simulateSolute(
  load.model,
  flux.or.conc = c("flux", "conc"),
  newdata,
  method = c("parametric", "non-parametric"),
  from.interval = c("confidence", "prediction"),
  rho,
  ...
)
```

## Arguments

- load.model:

  A loadLm object.

- flux.or.conc:

  character. Should the simulations be reported as flux rates or
  concentrations?

- newdata:

  `data.frame`, optional. Predictor data. Column names should match
  those given in the `loadLm` metadata. If `newdata` is not supplied,
  the original fitting data will be used.

- method:

  character. The method by which the model should be bootstrapped.
  "non-parametric": resample with replacement from the original fitting
  data, refit the model, and make new predictions. "parametric":
  resample the model coefficients based on the covariance matrix
  originally estimated for those coefficients, then make new
  predictions.

- from.interval:

  character. The interval type from which to resample (simulate) the
  solute. If "confidence", the regression model coefficients are
  resampled from their multivariate normal distribution and predictions
  are made from the new coefficients. If "prediction", an additional
  vector of noise is added to those "confidence"-based predictions.

- rho:

  An autocorrelation coefficient to assume for the residuals, applicable
  when from.interval=="prediction". If rho is missing and
  interval=="prediction", rho will be estimated from the residuals
  calculated from newdata with the fitted (not yet resampled)
  load.model.

- ...:

  Other arguments passed to inheriting methods

## Value

A vector of data.frame of predictions, as for the generic
[`predictSolute`](http://doi-usgs.github.io/loadflex/reference/predictSolute.md).

A vector of predictions that are distributed according to the
uncertainty of the coefficients and the estimated natural variability +
measurement error.

## See also

Other simulateSolute:
[`simulateSolute()`](http://doi-usgs.github.io/loadflex/reference/simulateSolute.md),
[`simulateSolute.loadModel()`](http://doi-usgs.github.io/loadflex/reference/simulateSolute.loadModel.md),
[`simulateSolute.loadReg2()`](http://doi-usgs.github.io/loadflex/reference/simulateSolute.loadReg2.md)
