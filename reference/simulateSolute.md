# Simulate solute concentrations based on the model and model uncertainty.

This function is an optional component of the
[loadModelInterface](http://doi-usgs.github.io/loadflex/reference/loadModelInterface.md).
It is unnecessary for model fitting, assessment, and prediction except
when used in conjunction with the composite method (i.e., within a
[`loadComp`](http://doi-usgs.github.io/loadflex/reference/loadComp.md)
model).

## Usage

``` r
simulateSolute(
  load.model,
  flux.or.conc = c("flux", "conc"),
  newdata,
  method = c("non-parametric", "parametric"),
  from.interval = c("confidence", "prediction"),
  rho,
  ...
)
```

## Arguments

- load.model:

  A load model object, typically inheriting from loadModel and always
  implementing the loadModelInterface.

- flux.or.conc:

  character. Should the simulations be reported as flux rates or
  concentrations?

- newdata:

  An optional data.frame of predictor observations. The column names in
  this data.frame must match those specified in the load model's
  metadata.

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

A vector or data.frame of predictions, as for the generic
[`predictSolute`](http://doi-usgs.github.io/loadflex/reference/predictSolute.md).
The simulated predictions are distributed according to the uncertainty
of the coefficients (if from.interval=="confidence") and also the
estimated natural variability + measurement error (if
from.interval=="prediction").

## See also

Other loadModelInterface:
[`estimateMSE()`](http://doi-usgs.github.io/loadflex/reference/estimateMSE.md),
[`getFittedModel()`](http://doi-usgs.github.io/loadflex/reference/getFittedModel.md),
[`getFittingData()`](http://doi-usgs.github.io/loadflex/reference/getFittingData.md),
[`getFittingFunction()`](http://doi-usgs.github.io/loadflex/reference/getFittingFunction.md),
[`getMetadata()`](http://doi-usgs.github.io/loadflex/reference/getMetadata.md),
[`predictSolute()`](http://doi-usgs.github.io/loadflex/reference/predictSolute.md),
[`summarizeModel()`](http://doi-usgs.github.io/loadflex/reference/summarizeModel.md),
[`validLoadModelInterface()`](http://doi-usgs.github.io/loadflex/reference/validLoadModelInterface.md)

Other simulateSolute:
[`simulateSolute.loadLm()`](http://doi-usgs.github.io/loadflex/reference/simulateSolute.loadLm.md),
[`simulateSolute.loadModel()`](http://doi-usgs.github.io/loadflex/reference/simulateSolute.loadModel.md),
[`simulateSolute.loadReg2()`](http://doi-usgs.github.io/loadflex/reference/simulateSolute.loadReg2.md)
