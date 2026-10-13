# Create a fitted loadComp object.

Generates a new model of class loadComp
([`loadComp-class`](http://doi-usgs.github.io/loadflex/reference/loadComp-class.md)).
loadComps themselves contain two inner load models, one for regression
and one for interpolation of the residuals of the regression
predictions.

## Usage

``` r
loadComp(
  reg.model,
  interp.format = c("flux", "conc"),
  abs.or.rel.resids = c("absolute", "relative"),
  use.log = TRUE,
  interp.data,
  interp.function = linearInterpolation,
  store = c("data", "fitting.function"),
  n.iter = 100,
  MSE.method = "parametric"
)
```

## Arguments

- reg.model:

  The model, usually a regression model, to whose predictions the
  residuals corrections should be added.

- interp.format:

  character specifying the load format in which residuals should be
  interpolated

- abs.or.rel.resids:

  Should residuals be computed as the difference ("absolute") or the
  ratio ("relative") of the observed and predicted values?

- use.log:

  logical. Should residuals be computed in log space (TRUE) or linear
  space (FALSE)?

- interp.data:

  the dataset, possibly differing from getFittingData(reg.model), from
  which regression residuals will be calculated and interpolated.

- interp.function:

  a function accepting args dates.in, y.in, and dates.out and returning
  y.out. See
  [interpolations](http://doi-usgs.github.io/loadflex/reference/interpolations.md)
  for pre-defined options, or write your own having the same form.

- store:

  One or more character strings specifying which information to write
  within the model. Options are 'data': the original fitting data;
  'fitting.function': a fitting function that can produce a new loadComp
  object from new data (this currently uses the same new data for both
  regression calibration and interpolation); 'uncertainty': an estimate
  of uncertainty, which can take some time to compute but will permit
  creation of uncertainty intervals, etc. in the prediction and
  aggregation phases.

- n.iter:

  The number of times to repeat the COMPLETE process of \[simulate
  predictions from the regression model and do leave-one-out cross
  validation (for all interpolation data points)\]. Each run through the
  process generates one estimate of the MSE, from which a mean and SD of
  the MSE estimates will be returned.

- MSE.method:

  character. The method by which the model should be bootstrapped.
  "non-parametric": resample with replacement from the original fitting
  data, refit the model, and make new predictions. "parametric":
  resample the model coefficients based on the covariance matrix
  originally estimated for those coefficients, then make new
  predictions.

## Value

A fitted loadComp model.

## See also

Other load.model.inits:
[`loadInterp()`](http://doi-usgs.github.io/loadflex/reference/loadInterp.md),
[`loadLm()`](http://doi-usgs.github.io/loadflex/reference/loadLm.md),
[`loadModel()`](http://doi-usgs.github.io/loadflex/reference/loadModel.md),
[`loadReg2()`](http://doi-usgs.github.io/loadflex/reference/loadReg2.md)
