# Estimate uncertainty in a composite model.

Calculates and returns the mean squared errors (MSEs) in the units and
transformation space (e.g., log space) of the left-hand side of the
model fit equation.

## Usage

``` r
# S3 method for class 'loadComp'
estimateMSE(load.model, n.iter = 100, method = "parametric", rho, ...)
```

## Arguments

- load.model:

  A load model object, typically inheriting from loadModel and always
  implementing the loadModelInterface.

- n.iter:

  The number of times to repeat the COMPLETE process of \[simulate
  predictions from the regression model and do leave-one-out cross
  validation (for all interpolation data points)\]. Each run through the
  process generates one estimate of the MSE, from which a mean and SD of
  the MSE estimates will be returned.

- method:

  character. The method by which the model should be bootstrapped.
  "non-parametric": resample with replacement from the original fitting
  data, refit the model, and make new predictions. "parametric":
  resample the model coefficients based on the covariance matrix
  originally estimated for those coefficients, then make new
  predictions.

- rho:

  The first-order autocorrelation coefficient to assume in
  simulateSolute(regression.model, interpolation.data). If missing, rho
  will be estimated from the interpolation data, but be warned that many
  data points are needed to make this estimation with precision.

- ...:

  Other arguments passed to inheriting methods for estimateMSE

## Details

This method is leave-one-out cross validation (LOOCV) for the
interpolation and involves repeated resampling of the coefficients from
which the regression predictions and residuals are calculated.

## See also

Other estimateMSE:
[`estimateMSE()`](http://doi-usgs.github.io/loadflex/reference/estimateMSE.md),
[`estimateMSE.loadInterp()`](http://doi-usgs.github.io/loadflex/reference/estimateMSE.loadInterp.md)
