# Estimate the autocorrelation of a mid- to high-resolution time series

Uses arima() to estimate the first-order autocorrelation constant, rho,
for the residuals calculated from a load model and new data.

## Usage

``` r
estimateRho(
  load.model,
  flux.or.conc = c("flux", "conc"),
  abs.or.rel.resids = c("absolute", "relative"),
  use.log = FALSE,
  newdata = NULL,
  plot.acf = TRUE,
  timestep.tol = .Machine$double.eps^0.5,
  irregular.timesteps.ok = NA
)
```

## Arguments

- load.model:

  a loadModel descendant

- flux.or.conc:

  The format in which residuals should be calculated

- abs.or.rel.resids:

  Should residuals be computed as the difference or the ratio of the
  observed and predicted values?

- use.log:

  logical. use log residuals?

- newdata:

  prediction values. If this is set to NULL,
  `getFittingData(load.model)` will be used.

- plot.acf:

  logical. Should the autocorrelation function be plotted?

- timestep.tol:

  the acceptable tolerance for considering timesteps regular.

- irregular.timesteps.ok:

  logical. If FALSE, this function requires that the timesteps between
  observations are identical to one another, and a plot is generated and
  an error is thrown if this requirement is not met. If TRUE, the check
  is not performed. If NA (the default), the check is performed but the
  function proceeds with a warning and no plot if the timesteps are
  found to be irregular. Estimates of autocorrelation are weak to wrong
  when timesteps are irregular, but timesteps are often at least a bit
  irregular in the real world.

## Value

Return the rho function and the fitted model.

## Details

For the purpose of estimating prediction error for a composite method
application, this function should be called with a dataset of comparable
(or identical) resolution to that of the interpolation data.

For the purpose of estimating the covariance of prediction errors for
aggregation, this function should not be called unless newdata are at
approximately the same (or higher) temporal resolution as that of the
data to be used for load estimation. This will almost always involve
sensor data.

If you have no data with sufficient resolution to reasonably call this
function, take your pick from the established assumptions implemented in
functions such as
[`rhoEqualDates`](http://doi-usgs.github.io/loadflex/reference/correlations-1D.md)
or
[`rho1DayBand`](http://doi-usgs.github.io/loadflex/reference/correlations-1D.md),
or write your own assumptions.

## See also

Other diagnostics:
[`getCorrectionFraction()`](http://doi-usgs.github.io/loadflex/reference/getCorrectionFraction.md),
[`residDurbinWatson()`](http://doi-usgs.github.io/loadflex/reference/residDurbinWatson.md)
