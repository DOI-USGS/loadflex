# Test for autocorrelation of residuals

Extracts residuals from a load.model (where residuals may be for the
calibration data or for a new set of observations). Applies
car::durbinWatsonTest to test for autocorrelation of those residuals.

## Usage

``` r
residDurbinWatson(
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

  character. The format in which residuals should be calculated

- abs.or.rel.resids:

  character. Should residuals be computed as the difference or the ratio
  of the observed and predicted values?

- use.log:

  logical. use log residuals?

- newdata:

  The data from which to compute residuals; if NULL, the original
  fitting data for load.model will be used.

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
  found to be irregular. Tests of autocorrelation are weak to wrong when
  timesteps are irregular, but timesteps are often at least a bit
  irregular in the real world.

## Value

A Durbin-Watson test statistic applied to residuals.

## See also

car::durbinWatsonTest

Other diagnostics:
[`estimateRho()`](http://doi-usgs.github.io/loadflex/reference/estimateRho.md),
[`getCorrectionFraction()`](http://doi-usgs.github.io/loadflex/reference/getCorrectionFraction.md)
