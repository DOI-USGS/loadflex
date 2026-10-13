# Extract model summary statistics from a loadComp model

Produce a 1-row data.frame of model metrics. The relevant metrics for
loadComp models include two sets of statistics about autocorrelation
(one for the regression residuals, one for the 'residuals' used to do
the composite correction).

## Usage

``` r
# S3 method for class 'loadComp'
summarizeModel(load.model, newdata, irregular.timesteps.ok = NA, ...)
```

## Arguments

- load.model:

  A load model object, typically inheriting from loadModel and always
  implementing the loadModelInterface.

- newdata:

  data.frame of data that was/will be used to predict concentration or
  load; should be the same as the `newdata` argument to `predictSolute`,
  e.g. a data.frame of daily or instantaneous dates and discharges.

- irregular.timesteps.ok:

  logical. If FALSE, this function requires that the timesteps between
  observations are identical to one another, and a plot is generated and
  an error is thrown if this requirement is not met. If TRUE, the check
  is not performed. If NA (the default), the check is performed but the
  function proceeds with a warning and no plot if the timesteps are
  found to be irregular. Tests and estimates of autocorrelation are weak
  to wrong when timesteps are irregular, but timesteps are often at
  least a bit irregular in the real world.

- ...:

  Other arguments passed to model-specific methods

## Value

Returns a 1-row data frame with the following columns:

- `site.id` - the unique identifier of the site, as in
  [`metadata()`](http://doi-usgs.github.io/loadflex/reference/metadata.md)

- `constituent` - the unique identifier of the constituent, as in
  [`metadata()`](http://doi-usgs.github.io/loadflex/reference/metadata.md)

- `RMSE.lin` or `RMSE.log` - the square root of the mean squared error,
  in log space (`RMSE.log`) if the `use.log` equalled `TRUE` in the call
  to
  [`loadComp()`](http://doi-usgs.github.io/loadflex/reference/loadComp.md)
  that created this model.

- `reg.durbin.watson` - the Durbin Watson test statistic as applied to
  the residuals from the regression model fitting process

- `reg.rho` - the autocorrelation coefficient of the residuals from the
  regression model fitting process

- `int.durbin.watson` - the Durbin Watson test statistic as applied to
  the residuals from the interpolation model fitting process. See
  [`residDurbinWatson()`](http://doi-usgs.github.io/loadflex/reference/residDurbinWatson.md)

- `int.rho` - the autocorrelation coefficient of the residuals from the
  interpolation model fitting process. See
  [`estimateRho()`](http://doi-usgs.github.io/loadflex/reference/estimateRho.md)

- `correction.frac` - the correction fraction, i.e., the fraction of
  total concentration or flux prediction that is attributable to a
  correction such as the residuals correction of composite method
  models. See
  [`getCorrectionFraction()`](http://doi-usgs.github.io/loadflex/reference/getCorrectionFraction.md)

## See also

Other summarizeModel:
[`summarizeModel()`](http://doi-usgs.github.io/loadflex/reference/summarizeModel.md),
[`summarizeModel.loadInterp()`](http://doi-usgs.github.io/loadflex/reference/summarizeModel.loadInterp.md),
[`summarizeModel.loadLm()`](http://doi-usgs.github.io/loadflex/reference/summarizeModel.loadLm.md),
[`summarizeModel.loadModel()`](http://doi-usgs.github.io/loadflex/reference/summarizeModel.loadModel.md),
[`summarizeModel.loadReg2()`](http://doi-usgs.github.io/loadflex/reference/summarizeModel.loadReg2.md)
