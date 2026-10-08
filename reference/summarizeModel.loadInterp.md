# Extract model summary statistics from a loadInterp model

Produce a 1-row data.frame of model metrics. The relevant metrics for
loadInterp models include RMSE, p-values, and others TBD.

## Usage

``` r
# S3 method for class 'loadInterp'
summarizeModel(load.model, irregular.timesteps.ok = NA, ...)
```

## Arguments

- load.model:

  A load model object, typically inheriting from loadModel and always
  implementing the loadModelInterface.

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

- `RMSE.lin`- the square root of the mean squared error

- `durbin.watson` - the Durbin Watson test statistic as applied to the
  observations used to fit the interpolation model

- `rho`, `acf1`, `acf1demean`, `corlag` - measures of the
  autocorrelation of the observations used to fit the model

## See also

Other summarizeModel:
[`summarizeModel()`](http://doi-usgs.github.io/loadflex/reference/summarizeModel.md),
[`summarizeModel.loadComp()`](http://doi-usgs.github.io/loadflex/reference/summarizeModel.loadComp.md),
[`summarizeModel.loadLm()`](http://doi-usgs.github.io/loadflex/reference/summarizeModel.loadLm.md),
[`summarizeModel.loadModel()`](http://doi-usgs.github.io/loadflex/reference/summarizeModel.loadModel.md),
[`summarizeModel.loadReg2()`](http://doi-usgs.github.io/loadflex/reference/summarizeModel.loadReg2.md)
