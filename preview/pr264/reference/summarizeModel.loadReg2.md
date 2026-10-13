# Extract model summary statistics from a loadReg2 model

Produce a 1-row data.frame of model metrics. The relevant metrics for
loadReg2 models are largely the same as those reported by the `rloadest`
package, though reported in this streamlined data.frame format for bulk
reporting. `summarizeModel.loadReg2` should rarely be accessed directly;
instead, call
[`summarizeModel()`](http://doi-usgs.github.io/loadflex/reference/summarizeModel.md)
on a
[loadReg2](http://doi-usgs.github.io/loadflex/reference/loadReg2.md)
object.

## Usage

``` r
# S3 method for class 'loadReg2'
summarizeModel(load.model, ...)
```

## Arguments

- load.model:

  A load model object, typically inheriting from loadModel and always
  implementing the loadModelInterface.

- ...:

  Other arguments passed to model-specific methods

## Value

Returns a 1-row data frame with the following columns:

- `site.id` - the unique identifier of the site, as in
  [`metadata()`](http://doi-usgs.github.io/loadflex/reference/metadata.md)

- `constituent` - the unique identifier of the constituent, as in
  [`metadata()`](http://doi-usgs.github.io/loadflex/reference/metadata.md)

- `eqn` - the regression equation, possibly in the form
  `const ~ model(x)` where `x` is the `Number` of a pre-defined equation
  in [rloadest::Models](https://rdrr.io/pkg/rloadest/man/Models.html)

- `RMSE` - the square root of the mean squared error. Errors will be
  computed from either fluxes or concentrations, as determined by the
  value of `pred.format` that was passed to
  [`loadReg2()`](http://doi-usgs.github.io/loadflex/reference/loadReg2.md)
  when this model was created

- `r.squared` - the r-squared value, generalized for censored data,
  describing the amount of observed variation explained by the model

- `p.value` - the p-value for the overall model fit

- `cor.resid` - the serial correlation of the model residuals

- `PPCC` - the probability plot correlation coefficient measuring the
  normality of the residuals

- `Intercept`, `lnQ`, `lnQ2`, `DECTIME`, `DECTIME2`, `sin.DECTIME`,
  `cos.DECTIME`, etc. - the fitted value of the intercept and other
  terms included in this model (list differs by model equation)

- `Intercept.SE`, `lnQ.SE`, `lnQ2.SE`, `DECTIME.SE`, `DECTIME2.SE`,
  `sin.DECTIME.SE`, `cos.DECTIME.SE`, etc. - the standard error of the
  fitted estimates of these terms

- `Intercept.p.value`, `lnQ.p.value`, `lnQ2.p.value`, `DECTIME.p.value`,
  `DECTIME2.p.value`, `sin.DECTIME.p.value`, `cos.DECTIME.p.value` - the
  p-values for each of these model terms

## See also

Other summarizeModel:
[`summarizeModel()`](http://doi-usgs.github.io/loadflex/reference/summarizeModel.md),
[`summarizeModel.loadComp()`](http://doi-usgs.github.io/loadflex/reference/summarizeModel.loadComp.md),
[`summarizeModel.loadInterp()`](http://doi-usgs.github.io/loadflex/reference/summarizeModel.loadInterp.md),
[`summarizeModel.loadLm()`](http://doi-usgs.github.io/loadflex/reference/summarizeModel.loadLm.md),
[`summarizeModel.loadModel()`](http://doi-usgs.github.io/loadflex/reference/summarizeModel.loadModel.md)

## Examples

``` r
if (FALSE) { # \dontrun{
library(rloadest)
no3_lr <- suppressWarnings(
  loadReg2(loadReg(NO3 ~ model(9), data=get(data(lamprey_nitrate)),
  flow="DISCHARGE", dates="DATE", time.step="instantaneous",
  flow.units="cfs", conc.units="mg/L", load.units="kg",
  station='Lamprey River, NH')))
summarizeModel(no3_lr)
} # }
```
