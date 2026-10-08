# Extract model summary statistics from a loadLm model

Produce a 1-row data.frame of model metrics.

## Usage

``` r
# S3 method for class 'loadLm'
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

A 1-row data.frame of model metrics

## See also

Other summarizeModel:
[`summarizeModel()`](http://doi-usgs.github.io/loadflex/reference/summarizeModel.md),
[`summarizeModel.loadComp()`](http://doi-usgs.github.io/loadflex/reference/summarizeModel.loadComp.md),
[`summarizeModel.loadInterp()`](http://doi-usgs.github.io/loadflex/reference/summarizeModel.loadInterp.md),
[`summarizeModel.loadModel()`](http://doi-usgs.github.io/loadflex/reference/summarizeModel.loadModel.md),
[`summarizeModel.loadReg2()`](http://doi-usgs.github.io/loadflex/reference/summarizeModel.loadReg2.md)

## Examples

``` r
data(eg_loadLm)
summarizeModel(eg_loadLm)
#>    site.id constituent                       eqn      RMSE  r.squared   p.value
#> 1 01073500         NO3 log(NO3) ~ log(DISCHARGE) 0.3398085 0.05099499 0.1148298
#>   cor.resid      PPCC Intercept log(DISCHARGE) Intercept.SE log(DISCHARGE).SE
#> 1 0.2373998 0.9732018 -1.515114    -0.07824805    0.2562421        0.04872181
#>   Intercept.p.value log(DISCHARGE).p.value
#> 1      3.405876e-07              0.1148298
```
