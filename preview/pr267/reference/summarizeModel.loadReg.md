# Extract model summary statistics from an [`rloadest::loadReg()`](https://rdrr.io/pkg/rloadest/man/loadReg.html) model

Produce a 1-row data.frame of model metrics. The relevant metrics for
loadReg models are largely the same as those reported by the `rloadest`
package, though reported in this streamlined data.frame format for bulk
reporting. `summarizeModel.loadReg` should rarely be accessed directly;
instead, call
[`summarizeModel()`](http://doi-usgs.github.io/loadflex/reference/summarizeModel.md)
on a [loadReg](https://rdrr.io/pkg/rloadest/man/loadReg.html) object.

## Usage

``` r
# S3 method for class 'loadReg'
summarizeModel(load.model, flux.or.conc = c("flux", "conc"), ...)
```

## Arguments

- load.model:

  A load model object, typically inheriting from loadModel and always
  implementing the loadModelInterface.

- flux.or.conc:

  character. Which internal model (the flux model or the concentration
  model) should be summarized? An
  [rloadest::loadReg](https://rdrr.io/pkg/rloadest/man/loadReg.html)
  model is actually two different models for (1) flux and (2)
  concentration, each fitted to the same data and with the same model
  structure except for whether the left-hand side of the model formula
  is flux or concentration. Some of the model metrics differ between
  these two internal models.

- ...:

  Other arguments passed to model-specific methods

## Value

Returns a 1-row data frame with the following columns:

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
