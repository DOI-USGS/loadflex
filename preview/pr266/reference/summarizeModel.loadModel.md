# Extract model summary statistics from a loadModel model

Produce a 1-row data.frame of model metrics. The relevant metrics for
loadModel models include two sets of statistics about autocorrelation
(one for the regression residuals, one for the 'residuals' used to do
the composite correction).

## Usage

``` r
# S3 method for class 'loadModel'
summarizeModel(load.model, ...)
```

## Arguments

- load.model:

  A load model object, typically inheriting from loadModel and always
  implementing the loadModelInterface.

- ...:

  Other arguments passed to model-specific methods

## Value

A 1-row data.frame of model metrics

## See also

Other summarizeModel:
[`summarizeModel()`](http://doi-usgs.github.io/loadflex/reference/summarizeModel.md),
[`summarizeModel.loadComp()`](http://doi-usgs.github.io/loadflex/reference/summarizeModel.loadComp.md),
[`summarizeModel.loadInterp()`](http://doi-usgs.github.io/loadflex/reference/summarizeModel.loadInterp.md),
[`summarizeModel.loadLm()`](http://doi-usgs.github.io/loadflex/reference/summarizeModel.loadLm.md),
[`summarizeModel.loadReg2()`](http://doi-usgs.github.io/loadflex/reference/summarizeModel.loadReg2.md)
