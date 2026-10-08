# Extract model summary statistics

summarizeModel produces a 1-row data.frame of model metrics. The
relevant metrics vary by model type; only the relevant metrics are
reported for each model.

## Usage

``` r
summarizeModel(load.model, ...)
```

## Arguments

- load.model:

  A load model object, typically inheriting from loadModel and always
  implementing the loadModelInterface.

- ...:

  Other arguments passed to model-specific methods

## See also

Other loadModelInterface:
[`estimateMSE()`](http://doi-usgs.github.io/loadflex/reference/estimateMSE.md),
[`getFittedModel()`](http://doi-usgs.github.io/loadflex/reference/getFittedModel.md),
[`getFittingData()`](http://doi-usgs.github.io/loadflex/reference/getFittingData.md),
[`getFittingFunction()`](http://doi-usgs.github.io/loadflex/reference/getFittingFunction.md),
[`getMetadata()`](http://doi-usgs.github.io/loadflex/reference/getMetadata.md),
[`predictSolute()`](http://doi-usgs.github.io/loadflex/reference/predictSolute.md),
[`simulateSolute()`](http://doi-usgs.github.io/loadflex/reference/simulateSolute.md),
[`validLoadModelInterface()`](http://doi-usgs.github.io/loadflex/reference/validLoadModelInterface.md)

Other summarizeModel:
[`summarizeModel.loadComp()`](http://doi-usgs.github.io/loadflex/reference/summarizeModel.loadComp.md),
[`summarizeModel.loadInterp()`](http://doi-usgs.github.io/loadflex/reference/summarizeModel.loadInterp.md),
[`summarizeModel.loadLm()`](http://doi-usgs.github.io/loadflex/reference/summarizeModel.loadLm.md),
[`summarizeModel.loadModel()`](http://doi-usgs.github.io/loadflex/reference/summarizeModel.loadModel.md),
[`summarizeModel.loadReg2()`](http://doi-usgs.github.io/loadflex/reference/summarizeModel.loadReg2.md)
