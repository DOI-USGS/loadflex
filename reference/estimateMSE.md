# Estimate model uncertainty algorithmically.

This function is an optional component of the
[loadModelInterface](http://doi-usgs.github.io/loadflex/reference/loadModelInterface.md).
It is unnecessary for model fitting, assessment, and prediction except
when used in conjunction with the composite method (i.e., within a
[`loadComp`](http://doi-usgs.github.io/loadflex/reference/loadComp.md)
model) or for models such as loadInterps for which the MSE cannot be
known without some estimation procedure.

## Usage

``` r
estimateMSE(load.model, ...)
```

## Arguments

- load.model:

  A load model object, typically inheriting from loadModel and always
  implementing the loadModelInterface.

- ...:

  Other arguments passed to inheriting methods for estimateMSE

## See also

Other loadModelInterface:
[`getFittedModel()`](http://doi-usgs.github.io/loadflex/reference/getFittedModel.md),
[`getFittingData()`](http://doi-usgs.github.io/loadflex/reference/getFittingData.md),
[`getFittingFunction()`](http://doi-usgs.github.io/loadflex/reference/getFittingFunction.md),
[`getMetadata()`](http://doi-usgs.github.io/loadflex/reference/getMetadata.md),
[`predictSolute()`](http://doi-usgs.github.io/loadflex/reference/predictSolute.md),
[`simulateSolute()`](http://doi-usgs.github.io/loadflex/reference/simulateSolute.md),
[`summarizeModel()`](http://doi-usgs.github.io/loadflex/reference/summarizeModel.md),
[`validLoadModelInterface()`](http://doi-usgs.github.io/loadflex/reference/validLoadModelInterface.md)

Other estimateMSE:
[`estimateMSE.loadComp()`](http://doi-usgs.github.io/loadflex/reference/estimateMSE.loadComp.md),
[`estimateMSE.loadInterp()`](http://doi-usgs.github.io/loadflex/reference/estimateMSE.loadInterp.md)
