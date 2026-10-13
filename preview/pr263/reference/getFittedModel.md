# Retrieve the fitted model, if appropriate, from a loadModel load model

A function in the loadModelInterface. Takes a load.model and returns a
function to fit a new load.model that is identical in every respect
except its training data and resulting model coefficients or other
paramters. The returned function should accept exactly one argument, the
training data, and should return an object of the same class as
load.model.

## Usage

``` r
getFittedModel(load.model)
```

## Arguments

- load.model:

  The load model for which to return the inner fitted model.

## Value

Object of class "function" which

## See also

Other loadModelInterface:
[`estimateMSE()`](http://doi-usgs.github.io/loadflex/reference/estimateMSE.md),
[`getFittingData()`](http://doi-usgs.github.io/loadflex/reference/getFittingData.md),
[`getFittingFunction()`](http://doi-usgs.github.io/loadflex/reference/getFittingFunction.md),
[`getMetadata()`](http://doi-usgs.github.io/loadflex/reference/getMetadata.md),
[`predictSolute()`](http://doi-usgs.github.io/loadflex/reference/predictSolute.md),
[`simulateSolute()`](http://doi-usgs.github.io/loadflex/reference/simulateSolute.md),
[`summarizeModel()`](http://doi-usgs.github.io/loadflex/reference/summarizeModel.md),
[`validLoadModelInterface()`](http://doi-usgs.github.io/loadflex/reference/validLoadModelInterface.md)

Other getFittedModel:
[`getFittedModel.loadModel()`](http://doi-usgs.github.io/loadflex/reference/getFittedModel.loadModel.md)
