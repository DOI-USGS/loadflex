# Get a function that can be used to refit the load.model with new data.

A function in the loadModelInterface. Takes a load.model and returns a
function to fit a new load.model that is identical in every respect
except its training data and resulting model coefficients or other
paramters. The returned function should accept exactly one argument, the
training data, and should return an object of the same class as
load.model.

## Usage

``` r
getFittingFunction(load.model)
```

## Arguments

- load.model:

  The model for which to return a new fitting function.

## Value

Object of class "function" which

## See also

Other loadModelInterface:
[`estimateMSE()`](http://doi-usgs.github.io/loadflex/reference/estimateMSE.md),
[`getFittedModel()`](http://doi-usgs.github.io/loadflex/reference/getFittedModel.md),
[`getFittingData()`](http://doi-usgs.github.io/loadflex/reference/getFittingData.md),
[`getMetadata()`](http://doi-usgs.github.io/loadflex/reference/getMetadata.md),
[`predictSolute()`](http://doi-usgs.github.io/loadflex/reference/predictSolute.md),
[`simulateSolute()`](http://doi-usgs.github.io/loadflex/reference/simulateSolute.md),
[`summarizeModel()`](http://doi-usgs.github.io/loadflex/reference/summarizeModel.md),
[`validLoadModelInterface()`](http://doi-usgs.github.io/loadflex/reference/validLoadModelInterface.md)

Other getFittingFunction:
[`getFittingFunction.loadModel()`](http://doi-usgs.github.io/loadflex/reference/getFittingFunction.loadModel.md)
