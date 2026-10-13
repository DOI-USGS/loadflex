# Extract metadata from a load model.

A function in the loadModelInterface. Returns a load model's metadata,
encapsulated as a `metadata` object.

## Usage

``` r
getMetadata(load.model)
```

## Arguments

- load.model:

  A load model, implementing the loadModelInterface, for which to return
  the metadata

## Value

Object of class "metadata" with slots reflecting the metadata for
load.model

## See also

Other loadModelInterface:
[`estimateMSE()`](http://doi-usgs.github.io/loadflex/reference/estimateMSE.md),
[`getFittedModel()`](http://doi-usgs.github.io/loadflex/reference/getFittedModel.md),
[`getFittingData()`](http://doi-usgs.github.io/loadflex/reference/getFittingData.md),
[`getFittingFunction()`](http://doi-usgs.github.io/loadflex/reference/getFittingFunction.md),
[`predictSolute()`](http://doi-usgs.github.io/loadflex/reference/predictSolute.md),
[`simulateSolute()`](http://doi-usgs.github.io/loadflex/reference/simulateSolute.md),
[`summarizeModel()`](http://doi-usgs.github.io/loadflex/reference/summarizeModel.md),
[`validLoadModelInterface()`](http://doi-usgs.github.io/loadflex/reference/validLoadModelInterface.md)

Other getMetadata:
[`getMetadata.loadModel()`](http://doi-usgs.github.io/loadflex/reference/getMetadata.loadModel.md),
[`getMetadata.loadReg()`](http://doi-usgs.github.io/loadflex/reference/getMetadata.loadReg.md)
