# Test whether a class implements the loadModelInterface

`validLoadModelInterface` can be used to test whether the
[`loadModelInterface`](http://doi-usgs.github.io/loadflex/reference/loadModelInterface.md)
has been successfully implemented for the class of a provided object.

## Usage

``` r
validLoadModelInterface(object, stop.on.error = TRUE, verbose = TRUE)
```

## Arguments

- object:

  an object with a LoadModelInterface

- stop.on.error:

  logical. If the interface is invalid, should the function throw an
  error (TRUE) or quietly return a warning object (FALSE)?

- verbose:

  logical. turn on or off verbose messages.

## Value

TRUE if interface for given load.model is well defined; otherwise,
either throws an error (if stop.on.error=TRUE) or returns a vector of
character strings describing the errors (if stop.on.error=FALSE).

## See also

Other loadModelInterface:
[`estimateMSE()`](http://doi-usgs.github.io/loadflex/reference/estimateMSE.md),
[`getFittedModel()`](http://doi-usgs.github.io/loadflex/reference/getFittedModel.md),
[`getFittingData()`](http://doi-usgs.github.io/loadflex/reference/getFittingData.md),
[`getFittingFunction()`](http://doi-usgs.github.io/loadflex/reference/getFittingFunction.md),
[`getMetadata()`](http://doi-usgs.github.io/loadflex/reference/getMetadata.md),
[`predictSolute()`](http://doi-usgs.github.io/loadflex/reference/predictSolute.md),
[`simulateSolute()`](http://doi-usgs.github.io/loadflex/reference/simulateSolute.md),
[`summarizeModel()`](http://doi-usgs.github.io/loadflex/reference/summarizeModel.md)
