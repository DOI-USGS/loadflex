# Functions implemented by any `loadflex`-compatible load model.

Solute load models in the `loadflex` package, such as `loadModel`,
`loadReg2`, and `loadComp`, all implement a common set of core
functions. These functions are conceptually packaged as the
`loadModelInterface` defined here.

## Format

A collection of functions which any load model for use with `loadflex`
should implement.

## Functions in the interface

- [`getMetadata`](http://doi-usgs.github.io/loadflex/reference/getMetadata.md)`(load.model) { return(metadata) }`

- [`getFittingData`](http://doi-usgs.github.io/loadflex/reference/getFittingData.md)`(load.model) { return(data.frame) }`

- [`getFittingFunction`](http://doi-usgs.github.io/loadflex/reference/getFittingFunction.md)`(load.model) { return(function) }`

- [`predictSolute`](http://doi-usgs.github.io/loadflex/reference/predictSolute.md)`(load.model, flux.or.conc, newdata, interval, level, se.fit, se.pred, attach.units, ...) { return(numeric vector or data.frame) }`

## Defining new load models

Users may define additional custom load models for use with `loadflex`
as long as those models, too, implement the loadModelInterface. One easy
way to implement the interface is to write the new load model class so
that it inherits from the
[`loadModel`](http://doi-usgs.github.io/loadflex/reference/loadModel.md)
class.

If a new load model class is defined, the user may confirm that the new
class implements the loadModelInterface by running
[`validLoadModelInterface`](http://doi-usgs.github.io/loadflex/reference/validLoadModelInterface.md).
