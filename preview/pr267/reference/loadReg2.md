# Create a fitted loadReg2 object.

Generates a new model of class loadReg2
([`loadReg2-class`](http://doi-usgs.github.io/loadflex/reference/loadReg2-class.md)).
loadReg2s are wrappers for loadReg objects produced by the USGS rloadest
package. `loadReg2`s can implement the
[`loadModelInterface`](http://doi-usgs.github.io/loadflex/reference/loadModelInterface.md)
more reliably than is possible for a `loadReg` object. Some metadata
information is set within the loadReg() call; entries that can be edited
may be given as arguments to the loadReg2() call (e.g., site.id, lat,
lon, basin.area).

## Usage

``` r
loadReg2(
  load.reg,
  pred.format = c("flux", "conc"),
  store = c("data", "fitting.function"),
  consti.name = "",
  load.rate = "",
  site.id = "",
  lat = as.numeric(NA),
  lon = as.numeric(NA),
  basin.area = as.numeric(NA),
  flow.site.name = "",
  flow.site.id = "",
  flow.lat = as.numeric(NA),
  flow.lon = as.numeric(NA),
  flow.basin.area = as.numeric(NA),
  basin.area.units = "km^2",
  custom = NULL,
  ...
)
```

## Arguments

- load.reg:

  An unevaluated call to
  [`loadReg`](https://rdrr.io/pkg/rloadest/man/loadReg.html). This call
  will be parsed and evaluated within `loadReg2` to create a fully
  functional load model for use within loadflex.

- pred.format:

  character. Should predictions be made for 'flux' (load rate) or 'conc'
  (concentration)? rloadest, and therefore loadReg2, uses different
  models for flux and concentration, though fitted to the same data and
  with the same model structure except for whether the left-hand side of
  the model formula is load rate or concentration. The model specific to
  `pred.format` will be used to generate predictions.

- store:

  One or more character strings specifying which information to write
  within the model. Options are 'data': the original fitting data;
  'fitting.function': a fitting function that can produce a new loadComp
  object from new data

- consti.name:

  character. Consituent long name, for use in plots and reports

- load.rate:

  character. The name of the data.frame column, if it exists, describing
  instantaneous load

- site.id:

  character Station ID for the site where the concentration of the
  constituent was measured.

- lat:

  numeric Station latitude where concentration (and possibly also
  discharge) was measured.

- lon:

  numeric Station longitude where concentration (and possibly also
  discharge) was measured.

- basin.area:

  numeric Area of the drainage basin contributing water to the site
  where concentrations were measured.

- flow.site.name:

  character Long name of the station where flow was monitored, if
  different from where concentration was monitored (as in `site.name`).

- flow.site.id:

  character Station ID of the station where flow was monitored, if
  different from where concentration was monitored (as in `site.id`).

- flow.lat:

  numeric Latitude of the station where flow was monitored, if different
  from where concentration was monitored (as in `site.lat`).

- flow.lon:

  numeric Longitude of the station where flow was monitored, if
  different from where concentration was monitored (as in `site.lon`).

- flow.basin.area:

  numeric Area of the drainage basin contributing water to the site
  where flow was monitored, if different from where concentration was
  monitored (as in `basin.area`).

- basin.area.units:

  character Units of the values in `basin.area` and `flow.basin.area`
  (must be the same for both). Basin areas are used to compute yields as
  loads per contributing (basin) area.

- custom:

  ANY. Empty by default, but may be modified to store any additional
  data the user wants to track.

- ...:

  Other arguments passed to this model.

## Value

A fitted loadReg2 model.

## See also

Other load.model.inits:
[`loadComp()`](http://doi-usgs.github.io/loadflex/reference/loadComp.md),
[`loadInterp()`](http://doi-usgs.github.io/loadflex/reference/loadInterp.md),
[`loadLm()`](http://doi-usgs.github.io/loadflex/reference/loadLm.md),
[`loadModel()`](http://doi-usgs.github.io/loadflex/reference/loadModel.md)

## Examples

``` r
if (FALSE) { # \dontrun{
library(rloadest)
no3_lr <- suppressWarnings(
  loadReg2(loadReg(NO3 ~ model(9), data=get(data(lamprey_nitrate)),
    flow="DISCHARGE", dates="DATE", time.step="instantaneous",
    flow.units="cfs", conc.units="mg/L", load.units="kg",
    station='Lamprey River, NH')))
} # }
```
