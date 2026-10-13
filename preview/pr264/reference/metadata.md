# Create or modify the metadata for a load model.

`metadata` creates a new metadata object (see
[metadata-class](http://doi-usgs.github.io/loadflex/reference/metadata-class.md))
from scratch.

## Usage

``` r
metadata(
  constituent,
  flow,
  load.rate = "",
  dates,
  conc.units,
  flow.units,
  load.units,
  load.rate.units,
  station = "",
  custom = NULL,
  validate = TRUE,
  consti.name = "",
  site.name = "",
  site.id = "",
  lat = NA,
  lon = NA,
  basin.area = NA,
  flow.site.name = site.name,
  flow.site.id = site.id,
  flow.lat = lat,
  flow.lon = lon,
  flow.basin.area = basin.area,
  basin.area.units = "km^2"
)

updateMetadata(metadata, new.metadata = NA, ..., validate = TRUE)
```

## Arguments

- constituent:

  character. The name of the solute or suspended material whose load is
  to be modeled. Also the name of the data.frame column describing that
  solute or material.

- flow:

  character. The name of the data.frame column describing flow
  (discharge)

- load.rate:

  character. The name of the data.frame column, if it exists, describing
  instantaneous load

- dates:

  character. The name of the data.frame column describing date and/or
  time

- conc.units:

  character. The units in which concentration is both input and
  exported.

- flow.units:

  character. The units in which flow is both input and exported.

- load.units:

  character. The units in which load (flux) is both input and exported.

- load.rate.units:

  character. The units in which load (flux) is reported by
  predict.loadModel().

- station:

  deprecated. character. A description of the sampling station or site.
  It's now recommended to use `site.name` and/or `site.id` instead.

- custom:

  ANY. Empty by default, but may be modified to store any additional
  data the user wants to track.

- validate:

  logical. If TRUE, validObject() must pass for the object to return.

- consti.name:

  character. Consituent long name, for use in plots and reports

- site.name:

  character. A description of the station or site where the
  concentration of the constituent was measured. This argument or
  `site.id` is a better place for a site identifier than the `station`
  argument, which is deprecated.

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

- metadata:

  the metadata object to update.

- new.metadata:

  Optional object of class "metadata". If new.metadata is not NA, all
  elements of `...` will be ignored and metadata will be updated with
  any non-empty elements of new.metadata.

- ...:

  named arguments describing metadata elements to be modified; the names
  may be any of those accepted by `metadata()`.

## Value

`metadata` returns a new metadata object with the specified entries.

`updateMetadata` returns a new metadata object that combines the values
in the initial `metadata` with EITHER the non-empty elements of
`new.metadata` OR the elements specified in `...`, depending on whether
`new.metadata` is provided.

## Details

By default, newly created metadata objects are checked for validity.
This check may be bypassed by setting `validate=FALSE`. However, except
where the user is only interested in using metadata objects in isolation
(i.e., outside a load model), this option will rarely be appropriate.

### Valid units options, loadflex style

Models used in
[loadflex](http://doi-usgs.github.io/loadflex/reference/loadflex.md)
require metadata with units in a standardized form. For any model not
produced using rloadest functions, this standardized format is a
space-separated list of text units with exponents denoted with "^".
Valid examples include "mg L^-1", "ft^3 s^-1", "kg", and "colonies
d^-1". Here is the full set of possibilites, separated by units type:

`flow.units` take the form VOLUME TIME^-1.

- VOLUME: m^3, ft^3, dL, L

- TIME: s, d, y

`conc.units` take the form MASS/VOLUME or COUNT/VOLUME.

- MASS: ng, ug, mg, g, kg, Mg, lb, ton

- COUNT: colonies, million_colonies

- VOLUME: m^3, ft^3, dL, L

`load.units` take the form MASS or COUNT.

- MASS: ng, ug, mg, g, kg, Mg, lb, ton

- COUNT: colonies, million_colonies

`load.rate.units` take the form MASS/TIME or COUNT/TIME.

- MASS: ng, ug, mg, g, kg, Mg, lb, ton

- COUNT: colonies, million_colonies

- TIME: s, d, y

\#' `basin.area.units` take the form AREA.

- AREA: m^2, ha, km^2, ft^2, ac, mi^2

For compatibility with rloadest models, `metadata` and `updateMetadata`
also accept all those units accepted by rloadest (see next section).

### Valid units options, rloadest style

Units passed to rloadest take the form "X/Y" or "X per Y" or an
abbreviation and accept a limited number of numerators and denominators.
Any valid rloadest units will be translated to valid
[loadflex](http://doi-usgs.github.io/loadflex/reference/loadflex.md)
units when supplied to
[`loadReg2`](http://doi-usgs.github.io/loadflex/reference/loadReg2.md),
`metadata` or `updateMetadata`. These rloadest-style units are currently
accepted:

`flow.units` take the form VOLUME/TIME, VOLUME per TIME, or ABV.

- VOLUME: cubic meter, cubic meters, m^3, cubic foot, cubic feet, ft^3,
  100mL, dL, liter, l, L

- TIME: second, sec, day, d, year, yr, y

- ABV: cms, cfs

`conc.units` take the form MASS/VOLUME or COUNT/VOLUME.

- MASS: nanograms, micrograms, milligrams, grams, kilograms, metric
  tons, ng, ug, mg, g, kg, Mg, tons, pounds, lbs, lb

- COUNT: col, colonies, million colonies

- VOLUME: cubic meter, cubic meters, m^3, cubic foot, cubic feet, ft^3,
  100mL, dL, liter, l, L

`load.units` take the form MASS or COUNT.

- MASS: nanograms, micrograms, milligrams, grams, kilograms, metric
  tons, ng, ug, mg, g, kg, Mg, tons, pounds, lbs, lb

- COUNT: col, colonies, million colonies

`load.rate.units` are derived as `load.units` per day.

## See also

Other metadata:
[`metadata-getters`](http://doi-usgs.github.io/loadflex/reference/metadata-getters.md)

## Examples

``` r
md <- metadata(constituent="NO3", flow="DISCHARGE", 
  dates="DATE", conc.units="mg L^-1", flow.units="cfs", load.units="kg", 
  load.rate.units="kg d^-1", site.name="Lamprey River, NH",
  site.id="1073500", custom=list(data_source="USGS NWIS, waterdata.usgs.gov"))
md1 <- metadata(constituent="NO3", flow="DISCHARGE", 
  dates="DATE", conc.units="mg L^-1", flow.units="cfs", load.units="kg", 
  load.rate.units="kg d^-1", site.name="Lamprey River, NH")
md2 <- updateMetadata(md1, site.id="1073500", 
  custom=list(data_source="USGS NWIS, waterdata.usgs.gov"))
```
