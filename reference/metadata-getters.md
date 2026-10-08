# Access information about a load model.

`getCol` locates a column of data specified by a field name (conc, flow,
flux rate, or date).

`getUnits` finds the units associated with a data type (conc, flow,
flux, or flux rate).

`getInfo` returns miscellaneous information (station, custom) about a
model.

## Usage

``` r
getCol(
  metadata,
  data,
  field = c("conc", "flow", "flux rate", "date"),
  attach.units = FALSE
)

getUnits(
  metadata,
  field = c("conc", "flow", "flux", "flux rate"),
  format = NA,
  stop.on.empty = FALSE
)

getInfo(
  metadata,
  field = c("constituent", "consti.name", "flow", "load.rate", "dates", "conc.units",
    "flow.units", "load.units", "load.rate.units", "station", "site.name", "site.id",
    "lat", "lon", "basin.area", "flow.site.name", "flow.site.id", "flow.lat", "flow.lon",
    "flow.basin.area", "basin.area.units", "custom"),
  stop.on.empty = FALSE
)
```

## Arguments

- metadata:

  a metadata object from which the information will be retrieved

- data:

  a data.frame from which the data column will be retrieved

- field:

  character string identifying the field to extract. Partial matching is
  allowed.

- attach.units:

  logical. If TRUE, the return value will be a
  [unitted](https://rdrr.io/pkg/unitted/man/unitted.html) vector.

- format:

  character. The format in which units should be returned. Set to
  'rloadest' to output units in a format recognized by `rloadest`, or
  'EGRET' for crosstalk with that package, or NA to keep units as stored
  in 'loadflex'.

- stop.on.empty:

  logical. Stops f the field is empty or non-character

## Value

`getCol` returns the specified column of data as a vector.

`getUnits` returns the specified units as a character string.

`getInfo` returns the miscellaneous information specified by `field`.

## Details

These metadata-getters provide a weak wall of abstraction between
metadata and clients of the metadata; more importantly, they do the
error checking to make sure that the designated field or data column
exists and return an informative error if it does not.

## See also

Other metadata:
[`metadata()`](http://doi-usgs.github.io/loadflex/reference/metadata.md)

## Examples

``` r
md <- metadata(constituent="NO3", flow="DISCHARGE", 
  dates="DATE", conc.units="mg L^-1", flow.units="cfs", load.units="kg", 
  load.rate.units="kg d^-1", site.name="Lamprey River, NH")
data(lamprey_nitrate)
head(getCol(md, lamprey_nitrate, 'date'))
#> [1] "1999-10-05 18:00:00 EDT" "1999-10-12 17:10:00 EDT"
#> [3] "1999-12-07 15:20:00 EST" "1999-12-14 14:30:00 EST"
#> [5] "1999-12-21 13:45:00 EST" "1999-12-28 09:04:59 EST"
head(getCol(md, lamprey_nitrate, 'conc'))
#> [1] 0.0913 0.0826 0.1138 0.0858 0.1213 0.1201
md <- metadata(constituent="NO3", flow="DISCHARGE", 
  dates="DATE", conc.units="mg L^-1", flow.units="cfs", load.units="kg", 
  load.rate.units="kg d^-1", site.name="Lamprey River, NH")
getUnits(md, 'flow')
#> [1] "ft^3 s^-1"
md <- metadata(constituent="NO3", flow="DISCHARGE", 
  dates="DATE", conc.units="mg L^-1", flow.units="cfs", load.units="kg", 
  load.rate.units="kg d^-1", site.name="Lamprey River, NH")
getInfo(md, 'site.name')
#> [1] "Lamprey River, NH"
```
