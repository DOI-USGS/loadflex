# Summarize the site and input data

Summarize the site and input data

## Usage

``` r
summarizeInputs(metadata, fitdat, estdat)
```

## Arguments

- metadata:

  metadata, used to access the appropriate columns of data. At a
  minimum, `metadata` should correctly specify the date column and the
  column indicated by `interp.format`.

- fitdat:

  data frame of constituent+discharge measurements, for fitting a model

- estdat:

  data frame of discharge measurements, for making predictions
  (estimations) from a model

## Value

Returns a 1-row data frame with the following columns:

- `site.name` - the long name of the site, as in
  [`metadata()`](http://doi-usgs.github.io/loadflex/reference/metadata.md)

- `site.id` - the unique identifier of the site, as in
  [`metadata()`](http://doi-usgs.github.io/loadflex/reference/metadata.md)

- `constituent` - the unique identifier of the constituent, as in
  [`metadata()`](http://doi-usgs.github.io/loadflex/reference/metadata.md)

- `consti.name` - the long name of the constituent, as in
  [`metadata()`](http://doi-usgs.github.io/loadflex/reference/metadata.md)

- `conc.units` - the units of concentration data, as in
  [`metadata()`](http://doi-usgs.github.io/loadflex/reference/metadata.md)

- `flow.units` - the units of discharge data, as in
  [`metadata()`](http://doi-usgs.github.io/loadflex/reference/metadata.md)

- `load.units` - the units of load estimates, as in
  [`metadata()`](http://doi-usgs.github.io/loadflex/reference/metadata.md)

- `load.rate.units` - the units of load rate units, as in
  [`metadata()`](http://doi-usgs.github.io/loadflex/reference/metadata.md)

- `lat` - the decimal latitude of the station where concentration (and
  possibly also discharge) was measured, as in
  [`metadata()`](http://doi-usgs.github.io/loadflex/reference/metadata.md)

- `lon` - the decimal longitude of the station where concentration (and
  possibly also discharge) was measured, as in
  [`metadata()`](http://doi-usgs.github.io/loadflex/reference/metadata.md)

- `basin.area` - the area of the drainage basin contributing water to
  the site where concentrations were measured, as in
  [`metadata()`](http://doi-usgs.github.io/loadflex/reference/metadata.md)

- `flow.site.name` - the long name of the station where flow was
  monitored, as in
  [`metadata()`](http://doi-usgs.github.io/loadflex/reference/metadata.md)

- `flow.site.id` - the unique identifier of the station where flow was
  monitored, as in
  [`metadata()`](http://doi-usgs.github.io/loadflex/reference/metadata.md)

- `flow.lat` - the decimal latitude of the station where discharge was
  measured, as in
  [`metadata()`](http://doi-usgs.github.io/loadflex/reference/metadata.md)

- `flow.lon` - the decimal longitude of the station where discharge was
  measured, as in
  [`metadata()`](http://doi-usgs.github.io/loadflex/reference/metadata.md)

- `flow.basin.area` - the area of the drainage basin contributing water
  to the site where discharge was measured, as in
  [`metadata()`](http://doi-usgs.github.io/loadflex/reference/metadata.md)

- `basin.area.units` - the units of the values in `basin.area` and
  `flow.basin.area` (same for both), as in
  [`metadata()`](http://doi-usgs.github.io/loadflex/reference/metadata.md)

- `[custom]` - if the `custom` slot of `metadata` is a 1-row data.frame
  or a list of length-1 elements, the columns of that data.frame are
  included in this summary

- `basin.area.ratio.QC` - the ratio of `flow.basin.area` to `basin.area`

- `fitdat.start` - the date of the first observation in the model
  fitting data

- `fitdat.end` - the date/time of the last observation in the model
  fitting data

- `fitdat.num.total` - the total number of observations in the model
  fitting data

- `fitdat.num.incomplete` - the number of NA observations in the model
  fitting data

- `fitdat.num.censored` - this field is currently a placeholder (will
  always be NA) for the number of censored observations in the model
  fitting data

- `fitdat.min.gap.days` - the length in days of the smallest gap between
  any two successive observations in the model fitting data

- `fitdat.max.gap.days` - the length in days of the largest gap between
  any two successive observations in the model fitting data

- `fitdat.median.gap.days` - the length in days of the median gap
  between successive observations in the model fitting data

- `estdat.start` - the date of the first observation in the estimation
  data

- `estdat.end` - the date/time of the last observation in the estimation
  data

- `estdat.num.total` - the total number of observations in the
  estimation data

- `estdat.num.incomplete` - the number of incomplete (NA) observations
  in the estimation data

- `estdat.min.gap.days` - the length in days of the smallest gap between
  any two successive observations in the estimation data

- `estdat.max.gap.days` - the length in days of the largest gap between
  any two successive observations in the estimation data

- `estdat.median.gap.days` - the length in days of the median gap
  between successive observations in the estimation data

## Examples

``` r
data(lamprey_nitrate)
data(lamprey_discharge)
md <- metadata(constituent="NO3", flow="DISCHARGE", 
  dates="DATE", conc.units="mg L^-1", flow.units="cfs", load.units="kg", 
  load.rate.units="kg d^-1", site.name="Lamprey River, NH",
  basin.area=50, flow.basin.area=65, basin.area.units='ha',
  site.id="1073500", custom=list(data.source="USGS NWIS, waterdata.usgs.gov"))
sitesum <- summarizeInputs(metadata=md, fitdat=lamprey_nitrate[,1:3], 
  estdat=lamprey_discharge)
```
