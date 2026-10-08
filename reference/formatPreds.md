# formatPreds raw to final predictions

Convert raw predictions to final predictions, possibly including a
switch between flux and conc. If there is a switch, the units will be
converted according to the metadata.

## Usage

``` r
formatPreds(
  preds,
  from.format = c("flux", "conc*flow", "flux/flow", "conc"),
  to.format = c("flux", "conc"),
  newdata,
  metadata,
  lin.or.log = c("linear", "log"),
  attach.units = FALSE
)
```

## Arguments

- preds:

  raw prediction values

- from.format:

  character in `c("flux","conc*flow","flux/flow","conc")`. Format of the
  raw predictions.

- to.format:

  character indicating whether the returned value should be a flux or a
  concentration.

- newdata:

  a data.frame with nrow() == length(preds) and containing any columns
  (named as in `metadata`) that will be needed to perform the requested
  conversion. For example, from="conc" and to="flux" implies that a
  discharge column will be available in `newdata`.

- metadata:

  An object of class
  [`metadata`](http://doi-usgs.github.io/loadflex/reference/metadata.md)
  used to determine the units of inputs and desired output

- lin.or.log:

  character. Either "linear" or "log" to say whether the predictions
  should be converted to log space or not. If converted to log space, a
  bias correction will be applied, see
  [`linToLog`](http://doi-usgs.github.io/loadflex/reference/lognormal-moments.md).

- attach.units:

  logical. Attach the units to the returned value?

## Value

converted predictions (in the format/units specified by to.format and
metadata)

## Examples

``` r
obs <- transform(data.frame(MyConc=1:10, MyFlow=rep(10,10)), 
  MyFlux=MyConc*MyFlow*rloadest::loadConvFactor("cms", "mg/l", "kg") )
md <- metadata(constituent="MyConc", flow="MyFlow", 
  load.rate="MyFlux", dates="none", flow.units="m^3 s^-1", conc.units="mg L^-1",
  load.units="kg", load.rate.units="kg/d", site.name='My River', custom=NULL)
  
all.equal(obs$MyFlux, formatPreds(
  preds=obs$MyConc, from.format="conc", to.format="flux", newdata=obs, metadata=md))
#> [1] TRUE
if (FALSE) { # \dontrun{
# these examples take too long to build regularly
all.equal(obs$MyFlux, formatPreds(
  preds=obs$MyConc*obs$MyFlow, from.format="conc*flow", to.format="flux",
    newdata=obs, metadata=md))
all.equal(obs$MyConc, formatPreds(
  preds=obs$MyFlux, from.format="flux", to.format="conc", newdata=obs, metadata=md))
all.equal(unitted::u(obs$MyConc, "mg L^-1"), formatPreds(
  preds=obs$MyFlux, from.format="flux", to.format="conc", newdata=obs, metadata=md, 
  attach.units=TRUE))
} # }
```
