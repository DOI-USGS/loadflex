# Convert loadflex to EGRET object

Convert a loadflex object into an EGRET object for plotting.

## Usage

``` r
convertToEGRET(load.model = NULL, newdata = NULL, data = NULL, meta = NULL)
```

## Arguments

- load.model:

  a load model (loadReg2, loadComp, loadInterp, loadLm, etc.) whose data
  and predictions are to be converted to EGRET format

- newdata:

  data.frame of data used to generate predictions from an already-fitted
  model

- data:

  data.frame of data used to fit a model. only required if load.model is
  omitted

- meta:

  loadflex metadata object; it must include constituent, flow, dates,
  conc.units, site.id, and consti.name. only required if load.model is
  omitted

## Examples

``` r
data(lamprey_nitrate)
fitdat <- lamprey_nitrate
data(lamprey_discharge)
estdat <- subset(lamprey_discharge, DATE < as.POSIXct("2012-10-01 00:00:00", tz="EST5EDT"))
estdat <- estdat[seq(1, nrow(estdat), by=96/4),] # only keep 4 observations per day
meta <- metadata(constituent="NO3", flow="DISCHARGE", dates="DATE",
  conc.units="mg L^-1", flow.units="cfs", load.units="kg", load.rate.units="kg d^-1",
  site.name="Lamprey River, NH", site.id='NWIS 01073500', consti.name = "nitrate")
no3_lm <- loadLm(formula=log(NO3) ~ log(DISCHARGE), pred.format="conc",
  data=fitdat, metadata=meta, retrans=exp)
minimalEGRET <- loadflex:::convertToEGRET(data=fitdat, meta=meta)
#> Many EGRET functions will not work with missing values.
#> Error in mutate(., dateTime = flowdat[[date.colname]]): ℹ In argument: `dateTime = flowdat[[date.colname]]`.
#> Caused by error:
#> ! `dateTime` must be size 4740 or 1, not 555.
maximalEGRET <- loadflex:::convertToEGRET(load.model=no3_lm, newdata=estdat)
#> Many EGRET functions will not work with missing values.
#> Error in mutate(., dateTime = flowdat[[date.colname]]): ℹ In argument: `dateTime = flowdat[[date.colname]]`.
#> Caused by error:
#> ! `dateTime` must be size 4749 or 1, not 18996.
```
