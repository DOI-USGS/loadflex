# Introduction to loadflex

The `loadflex` package lets you quickly fit and compare concentrations
and/or fluxes of solutes in watersheds. This vignette demonstrates the
application of `loadflex` to fit four different model types to the same
data, to assess and compare those models, and to estimate average solute
concentrations and fluxes at both quarter-hourly and monthly scales.

We will use the data supplied with the `loadflex` package. These include
nitrate concentration observations from the Lamprey River in
southeastern New Hampshire, where researchers in the NH Water Resources
Research Center (University of New Hampshire; PI: William H. McDowell)
have been monitoring water quality weekly since 1999. Discharge data are
from the same location, Packers Falls, from a USGS gaging station
(<http://waterdata.usgs.gov/usa/nwis/uv?site_no=01073500>).

Start by loading the package.

``` r

library(loadflex)
```

Load the data provided in this package.

``` r

# Interpolation data: Packers Falls NO3 grab sample observations
data(lamprey_nitrate)
intdat <- lamprey_nitrate[c("DATE","DISCHARGE","NO3")]

# Calibration data: Restrict to points separated by sufficient time
regdat <- subset(lamprey_nitrate, REGR)[c("DATE","DISCHARGE","NO3")]

# Estimation data: Packers Falls discharge
data(lamprey_discharge)
estdat <- subset(lamprey_discharge, DATE < as.POSIXct("2012-10-01 00:00:00", tz="EST5EDT"))
estdat <- estdat[seq(1, nrow(estdat), by=96/4),] # pare to 4 obs/day for speed
```

Create a metadata description of the dataset & desired output.

``` r

meta <- metadata(constituent="NO3", flow="DISCHARGE", 
  dates="DATE", conc.units="mg L^-1", flow.units="cfs", load.units="kg", 
  load.rate.units="kg d^-1", site.name="Lamprey River, NH",
  consti.name="Nitrate", site.id='01073500', lat=43.10259, lon=-70.95256)
```

Fit four models: interpolation, linear, rloadest, and composite. Many
variants on these models are possible. For example: `loadInterp` models
may use any of several interp.fun options; see
[`?interpolations`](http://doi-usgs.github.io/loadflex/reference/interpolations.md).
`loadLm` accepts any linear model acceptable to
[`lm()`](https://rdrr.io/r/stats/lm.html), not just the very simple
formula we have used here. `loadReg2` functions are also flexible as
specified in the documentation for . `loadComp` models accept a linear
model as fit by either `loadLm` or `loadReg2` and any of the interp.fun
options available to `loadInterp`.

``` r

no3_li <- loadInterp(interp.format="conc", interp.fun=rectangularInterpolation, 
  data=intdat, metadata=meta)
no3_lm <- loadLm(formula=log(NO3) ~ log(DISCHARGE), pred.format="conc", 
  data=regdat, metadata=meta, retrans=exp)
library(rloadest)
no3_lr <- loadReg2(loadReg(NO3 ~ model(9), data=regdat,
  flow="DISCHARGE", dates="DATE", time.step="instantaneous", 
  flow.units="cfs", conc.units="mg/L", load.units="kg",
  station='Lamprey River, NH'))
no3_lc <- loadComp(reg.model=no3_lr, interp.format="conc", 
  interp.data=intdat, store = "uncertainty")
```

You can inspect these models in a variety of model-specific ways. Here
are some commands to try (we won’t print them here because the output
can be lengthy):

``` r

getMetadata(no3_li)
getFittingFunction(no3_lm)
getFittedModel(no3_lr)
getFittingData(no3_lc)
```

Now generate point predictions from each model.

``` r

preds_li <- predictSolute(no3_li, "flux", estdat, se.pred=TRUE)
```

    ## Warning in regularize.values(x, y, ties, missing(ties), na.rm = na.rm): collapsing to unique 'x'
    ## values

``` r

preds_lm <- predictSolute(no3_lm, "flux", estdat, se.pred=TRUE, lin.or.log="linear")
preds_lr <- predictSolute(no3_lr, "flux", estdat, se.pred=TRUE)
preds_lc <- predictSolute(no3_lc, "flux", estdat, se.pred=TRUE)
```

A few lines from one of the resulting prediction data.frames (they’re
all structured the same way):

``` r

head(preds_lr)
```

    ##                  date     flux  se.pred
    ## 1 1999-10-01 01:00:00 15.72780 4.722913
    ## 2 1999-10-01 07:00:00 16.68311 5.010515
    ## 3 1999-10-01 13:00:00 17.39594 5.225214
    ## 4 1999-10-01 19:00:00 16.94764 5.090053
    ## 5 1999-10-02 01:00:00 16.72981 5.024355
    ## 6 1999-10-02 07:00:00 16.51078 4.958313

Here are a few ways to inspect the models:

``` r

summary(getFittedModel(no3_lm))
ggplot2::qplot(x=Date, y=Resid, data=getResiduals(no3_li, newdata=intdat))
residDurbinWatson(no3_lr, "conc", newdata=regdat, irreg=TRUE)
residDurbinWatson(no3_lr, "conc", newdata=intdat, irreg=TRUE)
estimateRho(no3_lr, "conc", newdata=regdat, irreg=TRUE)$rho
estimateRho(no3_lr, "conc", newdata=intdat, irreg=TRUE)$rho
getCorrectionFraction(no3_lc, "flux", newdat=intdat)
```

The `loadflex` process for aggregation of point predictions (to monthly,
annual, etc.) has changed since we published our 2015 manuscript, so
`aggregateSolute` from that manuscript is no longer offered as a
function in loadflex. Instead, you can use the `agg.by` argument to
`predictSolute` to generate predictions at the time interval of
interest. You can aggregate for mean concentration or flux rate, and for
months, water years, calendar years, or other time intervals. `n`
reports the number of observations going into the aggregated estimate in
each row. Some examples:

``` r

aggs_li <- predictSolute(no3_li, "flux", newdata=estdat, agg.by="month")
```

    ## Warning in regularize.values(x, y, ties, missing(ties), na.rm = na.rm): collapsing to unique 'x'
    ## values

``` r

aggs_lm <- predictSolute(no3_lm, "flux", newdata=estdat, agg.by="water year")
aggs_lr <- predictSolute(no3_lr, "flux", newdata=estdat, agg.by="calendar year", date=TRUE, se.fit=TRUE)
aggs_lc <- predictSolute(no3_lc, "flux", newdata=estdat, agg.by="day")
```

A few lines from each of the resulting aggregated flux data.frames:

``` r

head(aggs_li)
```

    ##     month flux.rate count
    ## 1 1999-10  36.25767   124
    ## 2 1999-11  61.72640   120
    ## 3 1999-12  75.05153   124
    ## 4 2000-01 100.70306   124
    ## 5 2000-02 124.53199   116
    ## 6 2000-03 214.33387   124

``` r

head(aggs_lm)
```

    ##   water.year flux.rate count
    ## 1       2000 109.80382  1464
    ## 2       2001  90.77553  1460
    ## 3       2002  53.84619  1460
    ## 4       2003  98.73848  1460
    ## 5       2004 115.79976  1464
    ## 6       2005 132.08674  1460

``` r

head(aggs_lr)
```

    ##   calendar.year count.days flux.rate   se.fit
    ## 1          1999         92        NA       NA
    ## 2          2000        366  81.59340 3.874249
    ## 3          2001        365  61.36536 2.507505
    ## 4          2002        365  63.47885 1.626333
    ## 5          2003        365 105.40671 2.806397
    ## 6          2004        366 103.29017 2.723026

``` r

head(aggs_lc)
```

    ##          day flux.rate count
    ## 1 1999-10-01  14.22684     4
    ## 2 1999-10-02  14.23201     4
    ## 3 1999-10-03  14.98515     4
    ## 4 1999-10-04  14.99027     4
    ## 5 1999-10-05  18.39978     4
    ## 6 1999-10-06  19.22265     4
