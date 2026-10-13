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
(<https://waterdata.usgs.gov/monitoring-location/USGS-01073500>).

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

    ## Warning in regularize.values(x, y, ties, missing(ties), na.rm = na.rm): collapsing to unique 'x' values

``` r

preds_lm <- predictSolute(no3_lm, "flux", estdat, se.pred=TRUE, lin.or.log="linear")
preds_lr <- predictSolute(no3_lr, "flux", estdat, se.pred=TRUE)
preds_lc <- predictSolute(no3_lc, "flux", estdat, se.pred=TRUE)
```

A few lines from one of the resulting prediction data.frames:

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
annual, etc.) has changed since we published our 2015 manuscript.
`aggregateSolute` from that manuscript is no longer exported as a
function in loadflex. Instead, you can use the `agg.by` argument to
`predictSolute` to generate predictions at the time interval of
interest. You can aggregate for for months, water years, calendar years,
or other time intervals, and for mean concentration (named `conc` in the
output) or flux (`flux.rate` in the output, using “rate” to remind users
that the time denominator does not change regardless of the time period
being aggregated over). Standard errors (`se.fit` and `se.pred`) are
available for `loadReg2` models. The units of the aggregated output are
what you passed into each model as metadata. `count` reports the number
of observations going into the aggregated estimate in each row. Some
examples:

``` r

getUnits(getMetadata(no3_lr), 'conc')
```

    ## [1] "mg L^-1"

``` r

agg_c_li <- predictSolute(no3_li, "conc", newdata=estdat, agg.by="month")
```

    ## Warning in regularize.values(x, y, ties, missing(ties), na.rm = na.rm): collapsing to unique 'x' values

``` r

agg_c_lm <- predictSolute(no3_lm, "conc", newdata=estdat, agg.by="water year")
agg_c_lr <- predictSolute(no3_lr, "conc", newdata=estdat, agg.by="unit", se.fit=TRUE, se.pred=TRUE)
agg_c_lc <- predictSolute(no3_lc, "conc", newdata=estdat, agg.by="day")

getUnits(getMetadata(no3_lr), 'flux rate')
```

    ## [1] "kg d^-1"

``` r

agg_f_li <- predictSolute(no3_li, "flux", newdata=estdat, agg.by="month")
```

    ## Warning in regularize.values(x, y, ties, missing(ties), na.rm = na.rm): collapsing to unique 'x' values

``` r

agg_f_lm <- predictSolute(no3_lm, "flux", newdata=estdat, agg.by="water year")
agg_f_lr <- predictSolute(no3_lr, "flux", newdata=estdat, agg.by="calendar year", se.fit=TRUE, se.pred=TRUE)
agg_f_lc <- predictSolute(no3_lc, "flux", newdata=estdat, agg.by="day")
```

A few lines from each of the resulting aggregated flux data.frames:

``` r

head(agg_c_li)
```

    ##     month       conc count
    ## 1 1999-10 0.08495235   124
    ## 2 1999-11 0.10463500   120
    ## 3 1999-12 0.11101811   124
    ## 4 2000-01 0.15563200   124
    ## 5 2000-02 0.19444202   116
    ## 6 2000-03 0.10493360   124

``` r

head(agg_c_lm)
```

    ##   water.year      conc count
    ## 1       2000 0.1617367  1464
    ## 2       2001 0.1671824  1460
    ## 3       2002 0.1728231  1460
    ## 4       2003 0.1647539  1460
    ## 5       2004 0.1615283  1464
    ## 6       2005 0.1610110  1460

``` r

head(agg_c_lr)
```

    ##                  date      conc      se.fit    se.pred
    ## 1 1999-10-01 01:00:00 0.1108355 0.006947463 0.03328288
    ## 2 1999-10-01 07:00:00 0.1099827 0.006917092 0.03303161
    ## 3 1999-10-01 13:00:00 0.1093890 0.006898088 0.03285714
    ## 4 1999-10-01 19:00:00 0.1099532 0.006918008 0.03302332
    ## 5 1999-10-02 01:00:00 0.1102905 0.006930643 0.03312284
    ## 6 1999-10-02 07:00:00 0.1106310 0.006943621 0.03322333

``` r

head(agg_c_lc)
```

    ##          day       conc count
    ## 1 1999-10-01 0.09380781     4
    ## 2 1999-10-02 0.09420547     4
    ## 3 1999-10-03 0.09386919     4
    ## 4 1999-10-04 0.09429498     4
    ## 5 1999-10-05 0.09147071     4
    ## 6 1999-10-06 0.08982873     4

``` r

head(agg_f_li)
```

    ##     month flux.rate count
    ## 1 1999-10  36.25767   124
    ## 2 1999-11  61.72640   120
    ## 3 1999-12  75.05153   124
    ## 4 2000-01 100.70306   124
    ## 5 2000-02 124.53199   116
    ## 6 2000-03 214.33387   124

``` r

head(agg_f_lm)
```

    ##   water.year flux.rate count
    ## 1       2000 109.80382  1464
    ## 2       2001  90.77553  1460
    ## 3       2002  53.84619  1460
    ## 4       2003  98.73848  1460
    ## 5       2004 115.79976  1464
    ## 6       2005 132.08674  1460

``` r

head(agg_f_lr)
```

    ##   calendar.year count.days flux.rate   se.fit  se.pred
    ## 1          1999         92        NA       NA       NA
    ## 2          2000        366  81.59340 3.874249 1.942207
    ## 3          2001        365  61.36536 2.507505 1.799239
    ## 4          2002        365  63.47885 1.626333 1.380100
    ## 5          2003        365 105.40671 2.806397 2.293839
    ## 6          2004        366 103.29017 2.723026 2.294192

``` r

head(agg_f_lc)
```

    ##          day flux.rate count
    ## 1 1999-10-01  14.22684     4
    ## 2 1999-10-02  14.23201     4
    ## 3 1999-10-03  14.98515     4
    ## 4 1999-10-04  14.99027     4
    ## 5 1999-10-05  18.39978     4
    ## 6 1999-10-06  19.22265     4
