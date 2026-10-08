# Produces a function that uses a first-order autocorrelation model to estimate the correlation between two dates.

Produces a function of the form described in
[correlations-1D](http://doi-usgs.github.io/loadflex/reference/correlations-1D.md).

`getRhoFirstOrderFun` accepts an assumed or empirically estimated value
of \\\rho\\ (`rho`), the correlation coefficient in the first-order
autocorrelation model:

\$\$cor(y(t1), y(t2)) = \rho^\tau\$\$

where

\$\$\tau = y(t2) - y(t1)\$\$

and \\\tau\\ is in the same units as those used to estimate \\\rho\\
(`rho`). To ensure that the units are the same, this function also
accepts a `time.step` argument indicating the interval between two
successive values in the time series used to estimate \\\rho\\ (`rho`).
The function returned by `getRhoFirstOrderFun` takes responsibility for
matching the units of `date1` and `date2` to those of `rho`.

## Usage

``` r
getRhoFirstOrderFun(rho, time.step = as.difftime(1, units = "hours"))
```

## Arguments

- rho:

  The coefficient of the first-order autocorrelation model, either
  asserted by the user or estimated by
  [`estimateRho`](http://doi-usgs.github.io/loadflex/reference/estimateRho.md).

- time.step:

  difftime. The time.step of the time series used to estimate `rho`. It
  is essential to use the right units; `as.difftime(1, units="hours")`
  is quite different from `as.difftime(1, units="days")`.

## Value

A function that accepts two date arguments and returns the predicted
correlation between those two dates. One of the date arguments may be a
vector as long as the other is scalar. This function takes the form,
`function(date1, date2)` with arguments as defined for
[`rhoEqualDates`](http://doi-usgs.github.io/loadflex/reference/correlations-1D.md)
and
[`rho1DayBand`](http://doi-usgs.github.io/loadflex/reference/correlations-1D.md).
