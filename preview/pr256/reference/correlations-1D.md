# Get the assumed correlation between residuals or predictions at pairs of dates.

These 1D correlation functions are made available to help users explore
and understand the assumptions that may be made about the correlation
structure of residuals or predictions. It is possible to use these
functions directly for aggregation by calling
[`aggregateSolute`](http://doi-usgs.github.io/loadflex/reference/aggregateSolute.md)
with the `cormat.function` argument set to either
[`getCormatCustom`](http://doi-usgs.github.io/loadflex/reference/getCormatCustom.md)`(rhoEqualDates)`
or
[`getCormatCustom`](http://doi-usgs.github.io/loadflex/reference/getCormatCustom.md)`(rho1DayBand)`.
However, the equivalent but more efficient options are to set
`cormat.function` to
[`cormatEqualDates`](http://doi-usgs.github.io/loadflex/reference/correlations-2D.md)
or
[`cormat1DayBand`](http://doi-usgs.github.io/loadflex/reference/correlations-2D.md),
respectively, because those `cormatXXX` functions produce the full
correlation matrix without costly intermediate steps.

## Usage

``` r
rhoEqualDates(date1, date2)

rho1DayBand(date1, date2)
```

## Arguments

- date1:

  The date of the first prediction

- date2:

  The date\[s\] of the second prediction\[s\].

## Value

A correlation coefficient or a vector of coefficients corresponding to
the pair or pairs in the rows of `cbind(date1, date2)`.

## Details

`rhoEqualDates` embodies the standard LOADEST/rloadest assumption about
correlation of residuals for the purpose of aggregation. Specifically,
if two point predictions are on the same calendar date, then rho (the
correlation of their errors) is set to 1, and otherwise rho = 0.

`rho1DayBand` returns 1 for pairs of date-times that are within 1 day
(86400 seconds) of each other, or 0 for pairs that are not. This is a
comparable but smoother assumption than `rhoEqualDates`.
