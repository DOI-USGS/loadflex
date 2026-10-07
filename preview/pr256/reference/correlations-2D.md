# Functions that each produce an autocorrelation matrix with a specified pattern.

Accepts a vector of date-times or dates and returns a correlation matrix
describing the assumed correlations between all possible pairs of those
dates.

`cormatEqualDates` formalizes the assumption used by LOADEST and
rloadest: if two date-times are on the same calendar date, the
correlation (rho) is 1. Otherwise, rho=0.

## Usage

``` r
cormatEqualDates(dates)

cormat1DayBand(dates)

cormatDiagonal(dates)
```

## Arguments

- dates:

  date-times (as Date, POSIXct, chron, etc.) from which the
  autocorrelation matrix should be produced.

## Value

A matrix of autocorrelation coefficients for all possible pairs of
date-times in `dates`.
