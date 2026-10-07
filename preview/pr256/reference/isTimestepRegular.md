# Check a time series for evenly spaced dates.

Even spacing of dates is required for many tests and assessments of
autocorrelation. This function checks for even spacing to within a
specified tolerance. If spacing is uneven, a histogram is immediately
produced (if `hist=TRUE`), and a handling function is run (e.g., to
produce an error or warning, as specified by `handler`).

## Usage

``` r
isTimestepRegular(
  dates,
  hist = TRUE,
  tol = .Machine$double.eps^0.5,
  handler = stop
)
```

## Arguments

- dates:

  A time series of dates in any format accepted by `diff`.

- hist:

  logical. If the time series is found to be irregular, should this
  function print a histogram of the observed time steps?

- tol:

  time step tolerance, the accepatable amount of difference between time
  steps to still consider them regular.

- handler:

  A function, e.g., `stop` or `warning`, to be run if the time series is
  irregular.

## Value

TRUE if the time steps in `dates` are identical to within the tolerance
set by tol, FALSE otherwise.
