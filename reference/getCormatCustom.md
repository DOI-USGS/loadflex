# Turn an autocorrelation function into a function that produces a correlation matrix

Produces a function of the form described in
[correlations-2D](http://doi-usgs.github.io/loadflex/reference/correlations-2D.md)
from a function of the form described in
[correlations-1D](http://doi-usgs.github.io/loadflex/reference/correlations-1D.md).

## Usage

``` r
getCormatCustom(cor1D.function, vectorized = FALSE)
```

## Arguments

- cor1D.function:

  An autocorrelation function - specifically, a function that accepts
  two date arguments and returns the correlation coefficient\[s\] for
  those dates. If each date argument is a single date, the output should
  be a single coefficient. If the second date argument is a vector of
  dates, the output should be a vector of the same length as that dates
  vector.

- vectorized:

  logical. Can rho.function accept its second argument as a vector
  rather than a scalar? If so, the matrix-generating function may be
  faster.

## Value

A function that accepts a new vector of N dates and produces an N-by-N
correlation matrix.

## Details

For use with custom autocorrelation functions. Although it is usually
faster to create your own matrix-generating function, this function will
accept a simpler autocorrelation function (one of the form described in
[correlations-1D](http://doi-usgs.github.io/loadflex/reference/correlations-1D.md))
and return a function that accepts a vector of dates generates a matrix
of correlations for all possible pairs of those dates (a function of the
form described in
[correlations-2D](http://doi-usgs.github.io/loadflex/reference/correlations-2D.md).
The matrix returned by that second function will be sparse if it is at
least half zeros.
