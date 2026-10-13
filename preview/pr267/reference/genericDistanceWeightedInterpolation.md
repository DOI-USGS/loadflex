# A parameterizable distance-weighted interpolation function.

Does not strictly adhere to the guidelines in
[interpolations](http://doi-usgs.github.io/loadflex/reference/interpolations.md),
but can be used by
[`getDistanceWeightedInterpolation`](http://doi-usgs.github.io/loadflex/reference/getDistanceWeightedInterpolation.md)
to produce a function that does.

## Usage

``` r
genericDistanceWeightedInterpolation(
  dates.in,
  y.in,
  dates.out,
  inv.dist.fun = function(a, b) {
1/((a - b)^2)
 }
)
```

## Arguments

- dates.in:

  A numeric vector desribing the dates for each of the values in `y.in`.
  Dates are represented as the number of seconds since 1970.

- y.in:

  A vector of values (typically fluxes or concentrations) to interpolate
  among.

- dates.out:

  A numeric vector of dates for which the corresponding output values
  are to be produced. Dates are represented as the number of seconds
  since 1970.

- inv.dist.fun:

  A function to calculate an inverse distance metric. Should be
  vectorized such that one of `a` or `b` may be a vector when the other
  is a scalar.
