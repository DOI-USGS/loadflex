# A parameterizable triangular interpolation function.

Does not strictly adhere to the guidelines in
[interpolations](http://doi-usgs.github.io/loadflex/reference/interpolations.md),
but can be used by
[`getTriangularInterpolation`](http://doi-usgs.github.io/loadflex/reference/getTriangularInterpolation.md)
to produce a function that does.

## Usage

``` r
genericTriangularInterpolation(dates.in, y.in, dates.out, y.mid)
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

- y.mid:

  The value or values to which midpoints between observations should be
  set. Good choices are: mean(y.in) for stand-alone interpolations; 0
  for composite method interpolations among absolute residuals (in log
  or linear space); 1 for composite method interpolations among relative
  residuals (again, log or linear space).
