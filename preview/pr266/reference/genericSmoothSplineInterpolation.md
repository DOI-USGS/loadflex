# A parameterizable smoothing spline function.

Does not strictly adhere to the guidelines in
[interpolations](http://doi-usgs.github.io/loadflex/reference/interpolations.md),
but can be used by
[`getSmoothSplineInterpolation`](http://doi-usgs.github.io/loadflex/reference/getSmoothSplineInterpolation.md)
to produce a function that does.

## Usage

``` r
genericSmoothSplineInterpolation(dates.in, y.in, dates.out, ...)
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

- ...:

  any arguments other than `x` and `y` to be passed to
  `stats::`[`smooth.spline`](https://rdrr.io/r/stats/smooth.spline.html).
