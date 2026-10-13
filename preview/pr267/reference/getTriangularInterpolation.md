# Generate a triangular interpolation function with the parameters of your choice.

Produces an interpolation function of the form described in
[interpolations](http://doi-usgs.github.io/loadflex/reference/interpolations.md).

## Usage

``` r
getTriangularInterpolation(y.mid = 0)
```

## Arguments

- y.mid:

  The value or values to which midpoints between observations should be
  set. Good choices are: mean(y.in) for stand-alone interpolations; 0
  for composite method interpolations among absolute residuals (in log
  or linear space); 1 for composite method interpolations among relative
  residuals (again, log or linear space).

## Value

A function of the form described in
[interpolations](http://doi-usgs.github.io/loadflex/reference/interpolations.md),
i.e., accepting the arguments `dates.in`, `y.in`, and `dates.out` and
returning predictions from a triangular interpolation function for
`y.out`. That function will use the `y.mid` argument supplied here.
