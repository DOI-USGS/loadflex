# Determine whether a 1D dimension string is known and of the expected dimension type

Determine whether a 1D dimension string is known and of the expected
dimension type

## Usage

``` r
validDim(
  dimstr,
  dim.type = c("ANY", "volume", "time", "mass", "count", "area")
)
```

## Arguments

- dimstr:

  A string representing one units dimension (just one at a time,
  please). For example: kg, d, or m^3, but not kg/d or d^-1

- dim.type:

  One or more acceptable dimension types

## Examples

``` r
loadflex:::validDim('kg') # TRUE
#> [1] TRUE
loadflex:::validDim('kg', 'mass') # TRUE
#> [1] TRUE
loadflex:::validDim('kg', 'volume') # FALSE
#> [1] FALSE
loadflex:::validDim('whoknows') # FALSE
#> [1] FALSE
loadflex:::validDim('whoknows', 'time') # FALSE
#> [1] FALSE
```
