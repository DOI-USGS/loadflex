# Return the unit.type of a unit string

Return the unit.type of a unit string

## Usage

``` r
unitType(unitstr)
```

## Arguments

- unitstr:

  A string representing units (just one at a time, please)

## Examples

``` r
loadflex:::unitType('kg') # 'load.units'
#> [1] "load.units"
loadflex:::unitType('kg/d') # NA
#> [1] NA
loadflex:::unitType(loadflex:::translateFreeformToUnitted('kg/d')) # 'load.rate.units'
#> [1] "load.rate.units"
loadflex:::unitType('nothing') # NA
#> [1] NA
```
