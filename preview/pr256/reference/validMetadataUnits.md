# Check whether these units are acceptable (without translation) for inclusion in metadata

Check whether these units are acceptable (without translation) for
inclusion in metadata

## Usage

``` r
validMetadataUnits(
  unitstr,
  unit.type = c("ANY", "flow.units", "conc.units", "load.units", "load.rate.units",
    "basin.area.units")
)
```

## Arguments

- unitstr:

  A string representing units (just one bundle at a time, please)

- unit.type:

  string. accepts "ANY","flow.units","conc.units","load.units", or
  "load.rate.units"

- type:

  A string describing the type of units desired

## Value

logical. TRUE if valid for that unit type, FALSE otherwise

## Examples

``` r
validMetadataUnits("colonies d^-1") # TRUE
#> [1] TRUE
validMetadataUnits("m^3 s^-1", unit.type="ANY") # TRUE
#> [1] TRUE
validMetadataUnits("nonsensical") # FALSE
#> [1] FALSE
validMetadataUnits("g", unit.type="load.units") # TRUE
#> [1] TRUE
validMetadataUnits("g", unit.type="flow.units") # FALSE
#> [1] FALSE
```
