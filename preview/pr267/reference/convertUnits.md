# Produce conversion factor to multiply old.units by to get new.units

Converts units; units can be arbitrarily complex as long as every
dimension of each unit is present in unit.conversions

## Usage

``` r
convertUnits(old.units, new.units, attach.units = FALSE)
```

## Arguments

- old.units:

  character. The units to convert from.

- new.units:

  character. The units to convert to.

- attach.units:

  logical. Should units be attached to the conversion factor?

## Value

a conversion factor that can be multiplied with data in the old.units to
achieve data in the new.units

## Examples

``` r
loadflex:::convertUnits('mg/L', 'kg/m^3')
#> [1] 0.001
loadflex:::convertUnits('kg d^-1', 'kg/yr')
#> [1] 365.25
loadflex:::convertUnits('kg/yr', 'kg/d', attach.units = TRUE)
#> unitted numeric (y d^-1)
#> [1] 0.002737851
if (FALSE) { # \dontrun{
loadflex:::convertUnits('mg/L', 'm^3/d') # error: dimensions must match
loadflex:::convertUnits(unitbundle('ft^3 g L^-1 s^-1'), 'kg/d') # error: too complicated
} # }
```
