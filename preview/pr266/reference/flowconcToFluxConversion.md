# Provide the conversion factor which, when multiplied by flow \* conc, gives the flux in the desired units

By dividing rather than multiplying by this factor, the output of this
function may also be used to convert from flux units to the units of the
product of flow and concentration.

## Usage

``` r
flowconcToFluxConversion(
  flow.units,
  conc.units,
  load.rate.units,
  attach.units = FALSE
)
```

## Arguments

- flow.units:

  character. The units of flow.

- conc.units:

  character. The units of concentration.

- load.rate.units:

  character. The units of flux.

- attach.units:

  logical. If TRUE, the conversion factor is returned with units
  attached.

## Value

numeric, or unitted numeric if unitted=TRUE. The conversion factor.

## Examples

``` r
flowconcToFluxConversion("cfs", "g/L", "kg/d") # 2446.589
#> [1] 2446.589
library(unitted); u(10, "ft^3 s^-1") * u(2, "mg L^-1") * 
  flowconcToFluxConversion("cfs", "mg/L", "kg/d", attach.units=TRUE) # u(48.9 ,"kg d^-1")
#> unitted numeric (kg d^-1)
#> [1] 48.93178
```
