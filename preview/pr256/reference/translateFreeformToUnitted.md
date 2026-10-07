# Convert units from a greater variety of forms, including rloadest form, to unitted form

Convert units from a greater variety of forms, including rloadest form,
to unitted form

## Usage

``` r
translateFreeformToUnitted(freeform.units, attach.units = FALSE)
```

## Arguments

- freeform.units:

  character string or list of character strings describing one or more
  sets of units. "Freeform" is an exaggeration but gets at the idea that
  these units can take a greater variety of forms than those accepted by
  unitted and the units conversion functions in `loadflex`.

- attach.units:

  logical. If TRUE, returned value is unitted; otherwise, it's
  character.

## Value

a unitbundle or list of unitbundles containing equivalent but simpler
and more uniform units

## Examples

``` r
loadflex:::translateFreeformToUnitted("cfs") # "ft^3 s^-1"
#> [1] "ft^3 s^-1"
loadflex:::translateFreeformToUnitted("kg/d") # "kg d^-1"
#> [1] "kg d^-1"
loadflex:::translateFreeformToUnitted("mg L^-1") # "mg L^-1"
#> [1] "mg L^-1"
```
