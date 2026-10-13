# Display a metadata object

Display a metadata object

## Usage

``` r
# S4 method for class 'metadata'
show(object)
```

## Arguments

- object:

  The metadata object to be displayed

## Examples

``` r
md <- metadata(constituent="NO3", flow="DISCHARGE", 
  dates="DATE", conc.units="mg L^-1", flow.units="cfs", load.units="kg", 
  load.rate.units="kg d^-1", site.name="Lamprey River, NH")
show(md) # or just md at the command prompt
#> Metadata for a load model
#> -NAME-       -VALUE-
#> constituent  NO3
#> consti.name  
#> flow         DISCHARGE
#> load.rate    
#> dates        DATE
#> conc.units   mg L^-1
#> flow.units   ft^3 s^-1
#> load.units   kg
#> load.rate.units kg d^-1
#> station      
#> site.name    Lamprey River, NH
#> site.id      
#> lat          NA
#> lon          NA
#> basin.area   NA
#> flow.site.name 
#> flow.site.id 
#> flow.lat     NA
#> flow.lon     NA
#> flow.basin.area NA
#> basin.area.units km^2
```
