# Return a data.frame describing each dimension of the units (1 row per dimension)

Return a data.frame describing each dimension of the units (1 row per
dimension)

## Usage

``` r
dimInfo(unitstr)
```

## Arguments

- unitstr:

  A single string representing one set of units dimension

## Examples

``` r
loadflex:::dimInfo('kg') # 'mg'
#>   Unit Power Str       Pos  Dim Std
#> 1   kg     1  kg numerator mass  mg
loadflex:::dimInfo('ha') # 'km^2'
#>   Unit Power Str       Pos  Dim  Std
#> 1   ha     1  ha numerator area km^2
loadflex:::dimInfo('kg d^-1') # NA
#>   Unit Power Str         Pos  Dim Std
#> 1   kg     1  kg   numerator mass  mg
#> 2    d    -1   d denominator time   d
loadflex:::dimInfo('m^3') # NA
#>   Unit Power Str       Pos    Dim Std
#> 1    m     3 m^3 numerator volume   L
loadflex:::dimInfo('kk') # NA
#>   Unit Power Str       Pos  Dim  Std
#> 1   kk     1  kk numerator <NA> <NA>
```
