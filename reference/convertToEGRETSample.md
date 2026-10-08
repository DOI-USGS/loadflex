# Convert the interpolation data.frame into the EGRET Sample dataframe.

Convert the interpolation data.frame into the EGRET Sample dataframe.

## Usage

``` r
convertToEGRETSample(data = NULL, meta = NULL, dailydat = NULL)
```

## Arguments

- data:

  data.frame of data used to fit a model. only required if load.model is
  omitted

- meta:

  loadflex metadata object; it must include constituent, flow, dates,
  conc.units, site.id, and consti.name. only required if load.model is
  omitted

- dailydat:

  an EGRET Daily data.frame of flow and prediction values
