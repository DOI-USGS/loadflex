# Convert estimation and load prediction data into the EGRET Daily data.frame

Convert estimation and load prediction data into the EGRET Daily
data.frame

## Usage

``` r
convertToEGRETDaily(newdata, load.model = NULL, meta = NULL)
```

## Arguments

- newdata:

  data.frame of data used to generate predictions from an already-fitted
  model

- load.model:

  a load model (loadReg2, loadComp, loadInterp, loadLm, etc.) whose data
  and predictions are to be converted to EGRET format

- meta:

  loadflex metadata object; it must include constituent, flow, dates,
  conc.units, site.id, and consti.name
