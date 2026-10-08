# Create an EGRET-style plot

Use loadflex data frames, but create an EGRET plot.

## Usage

``` r
plotEGRET(
  plot.name,
  load.model = NULL,
  newdata = NULL,
  data = NULL,
  meta = NULL,
  eList = NULL,
  moreTitle = "loadflex",
  plotFlowNorm = FALSE,
  ...
)
```

## Arguments

- plot.name:

  the name of the plot the user wants to create. See Details for current
  options. For now, only one allowed at time.

- load.model:

  a load model (loadReg2, loadComp, loadInterp, loadLm, etc.) whose data
  and predictions are to be converted to EGRET format

- newdata:

  data.frame of data used to generate predictions from an already-fitted
  model

- data:

  data.frame of data used to fit a model. only required if load.model is
  omitted

- meta:

  loadflex metadata object; it must include constituent, flow, dates,
  conc.units, site.id, and consti.name. only required if load.model is
  omitted

- eList:

  an EGRET eList, as output from EGRET or loadflex::convertToEGRET, to
  override separate inputs of `load.model`, `newdata`, `data`, and/or
  `meta`

- moreTitle:

  additional text to include in the fluxBiasMulti plot title. The EGRET
  default is "WRTDS", so this changes the default to "loadflex".

- plotFlowNorm:

  logical indicating whether or not to plot the normalized flow lines.
  This defaults to FALSE, which overrides the EGRET default TRUE.
  Applicable in plotFluxHist and plotConcHist.

- ...:

  additional arguments to pass to the plot

## Details

EGRET plots that require `data, meta`:

- boxConcMonth

- plotConcTime

- plotConcQ

- plotFluxQ

EGRET plots that require `data, newdata, preds, meta`:

- boxQTwice

- multiPlotDataOverview

- plotConcTimeDaily

- plotFluxTimeDaily

- plotConcPred

- plotFluxPred

- plotResidPred

- plotResidQ

- plotResidTime

- boxResidMonth

- boxConcThree

- plotConcHist

- plotFluxHist

- fluxBiasMulti

## Examples

``` r

# Load necessary data + create the appropriate metadata
data(lamprey_nitrate) # interpolation data (grab sample obs)
fitdat <- lamprey_nitrate

data("lamprey_discharge")
estdat <- lamprey_discharge # estimation data (15 min interval)
estdat <- subset(estdat, DATE < as.POSIXct("2012-10-01 00:00:00", tz="EST5EDT")) 
estdat <- estdat[seq(1, nrow(estdat), by=96/4),] # only keep 4 observations per day

meta <- metadata(
  constituent="NO3", 
  flow="DISCHARGE",
  dates="DATE", 
  conc.units="mg L^-1", 
  flow.units="cfs", 
  load.units="kg",
  load.rate.units="kg d^-1", 
  site.name="Lamprey River, NH",
  site.id="01073500",
  consti.name="Nitrate")

# Run your model and get your predictions
no3_lm <- loadLm(
  formula=log(NO3) ~ log(DISCHARGE), pred.format="conc", 
  data=fitdat, metadata=meta, retrans=exp)

# Now you can plot
plotEGRET("boxConcMonth", data = lamprey_nitrate, meta = meta)
#> Many EGRET functions will not work with missing values.
#> Error in mutate(., dateTime = flowdat[[date.colname]]): ℹ In argument: `dateTime = flowdat[[date.colname]]`.
#> Caused by error:
#> ! `dateTime` must be size 4740 or 1, not 555.
plotEGRET("multiPlotDataOverview", load.model=no3_lm, newdata=estdat)
#> Many EGRET functions will not work with missing values.
#> Error in mutate(., dateTime = flowdat[[date.colname]]): ℹ In argument: `dateTime = flowdat[[date.colname]]`.
#> Caused by error:
#> ! `dateTime` must be size 4749 or 1, not 18996.
```
