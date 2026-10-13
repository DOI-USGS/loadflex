# Example datasets and objects for the loadflex package

These datasets and pre-created objects are provided for exploring and
testing the loadflex package.

## Datasets

- `eg_fitdat`:

  Example dataset for fitting (calibrating) a model. These data are a
  lightweight subset of `lamprey_nitrate`, below.

- `eg_estdat`:

  Example dataset for generating predictions from a fitted model. These
  data are a lightweight subset of `lamprey_discharge`, below.

- `lamprey_discharge`:

  Discharge data for the Lamprey River from 10/1/1999 to 11/16/2014.
  Discharge in CFS measured every 15 minutes and collected by the US
  Geological Survey, site 01073500, waterdata.usgs.gov. The Lamprey
  River is an 81-km river flowing through southeastern New Hampshire.
  Its 548-km2 watershed empties into the Great Bay estuary.

- `lamprey_nitrate`:

  Nitrate data for the Lamprey River from 10/1/1999 to 11/16/2014.
  Nitrate is in mg/L and is collected weekly. The Lamprey River is an
  81-km river flowing through southeastern New Hampshire. Its 548-km2
  watershed empties into the Great Bay estuary. Nitrate concentrations
  have been monitored with weekly and event-based grab samples at
  Packers Falls on the Lamprey since 10 September 1999 and are measured
  by SmartChem discrete analyzer (Westco, Brookfield, CT).

## Objects

- `eg_metadata`:

  Example metadata object.

- `eg_loadInterp`:

  Example interpolation model object.

- `eg_loadLm`:

  Example linear regression model object.

- `eg_loadReg2`:

  Example model object containing an inner rloadest model.

- `eg_loadComp`:

  Example composite method model object.
