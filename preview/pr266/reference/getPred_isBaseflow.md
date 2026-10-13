# Create a logical vector indicating whether flow at each time point is dominantly baseflow.

Internal for now; not yet ready to be a part of the official API

## Usage

``` r
getPred_isBaseflow(data, metadata, baseflow, threshold = 0.8)
```

## Arguments

- data:

  a data.frame of data including a column for flow (with that column
  name specified by metadata).

- metadata:

  a metadata object specifying, at minimum, the column name in `data`
  where total flow is stored.

- baseflow:

  a vector of baseflow values of the same length as `nrow(data)`.

- threshold:

  numeric. The minimum fraction of flow that must be baseflow for a time
  point to be classified as baseflow (TRUE).

## See also

Other predictors:
[`getPred_baseflow()`](http://doi-usgs.github.io/loadflex/reference/getPred_baseflow.md)
