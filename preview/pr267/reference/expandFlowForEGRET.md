# Convert a date and discharge data.frame into EGRET format

Use EGRET functions to convert a data.frame of date and discharge
columns into "the basic Daily data frame used in WRTDS", i.e., a
13-column data.frame with parsed datetimes, log(Q), and 7-day and 30-day
smoothed Q, plus a column for dateTime. See
[`populateDaily`](https://rdrr.io/pkg/EGRET/man/populateDaily.html)

## Usage

``` r
expandFlowForEGRET(flowdat, flow.colname, date.colname, flow.units)
```

## Arguments

- flowdat:

  data frame with discharge values to convert

- flow.colname:

  character string giving the column name that corresponds to flow

- date.colname:

  character string giving the column name that corresponds to dates

- flow.units:

  character string giving the current units of flow in flowdat
