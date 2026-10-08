# Helper function for plotting: transform dates from something to something.

We definitely don't want to include this function in the official API,
so keeping it internal.

## Usage

``` r
transformDates(plotsols)
```

## Arguments

- plotsols:

  A data.frame with a DATE column

## Value

same data.frame but with DATE field converted to POSIXct or lt
