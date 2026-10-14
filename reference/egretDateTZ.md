# Pick a timezone for converting EGRET datetimes to calendar days

Use the timezone carried by the datetime vector so that \`as.Date()\`
does not silently fall back to UTC and shift observations to the wrong
calendar day.

## Usage

``` r
egretDateTZ(datetimes)
```

## Arguments

- datetimes:

  a Date or POSIXct vector
