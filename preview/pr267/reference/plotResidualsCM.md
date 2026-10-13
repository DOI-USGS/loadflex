# Plot concentration or flux residuals.

Not sure we want to include this function in the official API, so
keeping it internal for now.

## Usage

``` r
plotResidualsCM(
  type,
  load.model,
  observations,
  resids,
  dateField = "Date",
  verbose = FALSE,
  xObservations = FALSE,
  xFlow = FALSE,
  day.of.year = FALSE
)
```

## Arguments

- load.model:

  use this to pull names of date column and constituent name

- observations:

  these are the observations to plot the residuals for

- dateField:

  name of the date field in observations

- verbose:

  debugging flag

- day.of.year:

  flag if true will plot residuals by the day of year to,

- residuals:

  the observation residuals
