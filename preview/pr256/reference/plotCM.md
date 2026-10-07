# Create plots for examining the results from the composite method

Not sure we want to include this function in the official API, so
keeping it internal for now.

## Usage

``` r
plotCM(
  flux.or.conc,
  show.observations,
  load.model,
  finalloads,
  observations = NULL,
  composite = TRUE,
  linear.interpolation = FALSE,
  regression = TRUE,
  xrange = "none",
  dateField = "Date",
  verbose = FALSE
)
```

## Arguments

- flux.or.conc:

  character indicating whether the plots are of concentrations or loads,
  this is used for string concatation to name the columns of data
  produced through composite method.

- show.observations:

  true or false display observations or not.

- load.model:

  use this to pull names of date column and constituent name

- finalloads:

  these are the loads or concentrations to plot

- observations:

  these are the observations to plot, if you are plotting observations

- composite, :

  TRUE or FALSE flag for whether or not to display the composite results

- linear.interpolation:

  TRUE or FALSE flag for whether or not to display linear interpolation
  of the observations

- regression:

  TRUE or FALSE flag for whether or not to display the linear regression
  results

- xrange, :

  a user provded range for the xvalues (dates)

- verbose:

  flag for to print for debugging
