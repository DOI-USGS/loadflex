# observeSolute - instantaneous loads or concentrations

Calculates observed instantaneous loading rates or concentrations from
observed concentrations, flows, and/or fluxes, with units conversions
according to the supplied metadata.

## Usage

``` r
observeSolute(
  data,
  flux.or.conc = c("flux", "conc"),
  metadata,
  calculate = isTRUE(flux.or.conc == "flux"),
  attach.units = FALSE
)
```

## Arguments

- data:

  data.frame containing, at a minimum, the columns named by
  metadata@constituent and metadata@flow

- flux.or.conc:

  character giving the desired output format

- metadata:

  An object of class "metadata" describing the units of flow
  (flow.units) and concentration (conc.units) of the input data, and the
  desired units of load (load.rate.units) for the output data

- calculate:

  logical. If FALSE, looks for a column containing the output of
  interest. If true, uses the other two columns (out of those for conc,
  flow, and flux) to calculate the output of interest.

- attach.units:

  logical. If TRUE, the converted observations are returned with units
  attached.

## Examples

``` r
data(eg_metadata)
obs <- data.frame(MyConc=(1:10)/10, MyFlow=rep(10,10), MyFlux=2) # intentionally inconsistent
md <- updateMetadata(eg_metadata, constituent="MyConc", flow="MyFlow", 
  load.rate="MyFlux", dates="none", flow.units="cms", conc.units="mg/l", 
  load.units="g", load.rate.units="g/s", custom=NULL)

observeSolute(obs, "flux", md, attach.units=TRUE) # calculate from conc & flow
#> unitted numeric (g s^-1)
#>  [1]  1  2  3  4  5  6  7  8  9 10
observeSolute(obs, "flux", md, calculate=FALSE, attach.units=TRUE) # read flux from data
#> unitted numeric (g s^-1)
#>  [1] 2 2 2 2 2 2 2 2 2 2
observeSolute(obs, "conc", md, calculate=TRUE, attach.units=TRUE) # calculate from flow & flux
#> unitted numeric (mg L^-1)
#>  [1] 0.2 0.2 0.2 0.2 0.2 0.2 0.2 0.2 0.2 0.2
observeSolute(obs, "conc", md, calculate=FALSE, attach.units=TRUE) # read conc from data
#> unitted numeric (mg L^-1)
#>  [1] 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 1.0
```
