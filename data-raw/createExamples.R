# Create example datasets and R objects for use in exploring and testing 
# loadflex. Only needs to be run when re-creating the .rda objects
library(dplyr)
data(lamprey_nitrate)
data(lamprey_discharge)
date_range <- as.POSIXct(c("2008-10-01 00:00:00", "2012-10-01 00:00:00"), tz="EST5EDT")
set.seed('6509')

eg_fitdat <- lamprey_nitrate %>%
  dplyr::filter(between(DATE, date_range[1], date_range[2])) %>%
  dplyr::filter(REGR==TRUE) %>%
  slice(sort(sample.int(n(), size=50))) %>%
  select(-REGR)

eg_estdat <- lamprey_discharge %>%
  dplyr::filter(between(DATE, date_range[1], date_range[2])) %>%
  slice(seq(1, n(), 16))

eg_metadata <- metadata(
  constituent="NO3", flow="DISCHARGE", load.rate='NO3_FLUX',
  dates="DATE", conc.units="mg L^-1", flow.units="cfs", load.units="kg", 
  load.rate.units="kg d^-1", site.name="Lamprey River, NH",
  consti.name="Nitrate", site.id='01073500', lat=43.10259, lon=-70.95256)

eg_loadInterp <- loadInterp(
  interp.format="conc", interp.fun=rectangularInterpolation, 
  data=eg_fitdat, metadata=eg_metadata)

eg_loadLm <- loadLm(
  formula=log(NO3) ~ log(DISCHARGE), pred.format="conc", 
  data=eg_fitdat, metadata=eg_metadata, retrans=exp)

eg_loadReg2 <- loadReg2(
  loadReg(NO3 ~ model(9), data=eg_fitdat,
          flow="DISCHARGE", dates="DATE", time.step="instantaneous", 
          flow.units="cfs", conc.units="mg/L", load.units="kg",
          station='Lamprey River, NH'))

eg_loadComp <- loadComp(
  reg.model=eg_loadReg2, interp.format="conc", 
  interp.data=eg_fitdat)

devtools::use_data(
  eg_fitdat, eg_estdat, eg_metadata,
  eg_loadInterp, eg_loadLm, eg_loadReg2, eg_loadComp,
  overwrite=TRUE, compress='xz')

tools::checkRdaFiles('data')
