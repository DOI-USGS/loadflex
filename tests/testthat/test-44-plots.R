context("plotEGRET")

# Setup of data to use in tests
data(lamprey_nitrate)
fitdat <- lamprey_nitrate
data(lamprey_discharge)
estdat <- subset(lamprey_discharge, DATE < as.POSIXct("2012-10-01 00:00:00", tz="EST5EDT"))
estdat <- estdat[seq(1, nrow(estdat), by=96/4),] # only keep 4 observations per day
meta <- metadata(constituent="NO3", flow="DISCHARGE", dates="DATE",
                 conc.units="mg L^-1", flow.units="cfs", load.units="kg", load.rate.units="kg d^-1",
                 site.name="Lamprey River, NH", site.id='NWIS 01073500', consti.name = "nitrate")
conc_lm <- loadLm(formula=log(NO3) ~ log(DISCHARGE), pred.format="conc",
                  data=fitdat, metadata=meta, retrans=exp)
preds <- predictSolute(conc_lm, "conc", estdat, se.pred=TRUE, date=TRUE)
preds_flux <- predictSolute(conc_lm, "flux", estdat, se.pred=TRUE, date=TRUE)

test_that("plotEGRET will reject a fake plot name", {
  expect_error(plotEGRET(plot.name = 'myplot', meta = meta),
               'unrecognized plot.name: myplot')
})

test_that("plotEGRET will reject a plot type that is missing required data", {
  expect_error(plotEGRET(plot.name = 'plotConcTime', data = fitdat),
               'missing data requirements for plotConcTime')
})

test_that("plotEGRET works for fitdat and meta", {
  plotEGRET(plot.name = 'plotConcQ', data = fitdat, meta = meta)
  expect_false(is.null(dev.list()))
  dev.off()
})

test_that("plotEGRET works for fitdat, estdat, preds, and meta", {
  plotEGRET(plot.name = 'boxQTwice', load.model = conc_lm, newdata = estdat)
  expect_false(is.null(dev.list()))
  dev.off()
})
