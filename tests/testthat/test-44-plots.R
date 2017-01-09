
context("plotEGRET")

# Setup of data to use in tests

intdat <- data.frame(
  conc=c(5,4,2,6,9,8),
  discharge=10,
  datetime=strptime(paste0("2000-05-",c(2,5,13,15,23,31)),format="%Y-%m-%d"))
estdat <- data.frame(
  discharge=rep(c(13,15,1,14,23,31,7,11,5,27), 3),
  datetime=strptime(paste0("2000-05-",1:30),format="%Y-%m-%d"))
meta <- metadata(constituent="conc", flow="discharge", dates="datetime", 
                 conc.units="mg L^-1", flow.units="cfs", load.units="kg",
                 load.rate.units="kg d^-1", station="Example Station",
                 custom=list(sta.abbr = "examp",
                             consti.name = "nitrate"))
conc_lm <- loadLm(formula=log(conc) ~ log(discharge), pred.format="conc", 
                  data=intdat, metadata=meta, retrans=exp)
preds <- predictSolute(conc_lm, "conc", estdat, se.pred=TRUE, date=TRUE)
preds_flux <- predictSolute(conc_lm, "flux", estdat, se.pred=TRUE, date=TRUE)

test_that("plotEGRET will reject a fake plot name")
expect_error(plotEGRET(plot.name = 'myplot', meta = meta),
             'unrecognized plot.name: myplot')

test_that("plotEGRET will reject a plot type that is missing required data")
expect_error(plotEGRET(plot.name = 'plotConcTime', intdat = intdat),
             'missing data requirements for ConcTime')

test_that("plotEGRET works for intdat and meta")
plotEGRET(plot.name = 'plotConcQ', intdat = intdat, meta = meta)
expect_false(is.null(dev.list()))
dev.off()

test_that("plotEGRET works for intdat, estdat, preds, and meta")
plotEGRET(plot.name = 'boxQTwice', intdat = intdat, 
          estdat = estdat, preds = preds, meta = meta)
expect_false(is.null(dev.list()))
dev.off()
