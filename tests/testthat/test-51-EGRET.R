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

context("convertToEGRET")

test_that("convertToEGRET allows metadata only", {
  eList <- loadflex:::convertToEGRET(meta = meta)
  expect_is(eList, 'egret')
  expect_true(is.na(eList$Sample))
  expect_true(is.na(eList$Daily))
})

test_that("convertToEGRET fails when missing metadata", {
  expect_error(loadflex:::convertToEGRET(data = fitdat),
               'metadata is required to create an EGRET eList')
})

test_that("convertToEGRET complains about redundant metadata or data", {
  expect_warning(loadflex:::convertToEGRET(data = fitdat, load.model = conc_lm),
                 'data arg will be overridden by load.model')
  expect_warning(loadflex:::convertToEGRET(meta = meta, load.model = conc_lm),
                 'meta arg will be overridden by load.model')
})

test_that("convertToEGRET allows metadata and data or newdata without model", {
  eList <- loadflex:::convertToEGRET(data = fitdat, meta = meta)
  expect_true(is.na(eList$Daily))
  expect_is(eList$Sample, 'data.frame')
  
  eList <- loadflex:::convertToEGRET(newdata = estdat, meta = meta)
  expect_true(is.na(eList$Sample))
  expect_is(eList$Daily, 'data.frame')
  
  eList <- loadflex:::convertToEGRET(data = fitdat, newdata = estdat, meta = meta)
  expect_is(eList$Sample, 'data.frame')
  expect_is(eList$Daily, 'data.frame')
})

test_that("convertToEGRET returns prediction-free Daily when predictions are missing", {
  yespreds <- loadflex:::convertToEGRET(newdata = estdat, load.model=conc_lm)
  nopreds <- loadflex:::convertToEGRET(newdata = estdat, data = fitdat, meta = meta)
  expect_equal(c("ConcDay","FluxDay","SE","yHat"), sort(setdiff(names(yespreds$Daily), names(nopreds$Daily))))
})


context("convertToEGRETInfo")

test_that("convertToEGRETInfo fails without metadata", {
  expect_error(loadflex:::convertToEGRETInfo(meta = NULL),
               'metadata is required to create an EGRET eList')
})

test_that("convertToEGRETInfo correctly converts metadata", {
  INFO <- loadflex:::convertToEGRETInfo(meta = meta)
  expect_equal(INFO$staAbbrev, "NWIS 01073500")
  expect_equal(INFO$param.units, "mg/l")
  expect_equal(INFO$shortName, "Lamprey River, NH")
})


context("convertToEGRETSample")

test_that("convertToEGRETSample returns NA without fitdat", {
  expect_equal(loadflex:::convertToEGRETSample(meta = meta, data = NULL), NA)
})

test_that("convertToEGRETSample correctly converts", {
  Sample <- loadflex:::convertToEGRETSample(meta = meta, data = fitdat)
  expect_equal(nrow(Sample), nrow(data))
  expect_equal(ncol(Sample), 15)
  
  expected_cols <- c("Date", "ConcLow", "ConcHigh", "Uncen", "ConcAve", "Julian", 
                     "Month", "Day", "DecYear", "MonthSeq", "waterYear", "SinDY", 
                     "CosDY", "dateTime", "Q")
  expect_equal(sort(names(Sample)), sort(expected_cols))
})


context("convertToEGRETDaily")

test_that("convertToEGRETDaily returns NA without estdat or preds", {
  expect_equal(loadflex:::convertToEGRETDaily(meta = meta), NA)
})

test_that("convertToEGRETDaily returns partial info without preds", {
  Daily <- loadflex:::convertToEGRETDaily(meta = meta, newdata = estdat)
  expect_equal(ncol(Daily), 14)
})

test_that("convertToEGRETDaily correctly converts", {
  Daily <- loadflex:::convertToEGRETDaily(newdata = estdat, load.model = conc_lm)
  expect_equal(nrow(Daily), nrow(estdat))
  expect_equal(ncol(Daily), 18)
  
  expected_cols <- c("Date", "Q", "Julian", "Month", "Day", "DecYear", "MonthSeq",  
                     "waterYear", "Qualifier", "i", "LogQ", "Q7", "Q30", "dateTime",
                     "ConcDay", "SE", "FluxDay", "yHat")
  expect_equal(sort(names(Daily)), sort(expected_cols))
})


context("verify_meta")

test_that("verify_meta fails when the item you requested is empty", {
  expect_error(loadflex:::verify_meta(meta, "load.rate"),
               "metadata item `load.rate` must exist")
})

test_that("verify_meta works for custom metadata fields", {
  mu <- updateMetadata(meta, custom=list('fieldA'=7:10, 'B'='wahoo'))
  expect_equal(loadflex:::verify_meta(mu, c('custom', 'B')), 'wahoo')
})

test_that("verify_meta works for regular fields", {
  expect_equal(loadflex:::verify_meta(meta, 'flow'), 'DISCHARGE')
})


context("expandFlowForEGRET")

test_that("expandFlowForEGRET returns correct columns", {
  corrected_flow_df <- loadflex:::expandFlowForEGRET(estdat, 'DISCHARGE', 'DATE', 'ft^3 s^-1')
  expect_equal(nrow(corrected_flow_df), nrow(estdat))
  expect_equal(ncol(corrected_flow_df), 14)
  expected_cols <- c("Date", "Q", "Julian", "Month", "Day", "DecYear", "MonthSeq",
                     "waterYear", "Qualifier", "i", "LogQ", "Q7", "Q30", "dateTime")
  expect_equal(sort(names(corrected_flow_df)), sort(expected_cols))
})
