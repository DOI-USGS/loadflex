# Setup of data to use in tests

fitdat <- data.frame(
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
                  data=fitdat, metadata=meta, retrans=exp)
preds <- suppressWarnings(predictSolute(conc_lm, "conc", estdat, se.pred=TRUE, date=TRUE))
preds_flux <- suppressWarnings(predictSolute(conc_lm, "flux", estdat, se.pred=TRUE, date=TRUE))

context("convertToEGRET")

test_that("convertToEGRET fails when missing metadata", {
  expect_error(loadflex:::convertToEGRET(fitdat = fitdat),
               'metadata is required to create an EGRET eList')
})

test_that("convertToEGRET creates EGRET object without fitdat", {
  eList <- loadflex:::convertToEGRET(meta = meta, estdat = estdat, preds = preds)
  expect_true(is.na(eList$Sample))
})

test_that("convertToEGRET creates EGRET object without estdat & preds", {
  eList <- loadflex:::convertToEGRET(meta = meta, fitdat = fitdat)
  expect_true(is.na(eList$Daily))
})

test_that("convertToEGRET creates EGRET object with only metadata", {
  eList <- loadflex:::convertToEGRET(meta = meta)
  expect_true(is.na(eList$Sample))
  expect_true(is.na(eList$Daily))
})

test_that("convertToEGRET returns NA for Daily when predictions are missing", {
  eList <- loadflex:::convertToEGRET(meta = meta, estdat = estdat)
  expect_true(is.na(eList$Daily))
})

test_that("convertToEGRET returns NA for Daily when estimated data are missing", {
  eList <- loadflex:::convertToEGRET(meta = meta, preds = preds)
  expect_true(is.na(eList$Daily))
})


context("convertToEGRETInfo")

test_that("convertToEGRETInfo fails without metadata", {
  expect_error(loadflex:::convertToEGRETInfo(meta = NULL),
               'metadata is required to create an EGRET eList')
})

test_that("convertToEGRETInfo correctly converts metadata", {
  INFO <- loadflex:::convertToEGRETInfo(meta = meta)
  expect_equal(INFO$staAbbrev, "examp")
  expect_equal(INFO$param.units, "mg L^-1")
  expect_equal(INFO$shortName, "Example Station")
})


context("convertToEGRETSample")

test_that("convertToEGRETSample returns NA without fitdat", {
  expect_equal(loadflex:::convertToEGRETSample(meta = meta, fitdat = NULL), NA)
})

test_that("convertToEGRETSample correctly converts", {
  Sample <- loadflex:::convertToEGRETSample(meta = meta, fitdat = fitdat)
  expect_equal(nrow(Sample), 6)
  expect_equal(ncol(Sample), 15)
  
  expected_cols <- c("Date", "ConcLow", "ConcHigh", "Uncen", "ConcAve", "Julian", 
                     "Month", "Day", "DecYear", "MonthSeq", "waterYear", "SinDY", 
                     "CosDY", "dateTime", "Q")
  expect_false(any(is.na(match(names(Sample), expected_cols))))
})


context("convertToEGRETDaily")

test_that("convertToEGRETDaily returns NA without estdat or preds", {
  expect_equal(loadflex:::convertToEGRETDaily(meta = meta, estdat = NULL, preds = preds), NA)
  expect_equal(loadflex:::convertToEGRETDaily(meta = meta, estdat = estdat, preds = NULL), NA)
})

test_that("convertToEGRETDaily correctly converts", {
  Daily <- loadflex:::convertToEGRETDaily(meta = meta, estdat = estdat, preds = preds)
  expect_equal(nrow(Daily), 30)
  expect_equal(ncol(Daily), 18)
  
  expected_cols <- c("Date", "Q", "Julian", "Month", "Day", "DecYear", "MonthSeq",  
                     "waterYear", "Qualifier", "i", "LogQ", "Q7", "Q30", "dateTime",
                     "ConcDay", "SE", "FluxDay", "yHat")
  expect_false(any(is.na(match(names(Daily), expected_cols))))  
})

test_that("convertToEGRETDaily fails with an incorrect preds.type", {
  expect_error(loadflex:::convertToEGRETDaily(meta = meta, estdat = estdat, 
                                              preds = preds, preds.type = "nonsense"),
               'preds.type %in%')
})

test_that("convertToEGRETDaily works with preds.type = 'Flux'", {
  Daily_flux <- loadflex:::convertToEGRETDaily(meta = meta, estdat = estdat,
                                               preds = preds_flux, preds.type = "Flux")
  expect_equal(nrow(Daily_flux), 30)
  expect_equal(ncol(Daily_flux), 18)
  
  expected_cols <- c("Date", "Q", "Julian", "Month", "Day", "DecYear", "MonthSeq",  
                     "waterYear", "Qualifier", "i", "LogQ", "Q7", "Q30", "dateTime",
                     "ConcDay", "SE", "FluxDay", "yHat")
  expect_false(any(is.na(match(names(Daily_flux), expected_cols))))
})


context("verify_meta")

test_that("verify_meta fails when the item you requested is empty", {
  expect_error(loadflex:::verify_meta(meta, "load.rate"),
               "metadata item `load.rate` must exist")
})

test_that("verify_meta works for custom metadata fields", {
  expect_equal(loadflex:::verify_meta(meta, c('custom', 'sta.abbr')), 'examp')
})

test_that("verify_meta works for regular fields", {
  expect_equal(loadflex:::verify_meta(meta, 'flow'), 'discharge')
})


context("flowCorrectionEGRET")

test_that("flowCorrectionEGRET returns correct columns", {
  corrected_flow_df <- loadflex:::flowCorrectionEGRET(estdat, 'discharge', 'datetime', 35.314667)
  expect_equal(nrow(Daily_flux), 30)
  expect_equal(ncol(Daily_flux), 14)
  expected_cols <- c("Date", "Q", "Julian", "Month", "Day", "DecYear", "MonthSeq",
                     "waterYear", "Qualifier", "i", "LogQ", "Q7", "Q30", "dateTime")
  expect_false(any(is.na(match(names(corrected_flow_df), expected_cols))))
})
