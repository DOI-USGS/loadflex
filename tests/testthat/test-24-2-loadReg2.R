tryCatch({source("tests/testthat/helpers.R"); source("helpers.R")}, warning=function(w) invisible())

#' loadReg2 is a wrapper for loadReg produced by rloadest.
#library(rloadest)
# 
# test_that("loadReg2 models can be created", {
#   
#   simpledata <- transform(rloadest::app2.calib[-which(diff(rloadest::app2.calib$DATES) < 7),], 
#                           Period=smwrBase::seasons(DATES,breaks=c("Apr", "Jul")))
#   
#   # loadReg2 requires that you create the loadReg object within the loadReg2 call
#   datename <- "DATES"
#   library(smwrBase) # b/c rloadest:::loadestTadj calls dectime without declaring imports smwrBase
#   lr <- rloadest::loadReg(
#     Atrazine ~ Period*rloadest::center(log(FLOW)), 
#     data = simpledata,
#     flow = "FLOW", dates = datename, conc.units="mg/L")
#   #detach(package:smwrBase)
#   expect_error(loadReg2(lr), "load.reg must be a call")
#   expect_error(loadReg2(function(lr){lr}()), "load.reg must be a call")
#   
#   # Fit the model by the rules
#   expect_error(loadReg2(
#     loadReg(
#       Atrazine ~ Period*center(log(FLOW)), 
#       data = simpledata,
#       flow = "FLOW", dates = "DATES", conc.units="mg/L")), "library(rloadest)", fixed=TRUE)
#   library(rloadest)
#   expect_message(load.model <- loadReg2(
#     loadReg(
#       Atrazine ~ Period*center(log(FLOW)), 
#       data = simpledata,
#       flow = "FLOW", dates = "DATES", conc.units="mg/L")), "citation")
#   # expect no message the second and subsequent times:
#   load.model <- loadReg2(
#     loadReg(
#       Atrazine ~ Period*center(log(FLOW)), 
#       data = simpledata,
#       flow = "FLOW", dates = "DATES", conc.units="mg/L"))
#   # detach rloadest and all of the other packages that got loaded in order to
#   # load rloadest
#   detach(package:rloadest)
#   detach(package:smwrQW)
#   detach(package:smwrStats)
#   detach(package:smwrGraphs)
#   detach(package:smwrBase)
#   detach(package:dataRetrieval)
#   detach(package:lubridate)
#   expect_is(load.model, "loadReg2")
# })

# getting error on line 75 mcl 1-22-16
# test_that("loadReg2 models implement the loadModelInterface", {
#   simpledata <- transform(rloadest::app2.calib[-which(diff(rloadest::app2.calib$DATES) < 7),], 
#                           Period=smwrBase::seasons(DATES,breaks=c("Apr", "Jul")))
#   
#   # Fit the model
#   library(rloadest)
#   suppressMessages(load.model <- loadReg2(loadReg(
#     Atrazine ~ Period*center(log(FLOW)), 
#     data = simpledata,
#     flow = "FLOW", dates = "DATES", conc.units="mg/L")))
#   detach(package:rloadest)
#   detach(package:smwrQW)
#   detach(package:smwrStats)
#   detach(package:smwrGraphs)
#   detach(package:smwrBase)
#   detach(package:dataRetrieval)
#   detach(package:lubridate)
#   
#   # Try running these to see if we get errors
#   getMetadata(load.model)
#   getFittingData(load.model)
#   getFittingFunction(load.model)
#   predictSolute(load.model, flux.or.conc="flux")
#   
#   # Use the standard validation function to test this
#   expect_true(validLoadModelInterface(load.model))
# })

# 
# test_that("getFittingFunction.loadReg2() works robustly", {
#   # create a loadReg object to work with
#   simpledata <- transform(app2.calib[-which(diff(app2.calib$DATES) < 7),], 
#                           Period=seasons(DATES,breaks=c("Apr", "Jul")))
#   flowcolname <- "FLOW"
#   load.model <- loadReg2(loadReg(
#     Atrazine ~ Period*center(log(FLOW)), 
#     data = simpledata,
#     flow = flowcolname, dates = "DATES", conc.units="mg/L"))
#   
#   # A basic test passes: the refitting function will create a new load model
#   load.model.2 <- getFittingFunction(load.model)(simpledata, store=c("data","fitting.function"))
#   expect_is(load.model.2, "loadReg2")
#   expect_equal(load.model@fit$lfit[c(1:31,33:34)], load.model.2@fit$lfit[c(1:31,33:34)]) # the fits are identical in many ways...
#   expect_true(as.character(load.model@fit$lfit[32]) != as.character(load.model.2@fit$lfit[32])) # and the call is different, as we intended
#   
#   # And the new load.model.2 is reasonably stable because anything visible :
#   expect_equal(getFittingData(load.model), getFittingData(load.model.2)) # breaks for loadReg but works for loadReg2
#   
#   # print.censReg has this line: x1 <- update(x, formula = . ~ 1) which relies
#   # on the presence of training.data in the global environment, so this breaks:
#   expect_error(expect_output(load.model.2@fit$lfit, "loadReg(formula ="))
#   # I think I'm OK with letting that error persist; the error belongs primarily
#   # to USGSwsQW rather than to us
# })
# 
# 
# test_that("getFittingData.loadReg2() works robustly", {
#   # create a loadReg object to work with
#   simpledata <- transform(app2.calib[-which(diff(app2.calib$DATES) < 7),], 
#                           Period=seasons(DATES,breaks=c("Apr", "Jul")))
#   load.model <- loadReg2(loadReg(
#     Atrazine ~ Period*center(log(FLOW)), 
#     data = simpledata,
#     flow = "FLOW", dates = "DATES", conc.units="mg/L"))
#   
#   # Demonstrate simple use of getFittingData
#   fitdata1 <- getFittingData(load.model)
#   
#   # Try to trick the data function by changing simpledata - you can't!
#   simpledata <- simpledata[1:5,]
#   fitdata2 <- getFittingData(load.model)
#   
#   # The fitting data from load.model doesn't change when the original simpledata changes. That's good.
#   expect_equal(fitdata1, fitdata2)
# })

# 
# test_that("predictSolute.loadReg2() works", {
#   simpledata <- transform(app2.calib[-which(diff(app2.calib$DATES) < 7),], 
#                           Period=seasons(DATES,breaks=c("Apr", "Jul")))
#   
#   load.model <- loadReg2(loadReg(
#     Atrazine ~ Period*center(log(FLOW)), 
#     data = simpledata,
#     flow = "FLOW", dates = "DATES", conc.units="mg/L"))
#   
#   app2_est <- transform(app2.est, Period=seasons(DATES,breaks=c("Apr", "Jul")))
#   obs <- v(data.frame(simpledata, FLUX=observeSolute(simpledata, "flux", getMetadata(load.model))))
#   
#   # Predictions without newdata should equal those where newdata==training.data. They do.
#   expect_equal(predictSolute(load.model, "flux", newdata=simpledata), 
#                predictSolute(load.model, "flux"))
#   expect_equal(predictSolute(load.model, "conc", newdata=simpledata), 
#                predictSolute(load.model, "conc"))
#   
#   # Flux predictions for new data
#   predsF <- predictSolute(load.model, "flux", app2_est, attach.units=TRUE)
#   expect_equal(length(predsF), nrow(app2_est))
#   expect_equal(get_units(predsF), getMetadata(load.model)@load.rate.units)
#   
#   # Conc predictions for new data
#   predsC <- predictSolute(load.model, "conc", app2_est, attach.units=TRUE)
#   expect_equal(length(predsC), nrow(app2_est))
#   expect_equal(get_units(predsC), getMetadata(load.model)@conc.units)
#   
#   # Here's the graphical check on flux predictions:
#   pred <- data.frame(Date=app2_est$DATES, Flux=v(predsF))
#   print(ggplot(obs, aes(x=DATES, y=FLUX)) + theme_bw() + scale_y_log10() + 
#           geom_point(data=pred, aes(x=Date, y=Flux), color="blue") + 
#           geom_line(data=pred, aes(x=Date, y=Flux), color="blue") +
#           geom_line(color="green") + geom_point(color="green"))
#   expect_manual_OK("Flux: the blue dots (preds) should agree reasonably with the green dots (obs)")
#   
#   # and on conc predictions:
#   pred <- data.frame(Date=app2_est$DATES, Conc=v(predsC))
#   print(ggplot(obs, aes(x=DATES, y=Atrazine)) + theme_bw() + scale_y_log10() + 
#           geom_point(data=pred, aes(x=Date, y=Conc), color="blue") + 
#           geom_line(data=pred, aes(x=Date, y=Conc), color="blue") +
#           geom_line(color="green") + geom_point(color="green"))
#   expect_manual_OK("Conc: the blue dots (preds) should agree reasonably with the green dots (obs)")
#   
#   # Passing in load.units should change both the results and the attached units
#   expect_equivalent(predictSolute(load.model, "flux", newdata=simpledata, load.units="g"), 
#                     predictSolute(load.model, "flux", newdata=simpledata)*1000)
#   expect_equal(get_units(predictSolute(load.model, "flux", newdata=simpledata, load.units="g", attach.units=TRUE)), "g d^-1")
# })

# 
# test_that("loadReg2 models can find and report their uncertainty", {
#   
#   # Sample data and model
#   simpledata <- transform(app2.calib[-which(diff(app2.calib$DATES) < 7),], 
#                           Period=seasons(DATES,breaks=c("Apr", "Jul")))
#   
#   load.model <- loadReg2(loadReg(
#     Atrazine ~ Period*center(log(FLOW)), 
#     data = simpledata,
#     flow = "FLOW", dates = "DATES", conc.units="mg/L"))
#   
#   app2_est <- transform(app2.est, Period=seasons(DATES,breaks=c("Apr", "Jul")))
#   obs <- data.frame(simpledata, FLUX=observeSolute(simpledata, "flux", getMetadata(load.model)))
#     
#   # Predictions for new data
#   predsF <- predictSolute(load.model, "flux", app2_est)
#   predsC <- predictSolute(load.model, "conc", app2_est)
#   
#   # Setting se.fit or se.pred to TRUE should result in a data.frame with extra information
#   expect_is(predictSolute(load.model, "flux", newdata=simpledata, se.fit=TRUE), "data.frame")
#   expect_is(predictSolute(load.model, "conc", newdata=simpledata, se.fit=TRUE), "data.frame")
#   expect_equal(colnames(predictSolute(load.model, "flux", newdata=simpledata, se.fit=TRUE)), c("fit","se.fit"))
#   expect_equal(colnames(predictSolute(load.model, "conc", newdata=simpledata, se.fit=TRUE)), c("fit","se.fit"))
#   expect_equal(colnames(predictSolute(load.model, "flux", newdata=simpledata, se.fit=TRUE, se.pred=TRUE)), c("fit","se.fit","se.pred"))
#   expect_equal(colnames(predictSolute(load.model, "conc", newdata=simpledata, se.fit=TRUE, se.pred=TRUE)), c("fit","se.fit","se.pred"))
#   expect_equal(colnames(predictSolute(load.model, "flux", newdata=simpledata, se.pred=TRUE)), c("fit","se.pred"))
#   expect_equal(colnames(predictSolute(load.model, "conc", newdata=simpledata, se.pred=TRUE)), c("fit","se.pred"))
#   
#   # Confidence intervals make sense
#   expect_error(predictSolute(load.model, "flux", newdata=simpledata, interval = "confidence"), "not implemented for loadReg2")
#   print(ggplot(data.frame(date=app2_est$DATES[51:100], predictSolute(load.model, "flux", newdata=app2_est[51:100,], interval = "prediction")),
#          aes(x=date, y=fit)) + geom_point() + geom_line() + geom_ribbon(aes(ymin=lwr, ymax=upr), fill="green", color="green", alpha=0.2) + theme_bw())
#   expect_manual_OK("flux preds & pred intervals look good")
#   
#   expect_error(predictSolute(load.model, "conc", newdata=simpledata, interval = "confidence"), "not implemented for loadReg2")
#   print(ggplot(data.frame(date=app2_est$DATES[51:100], predictSolute(load.model, "conc", newdata=app2_est[51:100,], interval = "prediction")),
#                aes(x=date, y=fit)) + geom_point() + geom_line() + geom_ribbon(aes(ymin=lwr, ymax=upr), fill="brown", color="brown", alpha=0.2) + theme_bw())
#   expect_manual_OK("conc preds & pred intervals look good")
# })

# 
# test_that("simulateSolute.loadReg2 looks OK", {
#   # libraries for %>% and gather
#   library(dplyr)
#   library(tidyr)
#   
#   # data for testing
#   mydat <- transform(
#     data.frame(conc=c(5,4,2,6,9,8,9,7,4,3),discharge=c(4,3,3,7,10,8,9,6,5,2),datetime=strptime(paste0("2000-05-",1:10),format="%Y-%m-%d")),
#     dtsimple=(as.numeric(datetime)-mean(as.numeric(datetime)))/sd(as.numeric(datetime)))
#   mymd <- metadata(constituent="conc", flow="discharge", load.rate="", dates="datetime",
#                    conc.units="mg/L", flow.units="cfs", load.units="kg", load.rate.units="kg/day",
#                    station="", custom=NULL)
#   mydat$flux <- observeSolute(mydat, "flux", mymd, calc=TRUE)
#   
#   # simulations are pretty fast for both conc and flux
#   lr <- loadReg2(loadReg(conc ~ discharge + dtsimple, data = mydat, flow = "discharge", dates = "datetime", conc.units="mg/L", time.step="instantaneous"))
#   lr_c_p <- system.time(sims <- replicate(10, simulateSolute(lr, "conc", method="param", from.interval="confidence")))[[1]]
#   lr_c_n <- system.time(sims <- replicate(10, simulateSolute(lr, "conc", method="non-p", from.interval="confidence")))[[1]]
#   lr_f_p <- system.time(sims <- replicate(10, simulateSolute(lr, "flux", method="param", from.interval="confidence")))[[1]]
#   lr_f_n <- system.time(sims <- replicate(10, simulateSolute(lr, "flux", method="non-p", from.interval="confidence")))[[1]]
#   # The following are machine-dependent tests:
#   expect_less_than(lr_c_p, 0.2)
#   expect_less_than(lr_f_p, 0.2) # flux takes 3-4 times longer than conc, but they're both fast
#   expect_less_than(lr_c_n, 3) # non-parametric takes a few seconds
#   expect_less_than(lr_f_n, 3) # non-parametric takes a few seconds
#   
#   # repeatedly simulate, then plot all the sims
#   sims <- replicate(1000, simulateSolute(lr, "conc", method="parametric", from.interval="confidence"))
#   sims <- data.frame(mydat, sims) %>% gather(iter, concentration, X1:X1000)
#   print(ggplot(sims, aes(x=datetime, y=concentration)) + geom_line(aes(group=iter), alpha=0.1, color="blue") + theme_bw() +
#           geom_point(data=mydat, aes(y=conc), color="pink", size=2) +
#           geom_line(data=data.frame(mydat, concentration=predictSolute(lr, "conc")), color="cyan", size=1))
#   expect_manual_OK("simulated values make a cloud of lines around the original predictions")
#   # very interesting. relative to lm, they seem to possibly be skewed upward - more of a lognormal than a normal distribution of coefficients?
#   
#   warning("prediction intervals remain untested")
# })
