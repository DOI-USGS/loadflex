tryCatch({source("tests/testthat/helpers.R"); source("helpers.R")}, warning=function(w) invisible())

test_that("loadLm models can be created", {
  # Basic object creation
  expect_is(new("loadLm"),"loadLm")
  expect_error(validObject(new("loadLm"))) # checks validity of loadModel part first
  
  # tester data
  mydat <- data.frame(conc=c(5,4,2,6,9,8,9,7,4,3),discharge=10,datetime=strptime(paste0("2000-05-",1:10),format="%Y-%m-%d"))
  mymd <- updateMetadata(exampleMetadata(), constituent="conc", flow="discharge", dates="datetime")
  
  # Create a conc model
  lmc <- loadLm(log(conc) ~ discharge, data=mydat, pred.format="conc", metadata=updateMetadata(mymd))
  expect_is(lmc, "loadLm")
  
  # Create a flux model
  mydat$flux <- observeSolute(mydat, "flux", mymd, calc=TRUE)
  lmf <- loadLm(log(flux) ~ log(discharge), data=mydat, pred.format="flux", metadata=mymd)
  expect_is(lmf, "loadLm")
  
  # Expect warnings when formula and/or retrans.function don't suggest a log-exp transformation and retransformation
  expect_warning(lmc <- loadLm(conc ~ discharge, data=mydat, pred.format="conc", metadata=updateMetadata(mymd)), "formula")
  expect_warning(lmc <- loadLm(log(conc) ~ discharge, data=mydat, pred.format="conc", metadata=updateMetadata(mymd), retrans.function = NULL), "retrans.function")
  expect_warning(lmf <- loadLm(flux ~ discharge, data=mydat, pred.format="flux", metadata=updateMetadata(mymd)), "formula")
  expect_warning(lmf <- loadLm(log(flux) ~ discharge, data=mydat, pred.format="flux", metadata=updateMetadata(mymd), retrans.function = NULL), "retrans.function")
  
})


test_that("loadLm models implement the loadModelInterface", {
  mydat <- data.frame(conc=c(5,4,2,6,9,8,9,7,4,3),discharge=c(4,3,3,7,10,8,9,6,5,2),datetime=strptime(paste0("2000-05-",1:10),format="%Y-%m-%d"))
  mymd <- metadata(constituent="conc", flow="discharge", load.rate="",
                   dates="datetime", conc.units="mg/L", flow.units="cfs", load.units="kg",
                   load.rate.units="kg/day", station="", custom=NULL)
  
  # This is the interpolation you'd compare to a regression or composite method
  lmc <- loadLm(log(conc) ~ discharge, data=mydat, pred.format="conc", metadata=updateMetadata(mymd))
  expect_is(lmc, "loadLm")
  
  # Create a flux model
  mydat$flux <- observeSolute(mydat, "flux", mymd, calc=TRUE)
  lmf <- loadLm(log(flux) ~ log(discharge), data=mydat, pred.format="flux", metadata=mymd)
  expect_is(lmf, "loadLm")
  
  # Use the standard validation function to test the interface, suppressing messages about unimplemented optional functions
  expect_true(suppressMessages(validLoadModelInterface(lmc)))
  expect_true(suppressMessages(validLoadModelInterface(lmf)))
})


test_that("loadLm models make reasonable predictions", {
  mydat <- data.frame(conc=c(5,4,2,6,9,8,9,7,4,3),discharge=c(4,3,3,7,10,8,9,6,5,2),datetime=strptime(paste0("2000-05-",1:10),format="%Y-%m-%d"))
  mymd <- metadata(constituent="conc", flow="discharge", load.rate="", dates="datetime",
                   conc.units="mg/L", flow.units="cfs", load.units="kg", load.rate.units="kg/day",
                   station="", custom=NULL)
  
  ### Create loadLm models
  lmc <- loadLm(log(conc) ~ discharge, data=mydat, pred.format="conc", metadata=mymd)
  mydat$flux <- observeSolute(mydat, "flux", mymd, calc=TRUE)
  lmf <- loadLm(log(flux) ~ discharge, data=mydat, pred.format="flux", metadata=mymd)
  
  # Predict fluxes for the same dates
  par(mfrow=c(3,2), mar=c(2,2,0,0))
  #plot(predictSolute(lmc, "conc") ~ observeSolute(mydat, "conc", mymd), ylab="lmc-c", xlab="obs-c"); abline(a=0,b=1,col="gray")
  #plot(predictSolute(lmc, "flux") ~ observeSolute(mydat, "flux", mymd), ylab="lmc-f", xlab="obs-f"); abline(a=0,b=1,col="gray")
  #plot(predictSolute(lmf, "conc") ~ observeSolute(mydat, "conc", mymd), ylab="lmf-c", xlab="obs-c"); abline(a=0,b=1,col="gray")
  #plot(predictSolute(lmf, "flux") ~ observeSolute(mydat, "flux", mymd), ylab="lmf-f", xlab="obs-f"); abline(a=0,b=1,col="gray")
  #plot(predictSolute(lmf, "conc") ~ predictSolute(lmc, "conc"), ylab="lmf-c", xlab="lmc-c"); abline(a=0,b=1,col="gray")
  #plot(predictSolute(lmf, "flux") ~ predictSolute(lmc, "flux"), ylab="lmf-f", xlab="lmc-f"); abline(a=0,b=1,col="gray")
  par(mfrow=c(1,1), mar=c(5,4,4,2)+0.1)
  expect_manual_OK("Predictions by two models and observations from data agree, roughly")
  
  # Predict fluxes for new dates
  newdates <- data.frame(
    datetime=seq(from=strptime("2000-04-30", format="%Y-%m-%d"), to=strptime("2000-05-12", format="%Y-%m-%d"), length.out=150), 
    discharge=mean(mydat$discharge)+rnorm(150))
  expect_equal(length(predictSolute(lmc, "conc", newdates)), nrow(newdates))
  expect_equal(length(predictSolute(lmc, "flux", newdates)), nrow(newdates))
  expect_equal(length(predictSolute(lmf, "conc", newdates)), nrow(newdates))
  expect_equal(length(predictSolute(lmf, "flux", newdates)), nrow(newdates))
  #library(gridExtra)
  #grid.arrange(
   # ggplot(cbind(newdates, Conc=predictSolute(lmc, "conc", newdates)), aes(x=datetime, y=Conc)) + geom_point(color="pink") + 
   #  geom_point(data=data.frame(mydat, Conc=observeSolute(mydat, "conc", mymd)), pch=4, color="blue") + xlab("lmc") + theme_bw(),
   # ggplot(cbind(newdates, Flux=predictSolute(lmc, "flux", newdates)), aes(x=datetime, y=Flux)) + geom_point(color="pink") + 
   #   geom_point(data=data.frame(mydat, Flux=observeSolute(mydat, "flux", mymd)), pch=4, color="blue") + xlab("lmc") + theme_bw(),
   # ggplot(cbind(newdates, Conc=predictSolute(lmf, "conc", newdates)), aes(x=datetime, y=Conc)) + geom_point(color="pink") + 
   #   geom_point(data=data.frame(mydat, Conc=observeSolute(mydat, "conc", mymd)), pch=4, color="blue") + xlab("lmf") + theme_bw(),
   # ggplot(cbind(newdates, Flux=predictSolute(lmf, "flux", newdates)), aes(x=datetime, y=Flux)) + geom_point(color="pink") + 
   #   geom_point(data=data.frame(mydat, Flux=observeSolute(mydat, "flux", mymd)), pch=4, color="blue") + xlab("lmf") + theme_bw())
  expect_manual_OK("lm predictions (pink dots) are on the same order as observations (blue X's)")
  
})


test_that("loadLm models can find and report their uncertainty", {
  
  # models & data for testing
  mydat <- data.frame(conc=c(5,4,2,6,9,8,9,7,4,3),discharge=c(4,3,3,7,10,8,9,6,5,2),datetime=strptime(paste0("2000-05-",1:10),format="%Y-%m-%d"))
  mymd <- metadata(constituent="conc", flow="discharge", load.rate="", dates="datetime",
                   conc.units="mg/L", flow.units="cfs", load.units="kg", load.rate.units="kg/day",
                   station="", custom=NULL)
  mydat$flux <- observeSolute(mydat, "flux", mymd, calc=TRUE)
  
  # non-logged ys should raise warnings and ultimately errors
  expect_warning(lmc <- loadLm(conc ~ discharge, data=mydat, pred.format="conc", metadata=mymd), "log")
  expect_warning(lmf <- loadLm(flux ~ discharge, data=mydat, pred.format="flux", metadata=mymd), "log")
  expect_error(predictSolute(lmc, flux.or.conc="flux"), "log")
  expect_error(predictSolute(lmf, flux.or.conc="flux"), "log")
  # you can lie about the y values and generate crappy (e.g., infinite) predictions, but it takes effort
  lmf <- loadLm(flux ~ discharge, data=mydat, pred.format="conc", metadata=mymd, ylog=TRUE)
  expect_true(!all(is.finite(predictSolute(lmf, flux.or.conc="flux", se.pred=TRUE)$se.pred)))
  
  # now make the models for real, for testing of prediction when y values are valid (logged)
  lmc <- loadLm(log(conc) ~ discharge, data=mydat, pred.format="conc", metadata=mymd)
  lmf <- loadLm(log(flux) ~ discharge, data=mydat, pred.format="flux", metadata=mymd)
  
  # predictions with uncertainty intervals
  expect_equal(length(predictSolute(lmc, flux.or.conc="flux", interval="none")), nrow(mydat))
  expect_equal(ncol(predictSolute(lmc, flux.or.conc="flux", se.fit=TRUE)), 2)
  expect_equal(ncol(predictSolute(lmc, flux.or.conc="flux", se.pred=TRUE)), 2)
  expect_equal(ncol(predictSolute(lmc, flux.or.conc="flux", interval="confidence")), 3)
  expect_equal(ncol(predictSolute(lmc, flux.or.conc="flux", interval="prediction")), 3)
  expect_equal(ncol(predictSolute(lmc, flux.or.conc="flux", interval="confidence", se.fit=TRUE)), 4)
  expect_equal(ncol(predictSolute(lmc, flux.or.conc="flux", interval="prediction", se.fit=TRUE)), 4)
  expect_equal(ncol(predictSolute(lmc, flux.or.conc="flux", interval="confidence", se.pred=TRUE)), 4)
  expect_equal(ncol(predictSolute(lmc, flux.or.conc="flux", interval="prediction", se.pred=TRUE)), 4)
  expect_equal(ncol(predictSolute(lmc, flux.or.conc="flux", interval="confidence", se.fit=TRUE, se.pred=TRUE)), 5)
  expect_equal(ncol(predictSolute(lmc, flux.or.conc="flux", interval="prediction", se.fit=TRUE, se.pred=TRUE)), 5)
  expect_equal(ncol(predictSolute(lmc, flux.or.conc="conc", interval="confidence", se.fit=TRUE, se.pred=TRUE)), 5)
  expect_equal(ncol(predictSolute(lmc, flux.or.conc="conc", interval="prediction", se.fit=TRUE, se.pred=TRUE)), 5)
  
  # predictions with uncertainty intervals
  expect_equal(length(predictSolute(lmf, flux.or.conc="flux", interval="none")), nrow(mydat))
  expect_equal(ncol(predictSolute(lmf, flux.or.conc="flux", se.fit=TRUE)), 2)
  expect_equal(ncol(predictSolute(lmf, flux.or.conc="flux", se.pred=TRUE)), 2)
  expect_equal(ncol(predictSolute(lmf, flux.or.conc="flux", interval="confidence")), 3)
  expect_equal(ncol(predictSolute(lmf, flux.or.conc="flux", interval="prediction")), 3)
  expect_equal(ncol(predictSolute(lmf, flux.or.conc="flux", interval="confidence", se.fit=TRUE)), 4)
  expect_equal(ncol(predictSolute(lmf, flux.or.conc="flux", interval="prediction", se.fit=TRUE)), 4)
  expect_equal(ncol(predictSolute(lmf, flux.or.conc="flux", interval="confidence", se.pred=TRUE)), 4)
  expect_equal(ncol(predictSolute(lmf, flux.or.conc="flux", interval="prediction", se.pred=TRUE)), 4)
  expect_equal(ncol(predictSolute(lmf, flux.or.conc="flux", interval="confidence", se.fit=TRUE, se.pred=TRUE)), 5)
  expect_equal(ncol(predictSolute(lmf, flux.or.conc="flux", interval="prediction", se.fit=TRUE, se.pred=TRUE)), 5)
  expect_equal(ncol(predictSolute(lmf, flux.or.conc="conc", interval="confidence", se.fit=TRUE, se.pred=TRUE)), 5)
  expect_equal(ncol(predictSolute(lmf, flux.or.conc="conc", interval="prediction", se.fit=TRUE, se.pred=TRUE)), 5)
  
  # plot the predictions
  obs <- data.frame(mydat, obsconc=observeSolute(mydat, "conc", mymd), obsflux=observeSolute(mydat, "flux", mymd))
  # grid.arrange(
    # ggplot(data.frame(obs, pred=predictSolute(lmc, flux.or.conc="conc", interval="prediction", se.fit=TRUE, se.pred=TRUE)), 
           # aes(x=datetime, y=pred.fit)) + 
      # geom_errorbar(aes(ymin=pred.lwr, ymax=pred.upr), color="green") + geom_point(color="green", size=4) + 
      # geom_point(aes(y=obsconc), shape=4, size=4, color="magenta") +
      # theme_bw() + ggtitle("conc from lmc"),
    # ggplot(data.frame(obs, pred=predictSolute(lmf, flux.or.conc="conc", interval="prediction", se.fit=TRUE, se.pred=TRUE)), 
           # aes(x=datetime, y=pred.fit)) + 
      # geom_errorbar(aes(ymin=pred.lwr, ymax=pred.upr), color="green") + geom_point(color="green", size=4) + 
      # geom_point(aes(y=obsconc), shape=4, size=4, color="magenta") +
      # theme_bw() + ggtitle("conc from lmf"),
    # ggplot(data.frame(obs, pred=predictSolute(lmc, flux.or.conc="flux", interval="prediction", se.fit=TRUE, se.pred=TRUE)), 
           # aes(x=datetime, y=pred.fit)) + 
      # geom_errorbar(aes(ymin=pred.lwr, ymax=pred.upr), color="green") + geom_point(color="green", size=4) + 
      # geom_point(aes(y=obsflux), shape=4, size=4, color="magenta") +
      # theme_bw() + ggtitle("flux from lmc"),
    # ggplot(data.frame(obs, pred=predictSolute(lmf, flux.or.conc="flux", interval="prediction", se.fit=TRUE, se.pred=TRUE)), 
           # aes(x=datetime, y=pred.fit)) + 
      # geom_errorbar(aes(ymin=pred.lwr, ymax=pred.upr), color="green") + geom_point(color="green", size=4) + 
      # geom_point(aes(y=obsflux), shape=4, size=4, color="magenta") +
      # theme_bw() + ggtitle("flux from lmf")
  #)
  expect_manual_OK("lm predictions (green dots & bars) are on the same order as observations (pink X's)")
})

test_that("resampleCoefficients.lm looks OK", {
  # models & data for testing
  mydat <- transform(
    data.frame(conc=c(5,4,2,6,9,8,9,7,4,3),discharge=c(4,3,3,7,10,8,9,6,5,2),datetime=strptime(paste0("2000-05-",1:10),format="%Y-%m-%d")),
    dtsimple=(as.numeric(datetime)-mean(as.numeric(datetime)))/sd(as.numeric(datetime)))
  mylm <- lm(log(conc) ~ discharge + dtsimple, data=mydat)
  
  # resample 1000 times and plot the resampled coefficients
  new_coefs <- setNames(data.frame(t(replicate(n=1000, coef(resampleCoefficients.lm(mylm))))), c("intercept","discharge","dtsimple"))
  #print(ggplot(new_coefs, aes(x=intercept, y=discharge, color=dtsimple)) + geom_point() + theme_bw())
  #   library(lattice)
  #   cloud(intercept ~ dtsimple * discharge, data=new_coefs, alpha=0.6)
  print(cov.scaled <- (summary(mylm)$sigma)^2*summary(mylm)$cov.unscaled)
  expect_manual_OK("resampled coefficients follow the expected covariance structure")
})

# test_that("simulateSolute.loadLm looks OK", {
  # # libraries for %>% and gather
  # library(dplyr)
  # library(tidyr)
  
  # # data for testing
  # mydat <- transform(
    # data.frame(conc=c(5,4,2,6,9,8,9,7,4,3),discharge=c(4,3,3,7,10,8,9,6,5,2),datetime=strptime(paste0("2000-05-",1:10),format="%Y-%m-%d")),
    # dtsimple=(as.numeric(datetime)-mean(as.numeric(datetime)))/sd(as.numeric(datetime)))
  # mymd <- metadata(constituent="conc", flow="discharge", load.rate="", dates="datetime",
                   # conc.units="mg/L", flow.units="cfs", load.units="kg", load.rate.units="kg/day",
                   # station="", custom=NULL)
  # mydat$flux <- observeSolute(mydat, "flux", mymd, calc=TRUE)
  
  # # show that simulating flux is a LOT slower (by >2 seconds, even for just 10
  # # replicates) than simulating conc for a model fitted to conc
  # lmc <- loadLm(log(conc) ~ discharge + dtsimple, data=mydat, pred.format="conc", metadata=mymd)
  # lmc_c <- system.time(sims <- replicate(10, simulateSolute(lmc, "conc", method="parametric", from.interval="confidence")))[[1]]
  # lmc_f <- system.time(sims <- replicate(10, simulateSolute(lmc, "flux", method="parametric", from.interval="confidence")))[[1]]
  # lmf <- loadLm(log(conc*discharge) ~ discharge + dtsimple, data=mydat, pred.format="flux", metadata=mymd)
  # lmf_c <- system.time(sims <- replicate(10, simulateSolute(lmf, "conc", method="parametric", from.interval="confidence")))[[1]]
  # lmf_f <- system.time(sims <- replicate(10, simulateSolute(lmf, "flux", method="parametric", from.interval="confidence")))[[1]]
  # print(matrix(c(lmc_c, lmc_f, lmf_c, lmf_f), nrow=2, dimnames=list(c("predconc","predflux"),c("lmctime","lmftime"))))
  # expect_less_than(2+lmc_c, lmc_f)
  # expect_less_than(2+lmf_f, lmf_c)
  # warning("time to predict flux from conc or vice versa could be prohibitive for simulateSolute")

  # # fit a model with which to simulateSolute many times
  # lmc <- loadLm(log(conc) ~ discharge + dtsimple, data=mydat, pred.format="conc", metadata=mymd)
  
  # # PARAMETRIC: repeatedly simulate, then plot all the sims
  # print(system.time(sims <- replicate(1000, simulateSolute(lmc, "conc", method="parametric", from.interval="confidence"))))
  # sims <- data.frame(mydat, sims) %>% gather(iter, concentration, X1:X1000)
  # #print(ggplot(sims, aes(x=datetime, y=concentration)) + geom_line(aes(group=iter), alpha=0.1, color="blue") + theme_bw() +
  # #  geom_point(data=mydat, aes(y=conc), color="pink", size=2) +
  # #  geom_line(data=data.frame(mydat, concentration=predictSolute(lmc, "conc")), color="cyan", size=1))
  # expect_manual_OK("parametric bootstrap: simulated values make a cloud of lines around the original predictions")
  
  # # NONPARAMETRIC: repeatedly simulate, then plot all the sims
  # print(system.time(sims <- replicate(1000, simulateSolute(lmc, "conc", method="non-parametric", from.interval="confidence"))))
  # sims <- data.frame(mydat, sims) %>% gather(iter, concentration, X1:X1000)
  # #print(ggplot(sims, aes(x=datetime, y=concentration)) + geom_line(aes(group=iter), alpha=0.1, color="blue") + theme_bw() +
  # #        geom_point(data=mydat, aes(y=conc), color="pink", size=2) +
  # #        geom_line(data=data.frame(mydat, concentration=predictSolute(lmc, "conc")), color="cyan", size=1))
  # expect_manual_OK("parametric bootstrap: simulated values make a cloud of lines around the original predictions")
  
  # warning("prediction intervals remain untested")
# })