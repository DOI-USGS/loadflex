tryCatch({source("tests/testthat/helpers.R"); source("helpers.R")}, warning=function(w) invisible())

test_that("loadInterp models can be created", {
  # Basic object creation
  expect_is(new("loadInterp"),"loadInterp")
  expect_error(validObject(new("loadInterp"))) # checks validity of loadModel part first
  
  # Creation & validation of an inner interpModel object
  expect_true(validObject(new("interpModel", dates.in=1:10, y.in=11:20, interp.function=function(x) x)))
  
  mydat <- data.frame(conc=c(5,4,2,6,9,8,9,7,4,3),discharge=10,datetime=strptime(paste0("2000-05-",1:10),format="%Y-%m-%d"))
  data(eg_metadata)
  mymd <- updateMetadata(eg_metadata, constituent="conc", flow="discharge", dates="datetime")
    
  # This is the interpolation you'd compare to a regression or composite method
  lic <- loadInterp(interp.format="conc", 
             interp.function=function(dates.in, y.in, dates.out) {
               approx(x=dates.in, y=y.in, xout=dates.out, method="linear", rule=2)$y 
               }, 
             data=mydat, metadata=updateMetadata(mymd))
  expect_is(lic, "loadInterp")
  
  # This is the interpolation you'd use within a composite model
  mydat$Resid <- observeSolute(mydat, "flux", mymd)-100
  lif <- loadInterp(interp.format="flux", 
                   interp.function=function(dates.in, y.in, dates.out) {
                     approx(x=dates.in, y=y.in, xout=dates.out, method="linear", rule=2)$y 
                   }, 
                   data=mydat, metadata=updateMetadata(mymd, load.rate="Resid"))
  expect_is(lif, "loadInterp")
  
})


test_that("loadInterp models implement the loadModelInterface", {
  mydat <- data.frame(conc=c(5,4,2,6,9,8,9,7,4,3),discharge=10,datetime=strptime(paste0("2000-05-",1:10),format="%Y-%m-%d"))
  mymd <- metadata(constituent="conc", flow="discharge", load.rate="",
                      dates="datetime", conc.units="mg/L", flow.units="cfs", load.units="kg",
                      load.rate.units="kg/day", station="", custom=NULL)
  
  # This is the interpolation you'd compare to a regression or composite method
  lic <- loadInterp(interp.format="conc", linearInterpolation, data=mydat, metadata=updateMetadata(mymd))
  
  # Use the standard validation function to test the interface
  expect_true(validLoadModelInterface(lic))
  
  # Try an interpolation by flux
  mydat <- transform(mydat, loadrates=observeSolute(mydat, "flux", mymd, attach.units=FALSE))
  mymd <- updateMetadata(mymd, load.rate="loadrates")
  lif <- loadInterp(interp.format="flux", linearInterpolation, data=mydat, metadata=mymd)
  
  # Use the standard validation function to test the interface
  expect_true(validLoadModelInterface(lif))
})


test_that("loadInterp models make reasonable predictions", {
  mydat <- data.frame(conc=c(5,4,2,6,9,8,9,7,4,3),discharge=10,datetime=strptime(paste0("2000-05-",1:10),format="%Y-%m-%d"))
  mymd <- metadata(constituent="conc", flow="discharge", load.rate="", dates="datetime",
    conc.units="mg/L", flow.units="cfs", load.units="kg", load.rate.units="kg/day",
    station="", custom=NULL)
  
  ### This is the interpolation you'd compare to a regression or composite method
  lic <- loadInterp(interp.format="conc", data=mydat, interp.function=linearInterpolation, metadata=updateMetadata(mymd))
  
  # Predict fluxes for the same dates
  expect_equal(predictSolute(lic, "conc"), observeSolute(mydat, "conc", mymd))
  expect_equal(predictSolute(lic, "flux"), observeSolute(mydat, "flux", mymd))
  expect_equal(predictSolute(lic, "conc", mydat), observeSolute(mydat, "conc", mymd))
  expect_equal(predictSolute(lic, "flux", mydat), observeSolute(mydat, "flux", mymd))
  
  # Predict fluxes for new dates
  newdates <- data.frame(datetime=seq(from=strptime("2000-04-30", format="%Y-%m-%d"), to=strptime("2000-05-12", format="%Y-%m-%d"), length.out=150), discharge=10)
  expect_equal(length(predictSolute(lic, "flux", newdates)), nrow(newdates))
  #print(ggplot(cbind(newdates, Flux=predictSolute(lic, "flux", newdates)), aes(x=datetime, y=Flux)) + geom_point(color="pink") + 
  #  geom_point(data=data.frame(mydat, Flux=observeSolute(mydat, "flux", mymd)), pch=4, color="blue") + theme_bw())
  expect_manual_OK("Flux: the linear interpolation (pink dots) passes through each calibration point (blue X's)")
  
  # Predict concs for new dates
  expect_equal(length(predictSolute(lic, "conc", newdates)), nrow(newdates))
  #print(ggplot(cbind(newdates, Conc=predictSolute(lic, "conc", newdates)), aes(x=datetime, y=Conc)) + geom_point(color="pink") + 
  #  geom_point(data=data.frame(mydat, Conc=observeSolute(mydat, "conc", mymd)), pch=4, color="blue") + theme_bw())
  expect_manual_OK("Conc: the linear interpolation (pink dots) passes through each calibration point (X's)")
    
  # This is the interpolation you'd use within a composite model - same thing 
  # except for the load.rate column name. And the type of interpolation can be 
  # any valid interp fun.
  mydat$Resid <- observeSolute(mydat, "flux", mymd)-100
  lif <- loadInterp(interp.format="flux", splineInterpolation, data=mydat, metadata=updateMetadata(mymd, load.rate="Resid"))
  expect_equal(predictSolute(lif, "flux", mydat), mydat$Resid)
  
  # Predict concs & fluxes for new dates
  expect_true(length(predictSolute(lif, "flux", newdates[c("datetime","discharge")])) == nrow(newdates))
  #print(ggplot(cbind(newdates, Flux=predictSolute(lif, "flux", newdates)), aes(x=datetime, y=Flux)) + 
  #        geom_point(color="pink") + geom_point(data=mydat, aes(x=datetime, y=Resid), pch=4, color="blue") + theme_bw())
  expect_manual_OK("Spline interpolation by load (resids) to predict fluxes (resids)")
  # Now for concs
  expect_true(length(predictSolute(lif, "conc", newdates[c("datetime","discharge")])) == nrow(newdates))
  #print(ggplot(cbind(newdates, Conc=predictSolute(lif, "conc", newdates)), aes(x=datetime, y=Conc)) + 
  #        geom_point(color="pink") + theme_bw() + 
  #        geom_point(data=transform(mydat, Conc=observeSolute(mydat, "conc", updateMetadata(mymd, load.rate="Resid"), calculate=TRUE)), pch=4, color="blue"))
  expect_manual_OK("Spline interpolation by load (resids) can be converted back to concs (resids)")
  
  # This one uses a write-your-own interpolation method
  lif <- loadInterp(interp.format="flux", 
                    interp.function=function(dates.in, y.in, dates.out) {
                      approx(x=dates.in, y=rep(mean(y.in),length(dates.in)), xout=dates.out, method="linear", rule=2)$y 
                    }, 
                    data=mydat, metadata=updateMetadata(mymd, load.rate="Resid"))
  expect_equivalent(predictSolute(lif, "flux", mydat), rep(mean(mydat$Resid), nrow(mydat)))
  
  # Confirm that error checking gets done
  expect_error(loadInterp(interp.format="conc", data=mydat, interp.function=linearInterpolation, metadata=updateMetadata(mymd, constituent="wrongname")))
  expect_error(predictSolute(lic, "flux", transform(newdates, datetime=as.Date(datetime))))
  expect_error(predictSolute(lif, "conc", newdates["datetime"]), "data does not contain the expected")
  expect_error(predictSolute(lic, "flux", newdates["datetime"]), "data does not contain the expected")
})


test_that("loadInterp models can find and report their uncertainty", {
  
  # models & data for testing
  mydat <- data.frame(conc=c(5,4,2,6,9,8,9,7,4,3),discharge=c(4,3,3,7,10,8,9,6,5,2),datetime=strptime(paste0("2000-05-",1:10),format="%Y-%m-%d"))
  mymd <- metadata(constituent="conc", flow="discharge", load.rate="", dates="datetime",
                   conc.units="mg/L", flow.units="cfs", load.units="kg", load.rate.units="kg/day",
                   station="", custom=NULL)
  mydat$flux <- observeSolute(mydat, "flux", mymd, calc=TRUE)
  
  # non-null retrans.function should raise warnings and ultimately errors
  expect_error(loadInterp("conc", linearInterpolation, mydat, mymd, retrans.function=exp), "retrans.function")
  expect_error(loadInterp("flux", linearInterpolation, mydat, mymd, retrans.function=exp), "retrans.function")

  # now make the models for real, for testing of prediction when y values are valid (logged)
  lic <- loadInterp("conc", linearInterpolation, mydat, mymd)
  lif <- loadInterp("flux", linearInterpolation, mydat, mymd)
  
  # predictions with uncertainty intervals
  expect_equal(length(predictSolute(lic, flux.or.conc="flux", interval="none")), nrow(mydat))
  expect_equal(ncol(predictSolute(lic, flux.or.conc="flux", se.pred=TRUE)), 2)
  expect_equal(ncol(predictSolute(lic, flux.or.conc="flux", interval="prediction")), 3)
  expect_equal(ncol(predictSolute(lic, flux.or.conc="flux", interval="prediction", se.pred=TRUE)), 4)
  # options including interval="confidence" or se.fit=TRUE aren't permitted, at least for now
  expect_error(predictSolute(lic, flux.or.conc="flux", se.fit=TRUE))
  expect_error(predictSolute(lic, flux.or.conc="flux", interval="confidence"))
  expect_error(predictSolute(lic, flux.or.conc="flux", interval="confidence", se.fit=TRUE))
  expect_error(predictSolute(lic, flux.or.conc="flux", interval="prediction", se.fit=TRUE))
  expect_error(predictSolute(lic, flux.or.conc="flux", interval="confidence", se.pred=TRUE))
  expect_error(predictSolute(lic, flux.or.conc="flux", interval="confidence", se.fit=TRUE, se.pred=TRUE))
  expect_error(predictSolute(lic, flux.or.conc="flux", interval="prediction", se.fit=TRUE, se.pred=TRUE))
  expect_error(predictSolute(lic, flux.or.conc="conc", interval="confidence", se.fit=TRUE, se.pred=TRUE))
  expect_error(predictSolute(lic, flux.or.conc="conc", interval="prediction", se.fit=TRUE, se.pred=TRUE))
  
  # predictions with uncertainty intervals
  expect_equal(length(predictSolute(lif, flux.or.conc="flux", interval="none")), nrow(mydat))
  expect_equal(ncol(predictSolute(lif, flux.or.conc="flux", se.pred=TRUE)), 2)
  expect_equal(ncol(predictSolute(lif, flux.or.conc="flux", interval="prediction")), 3)
  expect_equal(ncol(predictSolute(lif, flux.or.conc="flux", interval="prediction", se.pred=TRUE)), 4)
  # options including interval="confidence" or se.fit=TRUE aren't permitted, at least for now
  expect_error(predictSolute(lif, flux.or.conc="flux", se.fit=TRUE))
  expect_error(predictSolute(lif, flux.or.conc="flux", interval="confidence"))
  expect_error(predictSolute(lif, flux.or.conc="flux", interval="confidence", se.fit=TRUE))
  expect_error(predictSolute(lif, flux.or.conc="flux", interval="prediction", se.fit=TRUE))
  expect_error(predictSolute(lif, flux.or.conc="flux", interval="confidence", se.pred=TRUE))
  expect_error(predictSolute(lif, flux.or.conc="flux", interval="confidence", se.fit=TRUE, se.pred=TRUE))
  expect_error(predictSolute(lif, flux.or.conc="flux", interval="prediction", se.fit=TRUE, se.pred=TRUE))
  expect_error(predictSolute(lif, flux.or.conc="conc", interval="confidence", se.fit=TRUE, se.pred=TRUE))
  expect_error(predictSolute(lif, flux.or.conc="conc", interval="prediction", se.fit=TRUE, se.pred=TRUE))
  
  # plot the predictions
  obs <- data.frame(mydat, obsconc=observeSolute(mydat, "conc", mymd), obsflux=observeSolute(mydat, "flux", mymd))
  #library(gridExtra)
  # grid.arrange(
    # ggplot(data.frame(obs, pred=predictSolute(lic, flux.or.conc="conc", interval="prediction", se.pred=TRUE)), 
           # aes(x=datetime, y=pred.fit)) + 
      # geom_errorbar(aes(ymin=pred.lwr, ymax=pred.upr), color="green") + geom_point(color="green", size=4) + 
      # geom_point(aes(y=obsconc), shape=4, size=4, color="magenta") +
      # theme_bw() + ggtitle("conc from lic"),
    # ggplot(data.frame(obs, pred=predictSolute(lif, flux.or.conc="conc", interval="prediction", se.pred=TRUE)), 
           # aes(x=datetime, y=pred.fit)) + 
      # geom_errorbar(aes(ymin=pred.lwr, ymax=pred.upr), color="green") + geom_point(color="green", size=4) + 
      # geom_point(aes(y=obsconc), shape=4, size=4, color="magenta") +
      # theme_bw() + ggtitle("conc from lif"),
    # ggplot(data.frame(obs, pred=predictSolute(lic, flux.or.conc="flux", interval="prediction",se.pred=TRUE)), 
           # aes(x=datetime, y=pred.fit)) + 
      # geom_errorbar(aes(ymin=pred.lwr, ymax=pred.upr), color="green") + geom_point(color="green", size=4) + 
      # geom_point(aes(y=obsflux), shape=4, size=4, color="magenta") +
      # theme_bw() + ggtitle("flux from lic"),
    # ggplot(data.frame(obs, pred=predictSolute(lif, flux.or.conc="flux", interval="prediction", se.pred=TRUE)), 
           # aes(x=datetime, y=pred.fit)) + 
      # geom_errorbar(aes(ymin=pred.lwr, ymax=pred.upr), color="green") + geom_point(color="green", size=4) + 
      # geom_point(aes(y=obsflux), shape=4, size=4, color="magenta") +
      # theme_bw() + ggtitle("flux from lif")
  # )
  expect_manual_OK("loadInterp predictions (green dots & bars) match the observations (pink X's)")
  
  # If you make the models with less complete data, uncertainty should be calculated for one format only
  lic <- loadInterp("conc", linearInterpolation, mydat[c("conc","datetime")], mymd)
  lif <- loadInterp("flux", linearInterpolation, mydat[c("flux","datetime")], updateMetadata(mymd, load.rate="flux"))
  expect_true(all(!is.na(lic@MSE[,1])))
  expect_true(all(is.na(lic@MSE[,2])))
  expect_true(all(is.na(lif@MSE[,1])))
  expect_true(all(!is.na(lif@MSE[,2])))
  # and uncertainty should be less available. conc should work fine from lic,
  # and flux without uncertainty. but not flux with uncertainty.
  expect_true(all(!is.na(predictSolute(lic, "conc", mydat, "pred"))))
  expect_true(all(!is.na(predictSolute(lif, "flux", mydat, "none"))))
  expect_error(predictSolute(lic, "flux", mydat, "pred"), "(unavailable).*(discharge)")
  expect_error(predictSolute(lic, "flux", mydat, se.pred=TRUE), "(unavailable).*(discharge)")
  # flux should work fine from lif, and conc without uncertainty. but not conc with uncertainty.
  expect_true(all(!is.na(predictSolute(lif, "flux", mydat, "pred"))))
  expect_true(all(!is.na(predictSolute(lif, "conc", mydat, "none"))))
  expect_error(predictSolute(lif, "conc", mydat, "pred"), "(unavailable).*(discharge)")
  expect_error(predictSolute(lif, "conc", mydat, se.pred=TRUE), "(unavailable).*(discharge)")
  
})

test_that("predictSolute agg.by arguments work", {
  mydat <- data.frame(conc=c(5,4,2,6,9,8,9,7,4,3),discharge=10,datetime=strptime(paste0("2000-05-",1:10),format="%Y-%m-%d"))
  mymd <- metadata(constituent="conc", flow="discharge", load.rate="", dates="datetime",
                   conc.units="mg/L", flow.units="cfs", load.units="kg", load.rate.units="kg/day",
                   site.id="", custom=NULL)
  
  ### This is the interpolation you'd compare to a regression or composite method
  lic <- loadInterp(interp.format="conc", data=mydat, interp.function=linearInterpolation, metadata=updateMetadata(mymd))
  expect_warning(monthAgg <- predictSolute(lic, "flux", agg.by = "month"))
  expect_warning(watYearAgg <- predictSolute(lic, "flux", agg.by = "water year", date = TRUE))
  expect_is(monthAgg, 'data.frame')
  expect_equal(nrow(monthAgg), 1)
  expect_true(all(is.na(monthAgg$CI_upper)))
  expect_is(watYearAgg, 'data.frame')
  expect_equal(nrow(watYearAgg), 1)
  expect_true(all(is.na(watYearAgg$CI_upper)))
})