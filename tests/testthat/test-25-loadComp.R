tryCatch({source("tests/testthat/helpers.R"); source("helpers.R")}, warning=function(w) invisible())

test_that("loadComp models can be created", {
  # Define & munge dataset
  library(rloadest)
  simpledata <- transform(app2.calib[-which(diff(app2.calib$DATES) < 7),], 
                          Period=seasons(DATES,breaks=c("Apr", "Jul")))
  
  # Create the regression model
  simpledata$DATES <- as.POSIXct(simpledata$DATES)
  reg.model <- loadReg2(loadReg(
    Atrazine ~ Period*center(log(FLOW)), 
    data = simpledata, time.step="instantaneous",
    flow = "FLOW", dates = "DATES", conc.units="mg/L"))
  
  ### HAD TO CONVERT SIMPLEDATA$DATES TO POSIXCT AND TIMESTEP TO INSTANTANEOUS
  ### BECAUSE OTHERWISE LOADREG REFUSES TO FIT TO THE BOOTSTRAP-RESAMPLED DATA
  ### (WHERE DATES ARE REPEATED). THIS IS HIGHLY INCONVENIENT.
  
  # Create the composite model
  load.model <- loadComp(reg.model=reg.model, interp.format="flux", interp.data=simpledata, interp.function=linearInterpolation)
  expect_is(load.model, "loadComp")
})


# Test composite method predictions for a variety of interpolation methods
checkLoadCompInterpPreds <- function(interp.fun, abs.or.rel.resids, use.log, flux.or.conc) {
  
  # Example data & models
  library(rloadest)
  simpledata <- transform(app2.calib[-which(diff(app2.calib$DATES) < 7),], Period=seasons(DATES,breaks=c("Apr", "Jul")))
  estdata <- transform(app2.est, Period=seasons(DATES,breaks=c("Apr", "Jul")))
  reg.model <- loadReg2(loadReg(Atrazine ~ center(log(FLOW)), data = simpledata, flow = "FLOW", dates = "DATES", conc.units="mg/L"))
  library(reshape2)
  
  for(interp.format in c("flux","conc")) {
    # Interpolate by flux: Generate observations, intermediates, and predictions
    load.model <- loadComp(reg.model=reg.model, interp.format=interp.format, interp.data=simpledata, 
                           interp.function=interp.fun, abs.or.rel.resids=abs.or.rel.resids, use.log=use.log, store=c())
    expect_error(predictSolute(load.model, flux.or.conc, estdata, se.fit=TRUE), "se.fit not implemented for loadComp")
    expect_error(predictSolute(load.model, flux.or.conc, estdata, se.pred=TRUE), "(unavailable).*(store)")
    expect_error(predictSolute(load.model, flux.or.conc, estdata, interval="confidence"), "confidence intervals not implemented for loadComp")
    expect_error(predictSolute(load.model, flux.or.conc, estdata, interval="prediction"), "(unavailable).*(store)")
    
    predobs <- rbind(
      melt(
        data.frame(
          Date=simpledata$DATES,
          Obs=observeSolute(simpledata, flux.or.conc, getMetadata(load.model)),
          ResidObs=observeSolute(getFittingData(load.model@fit@resid.model), flux.or.conc, load.model@fit@resid.model@metadata, 
                                 calculate=(flux.or.conc!=interp.format))  
        ), id.vars=.(Date)
      ), 
      melt(
        data.frame(
          Date=estdata$DATES,
          Reg=predictSolute(load.model@fit@reg.model, flux.or.conc, estdata),
          Comp=predictSolute(load.model, flux.or.conc, estdata),
          Resid=predictSolute(load.model@fit@resid.model, flux.or.conc, estdata)    
        ), id.vars=.(Date)
      )
    )
    predobs$IsResid <- ifelse(predobs$variable%in% c("Resid","ResidObs"),"Resids",flux.or.conc)
    
    # Plot the intermediates& results
    ttl <- paste0(as.character(substitute(interp.fun))[1],"; ",abs.or.rel.resids,"; pred ",flux.or.conc," by interp ",if(use.log) "log " else "",interp.format)
    print(ggplot(predobs, aes(x=Date, y=value, color=variable)) + 
            geom_line(data=predobs[predobs$variable %in% c("Reg","Comp","Resid"),], size=1) + 
            geom_point(data=predobs[predobs$variable %in% c("Obs","ResidObs"),], size=3) + 
            theme_bw() + facet_grid(IsResid ~ ., scales="free_y", space="free_y", shrink=TRUE) + 
            ylab(flux.or.conc) + ggtitle(ttl))
    expect_manual_OK(ttl)
  }
  
}

test_that("linearInterpolations look good within loadcomp", {
  checkLoadCompInterpPreds(linearInterpolation, "absolute", TRUE, "flux")
  checkLoadCompInterpPreds(linearInterpolation, "absolute", FALSE, "conc")
  checkLoadCompInterpPreds(linearInterpolation, "relative", FALSE, "flux")
  checkLoadCompInterpPreds(linearInterpolation, "relative", TRUE, "conc")
})
test_that("triangularInterpolations look good within loadcomp", {
  checkLoadCompInterpPreds(triangularInterpolation, "absolute", FALSE, "flux")
  checkLoadCompInterpPreds(triangularInterpolation, "absolute", TRUE, "conc") # looks bad
  checkLoadCompInterpPreds(triangularInterpolation, "relative", TRUE, "flux")
  checkLoadCompInterpPreds(triangularInterpolation, "relative", FALSE, "conc") # looks bad
})
test_that("rectangularInterpolations look good within loadcomp", {
  checkLoadCompInterpPreds(rectangularInterpolation, "absolute", T, "flux")
  checkLoadCompInterpPreds(rectangularInterpolation, "absolute", F, "conc")
  checkLoadCompInterpPreds(rectangularInterpolation, "relative", F, "flux")
  checkLoadCompInterpPreds(rectangularInterpolation, "relative", T, "conc")
})
test_that("splineInterpolations look good within loadcomp", {
  checkLoadCompInterpPreds(splineInterpolation, "absolute", F, "flux")
  checkLoadCompInterpPreds(splineInterpolation, "absolute", T, "conc")
  checkLoadCompInterpPreds(splineInterpolation, "relative", T, "flux")
  checkLoadCompInterpPreds(splineInterpolation, "relative", F, "conc")
})
test_that("smoothSplineInterpolations look good within loadcomp", {
  checkLoadCompInterpPreds(getSmoothSplineInterpolation(nknots=22), "absolute", T, "flux")
  checkLoadCompInterpPreds(getSmoothSplineInterpolation(nknots=22), "absolute", F, "conc")
  checkLoadCompInterpPreds(getSmoothSplineInterpolation(nknots=22), "relative", F, "flux")
  checkLoadCompInterpPreds(getSmoothSplineInterpolation(nknots=22), "relative", T, "conc")
})
test_that("distanceWeightedInterpolations look good within loadcomp", {
  checkLoadCompInterpPreds(distanceWeightedInterpolation, "absolute", F, "flux")
  checkLoadCompInterpPreds(distanceWeightedInterpolation, "absolute", T, "conc")
  checkLoadCompInterpPreds(distanceWeightedInterpolation, "relative", T, "flux")
  checkLoadCompInterpPreds(distanceWeightedInterpolation, "relative", F, "conc")
})


test_that("loadComp models can estimate their uncertainty", {
  
  # Example data & models
  library(rloadest)
  simpledata <- transform(app2.calib[-which(diff(app2.calib$DATES) < 7),], Period=seasons(DATES,breaks=c("Apr", "Jul")))
  estdata <- transform(app2.est, Period=seasons(DATES,breaks=c("Apr", "Jul")))
  rl.model <- loadReg2(loadReg(Atrazine ~ center(log(FLOW)), data = simpledata, flow = "FLOW", dates = "DATES", conc.units="mg/L"))
  atra_meta <- metadata(const="Atrazine", flow="FLOW", dates="DATES", conc.units="mg L^-1", flow.units="cfs", load.units="kg", load.rate.units="kg d^-1")
  lm.model <- loadLm(log(Atrazine) ~ center(log(FLOW)), data=simpledata, metadata=atra_meta)
  
  # Fit the loadComp with store=c("data","uncertainty") (to calculate uncertainty) for many
  # combinations of interp.functions, abs/rel, log/lin options
  system.time(MSEresults <- do.call("rbind", lapply(
    1:6, function(funID) { do.call("rbind", lapply(
        c("conc", "flux"), function(interpformat) { do.call("rbind", lapply(
          c("absolute", "relative"), function(absrel) { do.call("rbind", lapply(
            c(FALSE, TRUE), function(uselog) {
              fun <- list(linearInterpolation, triangularInterpolation, rectangularInterpolation, 
                          splineInterpolation, getSmoothSplineInterpolation(nknots=22), distanceWeightedInterpolation)[[funID]]
              load.model.rl <- loadComp(
                reg.model=rl.model, interp.format=interpformat, interp.data=simpledata, 
                interp.function=fun, abs.or.rel.resids=absrel, use.log=uselog, store=c("data","uncertainty"))
              load.model.lm <- loadComp(
                reg.model=lm.model, interp.format=interpformat, interp.data=simpledata, 
                interp.function=fun, abs.or.rel.resids=absrel, use.log=uselog, store=c("data","uncertainty"))
              data.frame(
                fun=c("linear","triangular","rectangular","spline","smoothspline","distanceweighted")[funID],
                interpby=interpformat,
                absrel=absrel,
                uselog=uselog,
                MSElog_rl_m=if(uselog) load.model.rl@MSE["mean","conc"] else NA,
                MSElog_lm_m=if(uselog) load.model.lm@MSE["mean","conc"] else NA,
                #MSElog_s=if(uselog) load.model@MSE["sd","conc"] else NA,
                # don't need the "flux" col of log MSE because it's the same as the "conc" col
                MSElin_rl_mc=if(!uselog) load.model.rl@MSE["mean","conc"] else NA,
                MSElin_lm_mc=if(!uselog) load.model.lm@MSE["mean","conc"] else NA,
                #MSElin_sc=if(!uselog) load.model@MSE["sd","conc"] else NA,
                MSElin_rl_mf=if(!uselog) load.model.rl@MSE["mean","flux"] else NA,
                MSElin_lm_mf=if(!uselog) load.model.lm@MSE["mean","flux"] else NA)
                #MSElin_sf=if(!uselog) load.model@MSE["sd","flux"] else NA)
            }))
          }))
        }))
    }))
  )
  print(subset(MSEresults, uselog==FALSE)[c(1:4,7:10)])
  expect_manual_OK("linear-space MSEs make sense")
  print(subset(MSEresults, uselog==TRUE)[1:6])
  expect_manual_OK("log-space MSEs make sense")
  
})


test_that("loadComp uncertainty reporting makes sense", {
  
  # Example data & models
  library(rloadest)
  simpledata <- transform(app2.calib[-which(diff(app2.calib$DATES) < 7),], Period=seasons(DATES,breaks=c("Apr", "Jul")))
  estdata <- transform(app2.est, Period=seasons(DATES,breaks=c("Apr", "Jul")))
  reg.model <- loadReg2(loadReg(Atrazine ~ center(log(FLOW)), data = simpledata, flow = "FLOW", dates = "DATES", conc.units="mg/L"))
  
  # Fit the loadComp with store=c("data","uncertainty") to calculate uncertainty
  load.model <- loadComp(reg.model=reg.model, interp.format="conc", interp.data=simpledata, 
                         interp.function=linearInterpolation, abs.or.rel.resids="absolute", use.log=TRUE, store=c("data","uncertainty"))
  print(load.model@MSE)
  obs <- data.frame(simpledata, FLUX=observeSolute(simpledata, "flux", getMetadata(load.model)))
  
  # Predictions for new data
  predsF <- predictSolute(load.model, "flux", estdata)
  predsC <- predictSolute(load.model, "conc", estdata)
  
  # Setting se.fit should give an error; setting se.pred to TRUE should result in a data.frame with extra information
  expect_error(predictSolute(load.model, "flux", newdata=simpledata, se.fit=TRUE), "not implemented for loadComp")
  expect_error(predictSolute(load.model, "conc", newdata=simpledata, se.fit=TRUE), "not implemented for loadComp")
  # se.pred values should be real numbers in the right place
  expect_equal(colnames(predictSolute(load.model, "flux", newdata=simpledata, se.pred=TRUE)), c("fit","se.pred"))
  expect_equal(colnames(predictSolute(load.model, "conc", newdata=simpledata, se.pred=TRUE)), c("fit","se.pred"))
  expect_true(!any(is.na(predictSolute(load.model, "flux", newdata=simpledata, se.pred=TRUE)$se.pred)))
  expect_true(!any(is.na(predictSolute(load.model, "conc", newdata=simpledata, se.pred=TRUE)$se.pred)))
  # Although there's a single sdlog value, we get different se.preds for each row because of the log retransformation
  predictSolute(load.model, "flux", newdata=simpledata, se.pred=TRUE)
  predictSolute(load.model, "conc", newdata=simpledata, se.pred=TRUE)
  expect_manual_OK("sd.preds seem reasonable")
  # Fit shouldn't change when uncertainty is included
  expect_equal(predictSolute(load.model, "flux", estdata), predictSolute(load.model, "flux", estdata, se.pred=TRUE)$fit)
  expect_equal(predictSolute(load.model, "conc", estdata), predictSolute(load.model, "conc", estdata, se.pred=TRUE)$fit)
  
  # Confidence intervals make sense
  expect_error(predictSolute(load.model, "flux", newdata=simpledata, interval = "confidence"), "not implemented for loadComp")
  print(ggplot(data.frame(date=estdata$DATES[51:100], predictSolute(load.model, "flux", newdata=estdata[51:100,], interval = "prediction")),
               aes(x=date, y=fit)) + geom_point() + geom_line() + geom_ribbon(aes(ymin=lwr, ymax=upr), fill="green", color="green", alpha=0.2) + theme_bw())
  expect_manual_OK("flux preds & pred intervals look good")
  
  expect_error(predictSolute(load.model, "conc", newdata=simpledata, interval = "confidence"), "not implemented for loadComp")
  print(ggplot(data.frame(date=estdata$DATES[51:200], predictSolute(load.model, "conc", newdata=estdata[51:200,], interval = "prediction")),
               aes(x=date, y=fit)) + geom_point() + geom_line() + geom_ribbon(aes(ymin=lwr, ymax=upr), fill="brown", color="brown", alpha=0.2) + theme_bw())
  expect_manual_OK("conc preds & pred intervals look good")
  
})

test_that("loadComp uncertainties make sense for all sorts of abs/reg, lin/log, conc/flux combinations", {
  # Example data & models
  library(rloadest)
  simpledata <- transform(app2.calib[-which(diff(app2.calib$DATES) < 7),], Period=seasons(DATES,breaks=c("Apr", "Jul")))
  estdata <- transform(app2.est, Period=seasons(DATES,breaks=c("Apr", "Jul")))
  reg.model <- loadReg2(loadReg(Atrazine ~ center(log(FLOW)), data = simpledata, flow = "FLOW", dates = "DATES", conc.units="mg/L"))
  simpledata$AtFlux <- observeSolute(simpledata, "flux", getMetadata(reg.model))
                
  library(gridExtra)
  MSEresults <- lapply(
    1:6, function(funID) { lapply(
      c("conc", "flux"), function(interpformat) { lapply(
        c("absolute", "relative"), function(absrel) { lapply(
          c(FALSE, TRUE), function(uselog) {
            fun <- list(linearInterpolation, triangularInterpolation, rectangularInterpolation, 
                        splineInterpolation, getSmoothSplineInterpolation(nknots=22), distanceWeightedInterpolation)[[funID]]
            load.model <- loadComp(
              reg.model=reg.model, interp.format=interpformat, interp.data=simpledata, 
              interp.function=fun, abs.or.rel.resids=absrel, use.log=uselog, store=c("data","uncertainty"))
            
            plottitle <- paste0(c("linear","triangular","rectangular","spline","smoothspline","distanceweighted")[funID],", ",
                                "interpby=",interpformat,", ",
                                "absrel=",absrel,", ",
                                "uselog=",uselog)
            print(plottitle)
            print(load.model@MSE)
            
            grid.arrange(
              ggplot(
                data.frame(date=estdata$DATES[51:200], predictSolute(load.model, "conc", newdata=estdata[51:200,], interval = "prediction")),
                aes(x=date, y=fit)) + geom_point(color="brown") + geom_line(color="brown") + 
                geom_ribbon(aes(ymin=lwr, ymax=upr), fill="brown", alpha=0.2) + 
                geom_point(data=subset(simpledata, DATES >= as.Date("1996-04-20") & DATES <= as.Date("1996-09-16")), aes(x=DATES, y=Atrazine), color="orange", size=2) +
                ylab("conc") + theme_bw() + ggtitle(plottitle),
              ggplot(
                data.frame(date=estdata$DATES[51:200], predictSolute(load.model, "flux", newdata=estdata[51:200,], interval = "prediction")),
                aes(x=date, y=fit)) + geom_point(color="green") + geom_line(color="green") + 
                geom_ribbon(aes(ymin=lwr, ymax=upr), fill="green", alpha=0.2) +
                geom_point(data=subset(simpledata, DATES>= as.Date("1996-04-20") & DATES <= as.Date("1996-09-16")), aes(x=DATES, y=AtFlux), color="forestgreen", size=2) +
                ylab("flux") + theme_bw()
            )
            
            expect_manual_OK("Reasonable prediction intervals?")
            
          })
        })
      })
    })
  
})
