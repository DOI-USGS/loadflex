tryCatch({source("inst/tests/helpers.R"); source("helpers.R")}, warning=function(w) invisible())

test_that("Unit aggregation works", {
  ex <- data.frame(preds=1:15, se.preds=1, dates=seq(as.Date("2000/1/1"), by = "week", length.out = 15))
  
  aggregateSolute(preds=ex$preds, se.preds=ex$se.preds, dates=ex$dates, format="conc", metadata=exampleMetadata(), agg.by="unit")
  
  # check that units are not attached by default
  expect_equal(aggregateSolute(preds=ex$preds, se.preds=ex$se.preds, dates=ex$dates, format="conc", metadata=exampleMetadata(), agg.by="unit")$Conc, 1:15)
  expect_equal(aggregateSolute(preds=ex$preds, se.preds=ex$se.preds, dates=ex$dates, format="flux rate", metadata=exampleMetadata(), agg.by="unit")$Flux_Rate, 1:15)
  # unit fluxes are calculated for the period beginning in that row and
  # extending to the next; hence, the last row gets NAs because we don't know
  # how long that period lasts.
  expect_equal(aggregateSolute(preds=ex$preds, se.preds=ex$se.preds, dates=ex$dates, format="flux total", metadata=exampleMetadata(), agg.by="unit")$Flux, c(1:14*7, NA))
  
  # check that units can be attached
  library(unitted)
  expect_equal(get_units(aggregateSolute(preds=ex$preds, se.preds=ex$se.preds, dates=ex$dates, format="conc", metadata=exampleMetadata(), agg.by="unit", attach.units=TRUE)$Conc), "mg L^-1")
  expect_equal(get_units(aggregateSolute(preds=ex$preds, se.preds=ex$se.preds, dates=ex$dates, format="flux rate", metadata=exampleMetadata(), agg.by="unit", attach.units=TRUE)$Flux_Rate), "kg d^-1")
  expect_equal(get_units(aggregateSolute(preds=ex$preds, se.preds=ex$se.preds, dates=ex$dates, format="flux total", metadata=exampleMetadata(), agg.by="unit", attach.units=TRUE)$Flux), "kg")
})

test_that("Aggregations by unit line up with rloadest counterparts", {
  # Define & munge dataset
  library(rloadest)
  simpledata <- transform(app2.calib[-which(diff(app2.calib$DATES) < 7),], 
                          Period=seasons(DATES,breaks=c("Apr", "Jul")))
  
  # Create the regression model
  reg.model <- loadReg2(loadReg(
    Atrazine ~ Period*center(log(FLOW)), 
    data = simpledata,
    flow = "FLOW", dates = "DATES", conc.units="mg/L"))
  
  # Generate point predictions
  reg.preds <- data.frame(
    simpledata, FLUX=observeSolute(simpledata, "flux", getMetadata(reg.model)), 
    conc=predictSolute(reg.model, "conc", newdata=simpledata, interval="prediction", se.pred=TRUE),
    flux=predictSolute(reg.model, "flux", newdata=simpledata, interval="prediction", se.pred=TRUE))
  
  # Aggregate by Unit - the easy one
  DF <- reg.model@fit$cfit$NOBSC - reg.model@fit$cfit$NPAR # calculate degrees of freedom as rloadest does
  agg_conc <- aggregateSolute(preds=reg.preds$conc.fit, se.preds=reg.preds$conc.se.pred, format="conc", metadata=getMetadata(reg.model), dates=reg.preds$DATES, agg.by="unit", na.rm=TRUE, deg.free=DF)
  agg_rate <- aggregateSolute(preds=reg.preds$flux.fit, se.preds=reg.preds$flux.se.pred, format="flux rate", metadata=getMetadata(reg.model), dates=reg.preds$DATES, agg.by="unit", na.rm=TRUE, deg.free=DF)
  agg_flux <- aggregateSolute(preds=reg.preds$flux.fit, se.preds=reg.preds$flux.se.pred, format="flux total", metadata=getMetadata(reg.model), dates=reg.preds$DATES, agg.by="unit", na.rm=TRUE, deg.free=DF)
  
  # Make the comparable predictions using rloadest
  agg_c_rl <- predConc(reg.model@fit, newdata=simpledata, by="unit")
  agg_l_rl <- predLoad(reg.model@fit, newdata=simpledata, by="unit")
  
  # Compare the concentration predictions from our aggregation and predConc
  expect_equal(agg_conc$Conc, agg_c_rl$Conc)
  expect_equal(agg_conc$SE, agg_c_rl$SEP)
  expect_equal(agg_conc$CI_lower, agg_c_rl$L95)
  expect_equal(agg_conc$CI_upper, agg_c_rl$U95)
  
  # Compare the load predictions from our aggregation and predLoad
  expect_equal(agg_rate$Flux, agg_l_rl$Flux)
  expect_equal(agg_rate$SE, agg_l_rl$SEP)
  expect_equal(agg_rate$CI_lower, agg_l_rl$L95)
  expect_equal(agg_rate$CI_upper, agg_l_rl$U95)
  
})


test_that("Confidence intervals can be calculated with normal or lognormal assumption", {
  # Define & munge dataset
  library(rloadest)
  simpledata <- transform(app2.calib[-which(diff(app2.calib$DATES) < 7),], 
                          Period=seasons(DATES,breaks=c("Apr", "Jul")))
  
  # Create the regression model
  reg.model <- loadReg2(loadReg(
    Atrazine ~ Period*center(log(FLOW)), 
    data = simpledata,
    flow = "FLOW", dates = "DATES", conc.units="mg/L"))
  
  # Generate point predictions
  reg.preds <- data.frame(
    simpledata, FLUX=observeSolute(simpledata, "flux", getMetadata(reg.model)), 
    conc=predictSolute(reg.model, "conc", newdata=simpledata, interval="prediction", se.pred=TRUE),
    flux=predictSolute(reg.model, "flux", newdata=simpledata, interval="prediction", se.pred=TRUE))
  
  # Aggregate by lognormal (default) or normal
  lognpreds <- aggregateSolute(reg.preds$conc.fit, se.preds=reg.preds$conc.se.pred, format="conc", metadata=getMetadata(reg.model), 
                               dates=reg.preds$DATES, agg.by="month", ci.distrib="lognormal")
  normpreds <- aggregateSolute(reg.preds$conc.fit, se.preds=reg.preds$conc.se.pred, format="conc", metadata=getMetadata(reg.model), 
                               dates=reg.preds$DATES, agg.by="month", ci.distrib="normal")
  #   
  #   regbox.preds <- data.frame(
  #     simpledata, FLUX=observeSolute(simpledata, "flux", getMetadata(reg.model)), 
  #     conc=predictSolute(reg.model, "conc", newdata=simpledata, interval="prediction", se.pred=TRUE),
  #     flux=predictSolute(reg.model, "flux", newdata=simpledata, interval="prediction", se.pred=TRUE))
  #   
  #   regbox2<- boxCox(regbox.preds$conc.fit)
  #   normpredsbox <- aggregateSolute(regbox2, se.preds=reg.preds$conc.se.pred, format="conc", metadata=getMetadata(reg.model), 
  #                                dates=reg.preds$DATES, agg.by="month", ci.distrib="normal")
  #   
  expect_equal(sum(lognpreds$Conc), sum(normpreds$Conc))
  
  #   print(ggplot(lognpreds, aes(x=as.Date(paste0(Month,"-15")), y=Conc)) + theme_bw() + 
  #           geom_point(color="blue", shape=4, size=3) + geom_ribbon(aes(ymin=CI_lower, ymax=CI_upper), color="blue", fill="blue", alpha=0.2) +
  #           geom_point(data=normpreds, color="red", shape=3, size=3) + geom_ribbon(data=normpreds, aes(ymin=CI_lower, ymax=CI_upper), color="red", fill="red", alpha=0.2))
  #   expect_manual_OK("Normal (red) and lognormal (blue) CIs make sense for monthly fluxes")
  #   
})


test_that("Aggregations by day (1 per day) line up with rloadest counterparts", {
  # Define & munge dataset
  library(rloadest)
  simpledata <- transform(app2.calib[-which(diff(app2.calib$DATES) < 7),], 
                          Period=seasons(DATES,breaks=c("Apr", "Jul")))
  simpledata_est <- transform(app2.est, Period=seasons(DATES,breaks=c("Apr", "Jul")))
  simpledata_h_est <- data.frame(DATES=seq(from=as.POSIXct(simpledata_est[12,"DATES"]), by="2 hours", length.out=700))
  simpledata_h_est$FLOW <- approx(as.POSIXct(simpledata_est$DATES), simpledata_est$FLOW, xout=simpledata_h_est$DATES)$y
  simpledata_h_est <- transform(simpledata_h_est, Period=seasons(DATES,breaks=c("Apr", "Jul")))
  
  # Create the regression model
  reg.model <- loadReg2(loadReg(
    Atrazine ~ Period*center(log(FLOW)), 
    data = simpledata,
    flow = "FLOW", dates = "DATES", conc.units="mg/L"))
  
  
  # Generate point predictions - these are one per day, so aggregating to day should be trivial
  reg.preds <- data.frame(
    simpledata_est,
    conc=predictSolute(reg.model, "conc", newdata=simpledata_est, interval="prediction", se.pred=TRUE),
    flux=predictSolute(reg.model, "flux", newdata=simpledata_est, interval="prediction", se.pred=TRUE))
  
  # Aggregate by day
  DF <- reg.model@fit$cfit$NOBSC - reg.model@fit$cfit$NPAR # calculate degrees of freedom as rloadest does
  expect_true(DF == reg.model@fit$lfit$NOBSC - reg.model@fit$lfit$NPAR) # check that DFs are the same for load and conc models
  agg_conc <- aggregateSolute(preds=reg.preds$conc.fit, se.preds=reg.preds$conc.se.pred, format="conc", metadata=getMetadata(reg.model), dates=reg.preds$DATES, agg.by="day", na.rm=TRUE, deg.free=DF)
  agg_rate <- aggregateSolute(preds=reg.preds$flux.fit, se.preds=reg.preds$flux.se.pred, format="flux rate", metadata=getMetadata(reg.model), dates=reg.preds$DATES, agg.by="day", na.rm=TRUE, deg.free=DF)
  agg_flux <- aggregateSolute(preds=reg.preds$flux.fit, se.preds=reg.preds$flux.se.pred, format="flux total", metadata=getMetadata(reg.model), dates=reg.preds$DATES, agg.by="day", na.rm=TRUE, deg.free=DF)
  
  # Make the comparable predictions using rloadest
  agg_c_rl <- predConc(reg.model@fit, newdata=simpledata_est, by="day")
  agg_l_rl <- predLoad(reg.model@fit, newdata=simpledata_est, by="day")
  
  # Compare the concentration predictions from our aggregation and predConc
  # expect_equal(as.character(agg_conc$day), format(agg_c_rl$Date,"%Y-%m-%d")) # I blame predConc here, which lumps 3 hours from the first day into a second second day
  expect_equal(as.character(agg_conc$Day)[-1], format(agg_c_rl$Date,"%Y-%m-%d")[-1]) # evereything but the first element matches
  expect_equal(agg_conc$Conc, agg_c_rl$Conc)
  expect_equal(agg_conc$SE, agg_c_rl$SEP)
  expect_equal(agg_conc$CI_lower, agg_c_rl$L95)
  expect_equal(agg_conc$CI_upper, agg_c_rl$U95)
  
  # Compare the load predictions from our aggregation and predLoad
  expect_equal(as.character(agg_rate$Day), format(agg_l_rl$Date,"%Y-%m-%d")) # with predLoad even the first day matches
  expect_equal(agg_rate$Flux, agg_l_rl$Flux)
  expect_equal(agg_rate$SE, agg_l_rl$SEP)
  expect_equal(agg_rate$CI_lower, agg_l_rl$L95)
  expect_equal(agg_rate$CI_upper, agg_l_rl$U95)
})

test_that("Aggregations by day (6 per day) pretty much line up with rloadest counterparts", {
  # Define & munge dataset
  library(rloadest)
  simpledata <- transform(app2.calib[-which(diff(app2.calib$DATES) < 7),], 
                          DATES=as.POSIXct(DATES),
                          Period=seasons(DATES,breaks=c("Apr", "Jul")))
  simpledata_est <- transform(app2.est, Period=seasons(DATES,breaks=c("Apr", "Jul")))
  simpledata_h_est <- data.frame(DATES=seq(from=as.POSIXct(simpledata_est[50,"DATES"]), by="2 hours", length.out=700))
  simpledata_h_est$FLOW <- approx(as.POSIXct(simpledata_est$DATES), simpledata_est$FLOW, xout=simpledata_h_est$DATES)$y
  simpledata_h_est <- transform(simpledata_h_est, Period=seasons(DATES,breaks=c("Apr", "Jul")))
  
  # Create the regression model. use instantaneous - i.e., assume these are 
  # instantaneous rather than time-averaged flow measurements - because those 
  # are more appropriate to the predictions we're going to do here. Ordinarily 
  # one would make this decision before analyzing the data, then use 
  # instantaneous throughout or time-averaged throughout. For code testing,
  # though, this should work fine.
  reg.model <- loadReg2(loadReg(
    Atrazine ~ Period*center(log(FLOW)), 
    data = simpledata,
    flow = "FLOW", dates = "DATES", load.units="kg", conc.units="mg/L",
    time.step="instantaneous"))
  
  # Generate point predictions again - this time challenge aggregateSolute more by using more than one point per day
  reg.preds <- data.frame(
    simpledata_h_est, #FLUX=observeSolute(simpledata_h_est, "flux", getMetadata(reg.model)), 
    conc=predictSolute(reg.model, "conc", newdata=simpledata_h_est, interval="prediction", se.pred=TRUE),
    flux=predictSolute(reg.model, "flux", newdata=simpledata_h_est, interval="prediction", se.pred=TRUE))
  #print(ggplot(reg.preds, aes(x=DATES, y=FLOW)) + geom_point() + theme_bw())
  #expect_manual_OK("Flows used for prediction")
  
  # Aggregate by day
  DF <- reg.model@fit$cfit$NOBSC - reg.model@fit$cfit$NPAR # calculate degrees of freedom as rloadest does
  expect_true(DF == reg.model@fit$lfit$NOBSC - reg.model@fit$lfit$NPAR) # check that DFs are the same for load and conc models
  agg_conc <- aggregateSolute(preds=reg.preds$conc.fit, se.preds=reg.preds$conc.se.pred, format="conc", metadata=getMetadata(reg.model), dates=reg.preds$DATES, agg.by="day", na.rm=TRUE, deg.free=DF)
  agg_rate <- aggregateSolute(preds=reg.preds$flux.fit, se.preds=reg.preds$flux.se.pred,  format="flux rate", metadata=getMetadata(reg.model), dates=reg.preds$DATES, agg.by="day", na.rm=TRUE, deg.free=DF)
  agg_flux <- aggregateSolute(preds=reg.preds$flux.fit, se.preds=reg.preds$flux.se.pred, format="flux total", metadata=getMetadata(reg.model), dates=reg.preds$DATES, agg.by="day", na.rm=TRUE, deg.free=DF)
  
  # Make the comparable predictions using rloadest
  agg_c_rl <- predConc(reg.model@fit, newdata=simpledata_h_est, by="day")
  # Here, with time.step=="instantaneous", predLoad predicts all NAs. I think this is an rloadest bug. Or maybe it's user error...?
  #MCL - 2-8-16 Needed to define load.units in order for predLoad to give a result. would be nice to predLoad gave some kind of error message 
  #if you don't.  also set allow.incomplete=TRUE
  agg_l_rl <- predLoad(reg.model@fit, newdata=simpledata_h_est, allow.incomplete=TRUE, by="day") 
  
  # Compare the concentration predictions from our aggregation and predConc. 
  # Besides the first data row, the values are close to within 3%. Are the
  # remaining discrepancies simply machine precision differences?
  #agg_conc <- agg_conc[-c(1,2),]
  #agg_c_rl <- agg_c_rl[-c(1),]
  #print(ggplot(reg.preds, aes(x=DATES, y=conc.fit)) + geom_errorbar(aes(ymin=conc.lwr, ymax=conc.upr), alpha=0.1) + geom_point() + theme_bw())
  #expect_manual_OK("Point conc predictions w/ SEs. The Period predictor makes the funny jump on May 1st.")
  #print(ggplot(agg_conc, aes(x=strptime(Day, format="%Y-%m-%d"), y=Conc)) + geom_point() + geom_errorbar(aes(ymin=CI_lower, ymax=CI_upper)) + theme_bw())
  #expect_manual_OK("Plot of agg_conc look OK?")
  #print(ggplot(agg_c_rl, aes(x=strptime(Date, format="%Y-%m-%d"), y=Conc)) + geom_point() + geom_errorbar(aes(ymin=L95, ymax=U95)) + theme_bw())
  #expect_manual_OK("Plot of agg_c_rl look OK?")
  # expect_equal(as.character(agg_conc$day), format(agg_c_rl$Date,"%Y-%m-%d")) # I blame predConc here, which lumps 3 hours from the first day into a second second day
  #agg_c_rl <- agg_c_rl[2:59,]
  expect_equal(as.character(agg_conc$Day), format(agg_c_rl$Date,"%Y-%m-%d")) # evereything but the first element matches (predConc discarded one)
  tol <- 0.03
  expect_equal(agg_conc$Conc/agg_c_rl$Conc, rep(1, nrow(agg_conc)), tolerance=tol) # tolerance describes the max MEAN difference, not the max MIN.
  expect_equal(agg_conc$SE/agg_c_rl$SEP, rep(1, nrow(agg_conc)), tolerance=tol) # tolerance describes the max MEAN difference, not the max MIN.
  expect_equal(agg_conc$CI_lower/agg_c_rl$L95, rep(1, nrow(agg_conc)), tolerance=tol) # tolerance describes the max MEAN difference, not the max MIN.
  expect_equal(agg_conc$CI_upper/agg_c_rl$U95, rep(1, nrow(agg_conc)), tolerance=tol) # tolerance describes the max MEAN difference, not the max MIN.
  #   plot(x=log(agg_conc$Conc), y=log(agg_c_rl$Conc)); abline(0,1)
  #   plot(x=log(agg_conc$SE), y=log(agg_c_rl$SEP)); abline(0,1)
  #   plot(x=log(agg_conc$CI_lower), y=log(agg_c_rl$L95)); abline(0,1)
  #   plot(x=log(agg_conc$CI_upper), y=log(agg_c_rl$U95)); abline(0,1)
  
  # Compare the load predictions from our aggregation and predLoad. I believe
  # the differences arise somewhere in estlday, but I'm not yet convinced that
  # it's worth tracking down for this first version of loadflex.
  #agg_rate <- agg_rate[-c(1,2),]
  #agg_l_rl <- agg_l_rl[-c(1),]
  #agg_l_rl <- agg_l_rl[2:59,]
  expect_equal(as.character(agg_rate$Day), format(agg_l_rl$Date,"%Y-%m-%d"))
  # Comparing values further makes no sense; the loadflex predictions look fine, qualitatively, but predLoad has produced all NAs.
  #print(ggplot(reg.preds, aes(x=DATES, y=flux.fit)) + geom_errorbar(aes(ymin=flux.lwr, ymax=flux.upr), alpha=0.1) + geom_point() + theme_bw())
  #expect_manual_OK("Point flux predictions w/ SEs. The Period predictor makes the funny jump on May 1st.")
  #print(ggplot(agg_rate, aes(x=strptime(Day, format="%Y-%m-%d"), y=Flux_Rate)) + geom_point() + geom_errorbar(aes(ymin=CI_lower, ymax=CI_upper)) + theme_bw())
  #expect_manual_OK("Plot look OK? Check back later to see if predLoad(by=day) starts working")
  #   tol <- 0.0005
  expect_equal(agg_rate$Flux/agg_l_rl$Flux, rep(1, nrow(agg_rate)), tolerance=tol)
  expect_equal(agg_rate$SE/agg_l_rl$SEP, rep(1, nrow(agg_rate)), tolerance=tol)
  expect_equal(agg_rate$CI_lower/agg_l_rl$L95, rep(1, nrow(agg_rate)), tolerance=tol)
  expect_equal(agg_rate$CI_upper/agg_l_rl$U95, rep(1, nrow(agg_rate)), tolerance=tol)
  #   plot(x=log(agg_rate$Flux), y=log(agg_l_rl$Flux)); abline(0,1)
  #   plot(y=log(agg_rate$SE), x=log(agg_l_rl$SEP)); abline(a=0,b=1)
  #   plot(y=log(agg_rate$CI_lower), x=log(agg_l_rl$L95)); abline(a=0,b=1)
  #   plot(y=log(agg_rate$CI_upper), x=log(agg_l_rl$U95)); abline(a=0,b=1)
  
})

test_that("Test custom An optional data.frame of one or more columns each containing factors or other labels on which to aggregate. Test se.preds as a dataframe.", {
  # Define & munge dataset
  #library(rloadest)
  simpledata <- transform(app2.calib[-which(diff(app2.calib$DATES) < 7),], 
                          Period=seasons(DATES,breaks=c("Apr", "Jul")))
  simpledata_est <- transform(app2.est, Period=seasons(DATES,breaks=c("Apr", "Jul")))
  
  # Create the regression model
  reg.model <- loadReg2(loadReg(
    Atrazine ~ Period*center(log(FLOW)), 
    data = simpledata,
    flow = "FLOW", dates = "DATES", conc.units="mg/L"))
  reg.preds <- data.frame(
    simpledata_est,
    conc=predictSolute(reg.model, "conc", newdata=simpledata_est, interval="prediction", se.pred=TRUE),
    flux=predictSolute(reg.model, "flux", newdata=simpledata_est, interval="prediction", se.pred=TRUE))
  reg.obs <- data.frame(
    simpledata,
    flux=observeSolute(simpledata, "flux", getMetadata(reg.model)),
    conc=observeSolute(simpledata, "conc", getMetadata(reg.model)))
  
  # Aggregate by various options
  st <- list()
  #check that custom is a data.frame or NA
  expect_error(aggregateSolute(agg.by="unit", custom="madeitup", preds=reg.preds$flux.fit, se.preds=reg.preds$flux.se.pred, format="flux total", metadata=getMetadata(reg.model), dates=reg.preds$DATES, na.rm=TRUE),"Custom must be NA or a data.frame")
  #custom check value count
  expect_error(aggregateSolute(agg.by="unit", custom=reg.preds[1:10,], preds=reg.preds$flux.fit, se.preds=reg.preds$flux.se.pred, format="flux total", metadata=getMetadata(reg.model), dates=reg.preds$DATES, na.rm=TRUE),"When custom is a data.frame, it must have as many rows as there are values in preds, se.preds")
  #pass preds as a data.frame
  st <- list()
  reg.preds$fit <- reg.preds$flux.fit
  reg.preds$se.pred <- reg.preds$flux.se.pred
  drops <- c("flux.se.pred","flux.fit")
  st[["cdf"]][["tot" ]]<- system.time({agg_flux__tot_preds_as_dataframe<- aggregateSolute(agg.by="total", preds=reg.preds, se.preds=reg.preds$conc.se.pred, format="flux total", metadata=getMetadata(reg.model), dates=reg.preds$DATES, na.rm=TRUE)})
  st[["c"]][["tot" ]] <- system.time({agg_flux_tot  <- aggregateSolute(agg.by="total", preds=reg.preds$conc.fit, se.preds=reg.preds$conc.se.pred, format="flux total", metadata=getMetadata(reg.model), dates=reg.preds$DATES, na.rm=TRUE)})
  expect_equal(agg_conc__tot_preds_as_dataframe,agg_conc_tot)
  drops <- c("se.pred")
  bad.reg.preds <- reg.preds[,!(names(reg.preds) %in% drops)]
  expect_error(aggregateSolute(agg.by="total", preds=bad.reg.preds, se.preds=reg.preds$conc.se.pred, format="conc", metadata=getMetadata(reg.model), dates=reg.preds$DATES, na.rm=TRUE),"could not find a column named se.pred in the custom preds dataframe.")
  drops <- c("fit")
  bad.reg.preds <- reg.preds[,!(names(reg.preds) %in% drops)]
  expect_error(aggregateSolute(agg.by="total", preds=bad.reg.preds, se.preds=reg.preds$conc.se.pred, format="conc", metadata=getMetadata(reg.model), dates=reg.preds$DATES, na.rm=TRUE),"could not find a column named fit in the custom preds dataframe.")
  
})

test_that("Aggregation can be done by day, month, year, water year, arbitrary columns, etc.", {
  # Define & munge dataset
  #library(rloadest)
  simpledata <- transform(app2.calib[-which(diff(app2.calib$DATES) < 7),], 
                          Period=seasons(DATES,breaks=c("Apr", "Jul")))
  simpledata_est <- transform(app2.est, Period=seasons(DATES,breaks=c("Apr", "Jul")))
  
  # Create the regression model
  reg.model <- loadReg2(loadReg(
    Atrazine ~ Period*center(log(FLOW)), 
    data = simpledata,
    flow = "FLOW", dates = "DATES", conc.units="mg/L"))
  reg.preds <- data.frame(
    simpledata_est,
    conc=predictSolute(reg.model, "conc", newdata=simpledata_est, interval="prediction", se.pred=TRUE),
    flux=predictSolute(reg.model, "flux", newdata=simpledata_est, interval="prediction", se.pred=TRUE))
  reg.obs <- data.frame(
    simpledata,
    flux=observeSolute(simpledata, "flux", getMetadata(reg.model)),
    conc=observeSolute(simpledata, "conc", getMetadata(reg.model)))
  
  # Aggregate by various options
  st <- list()
  
  # Aggregate concentrations
  st[["c"]][["unit"]] <- system.time({agg_conc_unit <- aggregateSolute(agg.by="unit", preds=reg.preds$conc.fit, se.preds=reg.preds$conc.se.pred, format="conc", metadata=getMetadata(reg.model), dates=reg.preds$DATES, na.rm=TRUE)})
  st[["c"]][["day" ]] <- system.time({agg_conc_day  <- aggregateSolute(agg.by="day", preds=reg.preds$conc.fit, se.preds=reg.preds$conc.se.pred, format="conc", metadata=getMetadata(reg.model), dates=reg.preds$DATES, na.rm=TRUE)})
  st[["c"]][["mon" ]] <- system.time({agg_conc_mon  <- aggregateSolute(agg.by="month", preds=reg.preds$conc.fit, se.preds=reg.preds$conc.se.pred, format="conc", metadata=getMetadata(reg.model), dates=reg.preds$DATES, na.rm=TRUE)})
  st[["c"]][["wy"  ]] <- system.time({agg_conc_wy   <- aggregateSolute(agg.by="water year", preds=reg.preds$conc.fit, se.preds=reg.preds$conc.se.pred, format="conc", metadata=getMetadata(reg.model), dates=reg.preds$DATES, na.rm=TRUE)})
  st[["c"]][["cy"  ]] <- system.time({agg_conc_cy   <- aggregateSolute(agg.by="calendar year", preds=reg.preds$conc.fit, se.preds=reg.preds$conc.se.pred, format="conc", metadata=getMetadata(reg.model), dates=reg.preds$DATES, na.rm=TRUE)})
  st[["c"]][["tot" ]] <- system.time({agg_conc_tot  <- aggregateSolute(agg.by="total", preds=reg.preds$conc.fit, se.preds=reg.preds$conc.se.pred, format="conc", metadata=getMetadata(reg.model), dates=reg.preds$DATES, na.rm=TRUE)})
  st[["c"]][["per" ]] <- system.time({agg_conc_per  <- aggregateSolute(agg.by="Period", custom=reg.preds, preds=reg.preds$conc.fit, se.preds=reg.preds$conc.se.pred, format="conc", metadata=getMetadata(reg.model), dates=reg.preds$DATES, na.rm=TRUE)})
  st[["c"]][["per2"]] <- system.time({agg_conc_per2 <- aggregateSolute(agg.by=c("Period","Group"), custom=transform(reg.preds,Group=rep(1:4,nrow(reg.preds))[1:nrow(reg.preds)]), preds=reg.preds$conc.fit, se.preds=reg.preds$conc.se.pred, format="conc", metadata=getMetadata(reg.model), dates=reg.preds$DATES, na.rm=TRUE)})
  expect_error(aggregateSolute(agg.by="madeitup", preds=reg.preds$conc.fit, se.preds=reg.preds$conc.se.pred, format="conc", metadata=getMetadata(reg.model), dates=reg.preds$DATES, na.rm=TRUE),"should be one of")
  expect_error(aggregateSolute(agg.by="madeitup", custom=reg.preds, preds=reg.preds$conc.fit, se.preds=reg.preds$conc.se.pred, format="conc", metadata=getMetadata(reg.model), dates=reg.preds$DATES, na.rm=TRUE),"should be one of")
  
  # Aggregate flux rates
  st[["r"]][["unit"]] <- system.time({agg_rate_unit <- aggregateSolute(agg.by="unit", preds=reg.preds$flux.fit, se.preds=reg.preds$flux.se.pred, format="flux rate", metadata=getMetadata(reg.model), dates=reg.preds$DATES, na.rm=TRUE)})
  st[["r"]][["day" ]] <- system.time({agg_rate_day  <- aggregateSolute(agg.by="day", preds=reg.preds$flux.fit, se.preds=reg.preds$flux.se.pred, format="flux rate", metadata=getMetadata(reg.model), dates=reg.preds$DATES, na.rm=TRUE)})
  st[["r"]][["mon" ]] <- system.time({agg_rate_mon  <- aggregateSolute(agg.by="month", preds=reg.preds$flux.fit, se.preds=reg.preds$flux.se.pred, format="flux rate", metadata=getMetadata(reg.model), dates=reg.preds$DATES, na.rm=TRUE)})
  st[["r"]][["wy"  ]] <- system.time({agg_rate_wy   <- aggregateSolute(agg.by="water year", preds=reg.preds$flux.fit, se.preds=reg.preds$flux.se.pred, format="flux rate", metadata=getMetadata(reg.model), dates=reg.preds$DATES, na.rm=TRUE)})
  st[["r"]][["cy"  ]] <- system.time({agg_rate_cy   <- aggregateSolute(agg.by="calendar year", preds=reg.preds$flux.fit, se.preds=reg.preds$flux.se.pred, format="flux rate", metadata=getMetadata(reg.model), dates=reg.preds$DATES, na.rm=TRUE)})
  st[["r"]][["tot" ]] <- system.time({agg_rate_tot  <- aggregateSolute(agg.by="total", preds=reg.preds$flux.fit, se.preds=reg.preds$flux.se.pred, format="flux rate", metadata=getMetadata(reg.model), dates=reg.preds$DATES, na.rm=TRUE)})
  st[["r"]][["per" ]] <- system.time({agg_rate_per  <- aggregateSolute(agg.by="Period", custom=reg.preds, preds=reg.preds$flux.fit, se.preds=reg.preds$flux.se.pred, format="flux rate", metadata=getMetadata(reg.model), dates=reg.preds$DATES, na.rm=TRUE)})
  st[["r"]][["per2"]] <- system.time({agg_rate_per2 <- aggregateSolute(agg.by=c("Period","Group"), custom=transform(reg.preds,Group=rep(1:4,nrow(reg.preds))[1:nrow(reg.preds)]), preds=reg.preds$flux.fit, se.preds=reg.preds$flux.se.pred, format="flux rate", metadata=getMetadata(reg.model), dates=reg.preds$DATES, na.rm=TRUE)})
  expect_error(aggregateSolute(agg.by="madeitup", preds=reg.preds$flux.fit, se.preds=reg.preds$flux.se.pred, format="flux rate", metadata=getMetadata(reg.model), dates=reg.preds$DATES, na.rm=TRUE),"should be one of")
  expect_error(aggregateSolute(agg.by="madeitup", custom=reg.preds, preds=reg.preds$flux.fit, se.preds=reg.preds$flux.se.pred, format="flux rate", metadata=getMetadata(reg.model), dates=reg.preds$DATES, na.rm=TRUE),"should be one of")
  
  # Aggregate bulk fluxes
  st[["f"]][["unit"]] <- system.time({agg_flux_unit <- aggregateSolute(agg.by="unit", preds=reg.preds$flux.fit, se.preds=reg.preds$flux.se.pred, format="flux total", metadata=getMetadata(reg.model), dates=reg.preds$DATES, na.rm=TRUE)})
  st[["f"]][["day" ]] <- system.time({agg_flux_day  <- aggregateSolute(agg.by="day", preds=reg.preds$flux.fit, se.preds=reg.preds$flux.se.pred, format="flux total", metadata=getMetadata(reg.model), dates=reg.preds$DATES, na.rm=TRUE)})
  st[["f"]][["mon" ]] <- system.time({agg_flux_mon  <- aggregateSolute(agg.by="month", preds=reg.preds$flux.fit, se.preds=reg.preds$flux.se.pred, format="flux total", metadata=getMetadata(reg.model), dates=reg.preds$DATES, na.rm=TRUE)})
  st[["f"]][["wy"  ]] <- system.time({agg_flux_wy   <- aggregateSolute(agg.by="water year", preds=reg.preds$flux.fit, se.preds=reg.preds$flux.se.pred, format="flux total", metadata=getMetadata(reg.model), dates=reg.preds$DATES, na.rm=TRUE)})
  st[["f"]][["cy"  ]] <- system.time({agg_flux_cy   <- aggregateSolute(agg.by="calendar year", preds=reg.preds$flux.fit, se.preds=reg.preds$flux.se.pred, format="flux total", metadata=getMetadata(reg.model), dates=reg.preds$DATES, na.rm=TRUE)})
  st[["f"]][["tot" ]] <- system.time({agg_flux_tot  <- aggregateSolute(agg.by="total", preds=reg.preds$flux.fit, se.preds=reg.preds$flux.se.pred, format="flux total", metadata=getMetadata(reg.model), dates=reg.preds$DATES, na.rm=TRUE)})
  st[["f"]][["per" ]] <- system.time({agg_flux_per  <- aggregateSolute(agg.by="Period", custom=reg.preds, preds=reg.preds$flux.fit, se.preds=reg.preds$flux.se.pred, format="flux total", metadata=getMetadata(reg.model), dates=reg.preds$DATES, na.rm=TRUE)})
  expect_warning(st[["f"]][["per2"]] <- system.time({agg_flux_per2 <- aggregateSolute(agg.by=c("Period","Group"), custom=transform(reg.preds,Group=rep(1:4,nrow(reg.preds))[1:nrow(reg.preds)]), preds=reg.preds$flux.fit, se.preds=reg.preds$flux.se.pred, format="flux total", metadata=getMetadata(reg.model), dates=reg.preds$DATES, na.rm=TRUE)}), "Highly suspicious")
  expect_error(aggregateSolute(agg.by="madeitup", preds=reg.preds$flux.fit, se.preds=reg.preds$flux.se.pred, format="flux total", metadata=getMetadata(reg.model), dates=reg.preds$DATES, na.rm=TRUE),"should be one of")
  expect_error(aggregateSolute(agg.by="madeitup", custom=reg.preds, preds=reg.preds$flux.fit, se.preds=reg.preds$flux.se.pred, format="flux total", metadata=getMetadata(reg.model), dates=reg.preds$DATES, na.rm=TRUE),"should be one of")
  
  # Report on run times
  #print(do.call(data.frame, lapply(st, function(stset) { setNames(as.data.frame(do.call(rbind, lapply(stset, function(pt) { (as.numeric(pt)) })))[1:3], c("user","system","elapsed")) })))
  #expect_manual_OK(paste("Run times for aggregation of",nrow(reg.preds),"rows"))
  
})