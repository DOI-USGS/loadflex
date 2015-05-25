library(rloadest)

#### Leverage ####
#'myFitPredictFun
#'
#'

myFitPredictFun <- function(X, Y) {
  
  # Step -1 - reconstruct the Y values needed to use loadReg.
  # The Y arg should be in the same units as Yhat, so now we have to convert
  # back to concentration units to build the loadReg model. Not pretty.
  XY <- cbind(X, Atrazine=Y / (X$FLOW * loadConvFactor(flow.units="cfs", conc.units="ug/L", load.units="pounds")))
  
  # Step 0 - fit the regression model
  my_lr <- suppressWarnings(loadReg(
    Atrazine ~ Period*center(log(FLOW)), 
    data = XY,
    flow = "FLOW", dates = "DATES", 
    flow.units="cfs", conc.units="ug/L", load.units="pounds", 
    time.step = "day",
    station="St.Joseph River near Newville, Ind."))
  
  # Step 1 - see what the regression model would predict
  regression_predictions <- suppressWarnings(predictSoluteFromRegression(
    "Flux", load.model=my_lr, data.to.predict=X))
  
  # Step 2 - calculate and interpolate the residuals for the same time points
  interpolated_residuals <- suppressWarnings(interpolateSoluteResiduals(
    "Flux", load.model=my_lr, data.to.predict=X, observations=XY, 
    interp.method="linear"))
  
  # Step 3 - combine the regression_predictions and interpolated_residuals
  adjusted_predictions <- adjustSoluteByInterpolation(
    "Flux", regression_predictions, interpolated_residuals, 
    replace.negatives=-1)
  
  adjusted_predictions$PredictedFlux
}
#XY <- transform(app2.calib, Period=seasons(DATES,breaks=c("Apr", "Jul")))
#preds <- myFitPredictFun(X=XY[,names(XY)!="Atrazine"], Y=XY$Atrazine*XY$FLOW*loadConvFactor(flow.units="cfs", conc.units="ug/L", load.units="pounds"))
#Y <- XY$Atrazine*XY$FLOW*loadConvFactor(flow.units="cfs", conc.units="ug/L", load.units="pounds")
#ggplot(data.frame(Yhat=preds,Y=Y), aes(x=Y, y=Yhat)) + geom_point() + geom_abline()

XY <- transform(app2.calib, Period=seasons(DATES,breaks=c("Apr", "Jul")))
levdf <- with(
  list(bs=seq(-1.05,1.05,by=0.1)),
  data.frame(
    b=bs, 
    levs=generalizedLeverage(
      b=bs, m=1, 
      fit.and.predict.function=myFitPredictFun, 
      X=XY[,names(XY)!="Atrazine"], 
      Y=XY$Atrazine*XY$FLOW*loadConvFactor(flow.units="cfs", conc.units="ug/L", load.units="pounds"))))
# Leverage is 1 except when the correction for negative predictions kicks in.
# When leverage = 1, it's because a change b in Y yields a change b in Yhat,
# which is because the residuals correction always pulls Yhat to meet Y.
levdf 
# So leverage is probably not be the metric we seek.


#### Influence ####

myFitRegressionFun <- function(obs.calib) {
  
  # Fit the regression model. Trying to get an imperfect model so we can see the
  # effects of the residual adjustment clearly
  #   setModelMetadata(
  #     lm(Atrazine ~ Period * log(FLOW), data=obs.calib),
  #     flow="FLOW",
  #     conc="Atrazine",
  #     dates="DATES",
  #     flow.units="cfs",
  #     conc.units="ug/L",
  #     load.units="kg",
  #     station="St.Joseph River near Newville, Ind.",
  #     time.step="unit",
  #     constituent = "Atrazine"
  #   )
  
  suppressWarnings(loadReg(
    Atrazine ~ Period * center(log(FLOW)),  #Period*
    data = obs.calib,
    flow = "FLOW", dates = "DATES", 
    flow.units="cfs", conc.units="ug/L", load.units="pounds", 
    time.step = "day",
    station="St.Joseph River near Newville, Ind."))
}

# myPredFun <- function(load.model, aggregation.interval) {
#   # Step -1 - reconstruct the matrix needed to use loadReg.
#   XY <- cbind(X.calib, Atrazine=Y.calib)
#   
#   # Step 1-4 - see what the regression model would predict
#   preds <- predLoadCM(
#     load.model=load.model, data.to.predict=XPest, observations=XY, 
#     aggregation.interval="ToTAlbY", correction.method="linear", replace.negatives=-1, verbose=FALSE)
#   
#   preds
# }
# 
# myFitTotalFun <- function(X.calib, Y.calib, X.est, P.est) {
#   myFitTotalFun(myFitRegressionFun(X.calib, Y.calib), X.calib, Y.calib, X.est, P.est)
# }



data(app2.calib)
data(app2.est)
XY <- transform(app2.calib, Period=ifelse(format(DATES, "%m") %in% c("05","06","07"), "Season Ending Jul", "Season Ending Apr"))
Xest <- transform(app2.est, Period=ifelse(format(DATES, "%m") %in% c("05","06","07"), "Season Ending Jul", "Season Ending Apr"), 
                  AggPeriod=paste0(year(DATES)," ",c("Q1-2","Q1-2","Q3-4","Q3-4")[quarter(DATES)]))
Xest <- Xest[which(year(Xest$DATES) < 1998),] #truncate so we only have 4 complete-ish periods instead of 4 plus a nearly empty period

infl1 <- fluxInfluence(
  fit.function=myFitRegressionFun, 
  obs.calib=XY, 
  obs.adjust=NA, #XY, 
  data.to.predict=Xest, 
  correction.method="linear",
  replace.negatives=0,
  aggregation.interval="AggPeriod",
  influence.on="regression")
infl2 <- fluxInfluence(
  fit.function=myFitRegressionFun, 
  obs.calib=XY, 
  obs.adjust=XY, 
  data.to.predict=Xest, 
  correction.method="linear",
  replace.negatives=0,
  aggregation.interval="AggPeriod",
  influence.on="adjustment")
infl3 <- fluxInfluence(
  fit.function=myFitRegressionFun, 
  obs.calib=XY, 
  obs.adjust=XY, 
  data.to.predict=Xest, 
  correction.method="linear",
  replace.negatives=0,
  aggregation.interval="AggPeriod",
  influence.on="both")
#rbind(infl1,infl2,infl3)

ggplot(transform(melt(infl1, id.vars="Period"), 
                 FluxPeriod=as.factor(Period), 
                 Date=XY[variable, "DATES"], 
                 ObsPeriod=as.factor(Xest[match(XY[variable, "DATES"], Xest$DATES), "AggPeriod"])), 
       aes(x=Date, y=value, color=ObsPeriod)) + 
  geom_hline(color="gray") + geom_line() + geom_point() + 
  theme_classic() + facet_grid(FluxPeriod~.) + ylab("Influence of point on load (load units), panels by aggregation interval")

ggplot(transform(melt(infl2, id.vars="Period"), 
                 FluxPeriod=as.factor(Period), 
                 Date=XY[variable, "DATES"], 
                 ObsPeriod=as.factor(Xest[match(XY[variable, "DATES"], Xest$DATES), "AggPeriod"])), 
       aes(x=Date, y=value, color=ObsPeriod)) + 
  geom_hline(color="gray") + geom_line() + geom_point() + 
  theme_classic() + facet_grid(FluxPeriod~.) + ylab("Influence of point on load (load units), panels by aggregation interval")

ggplot(transform(melt(infl3, id.vars="Period"), 
                 FluxPeriod=as.factor(Period), 
                 Date=XY[variable, "DATES"], 
                 ObsPeriod=as.factor(Xest[match(XY[variable, "DATES"], Xest$DATES), "AggPeriod"])), 
       aes(x=Date, y=value, color=ObsPeriod)) + 
  geom_hline(color="gray") + geom_line() + geom_point() + 
  theme_classic() + facet_grid(FluxPeriod~.) + ylab("Influence of point on load (load units), panels by aggregation interval")

load_model <- fit.function(XY)
load_preds <- predSoluteCM(
  load.or.conc="load", load.model=load_model, data.to.predict=Xest, observations=XY, 
  aggregation.interval="AggPeriod", correction.method="linear", replace.negatives=-1, verbose=FALSE)
load_preds$PeriodCode <- load_preds$Period
load_preds$Period <- aggregate(Xest$DATES, by=list(Xest$AggPeriod), FUN=mean)$x
plotLoadsCM(finalloads=load_preds, load.model=load_model)

m=1
load_model <- fit.function(XY[-m,])
load_preds <- predSoluteCM(
  load.or.conc="load", load.model=load_model, data.to.predict=Xest, observations=XY[-m,], 
  aggregation.interval="unit", correction.method="linear", replace.negatives=-1, verbose=FALSE)
plotLoadsCM(finalloads=load_preds, load.model=load_model)

m=5
load_model <- fit.function(XY[-m,])
load_preds <- predSoluteCM(
  load.or.conc="load", load.model=load_model, data.to.predict=Xest, observations=XY[-m,], 
  aggregation.interval="unit", correction.method="linear", replace.negatives=-1, verbose=FALSE)
plotLoadsCM(finalloads=load_preds, load.model=load_model)
