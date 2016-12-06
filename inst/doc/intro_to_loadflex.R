## ---- echo=FALSE, message=FALSE------------------------------------------
# Set options for producing the html file & figures
library(knitr)
opts_chunk$set(echo=TRUE, message=FALSE)

## ------------------------------------------------------------------------
library(loadflex)

## ----data----------------------------------------------------------------
# Interpolation data: Packers Falls NO3 grab sample observations
data(lamprey_nitrate)
intdat <- lamprey_nitrate[c("DATE","DISCHARGE","NO3")]

# Calibration data: Restrict to points separated by sufficient time
regdat <- subset(lamprey_nitrate, REGR)[c("DATE","DISCHARGE","NO3")]

# Estimation data: Packers Falls discharge
data(lamprey_discharge)
estdat <- subset(lamprey_discharge, DATE < as.POSIXct("2012-10-01 00:00:00", tz="EST5EDT"))
estdat <- estdat[seq(1, nrow(estdat), by=96/4),] # pare to 4 obs/day for speed

## ----fig_1A--------------------------------------------------------------
meta <- metadata(constituent="NO3", flow="DISCHARGE", 
  dates="DATE", conc.units="mg L^-1", flow.units="cfs", load.units="kg", 
  load.rate.units="kg d^-1", station="Lamprey River, NH")

## ----fig_1B--------------------------------------------------------------
no3_li <- loadInterp(interp.format="conc", interp.fun=rectangularInterpolation, 
  data=intdat, metadata=meta)
no3_lm <- loadLm(formula=log(NO3) ~ log(DISCHARGE), pred.format="conc", 
  data=regdat, metadata=meta, retrans=exp)
library(rloadest)
no3_lr <- loadReg2(loadReg(NO3 ~ model(9), data=regdat,
  flow="DISCHARGE", dates="DATE", time.step="instantaneous", 
  flow.units="cfs", conc.units="mg/L", load.units="kg"))
no3_lc <- loadComp(reg.model=no3_lr, interp.format="conc", 
  interp.data=intdat)

## ---- eval=FALSE---------------------------------------------------------
#  print(no3_li)
#  getFittingFunction(no3_lm)
#  getFittedModel(no3_lr)
#  getFittingData(no3_lc)

## ----fig_1C--------------------------------------------------------------
preds_li <- predictSolute(no3_li, "flux", estdat, se.pred=TRUE, date=TRUE)
preds_lm <- predictSolute(no3_lm, "flux", estdat, se.pred=TRUE, date=TRUE)
preds_lr <- predictSolute(no3_lr, "flux", estdat, se.pred=TRUE, date=TRUE)
preds_lc <- predictSolute(no3_lc, "flux", estdat, se.pred=TRUE, date=TRUE)

## ------------------------------------------------------------------------
head(preds_lr)

## ----fig_1D, eval=FALSE--------------------------------------------------
#  summary(getFittedModel(no3_lm))
#  ggplot2::qplot(x=Date, y=Resid, data=getResiduals(no3_li, newdata=intdat))
#  residDurbinWatson(no3_lr, "conc", newdata=regdat, irreg=TRUE)
#  residDurbinWatson(no3_lr, "conc", newdata=intdat, irreg=TRUE)
#  estimateRho(no3_lr, "conc", newdata=regdat, irreg=TRUE)$rho
#  estimateRho(no3_lr, "conc", newdata=intdat, irreg=TRUE)$rho
#  getCorrectionFraction(no3_lc, "flux", newdat=intdat)

## ---- fig_1E-------------------------------------------------------------
aggs_li <- aggregateSolute(preds_li, meta, "flux rate", "month")
aggs_lm <- aggregateSolute(preds_lm, meta, "flux rate", "month")
aggs_lr <- aggregateSolute(preds_lr, meta, "flux rate", "month")
aggs_lc <- aggregateSolute(preds_lc, meta, "flux rate", "month")

## ------------------------------------------------------------------------
head(aggs_lc)

