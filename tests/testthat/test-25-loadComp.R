context('loadComp')
tryCatch({source("tests/testthat/helpers.R"); source("helpers.R")}, warning=function(w) invisible())

# Define & munge dataset
library(rloadest)
simpledata <- transform(
  app2.calib[-which(diff(app2.calib$DATES) < 7),],
  Period = seasons(DATES,breaks=c("Apr", "Jul")))
estdata <- transform(app2.est, Period=seasons(DATES,breaks=c("Apr", "Jul")))

test_that("loadComp models can be created", {
  # Create the regression model
  simpledata2 <- transform(simpledata, DATES = as.POSIXct(format(DATES, '%Y-%m-%d'), tz='UTC'))
  reg.model <- loadReg2(loadReg(
    Atrazine ~ Period*center(log(FLOW)),
    data = simpledata2, time.step="instantaneous",
    flow = "FLOW", dates = "DATES", conc.units="mg/L"))

  ### HAD TO CONVERT SIMPLEDATA$DATES TO POSIXCT AND TIMESTEP TO INSTANTANEOUS
  ### BECAUSE OTHERWISE LOADREG REFUSES TO FIT TO THE BOOTSTRAP-RESAMPLED DATA
  ### (WHERE DATES ARE REPEATED). THIS IS HIGHLY INCONVENIENT.

  # Create the composite model
  load.model <- loadComp(reg.model=reg.model, interp.format="flux",
                         interp.data=simpledata2, interp.function=linearInterpolation)
  expect_is(load.model, "loadComp")
})


test_that("loadComp predictions can handle units", {
  # Create the regression and composite models
  reg.model <- loadReg2(loadReg(Atrazine ~ center(log(FLOW)), data = simpledata, flow = "FLOW", dates = "DATES", conc.units="mg/L"), pred.format = 'flux')
  load.model <- loadComp(reg.model=reg.model, interp.format="conc", interp.data=simpledata, store=c('data','fitting.function','uncertainty'), n.iter=1)
  eval.data <- simpledata %>% rename(obs_conc = Atrazine) %>% mutate(obs_flux = observeSolute(simpledata, 'flux', getMetadata(load.model)))

  args <- expand.grid(stringsAsFactors=FALSE,
    date=c(T,F), agg.by=c("unit","calendar year"), count=c(F,T), se.pred=c(T,F), flux.or.conc=c('flux','conc'),
    interval=c("none", "prediction"), lin.or.log = c("linear", "log"), attach.units=c(T,F)) %>%
    filter(agg.by == "unit" | date) # need date column if doing aggregation. error message is relatively informative
  eval_predSol <- function(i) {
    # display
    message(sprintf('\nargs[%d,]', i)); print(args[i,])

    # predict
    pred_warnings <- capture_warnings(preds <- do.call(predictSolute, args=c(list(load.model=load.model, newdata=simpledata, se.fit=FALSE), args[i,])))

    # evaluate
    preds_df <- if(is.data.frame(preds)) preds else unitted::u(setNames(data.frame(preds), c(flux='flux.rate',conc='conc')[args[i,'flux.or.conc']]))
    merged <- if(args[i,'agg.by'] == 'unit') {
      select(eval.data, DATES, obs_conc, obs_flux) %>% cbind(preds_df) %>% unitted::u()
    } else {
      select(eval.data, DATES, obs_conc, obs_flux) %>% mutate(YEAR=lubridate::year(DATES)) %>% group_by(YEAR) %>% summarize(across(c(obs_conc, obs_flux), mean)) %>% cbind(preds) %>% unitted::u()
    } %>% {if(args[i,'flux.or.conc']=='flux') {
      .[ ,names(.) != 'obs_conc']
    } else {
      .[ ,names(.) != 'obs_flux']
    }}
    arg_expect <- args[i,] %>%
      mutate(se.pred.warn = agg.by=="calendar year" & se.pred)

    arg_eval <- tibble(
      date = any(c('date','calendar.year') %in% names(preds_df)),
      agg.by = {npreds <- if(is.vector(preds_df)) length(preds_df) else nrow(preds_df); ifelse(npreds == 2, 'calendar year', ifelse(npreds == 27, 'unit', 'HMMMMM'))},
      count = 'count' %in% names(preds_df),
      se.pred = 'se.pred' %in% names(preds_df),
      se.pred.warn = isTRUE(pred_warnings == 'Uncertainty for aggregated predictions is unavailable for loadComp models; returning NAs'),
      flux.or.conc = grep('conc|flux|flux.rate', names(preds_df), value=TRUE) %>% gsub('.rate', '', ., fixed=TRUE),
      interval = ifelse('lwr.pred' %in% names(preds_df), 'prediction', 'none'),
      lin.or.log = ifelse(if(args[i,'flux.or.conc']=='flux') isTRUE(all.equal(v(merged$obs_flux), v(merged$flux))) else isTRUE(all.equal(v(merged$obs_conc), v(merged$conc))), 'linear',
                          ifelse(if(args[i,'flux.or.conc']=='flux') isTRUE(all.equal(log(v(merged$obs_flux)), v(merged$flux))) else isTRUE(all.equal(log(v(merged$obs_conc)), v(merged$conc))), 'log', 'HMMMMM')),
      attach.units = any(c(grepl('kg d^-1', get_units(preds_df), fixed=TRUE), grepl('mg L^-1', get_units(preds_df), fixed=TRUE))))
    arg_compare <- bind_rows(arg_expect, arg_eval)

    # display text
    if(nrow(distinct(arg_compare)) == 1) {
      message('output consistent with args')
    } else {
      message('output NOT consistent with args; printing')
      print(arg_compare)
    }

    # compare preds & plot if surprising
    preds_as_expected <- if(args[i,'flux.or.conc'] == 'conc') {
      isTRUE(all.equal(v(merged$obs_conc), v(merged$conc)))
    } else {
      isTRUE(all.equal(v(merged$obs_flux), v(merged$flux.rate)))
    }
    if(preds_as_expected) {
      message('predictions consistent with observations')
    } else {
      message('predictions not consistent with observations; plotting')
      p <- if(args[i,'flux.or.conc'] == 'conc') {
        if(args[i,'agg.by'] == 'unit') {
          v(merged) %>% ggplot(aes(x=DATES)) + geom_line(aes(y=conc), color='slateblue') + geom_point(aes(y=obs_conc), color='black') + theme_bw()
        } else {
          v(merged) %>% ggplot(aes(x=YEAR)) + geom_line(aes(y=conc), color='slateblue') + geom_point(aes(y=obs_conc), color='black') + theme_bw()
        }
      } else {
        if(args[i,'agg.by'] == 'unit') {
          v(merged) %>% ggplot(aes(x=DATES)) + geom_line(aes(y=flux.rate), color='slateblue') + geom_point(aes(y=obs_flux), color='black') + theme_bw()
        } else {
          v(merged) %>% ggplot(aes(x=YEAR)) + geom_line(aes(y=flux.rate), color='slateblue') + geom_point(aes(y=obs_flux), color='black') + theme_bw()
        }
      }
      (p + ggtitle(sprintf('args[%d, ]', i))) %>% print()
    }
    invisible()
  }
  for(i in 1:nrow(args)) {
    eval_predSol(i);
    readline('press Enter to advance, Ctrl-C to stop')
    # 4,5,10,11 should have count=TRUE but don't
    # 3,6 should have se.pred=TRUE but don't...but do provide the right warning about this
  }
})


test_that("loadComp preds can be made in log or linear space", {
  # Create the regression and composite models
  reg.model <- loadReg2(loadReg(Atrazine ~ center(log(FLOW)), data = simpledata, flow = "FLOW", dates = "DATES", conc.units="mg/L"), pred.format = 'conc')
  load.model <- loadComp(reg.model=reg.model, interp.data=simpledata, interp.function=linearInterpolation)

  obs <- dplyr::mutate(
    simpledata,
    AtrazineFlux=observeSolute(simpledata, 'flux', load.model@metadata),
    logAtrazine=log(Atrazine),
    logAtrazineFlux=log(AtrazineFlux))

  expect_error(predictSolute(load.model, flux.or.conc='flux', se.fit=TRUE, lin.or.log = 'lin'), 'se.fit not implemented for loadComp')
  expect_error(predictSolute(load.model, flux.or.conc='flux', se.fit=TRUE, lin.or.log = 'log'), 'se.fit not implemented for loadComp')

  # units should be respected. only available for flux, where for loadComps the flux.or.conc value passed to the reg.model is the pred format of the
  expect_equal(
    predictSolute(load.model, flux.or.conc='flux', lin.or.log = 'lin'),
    1000 * predictSolute(load.model, flux.or.conc='flux', load.units='Mg', lin.or.log = 'lin'))
  conc.model <- loadComp(reg.model=reg.model, interp.data=simpledata, interp.function=linearInterpolation, interp.format='conc')
  expect_warning(predictSolute(conc.model, flux.or.conc='flux', load.units='Mg', lin.or.log = 'lin'), "ignored for flux.or.conc='conc'")
  flux.model <- loadComp(reg.model=reg.model, interp.data=simpledata, interp.function=linearInterpolation, interp.format='flux')
  expect_equal(length(predictSolute(flux.model, flux.or.conc='conc', load.units='kg', lin.or.log = 'lin')), nrow(simpledata))

  # returns a vector unless additional columns are requested
  expect_null(dim(predictSolute(load.model, flux.or.conc='conc')))
  expect_equal(2, ncol(predictSolute(load.model, flux.or.conc='conc', se.fit=TRUE)))
  expect_equal(2, ncol(predictSolute(load.model, flux.or.conc='conc', se.pred=TRUE)))
  expect_equal(2, ncol(predictSolute(load.model, flux.or.conc='conc', date=TRUE)))
  expect_equal(4, ncol(predictSolute(load.model, flux.or.conc='conc', date=TRUE, se.fit=TRUE, se.pred=TRUE)))
  expect_error(ncol(predictSolute(load.model, flux.or.conc='conc', interval='confidence')), "confidence intervals not implemented")
  expect_equal(3, ncol(predictSolute(load.model, flux.or.conc='conc', interval='prediction')))
  expect_equal(5, ncol(predictSolute(load.model, flux.or.conc='conc', interval='prediction', date=TRUE, se.pred=TRUE)))

  # can predict in linear or log space
  expect_equal(names(predictSolute(load.model, flux.or.conc='conc', se.fit=TRUE)),
               names(predictSolute(load.model, flux.or.conc='conc', se.fit=TRUE, lin.or.log='log')))
  expect_equal(names(predictSolute(load.model, flux.or.conc='conc', se.pred=TRUE)),
               names(predictSolute(load.model, flux.or.conc='conc', se.pred=TRUE, lin.or.log='log')))
  expect_equal(names(predictSolute(load.model, flux.or.conc='conc', date=TRUE)),
               names(predictSolute(load.model, flux.or.conc='conc', date=TRUE, lin.or.log='log')))
  expect_equal(names(predictSolute(load.model, flux.or.conc='conc', date=TRUE, se.fit=TRUE, se.pred=TRUE)),
               names(predictSolute(load.model, flux.or.conc='conc', date=TRUE, se.fit=TRUE, se.pred=TRUE, lin.or.log='log')))
  expect_equal(names(predictSolute(load.model, flux.or.conc='conc', interval='prediction')),
               names(predictSolute(load.model, flux.or.conc='conc', interval='prediction', lin.or.log='log')))
  expect_equal(names(predictSolute(load.model, flux.or.conc='conc', interval='prediction', date=TRUE, se.pred=TRUE)),
               names(predictSolute(load.model, flux.or.conc='conc', interval='prediction', date=TRUE, se.pred=TRUE, lin.or.log='log')))


  library(ggplot2)
  # demo the simple: if you ask for preds in linear space, you get fit in linear
  # space going straight through predictions, and intervals computed in log
  # space and exp()ed back to linear (asymmetric around fit)
  fpreds <- predictSolute(load.model, newdata=estdata, flux.or.conc='flux', se.pred=TRUE, date=TRUE, interval='prediction')
  gf <- ggplot(fpreds, aes(x=date, y=fit)) + geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=0.2) +
    geom_line(aes(y=fit), color='blue') +
    geom_point(data=obs, aes(x=DATES, y=AtrazineFlux))
  gf
  gf + scale_y_log10()

  cpreds <- predictSolute(load.model, newdata=estdata, flux.or.conc='conc', se.pred=TRUE, date=TRUE, interval='prediction')
  gc <- ggplot(cpreds, aes(x=date)) + geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=0.2) +
    geom_line(aes(y=fit), color='blue') +
    geom_point(data=obs, aes(x=DATES, y=Atrazine))
  gc
  gc + scale_y_log10()

  # demo the compromise: if you ask for preds in log space, you get fit=log(linearfit), but youalso get fit.meanlog=linToLog(linearfit)
  flpreds <- predictSolute(load.model, newdata=estdata, flux.or.conc='flux', se.pred=TRUE, date=TRUE, interval='prediction', lin.or.log='log')
  gfl <- ggplot(flpreds, aes(x=date)) + geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=0.2) +
    geom_line(aes(y=fit), color='blue') + geom_line(aes(y=fit.meanlog), color='red') +
    geom_point(data=obs, aes(x=DATES, y=logAtrazineFlux))
  gfl

  clpreds <- predictSolute(load.model, newdata=estdata, flux.or.conc='conc', se.pred=TRUE, date=TRUE, interval='prediction', lin.or.log='log')
  gcl <- ggplot(clpreds, aes(x=date)) + geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=0.2) +
    geom_line(aes(y=fit), color='blue') + geom_line(aes(y=fit.meanlog), color='red') +
    geom_point(data=obs, aes(x=DATES, y=logAtrazine))
  gcl

})
# getting error mcl 1-22-16
# # Test composite method predictions for a variety of interpolation methods
# checkLoadCompInterpPreds <- function(interp.fun, abs.or.rel.resids, use.log, flux.or.conc) {
#
#   # Example data & models
#   library(rloadest)
#   simpledata <- transform(app2.calib[-which(diff(app2.calib$DATES) < 7),], Period=seasons(DATES,breaks=c("Apr", "Jul")))
#   estdata <- transform(app2.est, Period=seasons(DATES,breaks=c("Apr", "Jul")))
#   reg.model <- loadReg2(loadReg(Atrazine ~ center(log(FLOW)), data = simpledata, flow = "FLOW", dates = "DATES", conc.units="mg/L"))
#   library(reshape2)
#
#   for(interp.format in c("flux","conc")) {
#     # Interpolate by flux: Generate observations, intermediates, and predictions
#     load.model <- loadComp(reg.model=reg.model, interp.format=interp.format, interp.data=simpledata,
#                            interp.function=interp.fun, abs.or.rel.resids=abs.or.rel.resids, use.log=use.log, store=c())
#     expect_error(predictSolute(load.model, flux.or.conc, estdata, se.fit=TRUE), "se.fit not implemented for loadComp")
#     expect_error(predictSolute(load.model, flux.or.conc, estdata, se.pred=TRUE), "(unavailable).*(store)")
#     expect_error(predictSolute(load.model, flux.or.conc, estdata, interval="confidence"), "confidence intervals not implemented for loadComp")
#     expect_error(predictSolute(load.model, flux.or.conc, estdata, interval="prediction"), "(unavailable).*(store)")
#
#     predobs <- rbind(
#       melt(
#         data.frame(
#           Date=simpledata$DATES,
#           Obs=observeSolute(simpledata, flux.or.conc, getMetadata(load.model)),
#           ResidObs=observeSolute(getFittingData(load.model@fit@resid.model), flux.or.conc, load.model@fit@resid.model@metadata,
#                                  calculate=(flux.or.conc!=interp.format))
#         ), id.vars=.(Date)
#       ),
#       melt(
#         data.frame(
#           Date=estdata$DATES,
#           Reg=predictSolute(load.model@fit@reg.model, flux.or.conc, estdata),
#           Comp=predictSolute(load.model, flux.or.conc, estdata),
#           Resid=predictSolute(load.model@fit@resid.model, flux.or.conc, estdata)
#         ), id.vars=.(Date)
#       )
#     )
#     predobs$IsResid <- ifelse(predobs$variable%in% c("Resid","ResidObs"),"Resids",flux.or.conc)
#
#     # Plot the intermediates& results
#     ttl <- paste0(as.character(substitute(interp.fun))[1],"; ",abs.or.rel.resids,"; pred ",flux.or.conc," by interp ",if(use.log) "log " else "",interp.format)
#     print(ggplot(predobs, aes(x=Date, y=value, color=variable)) +
#             geom_line(data=predobs[predobs$variable %in% c("Reg","Comp","Resid"),], size=1) +
#             geom_point(data=predobs[predobs$variable %in% c("Obs","ResidObs"),], size=3) +
#             theme_bw() + facet_grid(IsResid ~ ., scales="free_y", space="free_y", shrink=TRUE) +
#             ylab(flux.or.conc) + ggtitle(ttl))
#     expect_manual_OK(ttl)
#   }
#
# }
#
# test_that("linearInterpolations look good within loadcomp", {
#   checkLoadCompInterpPreds(linearInterpolation, "absolute", TRUE, "flux")
#   checkLoadCompInterpPreds(linearInterpolation, "absolute", FALSE, "conc")
#   checkLoadCompInterpPreds(linearInterpolation, "relative", FALSE, "flux")
#   checkLoadCompInterpPreds(linearInterpolation, "relative", TRUE, "conc")
# })
# test_that("triangularInterpolations look good within loadcomp", {
#   checkLoadCompInterpPreds(triangularInterpolation, "absolute", FALSE, "flux")
#   checkLoadCompInterpPreds(triangularInterpolation, "absolute", TRUE, "conc") # looks bad
#   checkLoadCompInterpPreds(triangularInterpolation, "relative", TRUE, "flux")
#   checkLoadCompInterpPreds(triangularInterpolation, "relative", FALSE, "conc") # looks bad
# })
# test_that("rectangularInterpolations look good within loadcomp", {
#   checkLoadCompInterpPreds(rectangularInterpolation, "absolute", T, "flux")
#   checkLoadCompInterpPreds(rectangularInterpolation, "absolute", F, "conc")
#   checkLoadCompInterpPreds(rectangularInterpolation, "relative", F, "flux")
#   checkLoadCompInterpPreds(rectangularInterpolation, "relative", T, "conc")
# })
# test_that("splineInterpolations look good within loadcomp", {
#   checkLoadCompInterpPreds(splineInterpolation, "absolute", F, "flux")
#   checkLoadCompInterpPreds(splineInterpolation, "absolute", T, "conc")
#   checkLoadCompInterpPreds(splineInterpolation, "relative", T, "flux")
#   checkLoadCompInterpPreds(splineInterpolation, "relative", F, "conc")
# })
# test_that("smoothSplineInterpolations look good within loadcomp", {
#   checkLoadCompInterpPreds(getSmoothSplineInterpolation(nknots=22), "absolute", T, "flux")
#   checkLoadCompInterpPreds(getSmoothSplineInterpolation(nknots=22), "absolute", F, "conc")
#   checkLoadCompInterpPreds(getSmoothSplineInterpolation(nknots=22), "relative", F, "flux")
#   checkLoadCompInterpPreds(getSmoothSplineInterpolation(nknots=22), "relative", T, "conc")
# })
# test_that("distanceWeightedInterpolations look good within loadcomp", {
#   checkLoadCompInterpPreds(distanceWeightedInterpolation, "absolute", F, "flux")
#   checkLoadCompInterpPreds(distanceWeightedInterpolation, "absolute", T, "conc")
#   checkLoadCompInterpPreds(distanceWeightedInterpolation, "relative", T, "flux")
#   checkLoadCompInterpPreds(distanceWeightedInterpolation, "relative", F, "conc")
# })


test_that("loadComp models can estimate their uncertainty", {

  # Example data & models
  library(rloadest)
  simpledata <- transform(app2.calib[-which(diff(app2.calib$DATES) < 7),], Period=seasons(DATES,breaks=c("Apr", "Jul")))
  estdata <- transform(app2.est, Period=seasons(DATES,breaks=c("Apr", "Jul")))
  rl.model <- loadReg2(loadReg(Atrazine ~ center(log(FLOW)), data = simpledata, flow = "FLOW", dates = "DATES", conc.units="mg/L"))
  atra_meta <- metadata(constituent="Atrazine", flow="FLOW", dates="DATES", conc.units="mg L^-1", flow.units="cfs", load.units="kg", load.rate.units="kg d^-1")
  lm.model <- loadLm(log(Atrazine) ~ center(log(FLOW)), data=simpledata, metadata=atra_meta)

  # Fit the loadComp with store=c("data","uncertainty") (to calculate uncertainty) for many
  # combinations of interp.functions, abs/rel, log/lin options
  # system.time(MSEresults <- do.call("rbind", lapply(
  #   1:6, function(funID) { do.call("rbind", lapply(
  #       c("conc", "flux"), function(interpformat) { do.call("rbind", lapply(
  #         c("absolute", "relative"), function(absrel) { do.call("rbind", lapply(
  #           c(FALSE, TRUE), function(uselog) {
  #             fun <- list(linearInterpolation, triangularInterpolation, rectangularInterpolation,
  #                         splineInterpolation, getSmoothSplineInterpolation(nknots=22), distanceWeightedInterpolation)[[funID]]
  #             load.model.rl <- loadComp(
  #               reg.model=rl.model, interp.format=interpformat, interp.data=simpledata,
  #               interp.function=fun, abs.or.rel.resids=absrel, use.log=uselog, store=c("data","uncertainty"))
  #             load.model.lm <- loadComp(
  #               reg.model=lm.model, interp.format=interpformat, interp.data=simpledata,
  #               interp.function=fun, abs.or.rel.resids=absrel, use.log=uselog, store=c("data","uncertainty"))
  #             data.frame(
  #               fun=c("linear","triangular","rectangular","spline","smoothspline","distanceweighted")[funID],
  #               interpby=interpformat,
  #               absrel=absrel,
  #               uselog=uselog,
  #               MSElog_rl_m=if(uselog) load.model.rl@MSE["mean","conc"] else NA,
  #               MSElog_lm_m=if(uselog) load.model.lm@MSE["mean","conc"] else NA,
  #               #MSElog_s=if(uselog) load.model@MSE["sd","conc"] else NA,
  #               # don't need the "flux" col of log MSE because it's the same as the "conc" col
  #               MSElin_rl_mc=if(!uselog) load.model.rl@MSE["mean","conc"] else NA,
  #               MSElin_lm_mc=if(!uselog) load.model.lm@MSE["mean","conc"] else NA,
  #               #MSElin_sc=if(!uselog) load.model@MSE["sd","conc"] else NA,
  #               MSElin_rl_mf=if(!uselog) load.model.rl@MSE["mean","flux"] else NA,
  #               MSElin_lm_mf=if(!uselog) load.model.lm@MSE["mean","flux"] else NA)
  #               #MSElin_sf=if(!uselog) load.model@MSE["sd","flux"] else NA)
  #           }))
  #         }))
  #       }))
  #   }))
  # )
  # print(subset(MSEresults, uselog==FALSE)[c(1:4,7:10)])
  # expect_manual_OK("linear-space MSEs make sense")
  # print(subset(MSEresults, uselog==TRUE)[1:6])
  # expect_manual_OK("log-space MSEs make sense")

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


test_that("loadComp can summarize itself", {
  # Setup from intro_to_loadflex.Rmd
  data(lamprey_nitrate)
  intdat <- lamprey_nitrate[c("DATE","DISCHARGE","NO3")]
  regdat <- subset(lamprey_nitrate, REGR)[c("DATE","DISCHARGE","NO3")]
  data(lamprey_discharge)
  estdat <- subset(lamprey_discharge, DATE < as.POSIXct("2012-10-01 00:00:00", tz="EST5EDT"))
  estdat <- estdat[seq(1, nrow(estdat), by=96/4),] # pare to 4 obs/day for speed
  meta <- metadata(constituent="NO3", flow="DISCHARGE",
                   dates="DATE", conc.units="mg L^-1", flow.units="cfs", load.units="kg",
                   load.rate.units="kg d^-1", site.name="Lamprey River, NH",
                   consti.name="Nitrate", site.id='01073500', lat=43.10259, lon=-70.95256)
  library(rloadest)
  no3_lr <- loadReg2(loadReg(NO3 ~ model(9), data=regdat,
                             flow="DISCHARGE", dates="DATE", time.step="instantaneous",
                             flow.units="cfs", conc.units="mg/L", load.units="kg",
                             station='Lamprey River, NH'))
  no3_lc <- loadComp(reg.model=no3_lr, interp.format="conc",
                     interp.data=intdat)

  # test for expected errors and warnings about timesteps
  expect_error(summarizeModel(no3_lc, newdata=estdat, irregular.timesteps.ok=FALSE))
  expect_warning(summarizeModel(no3_lc, newdata=estdat, irregular.timesteps.ok=NA))
  expect_equal(nrow(summarizeModel(no3_lc, newdata=estdat, irregular.timesteps.ok=TRUE)), 1)

  # test that the function requires the newdata arg
  expect_error(summarizeModel(no3_lc, irregular.timesteps.ok=TRUE), "newdata")

})
