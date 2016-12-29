#TODO: functionalize this? and break out some sections?
#TODO: tests!

library(dplyr)
library(loadflex)
library(rloadest)

#run loadflex over multiple sites

outputFormat <- "simple" #or "complex"

#TODO: read-in function
load('data/ana_test.Rdata')
qDF <- ana_discharge
nutriDF <- as.data.frame(ana_no3, stringsAsFactors = FALSE)
siteDF <- test_sites
#one function/format for now
#needs to convert dates
nutriDF$date <- as.Date(nutriDF$date)
qDF$date <- as.Date(qDF$date)

siteSummaries <- data.frame()
modelSummaries <- data.frame()
allModels <- list()
annuals <- data.frame()

#loop over unique sites
for(site in unique(nutriDF$CODIGO_ESTACAO)){
  
  #pull out this site
  siteNutri <- filter(nutriDF, CODIGO_ESTACAO == site)
  siteQ <- filter(qDF, CODIGO_ESTACAO == site)
  #create metadata
  #not sure units etc are following the correct format
  siteMeta <- metadata(constituent = "NO3_mg_L", flow = "Q_m3s", dates = "date",
                       conc.units = "mg/L", flow.units = "m^3/s", load.units = "kg",
                       load.rate.units = "kg/d", station = site)
    
  #TODO: site metrics
  siteMetrics <- summarizeSites(site, siteDF, siteNutri)
  
  #fit models
  #can we expand getInfo to access the column names in the metadata object?
  #TODO: why are the "-3900 days between daily loads" warnings happening?
  rloadest5param <- loadReg2(loadReg(NO3_mg_L ~ model(7), data = nutriDF[1:3], 
                                     flow = "Q_m3s", dates = "date", time.step = "day",
                                     flow.units = "cms", conc.units = "mg/L", load.units = "kg"))
   
  interpRect <- loadInterp(interp.format = "conc", interp.function = rectangularInterpolation,
                           data = siteNutri, metadata = siteMeta)
  comp <- loadComp(reg.model = rloadest5param, interp.format = "conc", interp.function = rectangularInterpolation, 
                   interp.data = nutriDF)
  siteModelList <- list()
  siteModelList[['comp']] <- comp
  siteModelList[['interpRect']] <- interpRect
  siteModelList[['rloadest5param']] <- rloadest5param
  
  #list of all model objects
  allModels[[site]] <- siteModelList
  
  #make predictions
  pred_rload <- predictSolute(rloadest5param, "flux", siteQ, 
                              se.pred = TRUE, date = TRUE)
  pred_interp <- predictSolute(interpRect, "flux", siteQ, 
                               se.pred = TRUE, date = TRUE)
  pred_comp <- predictSolute(comp, "flux", siteQ, se.pred = TRUE,
                             date = TRUE)
  
  #TODO: model metrics
  annualSite <- bind_rows(summarizePreds(pred_rload, siteMeta, "total", modelName = "rloadest"),
                      summarizePreds(pred_interp, siteMeta, "total", modelName = "interpolation"),
                      summarizePreds(pred_comp, siteMeta, "total", modelName = "composite"))
  
  
  #TODO: plots
  
  #TODO: recombine into single dfs
   siteSummaries <- bind_rows(siteSummaries, siteMetrics)
   annuals <- bind_rows(annuals, annualSite)
}

#TODO: write to csv
print(siteSummaries)
print(annuals)
write.csv(x = siteSummaries, file = "siteSummaries.csv")
write.csv(x = annuals, file = "annuals.csv")