#TODO: functionalize this?
#TODO: tests!

#run loadflex over multiple sites

#TODO: read-in function
load('data/ana_test.Rdata')
qDF <- ana_discharge
nutriDF <- ana_no3
siteDF <- test_sites
#one function/format for now
#needs to convert dates

siteSummary <- data.frame()
modelSummary <- data.frame()


#loop over unique sites
for(site in unique(nutriDF$CODIGO_ESTACAO)){
  
  #pull out this site
  siteNutri <- filter(nutriDF, CODIGO_ESTACAO == site)
  #TODO: site metrics
  siteMetrics <- summarizeSites(site, siteDF, siteNutri)
  
  #TODO: run models
  
  #TODO: model metrics?
  
  #TODO: plots
  
  #TODO: recombine into single df
  
}

