#TODO: functionalize this?

#run loadflex over multiple sites

#TODO: read-in function
load('data/ana_test.Rdata')
qDF <- ana_discharge
nutriDF <- ana_no3
siteDF <- test_sites
#one function/format for now
#needs to convert dates

#loop over unique sites
for(site in unique(nutriDF$CODIGO_ESTACAO)){
  
  #TODO: site metrics
  
  #TODO: run models
  
  #TODO: model metrics?
  
  #TODO: plots
  
}

