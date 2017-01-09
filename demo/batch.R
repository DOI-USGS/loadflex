#TODO: tests!

library(dplyr)
library(loadflex)
library(rloadest)

#run loadflex over multiple sites

#------------------User Inputs--------------------#

#TODO: implement this
outputFormat <- "simple" #or "complex"

#input constituents
#script will look for folders with this name, and use in site metadata
constituents <- c("NO3", "PT")
inputFolder <- "three_ANA_sites"
outputFolder <- "output"
discharge <- "discharge"
siteInfo <- "siteInfo.csv"

#-------------------------Check files, set up directories-----------------------# 
#read-in function
makeFileDF <- function(input.folder, constits, discharge) {
  #check that all constituent files have matching discharge records
  #will generate df of corresponding files, constituents?
  #or just list?
  #loop over those instead of unique sites
  allFolders <- file.path(input.folder, c(constits, discharge))
  if(!all(dir.exists(paths))) {
    stop("Input or constituent folder does not exist")
  }
  
  #get all constituent files
  constitFiles <- list.files(file.path(input.folder,constits))
  constitNameOnly <- basename(constitFiles)
  qFiles <- file.path(input.folder, discharge, constitNameOnly)
  #TODO: warning if not matching discharge, will be skipped
  #deal with if a discharge file doesn't exist
  
  fileDF <- data.frame(constitFile = constitFiles, qFile=qFiles)
  return(fileDF)
}

fileDF <- readFiles(inputFolder, constits = constituents, discharge = discharge)
allSiteInfo <- read.csv(file.path(inputFolder, siteInfo), stringsAsFactors = FALSE)

#setup output directories
nConstits <- length(constituents)
outConstit <- file.path(rep(outputFolder, nConstits), constituents)
dir.create(outConstit, recursive = TRUE)
dir.create(file.path(rep(outConstit,2), c(rep("annual", nConstits), rep("multiYear", nConstits))))

#-----------------loadflex--------------#

siteSummaries <- data.frame()
modelSummaries <- data.frame()
allModels <- list()
annuals <- data.frame()

#loop over unique sites
for(i in 1:nrow(fileDF)) {
  message(paste('processing constituent file', fileDF$constitFile[i]))
  
  #read in appropriate files
  siteQ <- read.csv(fileDF$qFile[i], stringsAsFactors = FALSE)
  siteConstit <- read.csv(fileDF$constitFile[i], stringsAsFactors = FALSE)
  
  #needs to convert dates 
  siteConstit$date <- as.Date(constitDF$date)
  siteQ$date <- as.Date(qDF$date)
  
  #pull out appropriate rows of allSiteInfo for Q and constit
  #need to extract constit and stations from file paths, so we know 
  #what row of site info to look at
  #TODO: deal with different discharge/consituent drainage areas here?
  constitStation <- basename(file_path_sans_ext(fileDF$constitFile[i])) 
  constitName <- basename(dirname(fileDF$constitFile[i]))
  
  constitSiteInfo <- filter(allSiteInfo, station == constitStation, constituent == constitStation)
  qSiteInfo <- filter(allSiteInfo, station = constitStation, constituent == 'Q')
  
  #create metadata
  #not sure units etc are following the correct format
  siteMeta <- metadata(constituent = "NO3_mg_L", flow = "Q_m3s", dates = "date",
                       conc.units = constitSiteInfo$units, flow.units = qSiteInfo$units, load.units = "kg",
                       load.rate.units = "kg/d", station = constitStation)
    
  #TODO: site metrics
  siteMetrics <- summarizeSite(siteDF, siteConstit)
  
  #fit models
  #TODO: decide on standard column names?  user input timestep above?
  rloadest5param <- loadReg2(loadReg(NO3_mg_L ~ model(7), data = siteConstit[1:3], 
                                     flow = "Q_m3s", dates = "date", time.step = "day",
                                     flow.units = getInfo(siteMeta, 'flow.units'), 
                                     conc.units = getInfo(siteMeta, 'conc.units'),
                                     load.units = getInfo(siteMeta, 'load.units')))
   
  interpRect <- loadInterp(interp.format = "conc", interp.function = rectangularInterpolation,
                           data = siteConstit, metadata = siteMeta)
  comp <- loadComp(reg.model = rloadest5param, interp.format = "conc", interp.function = rectangularInterpolation, 
                   interp.data = constitDF)
 
  #list of all model objects
  allModels[[site]] <- list(comp = comp, interpRect = interpRect, 
                            rloadest5param = rloadest5param)
  
  #make predictions
  pred_rload <- predictSolute(rloadest5param, "flux", siteQ, 
                              se.pred = TRUE, date = TRUE)
  pred_interp <- predictSolute(interpRect, "flux", siteQ, 
                               se.pred = TRUE, date = TRUE)
  pred_comp <- predictSolute(comp, "flux", siteQ, se.pred = TRUE,
                             date = TRUE)
  
  #TODO: model metrics
  annualSite <- bind_rows(summarizePreds(pred_rload, siteMeta, "total", model.name = "rloadest"),
                      summarizePreds(pred_interp, siteMeta, "total", model.name = "interpolation"),
                      summarizePreds(pred_comp, siteMeta, "total", model.name = "composite"))
  
  
  #TODO: plots
  
  #TODO: recombine into single dfs
   siteSummaries <- bind_rows(siteSummaries, siteMetrics)
   annuals <- bind_rows(annuals, annualSite)
   
   #TODO: write to csv separate file per site
   #use output file user variable at top
   #inside loop
   print(siteSummaries)
   print(annuals)
   write.csv(x = siteMetrics, file = file.path(outputFolder, "multiYear", constitStation))
   write.csv(x = annuals, file = "annuals.csv")
   message(paste('Finished processing constituent file', fileDF$constitFile[i]))
}

