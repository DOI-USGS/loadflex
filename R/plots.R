library(ggplot2) 
library(reshape2)
library(RColorBrewer)

plotConcCM <- function(...) {
  plotCM("Conc",FALSE, ...)
}

plotLoadsCM <- function(...) {
  plotCM("Flux",FALSE, ...)
}
plotObservationsCM <- function(...) {
  plotCM("Conc",TRUE, ...)
}

#' create plots
#' 
#' create plots for examining the results from the composite method
#' 
#' @param flux.or.conc character indicating whether the plots are of 
#'   concentrations or loads, this is used for string concatation to name the
#'   columns of data produced through composite method.
#' @param show.observations true or false display observations or not.
#' @param load.model use this to pull names of date column and constituent name
#' @param finalloads these are the loads or concentrations to plot
#' @param observations these are the observations to plot, if you are plotting
#'   observations
#' @param composite, TRUE or FALSE flag for whether or not to display the
#'   composite results
#' @param linear.interpoltation TRUE or FALSE flag for whether or not to display
#'   linear interpolation of the observations
#' @param regression TRUE or FALSE flag for whether or not to display the linear
#'   regression results
#' @param xrange, a user provded range for the xvalues (dates)
#' @param verbose flag for to print for debugging
plotCM <- function(flux.or.conc, show.observations, load.model, finalloads, observations=NULL, 
                   composite=TRUE, linear.interpolation=FALSE, regression=TRUE,  xrange="none", 
                   dateField="Date", verbose=FALSE) {
  
  # Check to make sure the argument is one of the two currently accepted choices
  match.arg.loadflex(flux.or.conc)
  
  # The basic plan: construct a data.frame that contains exactly, and only, what
  # we want to plot. Then use the smart defaults in ggplot to plot it.
  ylabel <- ""
  dateName <- load.model$dates
  soluteName <- load.model$constituent
  
  #if the user wants to display residuals then make sure this is possible
  #otherwise give error message and stop  
  if(show.observations & !is.null(observations)){
    observations[[dateName]] <- as.POSIXct(observations[[dateName]])
  }
  if(show.observations & is.null(observations)){
    stop("please supply your observation data in order to display residual values")
  }  
  if(show.observations & flux.or.conc =="Flux"){
    stop("Observations concentrations, please compare them to predictions of concentrations.") 
  }
  
  # create a y axis label based on conc.units and load.units 
  switch(flux.or.conc, 
         "Flux"={
           ylabelp2 <- paste0(load.model$load.units, "/day loads")
         },
         "Conc"={
           ylabelp2 <- paste0(load.model$conc.units, " concentrations")
         })
  ylabel <- ylabelp2
  ### Get & format the date column
  # Pick out the first column name in c(dateField, "Period", "Date") that is present in finalloads
  date_col <- names(which(sapply(c(dateField, "Period", "Date",dateName), 
                                 function(field) { !is.null(finalloads[[field]]) } ))[1])
  
  # Add to the new plotsols data.frame. Since this is the first column we're
  # adding to plotsols, we'll actually create the data.frame right here - it
  # will be a data.frame with only one column, called DATE.
  
  
  plotsols <- setNames(finalloads[date_col], "DATE")
  if(verbose){
    print("plotsols ----")
    print(head(plotsols))
  }
  plotsols <- transformDates(plotsols)  # Convert to POSIXct if feasible.
  # Now do the same thing for a new plotsols_obs data.frame if show.observations==TRUE
  if(show.observations){
    plotsols_obs <- setNames(observations[dateName], "DATE")
    plotsols_obs <- transformDates(plotsols_obs)
  }
  if(verbose){
    print("plotsols ----")
    print(head(plotsols))
  }
  
  ### Add y values for lines to plot
  if(!any(c(composite, linear.interpolation, regression)))
    stop("No lines selected; choose at least one of composite, linear.interpolation, or regression")
  if(composite) {
    plotsols[["Composite"]] <- finalloads[[paste0("Composite",flux.or.conc)]]
    #ylabel <- paste0(ylabel," ", "Composite")
    
  }
  if(linear.interpolation) {
    plotsols[["LinearInterpolation"]] <- finalloads[[paste0("LinearInterp",flux.or.conc)]]
    #ylabel <- paste0(ylabel," ", "Linear Interpolation")
  }
  if(regression) {
    plotsols[["Regression"]] <- finalloads[[paste0("Regression",flux.or.conc)]]
    #ylabel <- paste0(ylabel," ", "Regression")
    if(verbose){
      print("regression added")
      print(head(plotsols))
    }
  }
  
  # Reshape the main data.frame to take full advantage of ggplot functions.
  ggdata <- meltDates(plotsols)
  

  # Add observations if requested. Restrict their date range, reshape and
  # combine data, and print the heck out of the intermediate results
  #
  #Miguel -- I was having issues with the minumum range when including finite=TRUE parameter
  # It would seem that setting finite=TRUE would help ensure a valid min but it 
  #was having the opposite effect not sure why. I left it in for the max but it should probably be
  # one way or another. 
  if(show.observations){
    # Restrict to a specific x range if requested; since this was only being
    # applied to observations, I moved it inside the if block.
    if(xrange=="none"){
      xrange <- c(min(as.POSIXct(ggdata$DATE)),max(as.POSIXct(ggdata$DATE), finite=TRUE))
      if(verbose){
        print("min")
        print(min(as.POSIXct(ggdata$DATE)))
      }
    }
    
    plotsols_obs[["Observations"]] <- observations[[soluteName]]
    plotsols_obs <- subset(plotsols_obs, DATE >= xrange[1] 
                           & DATE <= xrange[2])
    if(verbose){
      print("Observations added")
      print(head(plotsols_obs))
      
      print("xrange")
      print(xrange)  
    }
    
    if(nrow(plotsols_obs)==0){
      warning("no observations to plot within the range of the predicted values")
      show.observations <- FALSE
    }
  }
  # test again in case we've changed show.observations from TRUE to FALSE
  if(show.observations) {
    
    # Reshape the SECOND data.frame to take full advantage of ggplot functions.    
    ggdata2 <- meltDates(plotsols_obs)
    
    if(verbose){
      print("gdata2")
      print(head(ggdata2))
      print("gdata")
      print(head(ggdata))
    }

    # Combine the two reshaped data.frames (couldn't these be reshaped just once?)
    ggdata3 <- rbind(ggdata,ggdata2)
    
    if(verbose){
      print("head of ggdata3")
      print(head(ggdata3))
    }
  }
  if(verbose){
    print("ggdata ----")
    print(head(ggdata))
  }
  ### Decide on legend colors and labels
  include_line <- c("Composite"=composite, "LinearInterpolation"=linear.interpolation, 
                    "Regression"=regression,"Observations"=show.observations)
  make_legend <- function(legend_fun, values) {
    legend_fun(
      values=c(Composite=values[1],LinearInterpolation=values[2],Regression=values[3],Observations=values[4])[include_line],
      breaks=c("Composite","LinearInterpolation","Regression","Observations")[include_line],
      labels=c("Composite Method", "Linear Interpolation", "Regression Only","Observations")[include_line],
      name="")
  }
  
  # Recent edits have made it so that the only difference between
  # show.observations and not is that we use ggdata3 rather than ggdata. So
  # let's just assign the right data to ggdata right here.
  if(show.observations) {
    ggdata <- ggdata3
  }
  
  # Another label manipulation. Any chance this could appear earlier in the
  # function, grouped with other label manipulations?
  # Miguel - we probably don't need the names of the prediction types in the y axis label 
  # ylabel <- paste0(ylabel," ",ylabelp2) 
  
  # Make and print the plot (scoping problems would arise if we gave the ggobject to the user)
  ggsPlot <- ggplot(data=ggdata, aes(x=DATE, y=value, color=variable, linetype=variable, shape=variable)) +
    geom_line(na.rm=TRUE) + geom_point(na.rm=TRUE) +
    make_legend(scale_colour_manual, brewer.pal(4,"Set1")[1:4]) +
    make_legend(scale_linetype_manual, c(1,1,1,0)) +
    make_legend(scale_shape_manual, c(NA,NA,NA,19)) + 
    ylab(ylabel) +
    theme_bw()
  
  print(ggsPlot)
  
  invisible(NULL)
}

plotConcResidualsCM <- function(...){
  plotResidualsCM("conc", ...)
}
plotLoadResidualsCM <- function(...){
  plotResidualsCM("Flux", ...)
}
#' plot residuals 
#' 
#' plot residuals for visual analysis. 
#' 
#' @param load.model use this to pull names of date column and constituent name 
#' @param observations these are the observations to plot the residuals for
#' @param dateField name of the date field in observations 
#' @param verbose debugging flag 
#' @param day.of.year flag if true will plot residuals by the day of year to, can help user look for senasonality  
plotResidualsCM <- function(type, load.model, observations, dateField="Date", verbose=FALSE,  xObservations=FALSE, xFlow=FALSE, day.of.year=FALSE){
 
  dateName <- load.model$dates
  soluteName <- load.model$constituent
  flowName <- load.model$flow
  date_col <- names(which(sapply(c(dateField,  "Date",  dateName,"Period"), function(field) { !is.null(observations[[field]]) } ))[1])
  estName <- ""
  if(type=="conc"){
    resids <- predictSoluteFromRegression("conc", load.model, observations)
    names(resids)[which(names(resids)==paste0(date_col))] <- "Date"
    names(resids)[which(names(resids)=="Conc")] <- "Conc_Est"
    resids$Conc_Obs <- observations[match(resids$Date, observations[[date_col]]),paste0(soluteName)]
    resids <- resids[c("Date","Flow","Conc_Obs","Conc_Est")]
    resids$Conc_Resid <- resids$Conc_Obs - resids$Conc_Est
    estName <- "Conc_Resid"
  }else{
    loadResids <- residuals(load.model)
    
    #this won't work unless the observations are identical to those passed into predLoad...but see getRegressionSoluteResiduals()
    
    # Add to the new plotsols data.frame. Since this is the first column we're
    # adding to plotsols, we'll actually create the data.frame right here - it
    # will be a data.frame with only one column, called DATE.
    resids <- setNames(observations[date_col], "Date")
    resids$Load_Est <- loadResids
    resids$Flow <- observations[[flowName]]
    resids$Conc_Obs <- observations[[soluteName]]
    estName <- "Load_Est"
  }


  if(verbose){
    print(date_col)
    print(head(observations[[date_col]]))
  }
 
  if(verbose){
    print(head(resids))
  }
  if(day.of.year & xFlow){
    stop("can only have either one of flow or day of year or Observations on x-axis")
  }
  if(xObservations & xFlow){
    stop("can only have either one of flow or day of year or Observations on x-axis")
  }
  if(day.of.year & xObservations){
    stop("can only have either one of flow or day of year or Observations on x-axis")
  }
  if(!day.of.year & !xFlow & !xObservations){
    ggsPlot <-ggplot(resids, aes_string(x="Date", y=estName)) + geom_point()
  }else if(day.of.year){
    resids$ydays <- yday(resids$Date)
    ggsPlot <-ggplot(resids, aes_string(x="ydays", y=estName)) + geom_point() + xlab("Day of Year") + ylab("Residual Value")
  }else if(xFlow){
    ggsPlot <-ggplot(resids, aes_string(x="Flow", y=estName)) + geom_point() + xlab("Flow")+ ylab("Residual Value")
  }else if(xObservations){
    ggsPlot <-ggplot(resids, aes_string(x="Conc_Obs", y=estName)) + geom_point() + xlab("Observed Value")+ ylab("Residual Value")
  }
  print(ggsPlot)
  invisible(NULL)
}

# plotLoadResidualsCM <- function(load.model, observations, dateField="Date", verbose=FALSE){
#   resids <- residuals(load.model)
#   
#   #this won't work unless the observations are identical to those passed into predLoad...but see getRegressionSoluteResiduals()
#   dateName <- load.model$dates
#   date_col <- names(which(sapply(c(dateField, "Date",  dateName, "Period"), function(field) { !is.null(observations[[field]]) } ))[1])
#   if(verbose){
#     print(date_col)
#   }
#   # Add to the new plotsols data.frame. Since this is the first column we're
#   # adding to plotsols, we'll actually create the data.frame right here - it
#   # will be a data.frame with only one column, called DATE.
#   plotsols <- setNames(observations[date_col], "DATE")
#   plotsols$resids <- resids
#   if(verbose) print(head(plotsols))
#   # Convert to POSIXct if feasible. This will require some thought to figure out
#   # which sorts of Period formats can be generated by predLoad and predConc. In
#   # the meantime, here we address just a couple of the possibilities.
#   plotsols <- transformDates(plotsols)
#   plotsols <- meltDates(plotsols)
#   if(verbose) print(head(plotsols))
#   
#   # Make and print the plot (scoping problems would arise if we gave the ggobject to the user)
#  
#     ggsPlot <- ggplot(plotsols, aes(x=DATE, y=value)) + 
#       geom_hline(color="darkgray") + geom_point() + theme_bw() +ylab("residuals")
# 
#   print(ggsPlot)
#   
#   invisible(NULL)
# }

transformDates <- function(plotsols){
  # Convert to POSIXct if feasible. This will require some thought to figure out
  # which sorts of Period formats can be generated by predLoad and predConc. In
  # the meantime, here we address just a couple of the possibilities.
  plotsols <- transform(
    plotsols, 
    DATE={
      if(is.POSIXt(DATE)) {
        DATE
      } else if(is.Date(DATE)) {
        as.POSIXct(DATE)
      } else if(is.character(DATE) & all(grepl("^..-....-..$", DATE))) {
        strptime(DATE, format="%m-%Y-%d")
      }else if(is.character(DATE) & all(grepl("[A-z]+[ ]?....$", DATE))) {
        strptime(paste0(DATE,"-15"), format="%B %Y-%d")
      }else if(is.character(DATE) & all(grepl("^..-....$", DATE))) {
        strptime(paste0(DATE,"-15"), format="%m-%Y-%d") 
      }
      else {
        stop("Not yet sure how to deal with dates of this format")
      }})

  return(plotsols)
}

meltDates <- function(plotsols){
  ### Sort by date (don't do this before adding all the columns!)
  # this appears to be unnecessary; ggplot still plots them in chronological order even when they're out of order in the data.frame
  
  ### reshape the data.frame to take full advantage of ggplot functions.
  #reshape2::melt isn't handling POSIXct dates (?!), so use a character version of the date for melt, then replace with the POSIXt date
  plotsols$CDATE <- as.character(plotsols$DATE)
  ggdata <- melt(plotsols[-1], id.vars=c("CDATE"))
  
  ggdata$DATE <- plotsols$DATE[match(ggdata$CDATE, plotsols$CDATE)]
  return(ggdata)
}
