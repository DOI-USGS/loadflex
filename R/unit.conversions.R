#### Data ####

#' Defines the units (or pieces of units) that are permissible in valid load
#' models
#' 
#' Contains a dictionary of valid units and their dimensions
#' 
#' @name valid.metadata.units
#' @docType data
#' @format A data.frame with character columns "unit" and "dimension"
#' @examples
#' data(valid.metadata.units); valid.metadata.units
NULL

#' Defines the units that are permissible to pass to load models
#' 
#' Contains a dictionary with which we will translate a range of possibilities
#' into a smaller, more uniform set of units
#' 
#' @name freeform.unit.translations
#' @docType data
#' @format A data.frame with character columns "new" and "old"
#' @keywords data units
#' @examples
#' data(freeform.unit.translations); freeform.unit.translations
NULL

#' Defines the units that are permissible in load models, along with the 
#' multiplers that will allow common conversions among units
#' 
#' @name unit.conversions
#' @docType data
#' @format A data.frame with character columns "numerator", "denominator", and
#'   numeric column "value", one conversion per row
#' @keywords data units
#' @examples
#' data(unit.conversions); unit.conversions
NULL

#' Generate units conversion & parsing data
#' 
#' Should only need to be run once on a change in the function contents.
#' Generates the units data to be saved in data/valid.metadata.units, 
#' data/freeform.unit.translations, data/unit.conversions, and (all three 
#' combined) R/sysdata.rda. These are then saved with the package.
#' 
#' @importFrom stats setNames
#' @import dplyr
#' @keywords data units internal
generateUnitsData <- function() {
  # valid.metadata.units
  
  valid.metadata.units <- bind_rows(
    data_frame(
      dimension="volume",
      standard="L",
      unit=c("m^3", "ft^3", "dL", "L")),
    data_frame(
      dimension="time",
      standard="d",
      unit=c("s", "d", "y")),
    data_frame(
      dimension="mass",
      standard="mg",
      unit=c("lb", "ton", "ng", "ug", "mg", "g", "kg", "Mg")),
    data_frame(
      dimension="count",
      standard="million_colonies",
      unit=c("colonies", "million_colonies")),
    data_frame(
      dimension="area",
      standard="km^2",
      unit=c("m^2", "ha", "km^2", "ft^2", "ac", "mi^2")
    )
  ) %>% as.data.frame(stringsAsFactors=FALSE)
  save(valid.metadata.units, file="data/valid.metadata.units.RData")

  # freeform.unit.translations
  freeform.unit.translations <- bind_rows(
    # Volumes
    data_frame(new="m^3", old=c("cubic meter", "cubic meters", "m^3")),
    data_frame(new="ft^3", old=c("cubic foot", "cubic feet", "ft^3")),
    data_frame(new="dL", old=c("100mL", "dL")),
    data_frame(new="L", old=c("liter", "l", "L")),
    
    # Times
    data_frame(new="s", old=c("second", "sec", "s")),
    data_frame(new="d", old=c("day", "d")),
    data_frame(new="y", old=c("year", "yr", "y")),
    
    # Masses and counts
    data_frame(new="lb", old=c("pounds", "lbs", "lb")),
    data_frame(new="ton", old=c("tons")),
    data_frame(new="ng", old=c("nanograms", "ng")),
    data_frame(new="ug", old=c("micrograms", "ug")),
    data_frame(new="mg", old=c("milligrams", "mg")),
    data_frame(new="g", old=c("grams", "g")),
    data_frame(new="kg", old=c("kilograms", "kg")),
    data_frame(new="Mg", old=c("metric tons", "Mg")),
    data_frame(new="colonies", old=c("col", "colonies")),
    data_frame(new="million_colonies", old=c("million colonies")),
    
    # Abbreviations
    data_frame(new="m^3 s^-1", old=c("cubic meter per second", "cms")),
    data_frame(new="ft^3 s^-1", old=c("cubic feet per second", "cubic foot per second", "cfs"))
  ) %>% as.data.frame(stringsAsFactors=FALSE)
  save(freeform.unit.translations, file="data/freeform.unit.translations.RData")
  
  # unit.conversions
  unit.conversions <- setNames(bind_rows(
    # Volumes
    data.frame(num="L", bind_rows(
      data_frame(den="m^3", val=1000),
      data_frame(den="ft^3", val=28.317),
      data_frame(den="dL", val=0.1),
      data_frame(den="L", val=1)),
      stringsAsFactors=FALSE
    ),
    
    # Times
    data.frame(num="d", bind_rows(
      data_frame(den="s", val=1/(60*60*24)),
      data_frame(den="d", val=1),
      data_frame(den="y", val=365.25)),
      stringsAsFactors=FALSE
    ),
    
    # Masses and counts
    data.frame(den="mg", bind_rows(
      data_frame(num="lb", val=2.204623e-6),
      data_frame(num="ton", val=1.102311e-9),
      data_frame(num="ng", val=1.0e6),
      data_frame(num="ug", val=1.0e3),
      data_frame(num="mg", val=1),
      data_frame(num="g", val=1.0e-3),
      data_frame(num="kg", val=1.0e-6),
      data_frame(num="Mg", val=1.0e-9)),
      stringsAsFactors=FALSE
    )[,c(2,1,3)],
    
    data.frame(num="million_colonies", bind_rows(
      data_frame(den="colonies", val=1.0e-6),
      data_frame(den="million_colonies", val=1)),
      stringsAsFactors=FALSE
    ),
    
    # Areas
    data.frame(num="km^2", bind_rows(
      data_frame(den="ft^2", val=9.290304e-8),
      data_frame(den="ac", val=0.0040468564224),
      data_frame(den="mi^2", val=2.589988110336),
      data_frame(den="m^2", val=1e-6),
      data_frame(den="ha", val=0.01),
      data_frame(den="km^2", val=1)),
      stringsAsFactors=FALSE
    )
  ), c("numerator", "denominator", "value"))
  # Append the reverse conversions
  numerator<-denominator<-value<-".transform.var"
  
  unit.conversions <- bind_rows(
    unit.conversions,
    transform( # mutate would overwrite numerator prematurely
      unit.conversions, 
      numerator=denominator,
      denominator=numerator,
      value=1/value))
  unit.conversions$numerator <- as.character(unit.conversions$numerator)
  unit.conversions$denominator <- as.character(unit.conversions$denominator)
  unit.conversions <- unique(unit.conversions) %>% as.data.frame(stringsAsFactors=FALSE)
  save(unit.conversions, file="data/unit.conversions.RData")
  
  # The above save calls put the data into a user-accessible location (the data 
  # folder). But the following is the important line for the functionality of 
  # flowconcToFluxConversion(), translateFreeformToUnitted(), 
  # validMetadataUnits(), etc.:
  save(valid.metadata.units, unit.conversions, freeform.unit.translations, file="R/sysdata.rda")
  
}


#### Functions ####

#' Check whether these units are acceptable (without translation) for inclusion in metadata
#' 
#' @importFrom unitted separate_units get_units unitbundle
#' @param unitstr A string representing units (just one bundle at a time, please)
#' @param unit.type string. accepts "ANY","flow.units","conc.units","load.units", or "load.rate.units"
#' @param type A string describing the type of units desired
#' @return logical. TRUE if valid for that unit type, FALSE otherwise
#' @keywords units
#' @export
#' @examples
#' validMetadataUnits("colonies d^-1") # TRUE
#' validMetadataUnits("nonsensical") # FALSE
#' validMetadataUnits("g", unit.type="load.units") # TRUE
#' validMetadataUnits("g", unit.type="flow.units") # FALSE
validMetadataUnits <- function(unitstr, unit.type=c("ANY","flow.units","conc.units","load.units","load.rate.units","basin.area.units")) {
  unit.type <- match.arg(unit.type)

  # Parse the unit string into numerator and denominator strings
  unitpieces <- separate_units(unitbundle(unitstr))
  numerator <- get_units(unitbundle(unitpieces[which(unitpieces$Power > 0),]))
  denominator <- get_units(1/unitbundle(unitpieces[which(unitpieces$Power < 0),]))
  
  # A useful helper function: returns TRUE iif oneunit is present in valid.metadata.units
  # and has a dimension that's in dims
  unit <- "subset.var"
  hasDim <- function(oneunit, dims) {
    unit_row <- subset(valid.metadata.units, unit==oneunit)
    if(nrow(unit_row) != 1) {
      return(FALSE)
    } else if(!(unit_row$dimension %in% dims)) {
      return(FALSE)
    } else {
      return(TRUE)
    }
  }

  # The numerator (and denominator if it exists) should have specific dimensions
  # for each units type. Check.
  switch(
    unit.type,
    "ANY" = any(sapply(c("flow.units","conc.units","load.units","load.rate.units","basin.area.units"), function(eachtype) {
        validMetadataUnits(unitstr, eachtype)
      })),
    flow.units = hasDim(numerator, "volume") & hasDim(denominator, "time"),
    conc.units = hasDim(numerator, c("mass","count")) & hasDim(denominator, "volume"),
    load.units = hasDim(numerator, c("mass","count")) & denominator=="",
    load.rate.units = hasDim(numerator, c("mass","count")) & hasDim(denominator, c("time")),
    basin.area.units = hasDim(numerator, c("area")) & denominator==""
  )
}

#' Return the unit.type of a unit string
#' 
#' @param unitstr A string representing units (just one at a time, please)
#' @examples
#' loadflex:::unitType('kg') # 'load.units'
#' loadflex:::unitType('kg/d') # NA
#' loadflex:::unitType(loadflex:::translateFreeformToUnitted('kg/d')) # 'load.rate.units'
#' loadflex:::unitType('nothing') # NA
unitType <- function(unitstr) {
  unit.type <- names(which(sapply(c("flow.units","conc.units","load.units","load.rate.units","basin.area.units"), function(eachtype) {
    validMetadataUnits(unitstr, eachtype)
  })))
  if(length(unit.type) == 0) unit.type <- NA
  return(unit.type)
}

#' Determine whether a 1D dimension string is known and of the expected
#' dimension type
#' 
#' @param dimstr A string representing one units dimension (just one at a time, 
#'   please). For example: kg, d, or m^3, but not kg/d or d^-1
#' @param dim.type One or more acceptable dimension types
#' @examples 
#' loadflex:::validDim('kg') # TRUE
#' loadflex:::validDim('kg', 'mass') # TRUE
#' loadflex:::validDim('kg', 'volume') # FALSE
#' loadflex:::validDim('whoknows') # FALSE
#' loadflex:::validDim('whoknows', 'time') # FALSE
validDim <- function(dimstr, dim.type=c("ANY","volume","time","mass","count","area")) {
  # settle on 1+ dimensions to expect
  dim.type <- match.arg(dim.type, several.ok=TRUE)
  if('ANY' %in% dim.type) dim.type <- setdiff(eval(formals(loadflex:::validDim)$dim.type), 'ANY')
  
  dim_row <- subset(valid.metadata.units, unit==dimstr)
  if(nrow(dim_row) != 1 || !(dim_row$dimension %in% dim.type)) {
    return(FALSE)
  } else {
    return(TRUE)
  }
}

#' Convert units from a greater variety of forms, including rloadest form, to 
#' unitted form
#' 
#' @importFrom unitted unitbundle get_units
#' @param freeform.units character string or list of character strings 
#'   describing one or more sets of units. "Freeform" is an exaggeration but 
#'   gets at the idea that these units can take a greater variety of forms than 
#'   those accepted by unitted and the units conversion functions in 
#'   \code{loadflex}.
#' @param attach.units logical. If TRUE, returned value is unitted; otherwise, 
#'   it's character.
#' @return a unitbundle or list of unitbundles containing equivalent but simpler
#'   and more uniform units
#' @examples
#' loadflex:::translateFreeformToUnitted("cfs") # "ft^3 s^-1"
#' loadflex:::translateFreeformToUnitted("kg/d") # "kg d^-1"
#' loadflex:::translateFreeformToUnitted("mg L^-1") # "mg L^-1"
translateFreeformToUnitted <- function(freeform.units, attach.units=FALSE) {
  # Quick escape if our work is already done.

  if(validMetadataUnits(freeform.units, "ANY")) {
    return(if(attach.units) unitbundle(freeform.units) else get_units(unitbundle(freeform.units)))
  }
  
  # format the string[s] in the vector (may be length 1) and convert it to a
  # list of elements split on "/"; each list element is one bundle of units
  units <- gsub(" per ", "/", freeform.units)
  unitslist <- strsplit(units, "/")
  # for each bundle of units, find each element in the dictionary and translate if needed
  #data(freeform.unit.translations)
  old <- "tidyunit.var"
  units <- lapply(unitslist, function(units) {
    units <- lapply(units, function(unit) {
      tidyunit <- gsub("^ +| +$", "", unit) # strip surrounding spaces
      tidyunit <- as.character(subset(freeform.unit.translations, old == tidyunit)$new) # find in data.frame
      if(length(tidyunit) == 0) {
        stop("unexpected unit '",unit,"'. See 'Valid units options' in ?metadata")
      }
      unitbundle(tidyunit)
    })
    if(length(units) == 1) return(units[[1]])
    else if(length(units) != 2) stop(paste0("unexpected units: '", paste(units, collapse="';'"),"'. See 'Valid units options' in ?metadata"))
    else return(units[[1]]/units[[2]])
  })
  
  if(!attach.units) {
    units <- lapply(units, get_units)
  }
  
  if(length(units) == 1) return(units[[1]]) else return(units)
}


#' Provide the conversion factor which, when multiplied by flow * conc, gives 
#' the flux in the desired units
#' 
#' By dividing rather than multiplying by this factor, the output of this 
#' function may also be used to convert from flux units to the units of the 
#' product of flow and concentration.
#' 
#' @importFrom unitted separate_units get_units unitbundle u
#' @export
#' @param flow.units character. The units of flow.
#' @param conc.units character. The units of concentration.
#' @param load.rate.units character. The units of flux.
#' @param attach.units logical. If TRUE, the conversion factor is returned with
#'   units attached.
#' @return numeric, or unitted numeric if unitted=TRUE. The conversion factor.
#' @examples
#' flowconcToFluxConversion("L/d", "g/L", "g/d") # 1
#' flowconcToFluxConversion("cfs", "g/L", "kg/d") # 2446.589
#' library(unitted); u(10, "ft^3 s^-1") * u(2, "mg L^-1") * 
#' flowconcToFluxConversion("cfs", "mg/L", "kg/d", attach.units=TRUE) # u(48.9 ,"kg d^-1")
flowconcToFluxConversion <- function(flow.units, conc.units, load.rate.units, attach.units=FALSE) {
  ## Code inspired by rloadest::loadConvFactor code by DLLorenz and ldecicco
  ## Makes heavy use of unitted package by A Appling
  
  
  # Translate units - goes quickly if they're good already
  flow.units <- translateFreeformToUnitted(flow.units, TRUE)
  conc.units <- translateFreeformToUnitted(conc.units, TRUE)
  load.rate.units <- translateFreeformToUnitted(load.rate.units, TRUE)
  
  # split the flow.units*conc.units into numerator and denominator
  flow.conc.units <- flow.units * conc.units
  fcu_separated <- separate_units(flow.conc.units)
  fcu_numerstrs <- strsplit(get_units(unitbundle(fcu_separated[which(fcu_separated$Power > 0),])), " ")[[1]]
  fcu_denomstrs <- strsplit(get_units(1/unitbundle(fcu_separated[which(fcu_separated$Power < 0),])), " ")[[1]]
  
  # split the load.rate.units into numerator and denominator
  lru_separated <- separate_units(load.rate.units)
  lru_numerstr <- strsplit(get_units(unitbundle(lru_separated[which(lru_separated$Power > 0),])), " ")[[1]]
  lru_denomstr <- strsplit(get_units(1/unitbundle(lru_separated[which(lru_separated$Power < 0),])), " ")[[1]]
  
  # Identify the right components of the multiplier. Components that are
  # unavailable will be omitted from the multipliers data.frame
  #data(unit.conversions)
  numerator <- denominator <- "rbind.var"
  multipliers <- rbind(
    # Convert to mg/day
    numer_to_mg = subset(unit.conversions, numerator == "mg" & denominator %in% fcu_numerstrs),
    numer_to_L = subset(unit.conversions, numerator == "L" & denominator %in% fcu_numerstrs),
    denom_to_L = subset(unit.conversions, denominator == "L" & numerator %in% fcu_denomstrs),
    denom_to_d = subset(unit.conversions, denominator == "d" & numerator %in% fcu_denomstrs),
    # Convert mg/day to load.rate.units
    mg_to_load = subset(unit.conversions, numerator == lru_numerstr & denominator == "mg"),
    d_to_load = subset(unit.conversions, numerator == "d" & denominator == lru_denomstr)
  )
  
  # Combine the component multipliers into a single multiplier
  multiplier <- u(1,"")
  for(row in 1:nrow(multipliers)) {
    multrow <- multipliers[row,]
    multiplier <- multiplier * u(multrow$value, unitbundle(multrow$numerator)/unitbundle(multrow$denominator)) 
  }
  
  # If the conversion isn't right yet, look for conversions that don't pass through "mg"
  if(unitbundle(get_units(u(1,flow.conc.units) * multiplier)) != load.rate.units) {
    # Identify the numerator still to be converted
    residual_units <- load.rate.units / unitbundle(get_units(u(1,flow.conc.units) * multiplier))
    ru_separated <- separate_units(residual_units)
    ru_numerstr <- strsplit(get_units(unitbundle(ru_separated[which(ru_separated$Power > 0),])), " ")[[1]]
    ru_denomstr <- strsplit(get_units(1/unitbundle(ru_separated[which(ru_separated$Power < 0),])), " ")[[1]]
    # We want the multiplier with the same units as residual_units
    num_to_load <- subset(unit.conversions, denominator == ru_denomstr & numerator == ru_numerstr)
    # Do the additional conversion
    if(nrow(num_to_load) == 1) {
      multiplier <- multiplier * u(num_to_load$value, unitbundle(num_to_load$numerator)/unitbundle(num_to_load$denominator))
    }
    
    # Now confirm that the conversion will work - it really should now.
    if(unitbundle(get_units(u(1,flow.conc.units) * multiplier)) != load.rate.units) {
      stop("Failed to identify the right multiplier. Check that all units are valid")
    }
  }
   
  return(if(attach.units) multiplier else v(multiplier))
}

#' Get a conversion factor to convert between flow units
#' 
#' @importFrom unitted separate_units unitbundle get_units u v
#' @export
#' @param old.units character. The current units of flow.
#' @param new.units character. The desired units of flow.
#' @param attach.units logical. If TRUE, the conversion factor is returned with 
#'   units attached.
#' @return numeric conversion factor, to be multiplied by values in old units to
#'   determine the values in new units.
#' @examples 
#' flowUnitsConversion(old.units='cfs', new.units='cms')
#' flowUnitsConversion(old.units='m^3 s^-1', new.units='ft^3 s^-1')
#' flowUnitsConversion(old.units='m^3 s^-1', new.units='ft^3 s^-1', attach.units=TRUE)
#' 
#' # use this multiplier to convert a vector
#' Q_cfs <- seq(10, 12, length.out=10) # example data
#' Q_cms <- Q_cfs * flowUnitsConversion(old.units='cfs', new.units='cms')
flowUnitsConversion <- function(old.units, new.units, attach.units=FALSE) {
  # Translate units - goes quickly if they're good already
  old.units <- translateFreeformToUnitted(old.units, TRUE)
  new.units <- translateFreeformToUnitted(new.units, TRUE)
  
  # Separate the units into numerator and denominator
  old_separated <- separate_units(old.units)
  old_numerstrs <- strsplit(get_units(unitbundle(old_separated[which(old_separated$Power > 0),])), " ")[[1]]
  old_denomstrs <- strsplit(get_units(1/unitbundle(old_separated[which(old_separated$Power < 0),])), " ")[[1]]
  new_separated <- separate_units(new.units)
  new_numerstrs <- strsplit(get_units(unitbundle(new_separated[which(new_separated$Power > 0),])), " ")[[1]]
  new_denomstrs <- strsplit(get_units(1/unitbundle(new_separated[which(new_separated$Power < 0),])), " ")[[1]]
  
  # Identify the volumetric pieces of the conversion from the units table
  numerator <- denominator <- "subset.var"
  toL <- subset(unit.conversions, denominator == old_numerstrs & numerator == 'L')
  fromL <- subset(unit.conversions, denominator == 'L' & numerator == new_numerstrs)
  if(nrow(toL) != 1 || nrow(fromL) != 1) {
    stop("couldn't find a path between the volume units")
  }
  volConv <- u(toL$value, toL$numerator) / u(1, toL$denominator) *
    u(fromL$value, fromL$numerator) / u(1, fromL$denominator)
  
  # Identify the time pieces of the conversion from the units table
  tod <- subset(unit.conversions, numerator == old_denomstrs & denominator == 'd')
  fromd <- subset(unit.conversions, numerator == 'd' & denominator == new_denomstrs)
  if(nrow(tod) != 1 || nrow(fromd) != 1) {
    stop("couldn't find a path between the time units")
  }
  timeConv <- u(tod$value, tod$numerator) / u(1, tod$denominator) *
    u(fromd$value, fromd$numerator) / u(1, fromd$denominator)

  # For now, just require that the old.units are cfs and the new ones are cms
  conv <- volConv*timeConv
  if(!attach.units) conv <- v(conv)
  return(conv)
}

#' observeSolute - instantaneous loads or concentrations
#' 
#' Calculates observed instantaneous loading rates or concentrations from 
#' observed concentrations, flows, and/or fluxes, with units conversions 
#' according to the supplied metadata.
#' 
#' @param data data.frame containing, at a minimum, the columns named by 
#'   metadata@@constituent and metadata@@flow
#' @param flux.or.conc character giving the desired output format
#' @param metadata An object of class "metadata" describing the units of flow 
#'   (flow.units) and concentration (conc.units) of the input data, and the 
#'   desired units of load (load.rate.units) for the output data
#' @param calculate logical. If FALSE, looks for a column containing the output 
#'   of interest. If true, uses the other two columns (out of those for conc, 
#'   flow, and flux) to calculate the output of interest.
#' @param attach.units logical. If TRUE, the converted observations are returned
#'   with units attached.
#' @export
#' @keywords units
#' @examples
#' obs <- data.frame(MyConc=(1:10)/10, MyFlow=rep(10,10), MyFlux=2) # intentionally inconsistent
#' # between conc*flow and flux
#' md <- updateMetadata(exampleMetadata(), constituent="MyConc", flow="MyFlow", 
#' load.rate="MyFlux", dates="none", flow.units="cms", conc.units="mg/l", 
#' load.units="g", load.rate.units="g/s", custom=NULL)
#'   
#' observeSolute(obs, "flux", md, attach.units=TRUE) # calculate flux from conc & flow
#' observeSolute(obs, "flux", md, calculate=FALSE, attach.units=TRUE) # read flux from data column
#' observeSolute(obs, "conc", md, calculate=TRUE, attach.units=TRUE) # calculate conc 
#' # from flow & flux
observeSolute <- function(
  data, flux.or.conc=c("flux","conc"), metadata, 
  calculate=isTRUE(flux.or.conc=="flux"), 
  attach.units=FALSE) {
  
  # Validate arguments
  flux.or.conc <- match.arg.loadflex(flux.or.conc)
  calculate <- match.arg.loadflex(calculate, c(TRUE, FALSE, NA))
  
  out <- switch(
    flux.or.conc,
    "flux"={
      if(is.na(calculate)) {
        # if calculate==NA, don't calculate unless there's no flux rate column in the data
        calculate <- FALSE
        tryCatch(getCol(metadata, data, "flux rate"), error=function(e) { calculate <<- TRUE })
      }
      if(calculate) {
        loads <- getCol(metadata, data, "conc") * getCol(metadata, data, "flow") * 
          flowconcToFluxConversion(getUnits(metadata, "flow"), getUnits(metadata, "conc"), getUnits(metadata, "flux rate"), TRUE)
      } else {
        loads <- getCol(metadata, data, "flux rate")
      }
      if(attach.units) {
        loads <- u(loads, getUnits(metadata, "flux rate"))
      }
      loads
    },
    "conc"={
      if(is.na(calculate)) {
        # if calculate==NA, don't calculate unless there's no conc column in the data
        calculate <- FALSE
        tryCatch(getCol(metadata, data, "conc"), error=function(e) { calculate <- TRUE })
      }
      if(calculate) {
        concs <- (getCol(metadata, data, "flux rate") / getCol(metadata, data, "flow")) / 
          flowconcToFluxConversion(getUnits(metadata, "flow"), getUnits(metadata, "conc"), getUnits(metadata, "flux rate"), TRUE)
      } else {
        concs <- getCol(metadata, data, "conc")
      }
      if(attach.units) {
        concs <- u(concs, getUnits(metadata, "conc"))
      }
      concs
    }
  )
  if(!attach.units) {
    return(v(out))
  } else {
    return(out)
  }
}

#' formatPreds raw to final predictions
#' 
#' Convert raw predictions to final predictions, possibly including a switch 
#' between flux and conc. If there is a switch, the units will be converted 
#' according to the metadata.
#' 
#' @param preds raw prediction values
#' @param from.format character in 
#'   \code{c("flux","conc*flow","flux/flow","conc")}. Format of the raw 
#'   predictions.
#' @param to.format character indicating whether the returned value should be a 
#'   flux or a concentration.
#' @param newdata a data.frame with nrow() == length(preds) and containing any 
#'   columns (named as in \code{metadata}) that will be needed to perform the
#'   requested conversion. For example, from="conc" and to="flux" implies that
#'   a discharge column will be available in \code{newdata}.
#' @param metadata An object of class \code{\link{metadata}} used to determine 
#'   the units of inputs and desired output
#' @param lin.or.log character. Either "linear" or "log" to say whether the predictions
#'   should be converted to log space or not. If converted to log space, a bias correction
#'   will be applied, see \code{\link{linToLog}}.
#' @param attach.units logical. Attach the units to the returned value?
#' @return converted predictions (in the format/units specified by to.format and
#'   metadata)
#' @export
#' @keywords units
#' @examples
#' obs <- transform(data.frame(MyConc=1:10, MyFlow=rep(10,10)), 
#' MyFlux=MyConc*MyFlow*rloadest::loadConvFactor("cms", "mg/l", "kg") )
#' md <- updateMetadata(exampleMetadata(), constituent="MyConc", flow="MyFlow", 
#' load.rate="MyFlux", dates="none", flow.units="cms", conc.units="mg/l", load.units="kg", 
#' load.rate.units="kg/d", custom=NULL)
#'   
#' formatPreds(preds=obs$MyConc, from.format="conc", to.format="flux", newdata=obs, 
#' metadata=md) # == obs$MyFlux
#' formatPreds(preds=obs$MyConc*obs$MyFlow, from.format="conc*flow", to.format="flux", newdata=obs, 
#' metadata=md) # == obs$MyFlux
#' formatPreds(preds=obs$MyFlux, from.format="flux", to.format="conc", newdata=obs, 
#' metadata=md) # == obs$MyConc
#' formatPreds(preds=obs$MyFlux, from.format="flux", to.format="conc", newdata=obs, metadata=md, 
#' attach.units=TRUE) # == u(obs$MyConc, "mg L^-1")
formatPreds <- function(preds, 
                        from.format=c("flux","conc*flow","flux/flow","conc"), 
                        to.format=c("flux","conc"), 
                        newdata, metadata, lin.or.log=c("linear","log"), attach.units=FALSE) {
  
  # Error checking for formats, with case flexibility
  from.format <- match.arg.loadflex(from.format, c("flux","conc*flow","flux/flow","conc"))
  to.format <- match.arg.loadflex(to.format, c("flux","conc"))
  lin.or.log <- match.arg.loadflex(lin.or.log)
  
  if(lin.or.log=='log') {
    if(from.format != to.format) {
      stop("formatPreds cannot currently handle flux-conc conversions in log space.")
    }
    
    # Placeholder for when formatPred conc-flux conversions in log space are allowed.
  }

  # Do the conversion. Use units within flowconcToFluxConversion but not here, to save time.
  preds <-
    if(to.format=="flux") {
      switch(
        from.format,
        "flux"=preds,
        "conc*flow"=preds * flowconcToFluxConversion(getUnits(metadata, "flow"), getUnits(metadata, "conc"), getUnits(metadata, "flux rate"), FALSE),
        "flux/flow"=preds * getCol(metadata, newdata, "flow"),
        "conc"=preds * getCol(metadata, newdata, "flow") * flowconcToFluxConversion(getUnits(metadata, "flow"), getUnits(metadata, "conc"), getUnits(metadata, "flux rate"), FALSE)
      )
    } else { #to.format=="conc"
      switch(
        from.format,
        "flux"=(preds / getCol(metadata, newdata, "flow")) / flowconcToFluxConversion(getUnits(metadata, "flow"), getUnits(metadata, "conc"), getUnits(metadata, "flux rate"), FALSE),
        "conc*flow"=preds / getCol(metadata, newdata, "flow"),
        "flux/flow"=preds / flowconcToFluxConversion(getUnits(metadata, "flow"), getUnits(metadata, "conc"), getUnits(metadata, "flux rate"), FALSE),
        "conc"=preds
      )
    }
  
  # Attach units to predictions if requested
  if(attach.units) {
    preds <- u(preds, switch(
      to.format,
      "flux"=getUnits(metadata, "flux rate"), # this is a little strange now - maybe we should switch flux.or.conc to take c("flux rate","conc") everywhere
      "conc"=getUnits(metadata, "conc")
    ))
  }
  
  preds
}
