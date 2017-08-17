#### Data Documentation ####

#' @name units_loadflex
#' @aliases valid.metadata.units freeform.unit.translations unit.conversions
#' @title Units-related datasets for the \pkg{loadflex} package
#' @description These datasets are provided for managing units within the 
#'   \pkg{loadflex} package. It is recommended NOT to use these datasets 
#'   directly within your code, as they may change structure or contents in the 
#'   future.
#' @section Datasets:
#'   
#'   \describe{
#'   
#'   \item{\code{valid.metadata.units}}{A data.frame with character columns 
#'   "unit", "standard", and "dimension". Used (1) to determine whether units in
#'   slots of a `metadata` object have the appropriate dimensionality; e.g., 
#'   \code{conc.units} must have dimensions of mass or count per volume, and (2)
#'   to convert among units of the same dimensionality.}
#'   
#'   \item{\code{freeform.unit.translations}}{A data.frame with character 
#'   columns "new" and "old". Used for mapping freeform units ("old") to the 
#'   specific format required by \pkg{loadflex} ("new").}
#'   
#'   \item{\code{unit.conversions}}{A data.frame with character columns 
#'   "numerator", "denominator", and numeric column "value", one conversion per 
#'   row. Used for converting among }
#'   
#'   }
#' @docType data
NULL

#### Data ####

#' @rdname loadflex-deprecated-internal
#' @details `generateUnitsData` (defunct as of v1.2.0) has been replaced by the
#'   script at data-raw/createUnitsData.R
#' @md
#' @importFrom stats setNames
#' @import dplyr
#' @keywords internal
generateUnitsData <- function() {
  stop('Defunct. To regenerate units data, run the script in the GitHub repo at data-raw/createUnitsData.R')
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
#' validMetadataUnits("m^3 s^-1", unit.type="ANY") # TRUE
#' validMetadataUnits("nonsensical") # FALSE
#' validMetadataUnits("g", unit.type="load.units") # TRUE
#' validMetadataUnits("g", unit.type="flow.units") # FALSE
validMetadataUnits <- function(unitstr, unit.type=c("ANY","flow.units","conc.units","load.units","load.rate.units","basin.area.units")) {
  unit.type <- match.arg(unit.type)

  # Parse the unit string into numerator and denominator strings
  unitpieces <- separate_units(unitbundle(unitstr))
  numerator <- get_units(unitbundle(unitpieces[which(unitpieces$Power > 0),]))
  denominator <- get_units(1/unitbundle(unitpieces[which(unitpieces$Power < 0),]))

  # The numerator (and denominator if it exists) should have specific dimensions
  # for each units type. Check.
  switch(
    unit.type,
    "ANY" = !is.na(unitType(unitstr)) && validMetadataUnits(unitstr, unitType(unitstr)),
    flow.units = validDim(numerator, "volume") & validDim(denominator, "time"),
    conc.units = validDim(numerator, c("mass","count")) & validDim(denominator, "volume"),
    load.units = validDim(numerator, c("mass","count")) & denominator=="",
    load.rate.units = validDim(numerator, c("mass","count")) & validDim(denominator, c("time")),
    basin.area.units = validDim(numerator, c("area")) & denominator==""
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
  if('ANY' %in% dim.type) dim.type <- setdiff(eval(formals(validDim)$dim.type), 'ANY')
  
  unit <- '.subset.var'
  dim_row <- subset(valid.metadata.units, unit==dimstr)
  if(nrow(dim_row) != 1 || !(dim_row$dimension %in% dim.type)) {
    return(FALSE)
  } else {
    return(TRUE)
  }
}

#' Return a data.frame describing each dimension of the units (1 row per dimension)
#' 
#' @param unitstr A single string representing one set of units dimension
#' @importFrom unitted unitbundle separate_units
#' @examples 
#' loadflex:::dimInfo('kg') # 'mg'
#' loadflex:::dimInfo('ha') # 'km^2'
#' loadflex:::dimInfo('kg d^-1') # NA
#' loadflex:::dimInfo('m^3') # NA
#' loadflex:::dimInfo('kk') # NA
dimInfo <- function(unitstr) {
  Unit <- Power <- dimension <- standard <- '.dplyr.var'
  unitbundle(unitstr) %>%
    separate_units() %>%
    mutate(
      Str = mapply(function(u, p) {
        get_units(unitbundle(data.frame(Unit=u, Power=abs(p))))
      }, Unit, Power),
      Pos = ifelse(Power > 0, 'numerator', 'denominator')) %>%
    left_join(rename(valid.metadata.units, Dim=dimension, Std=standard), by=c("Str"="unit"))
}

#' Produce conversion factor to multiply old.units by to get new.units
#' 
#' Converts units; units can be arbitrarily complex as long as every dimension 
#' of each unit is present in unit.conversions
#' 
#' @param old.units character. The units to convert from.
#' @param new.units character. The units to convert to.
#' @param attach.units logical. Should units be attached to the conversion 
#'   factor?
#' @return a conversion factor that can be multiplied with data in the old.units
#'   to achieve data in the new.units
#' @importFrom unitted separate_units get_units unitbundle u
#' @importFrom methods is
#' @import dplyr
#' @examples 
#' loadflex:::convertUnits('mg/L', 'kg/m^3')
#' loadflex:::convertUnits('kg d^-1', 'kg/yr')
#' loadflex:::convertUnits('kg/yr', 'kg/d', attach.units = TRUE)
#' \dontrun{
#' loadflex:::convertUnits('mg/L', 'm^3/d') # error: dimensions must match
#' loadflex:::convertUnits(unitbundle('ft^3 g L^-1 s^-1'), 'kg/d') # error: too complicated
#' }
convertUnits <- function(old.units, new.units, attach.units = FALSE) {
  # Translate units - goes quickly if they're good already
  if(!is(old.units, 'unitbundle')) old.units <- translateFreeformToUnitted(old.units, attach.units = TRUE)
  if(!is(new.units, 'unitbundle')) new.units <- translateFreeformToUnitted(new.units, attach.units = TRUE)
  
  # split the units into pieces and identify the dimension, etc. of each piece
  old.info <- dimInfo(old.units)
  new.info <- dimInfo(new.units)
  
  # combine if possible
  Dim <- Pos <- '.dplyr.var'
  old.dimunits <- unitbundle(transmute(old.info, Unit=Dim, Power=ifelse(Pos=='numerator', 1, -1)))
  new.dimunits <- unitbundle(transmute(new.info, Unit=Dim, Power=ifelse(Pos=='numerator', 1, -1)))
  if(old.dimunits != new.dimunits) {
    stop("dimensions must match between old.units (", get_units(old.dimunits), ") and new.units (", get_units(new.dimunits), ")")
  }
  conv.info <- full_join(old.info, new.info, suffix=c('.old','.new'), by=c('Pos','Dim','Std'))
  if(any(is.na(conv.info$Unit.old))  || any(is.na(conv.info$Unit.new)) || 
     nrow(old.info) != nrow(conv.info) || nrow(new.info) != nrow(conv.info)) {
    # e.g., convertUnits(unitbundle('ft^3 g L^-1 s^-1'), 'kg/d')
    stop("despite equal dimensions, still failed to match old and new units")
  }
  
  # find the conversion from Str.old. into Str.std
  numerator <- denominator <- '.dplyr.var'
  conv.info$old2std <- mapply(
    function(num, den) { filter(unit.conversions, numerator==num, denominator==den)$value },
    num=conv.info$Std, den=conv.info$Str.old)
  conv.info$std2new <- mapply(
    function(num, den) { filter(unit.conversions, numerator==num, denominator==den)$value },
    num=conv.info$Str.new, den=conv.info$Std)
  conv.info$Value <- conv.info$old2std * conv.info$std2new
  conv.info$Unit <- mapply(function(n, o) unitbundle(n)/unitbundle(o), n=conv.info$Str.new, o=conv.info$Str.old, USE.NAMES=FALSE)
  # conv.info is now unprintable because Unit is S4, but you can print cols 1:12
  # and use all the cols in the calcs that follow
  
  # combine into a single conversion factor and unitbundle
  Conv <- u(1, NA)
  for(i in seq_len(nrow(conv.info))) {
    iConv <- u(conv.info$Value[[i]], conv.info$Unit[[i]])
    if(conv.info$Pos[[i]] == 'numerator') {
      Conv <- Conv * iConv
    } else {
      Conv <- Conv / iConv
    }
  }
  # confirm the direction/completeness of our conversion factor: Unit*old.units
  # should get us new.units
  stopifnot(old.units*unitbundle(get_units(Conv)) == new.units)
  
  return(if(attach.units) Conv else v(Conv))
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
#' @importFrom unitted separate_units get_units unitbundle u v
#' @export
#' @param flow.units character. The units of flow.
#' @param conc.units character. The units of concentration.
#' @param load.rate.units character. The units of flux.
#' @param attach.units logical. If TRUE, the conversion factor is returned with
#'   units attached.
#' @return numeric, or unitted numeric if unitted=TRUE. The conversion factor.
#' @examples
#' flowconcToFluxConversion("cfs", "g/L", "kg/d") # 2446.589
#' library(unitted); u(10, "ft^3 s^-1") * u(2, "mg L^-1") * 
#'   flowconcToFluxConversion("cfs", "mg/L", "kg/d", attach.units=TRUE) # u(48.9 ,"kg d^-1")
flowconcToFluxConversion <- function(flow.units, conc.units, load.rate.units, attach.units=FALSE) {
  
  # Translate units - goes quickly if they're good already
  flow.units <- translateFreeformToUnitted(flow.units, TRUE)
  conc.units <- translateFreeformToUnitted(conc.units, TRUE)
  load.rate.units <- translateFreeformToUnitted(load.rate.units, TRUE)
  
  # Get the conversion that turns flow*conc into a simple mass/time unit
  Dim <- . <- '.dplyr.var'
  vol.flow <- dimInfo(flow.units) %>% filter(Dim=='volume') %>% .$Str
  vol.conc <- dimInfo(conc.units) %>% filter(Dim=='volume') %>% .$Str
  conv.vol.flow2conc <- convertUnits(vol.flow, vol.conc, attach.units = TRUE)
  
  # Get the conversion that turns the simple mass/time unit into load.rate.units
  conv.masstime <- convertUnits(
    get_units(flow.units*unitbundle(get_units(conv.vol.flow2conc))*conc.units), 
    get_units(load.rate.units), 
    attach.units=TRUE)
  
  # The final conversion is the product of the two preceding conversion factors
  conv.full <- conv.vol.flow2conc * conv.masstime
  return(if(attach.units) conv.full else v(conv.full))
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
#' data(eg_metadata)
#' obs <- data.frame(MyConc=(1:10)/10, MyFlow=rep(10,10), MyFlux=2) # intentionally inconsistent
#' md <- updateMetadata(eg_metadata, constituent="MyConc", flow="MyFlow", 
#'   load.rate="MyFlux", dates="none", flow.units="cms", conc.units="mg/l", 
#'   load.units="g", load.rate.units="g/s", custom=NULL)
#' 
#' observeSolute(obs, "flux", md, attach.units=TRUE) # calculate from conc & flow
#' observeSolute(obs, "flux", md, calculate=FALSE, attach.units=TRUE) # read flux from data
#' observeSolute(obs, "conc", md, calculate=TRUE, attach.units=TRUE) # calculate from flow & flux
#' observeSolute(obs, "conc", md, calculate=FALSE, attach.units=TRUE) # read conc from data
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
#'   MyFlux=MyConc*MyFlow*rloadest::loadConvFactor("cms", "mg/l", "kg") )
#' md <- metadata(constituent="MyConc", flow="MyFlow", 
#'   load.rate="MyFlux", dates="none", flow.units="m^3 s^-1", conc.units="mg L^-1",
#'   load.units="kg", load.rate.units="kg/d", site.name='My River', custom=NULL)
#'   
#' all.equal(obs$MyFlux, formatPreds(
#'   preds=obs$MyConc, from.format="conc", to.format="flux", newdata=obs, metadata=md))
#' \dontrun{
#' # these examples take too long to build regularly
#' all.equal(obs$MyFlux, formatPreds(
#'   preds=obs$MyConc*obs$MyFlow, from.format="conc*flow", to.format="flux",
#'     newdata=obs, metadata=md))
#' all.equal(obs$MyConc, formatPreds(
#'   preds=obs$MyFlux, from.format="flux", to.format="conc", newdata=obs, metadata=md))
#' all.equal(unitted::u(obs$MyConc, "mg L^-1"), formatPreds(
#'   preds=obs$MyFlux, from.format="flux", to.format="conc", newdata=obs, metadata=md, 
#'   attach.units=TRUE))
#' }
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
