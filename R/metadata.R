#' Store metadata relevant to a load model.
#' 
#' \code{metadata} objects store metadata for a load model of any type (e.g., 
#' \code{\link{loadModel}}, \code{\link{loadInterp}}, \code{\link{loadReg2}}, or
#' \code{\link{loadComp}}). Metadata can be created and modified by calls to 
#' \code{\link{metadata}}, \code{\link{updateMetadata}}, or 
#' \code{\link{exampleMetadata}}. Values and relevant information can be 
#' accessed by \code{\link{getCol}}, \code{\link{getUnits}}, or 
#' \code{\link{getInfo}}.
#' 
#' @rdname metadata-class
#' @name metadata-class
#' @slot constituent character. The name of the data.frame column describing the
#'   concentration of the solute or material to be modeled.
#' @slot consti.name character. Consituent long name, for use in plots and 
#'   reports
#' @slot flow character. The name of the data.frame column describing flow 
#'   (discharge)
#' @slot load.rate character. The name of the data.frame column, if it exists, 
#'   describing instantaneous load
#' @slot dates character. The name of the data.frame column describing date 
#'   and/or time
#' @slot flow.units character. The units in which flow is both input and 
#'   exported.
#' @slot conc.units character. The units in which concentration is both input 
#'   and exported.
#' @slot load.units character. The units in which load (flux) is both input and 
#'   exported.
#' @slot load.rate.units character. The units in which load (flux) is reported 
#'   by predict.loadModel().
#' @slot station deprecated. character. A description of the sampling station or
#'   site. It's now recommended to use \code{site.name} and/or \code{site.id} 
#'   instead.
#' @slot site.name character. A description of the station or site where the 
#'   concentration of the constituent was measured. This slot or \code{site.id} 
#'   is a better place for a site identifier than the \code{station} slot, which
#'   is deprecated.
#' @slot site.id character Station ID for the site where the concentration of 
#'   the constituent was measured.
#' @slot lat numeric Station latitude where concentration (and possibly also 
#'   discharge) was measured.
#' @slot lon numeric Station longitude where concentration (and possibly also 
#'   discharge) was measured.
#' @slot basin.area numeric Area of the drainage basin contributing water to the
#'   site where concentrations were measured.
#' @slot flow.site.name character Long name of the station where flow was 
#'   monitored, if different from where concentration was monitored (as in 
#'   \code{site.name}).
#' @slot flow.site.id character Station ID of the station where flow was 
#'   monitored, if different from where concentration was monitored (as in 
#'   \code{site.id}).
#' @slot flow.lat numeric Latitude of the station where flow was monitored, if 
#'   different from where concentration was monitored (as in \code{site.lat}).
#' @slot flow.lon numeric Longitude of the station where flow was monitored, if 
#'   different from where concentration was monitored (as in \code{site.lon}).
#' @slot flow.basin.area numeric Area of the drainage basin contributing water 
#'   to the site where flow was monitored, if different from where concentration
#'   was monitored (as in \code{basin.area}).
#' @slot basin.area.units character Units of the values in \code{basin.area} and
#'   \code{flow.basin.area} (must be the same for both). Basin areas are used to
#'   compute yields as loads per contributing (basin) area.
#' @slot custom ANY. Empty by default, but may be modified to store any 
#'   additional data the user wants to track.
#' @importFrom methods setClass
#' @exportClass metadata
setClass(
  "metadata",
  slots=c(
    # Column names (except consti.name)
    constituent="character",
    consti.name="character",
    flow="character",
    load.rate="character",
    dates="character",
    # Units
    conc.units="character",
    flow.units="character",
    load.units="character",
    load.rate.units="character",
    # Site info
    station="character", # deprecated in favor of site.name, site.id
    site.name="character",
    site.id="character",
    lat="numeric",
    lon="numeric",
    basin.area="numeric",
    flow.site.name="character",
    flow.site.id="character",
    flow.lat="numeric",
    flow.lon="numeric",
    flow.basin.area="numeric",
    basin.area.units="character",
    # Other
    custom="ANY"),
  
  prototype=list(
    constituent="",
    consti.name="",
    flow="",
    load.rate="",
    dates="",
    conc.units="",
    flow.units="",
    load.units="",
    load.rate.units="",
    # Site info
    station="", # deprecated in favor of site.name, site.id
    site.name="",
    site.id="",
    lat=as.numeric(NA),
    lon=as.numeric(NA),
    basin.area=as.numeric(NA),
    flow.site.name="",
    flow.site.id="",
    flow.lat=as.numeric(NA),
    flow.lon=as.numeric(NA),
    flow.basin.area=as.numeric(NA),
    basin.area.units="km^2", # need default units for backwards compatibility
    # Other
    custom=NULL),
  
  # from the setClass documentation: "a validity-checking method for objects
  # from this class (a function that returns TRUE if its argument is a valid
  # object of this class and one or more strings describing the failures
  # otherwise)"
  validity=function(object) {
    errorstrs <- character()
    
    # Require key elements to be non-empty strings
    non_empty_elements <- c("constituent", "flow", "dates")
    for(elem in non_empty_elements) {
      if(slot(object, elem) == "") {
        errorstrs <- c(errorstrs, paste(elem, "must be a non-empty string"))
      }
    }
    
    # Check units against lists defined in unit.conversions.R
    if(!validMetadataUnits(object@flow.units, "flow.units")) {
      errorstrs <- c(errorstrs, "flow.units are invalid. See 'Valid units options' in ?metadata")
    }
    if(!validMetadataUnits(object@conc.units, "conc.units")) {
      errorstrs <- c(errorstrs, "conc.units are invalid. See 'Valid units options' in ?metadata")
    }
    if(!validMetadataUnits(object@load.units, "load.units")) {
      errorstrs <- c(errorstrs, "load.units are invalid. See 'Valid units options' in ?metadata")
    }
    if(!validMetadataUnits(object@load.rate.units, "load.rate.units")) {
      errorstrs <- c(errorstrs, "load.rate.units are invalid. See 'Valid units options' in ?metadata")
    }
    if(!validMetadataUnits(object@basin.area.units, "basin.area.units")) {
      errorstrs <- c(errorstrs, "basin.area.units are invalid. See 'Valid units options' in ?metadata")
    }
    
    if(length(errorstrs) == 0) {
      return(TRUE)
    } else {
      return(errorstrs)
    }
  }
)



#### Creation and modification ####

#' Create or modify the metadata for a load model.
#' 
#' \code{metadata} creates a new metadata object (see \link{metadata-class}) 
#' from scratch.
#' 
#' By default, newly created metadata objects are checked for validity. This 
#' check may be bypassed by setting \code{validate=FALSE}. However, except where
#' the user is only interested in using metadata objects in isolation (i.e., 
#' outside a load model), this option will rarely be appropriate.
#' 
#' \subsection{Valid units options, \pkg{loadflex} style}{
#' 
#' Models used in \pkg{\link{loadflex}} require metadata with units in a 
#' standardized form. For any model not produced using \pkg{rloadest} functions,
#' this standardized format is a space-separated list of text units with 
#' exponents denoted with "^". Valid examples include "mg L^-1", "ft^3 s^-1", 
#' "kg", and "colonies d^-1". Here is the full set of possibilites, separated by
#' units type:
#' 
#' \code{flow.units} take the form VOLUME TIME^-1.
#' 
#' \itemize{
#' 
#' \item VOLUME: m^3, ft^3, dL, L
#' 
#' \item TIME: s, d, y
#' 
#' }
#' 
#' \code{conc.units} take the form MASS/VOLUME or COUNT/VOLUME.
#' 
#' \itemize{
#' 
#' \item MASS: ng, ug, mg, g, kg, Mg, lb, ton
#' 
#' \item COUNT: colonies, million_colonies
#' 
#' \item VOLUME: m^3, ft^3, dL, L
#' 
#' }
#' 
#' \code{load.units} take the form MASS or COUNT.
#' 
#' \itemize{
#' 
#' \item MASS: ng, ug, mg, g, kg, Mg, lb, ton
#' 
#' \item COUNT: colonies, million_colonies
#' 
#' }
#' 
#' \code{load.rate.units} take the form MASS/TIME or COUNT/TIME.
#' 
#' \itemize{
#' 
#' \item MASS: ng, ug, mg, g, kg, Mg, lb, ton
#' 
#' \item COUNT: colonies, million_colonies
#' 
#' \item TIME: s, d, y
#' 
#' }
#' 
#' #' \code{basin.area.units} take the form AREA.
#' 
#' \itemize{
#' 
#' \item AREA: m^2, ha, km^2, ft^2, ac, mi^2
#' 
#' }
#' 
#' For compatibility with \pkg{rloadest} models, \code{metadata} and 
#' \code{updateMetadata} also accept all those units accepted by \pkg{rloadest} 
#' (see next section).
#' 
#' }
#' 
#' \subsection{Valid units options, \pkg{rloadest} style}{
#' 
#' Units passed to \pkg{rloadest} take the form "X/Y" or "X per Y" or an 
#' abbreviation and accept a limited number of numerators and denominators. Any 
#' valid \pkg{rloadest} units will be translated to valid \pkg{\link{loadflex}} 
#' units when supplied to \code{\link{loadReg2}}, \code{metadata} or 
#' \code{updateMetadata}. These rloadest-style units are currently accepted:
#' 
#' \code{flow.units} take the form VOLUME/TIME, VOLUME per TIME, or ABV.
#' 
#' \itemize{
#' 
#' \item VOLUME: cubic meter, cubic meters, m^3, cubic foot, cubic feet, ft^3, 
#' 100mL, dL, liter, l, L
#' 
#' \item TIME: second, sec, day, d, year, yr, y
#' 
#' \item ABV: cms, cfs
#' 
#' }
#' 
#' \code{conc.units} take the form MASS/VOLUME or COUNT/VOLUME.
#' 
#' \itemize{
#' 
#' \item MASS: nanograms, micrograms, milligrams, grams, kilograms, metric tons,
#' ng, ug, mg, g, kg, Mg, tons, pounds, lbs, lb
#' 
#' \item COUNT: col, colonies, million colonies
#' 
#' \item VOLUME: cubic meter, cubic meters, m^3, cubic foot, cubic feet, ft^3, 
#' 100mL, dL, liter, l, L
#' 
#' }
#' 
#' \code{load.units} take the form MASS or COUNT.
#' 
#' \itemize{
#' 
#' \item MASS: nanograms, micrograms, milligrams, grams, kilograms, metric tons,
#' ng, ug, mg, g, kg, Mg, tons, pounds, lbs, lb
#' 
#' \item COUNT: col, colonies, million colonies
#' 
#' }
#' 
#' \code{load.rate.units} are derived as \code{load.units} per day.
#' 
#' }
#' 
#' @rdname metadata
#' @importFrom methods new
#' @param constituent character. The name of the solute or suspended material 
#'   whose load is to be modeled. Also the name of the data.frame column 
#'   describing that solute or material.
#' @param flow character. The name of the data.frame column describing flow 
#'   (discharge)
#' @param load.rate character. The name of the data.frame column, if it exists, 
#'   describing instantaneous load
#' @param dates character. The name of the data.frame column describing date 
#'   and/or time
#' @param flow.units character. The units in which flow is both input and 
#'   exported.
#' @param conc.units character. The units in which concentration is both input 
#'   and exported.
#' @param load.units character. The units in which load (flux) is both input and
#'   exported.
#' @param load.rate.units character. The units in which load (flux) is reported 
#'   by predict.loadModel().
#' @param station deprecated. character. A description of the sampling station 
#'   or site. It's now recommended to use \code{site.name} and/or \code{site.id}
#'   instead.
#' @param custom ANY. Empty by default, but may be modified to store any 
#'   additional data the user wants to track.
#' @param validate logical. If TRUE, validObject() must pass for the object to 
#'   return.
#' @param consti.name character. Consituent long name, for use in plots and 
#'   reports
#' @param site.name character. A description of the station or site where the 
#'   concentration of the constituent was measured. This argument or
#'   \code{site.id} is a better place for a site identifier than the
#'   \code{station} argument, which is deprecated.
#' @param site.id character Station ID for the site where the concentration of 
#'   the constituent was measured.
#' @param lat numeric Station latitude where concentration (and possibly also 
#'   discharge) was measured.
#' @param lon numeric Station longitude where concentration (and possibly also 
#'   discharge) was measured.
#' @param basin.area numeric Area of the drainage basin contributing water to 
#'   the site where concentrations were measured.
#' @param flow.site.name character Long name of the station where flow was 
#'   monitored, if different from where concentration was monitored (as in 
#'   \code{site.name}).
#' @param flow.site.id character Station ID of the station where flow was 
#'   monitored, if different from where concentration was monitored (as in 
#'   \code{site.id}).
#' @param flow.lat numeric Latitude of the station where flow was monitored, if 
#'   different from where concentration was monitored (as in \code{site.lat}).
#' @param flow.lon numeric Longitude of the station where flow was monitored, if
#'   different from where concentration was monitored (as in \code{site.lon}).
#' @param flow.basin.area numeric Area of the drainage basin contributing water 
#'   to the site where flow was monitored, if different from where concentration
#'   was monitored (as in \code{basin.area}).
#' @param basin.area.units character Units of the values in \code{basin.area} 
#'   and \code{flow.basin.area} (must be the same for both). Basin areas are 
#'   used to compute yields as loads per contributing (basin) area.
#' @return \code{metadata} returns a new metadata object with the specified 
#'   entries.
#' @export
#' @family metadata
#' @examples 
#' md <- metadata(constituent="NO3", flow="DISCHARGE", 
#'   dates="DATE", conc.units="mg L^-1", flow.units="cfs", load.units="kg", 
#'   load.rate.units="kg d^-1", site.name="Lamprey River, NH",
#'   site.id="1073500", custom=list(data_source="USGS NWIS, waterdata.usgs.gov"))
metadata <- function(constituent, flow, load.rate="", dates, 
                     conc.units, flow.units, load.units, load.rate.units, 
                     station="", # station is deprecated; use site.name and/or site.id
                     custom=NULL, validate=TRUE, 
                     consti.name="",
                     site.name="", site.id="", lat=NA, lon=NA, basin.area=NA,
                     flow.site.name=site.name, flow.site.id=site.id, flow.lat=lat, flow.lon=lon, flow.basin.area=basin.area,
                     basin.area.units="km^2") {
  
  # Create a list of non-missing args
  all_possible <- names(formals(metadata))
  not_missing <- setdiff(names(as.list(match.call())[-1]), 'validate') # the arguments besides 'validate' that were given explicitly
  slotvals <- lapply(setNames(nm=not_missing), function(val) get(val, envir=as.environment(-1)))
  
  # Create the object by updating a prototype object. Do it this way rather than
  # creating a new metadata object because updateMetadata does a bunch of
  # checking and most importantly does the translation of "freeform" units,
  # e.g., "mg/L" or "cfs", into unitted-form units, e.g., "mg L^-1" and "ft^3
  # s^-1"
  metadata <- do.call(updateMetadata, c(list(new("metadata")), slotvals, list(validate=validate)))
  
  return(metadata)
}


#' \code{updateMetadata} modifies an existing metadata object.
#' 
#' @rdname metadata
#' @importFrom methods is getSlots slot slot<- validObject
#' @param metadata the metadata object to update. 
#' @param new.metadata Optional object of class "metadata". If new.metadata is 
#'   not NA, all elements of \code{...} will be ignored and metadata will be updated 
#'   with any non-empty elements of new.metadata.
#' @param ... named arguments describing metadata elements to be modified; the 
#'   names may be any of those accepted by \code{metadata()}.
#' @return \code{updateMetadata} returns a new metadata object that combines the
#'   values in the initial \code{metadata} with EITHER the non-empty elements of
#'   \code{new.metadata} OR the elements specified in \code{...}, depending on
#'   whether \code{new.metadata} is provided.
#' @export
#' @examples
#' md1 <- metadata(constituent="NO3", flow="DISCHARGE", 
#'   dates="DATE", conc.units="mg L^-1", flow.units="cfs", load.units="kg", 
#'   load.rate.units="kg d^-1", site.name="Lamprey River, NH")
#' md2 <- updateMetadata(md1, site.id="1073500", 
#'   custom=list(data_source="USGS NWIS, waterdata.usgs.gov"))
updateMetadata <- function(metadata, new.metadata=NA, ..., validate=TRUE) {
  if(!is(metadata, "metadata")) {
    stop("metadata must be of class 'metadata'")
  }
  
  slotNames <- names(getSlots("metadata"))
  if(!is.na(new.metadata)) {
    # Amend the existing metadata as specified in new.metadata
    if(!is(new.metadata, "metadata")) {
      stop("new.metadata must be of class 'metadata'")
    }
    for(sn in slotNames[getSlots("metadata")=="character"]) {
      if(slot(new.metadata, sn) != "") {
        slot(metadata, sn) <- slot(new.metadata, sn)
      }
    }
    for(sn in slotNames[getSlots("metadata")=="ANY"]) {
      if(!is.null(slot(new.metadata, sn))) {
        slot(metadata, sn) <- slot(new.metadata, sn)
      }
    }
    
  } else {  
    # Amend the existing metadata as specified in ...
    metaargs <- list(...)
    for(i in seq_len(length(metaargs))) {
      if(names(metaargs[i]) %in% c("conc.units","flow.units","load.units","load.rate.units","basin.area.units")) {
        # Make sure units are in an accepted form
        slot(metadata, names(metaargs[i])) <- translateFreeformToUnitted(metaargs[[i]], attach.units=FALSE)
      } else if(names(metaargs[i]) %in% slotNames) {
        if(names(metaargs[i]) == 'station') {
          message("'station' has been deprecated; 'site.name' and 'site.id' are recommended instead")
        } # if station is provided, give the above message, but then proceed anyway
        slot(metadata, names(metaargs[i])) <- metaargs[[i]]
      } else {
        stop("unrecognized metadata element: ",names(metaargs[i]))
      }
    }
  }
  
  # Check to make sure the object is still valid
  if(validate) {
    validObject(metadata)
  }
  
  return(metadata)
}

#' \code{exampleMetadata} produces an example metadata object, useful in testing
#' or to see how a complete metadata object looks.
#' 
#' @rdname metadata
#' @return \code{exampleMetadata} returns a metadata object with example entries.
#' @export
#' @examples
#' exampleMetadata()
exampleMetadata <- function() {
  metadata("NO3","Q","NO3_FLUX","DATE","mg/L","cms","kg","kg/day",
           site.name="Lamprey River @ Wiswall Dam, Durham, New Hampshire",
           custom=list(a="anything you want goes here", b=1:10))  
}


#### Use metadata to access data ####

#' Access information about a load model.
#' 
#' \code{getCol} locates a column of data specified by a field name (conc, flow,
#' flux rate, or date).
#' 
#' These metadata-getters provide a weak wall of abstraction between metadata 
#' and clients of the metadata; more importantly, they do the error checking to 
#' make sure that the designated field or data column exists and return an 
#' informative error if it does not.
#' 
#' @rdname metadata-getters
#' @name metadata-getters
#' @importFrom methods slot
#' @param metadata a metadata object from which the information will be 
#'   retrieved
#' @param data a data.frame from which the data column will be retrieved
#' @param field character string identifying the field to extract. Partial 
#'   matching is allowed.
#' @param attach.units logical. If TRUE, the return value will be a
#'   \pkg{\link{unitted}} vector.
#' @return \code{getCol} returns the specified column of data as a vector.
#' @export
#' @family metadata
#' @examples 
#' md <- metadata(constituent="NO3", flow="DISCHARGE", 
#'   dates="DATE", conc.units="mg L^-1", flow.units="cfs", load.units="kg", 
#'   load.rate.units="kg d^-1", site.name="Lamprey River, NH")
#' data(lamprey_nitrate)
#' head(getCol(md, lamprey_nitrate, 'date'))
#' head(getCol(md, lamprey_nitrate, 'conc'))
getCol <- function(metadata, data, field=c("conc", "flow", "flux rate", "date"), attach.units=FALSE) {
  
  # Standardize the field input; match.arg allows partial matching for lazy typers
  if(field == "flux") {
    warning("you specified field=='flux', but the nearest metadata match is 'flux rate'")
  }
  field <- match.arg.loadflex(field, c("conc", "flow", "flux rate", "date"))
  
  # Identify the corresponding slot and look up the column name in the metadata
  slotname <- switch(
    field,
    "conc"="constituent",
    "flow"="flow",
    "flux rate"="load.rate",
    "date"="dates")
  colname <- slot(metadata, slotname)
  
  # Check that the column name refers to an existing column in the data
  if(!(colname %in% names(data))) {
    stop("data does not contain the expected ",field," column, '",colname,"'")
  }
  colvals <- data[[colname]]
  
  if(attach.units) {
    units <- switch(
      field,
      "conc"=slot(metadata, "conc.units"),
      "flow"=slot(metadata, "flow.units"),
      "flux rate"=slot(metadata, "load.rate.units"),
      "date"="")
    colvals <- u(colvals, units)
  }
  
  # Return the specified data column
  colvals
}

#' Return the units from a metadata object.
#' 
#' \code{getUnits} finds the units associated with a data type (conc, flow, 
#' flux, or flux rate).
#' 
#' @rdname metadata-getters
#' @param format character. The format in which units should be returned. Set to
#'   'rloadest' to output units in a format recognized by \code{rloadest}, or
#'   'EGRET' for crosstalk with that package, or NA to keep units as stored in
#'   'loadflex'.
#' @importFrom methods slot
#' @return \code{getUnits} returns the specified units as a character string.
#' @export
#' @examples 
#' md <- metadata(constituent="NO3", flow="DISCHARGE", 
#'   dates="DATE", conc.units="mg L^-1", flow.units="cfs", load.units="kg", 
#'   load.rate.units="kg d^-1", site.name="Lamprey River, NH")
#' getUnits(md, 'flow')
getUnits <- function(metadata, field=c("conc", "flow", "flux", "flux rate"),
                     format = NA, stop.on.empty = FALSE) {
  
  # Standardize the field input; match.arg allows partial matching for lazy typers
  field <- match.arg.loadflex(field, c("conc", "flow", "flux", "flux rate"))
  
  # Identify the corresponding slot
  slotname <- switch(
    field,
    "conc"="conc.units",
    "flow"="flow.units",
    "flux"="load.units",
    "flux rate"="load.rate.units")
  
  # Retrieve the metadata
  retVal <- getInfo(metadata, slotname, stop.on.empty)
  
  # Optionally reformat for rloadest or EGRET
  if(!is.na(format)) {
    switch(
      format,
      "rloadest" = {
        retVal <- switch(
          retVal,
          "m^3 s^-1"="cms",
          "mg L^-1"="mg/L",
          retVal
        )
      },
      "EGRET" = {
        retVal <- switch(
          retVal,
          "mg L^-1"="mg/l",
          retVal
        )
      },
      stop("unrecognized unit format")
    )
  }
  
  # Return the units info stored in that slot
  return(retVal)
}

#' Return miscellaneous information from a metadata object.
#' 
#' \code{getInfo} returns miscellaneous information (station, custom) about a 
#' model.
#' 
#' @rdname metadata-getters
#' @param stop.on.empty logical. Stops f the field is empty or non-character
#' @importFrom methods slot getSlots
#' @return \code{getInfo} returns the miscellaneous information specified by
#'   \code{field}.
#' @export
#' @examples 
#' md <- metadata(constituent="NO3", flow="DISCHARGE", 
#'   dates="DATE", conc.units="mg L^-1", flow.units="cfs", load.units="kg", 
#'   load.rate.units="kg d^-1", site.name="Lamprey River, NH")
#' getInfo(md, 'site.name')
getInfo <- function(metadata, 
                    field=c("constituent", "consti.name", "flow", "load.rate", "dates", 
                            "conc.units", "flow.units", "load.units", "load.rate.units", "station", 
                            "site.name", "site.id", "lat", "lon", "basin.area", 
                            "flow.site.name", "flow.site.id", "flow.lat", "flow.lon", "flow.basin.area", 
                            "basin.area.units", "custom"), 
                    stop.on.empty = FALSE) {
  
  # Standardize the field input
  field <- match.arg(field)
  
  # Get the metadata stored in the corresponding field
  retVal <- slot(metadata, field)
  
  # Optionally check for an empty string and stop if found
  if(isTRUE(stop.on.empty) && 
     (is.null(retVal) ||
      (length(retVal) == 1 && (is.na(retVal) || nchar(retVal) == 0)))) {
    stop(paste0("metadata item `", field, "` must be a non-empty string"))
  } 
  
  return(retVal)
}


#### Show ####

#' Display a metadata object
#' 
#' @rdname show.metadata
#' @param object The metadata object to be displayed
#' @importFrom methods setMethod getSlots slot show
#' @exportMethod show
#' @export
#' @examples
#' md <- metadata(constituent="NO3", flow="DISCHARGE", 
#'   dates="DATE", conc.units="mg L^-1", flow.units="cfs", load.units="kg", 
#'   load.rate.units="kg d^-1", site.name="Lamprey River, NH")
#' show(md) # or just md at the command prompt
setMethod(
  "show", "metadata", 
  function(object) {
    cat("Metadata for a load model\n")
    cat(sprintf("%-12s %s\n", "-NAME-", "-VALUE-"))
    slot_names <- names(getSlots("metadata"))
    for(sv in slot_names[!(slot_names %in% c("custom"))]) {
      cat(sprintf("%-12s %s\n", sv, slot(object, sv)))
    }
    if(!is.null(slot(object, "custom"))) {
      cat("custom:\n")
      show(slot(object, "custom"))
    }
  }
)

#### Equals #### usage \\method== \\usage {==}(e1) == e2 

#' Basic equality test for two metadata objects.
#' 
#' Compares the contents of each slot using the == operator.
#' 
#' @rdname equals.metadata
#' @importFrom methods setMethod getSlots slot is
#' @param e1 metadata object to be compared.
#' @param e2 metadata object to be compared.
#' @return logical value indicating whether the two metadata objects have
#'   identical contents.
#' @exportMethod ==
#' @export
#' @family metadata
#' md1 <- metadata(constituent="NO3", flow="DISCHARGE", 
#'   dates="DATE", conc.units="mg L^-1", flow.units="cfs", load.units="kg", 
#'   load.rate.units="kg d^-1", site.name="Lamprey River, NH")
#' md2 <- updateMetadata(md1, site.id="1073500", 
#'   custom=list(data_source="USGS NWIS, waterdata.usgs.gov"))
#' md1 == md2
setMethod(
  "==", c(e1="metadata", e2="metadata"),
  function(e1, e2) {
    for(sv in names(getSlots("metadata"))) {
      s1 <- slot(e1, sv)
      s2 <- slot(e2, sv)
      if(is.null(s1)) {
        if(!is.null(s2)) {
          return(FALSE)
        }
      } else {
        unequal <- try(!(s1 == s2))
        if(isTRUE(unequal)) {
          return(FALSE)
        } else if(is(unequal, "try-error")) {
          warning("elements of slot '",sv,"' could not be compared; comparison of these types is not implemented; assuming equality.")
        }
      }
    }
    return(TRUE)
  }
)

#' Convert a metadata object to a 1-row data.frame
#' 
#' Organize the fields of a metadata object into a 1-row data.frame. If there is
#' a custom field, attempt to coerce that field into 1-row data.frame columns 
#' using \code{as.data.frame}; if that effort fails, the custom field will be 
#' excluded.
#' 
#' @export
#' @param x a loadflex metadata object
#' @param row.names NULL or a character vector giving the row names for the data
#'   frame. Missing values are not allowed.
#' @param optional logical. If TRUE, setting row names and converting column 
#'   names (to syntactic names: see make.names) is optional. Note that all of 
#'   R's base package as.data.frame() methods use optional only for column names
#'   treatment, basically with the meaning of data.frame(*, check.names = 
#'   !optional).
#' @param ... additional arguments to be passed to or from methods.
#' @param stringsAsFactors logical: should the character vector be converted to 
#'   a factor?
#' @importFrom dplyr bind_cols
as.data.frame.metadata <- function(x, row.names=NULL, optional=FALSE, ..., stringsAsFactors=FALSE) {
  cust.df <- tryCatch({
    custdf <- as.data.frame(slot(x, 'custom'))
    if(is.data.frame(custdf) && nrow(custdf) == 1) custdf else stop()
  }, error=function(e) NULL)
  meta.df <- as.data.frame(attributes(x)[-which(names(attributes(x)) %in% c('custom','class'))], row.names=row.names, optional=optional, ..., stringsAsFactors=stringsAsFactors)
  if(!is.null(cust.df)) {
    meta.df <- bind_cols(meta.df, cust.df)
  }
  meta.df
}
