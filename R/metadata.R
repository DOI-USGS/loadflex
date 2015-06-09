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
#' @slot constituent character. The name of the data.frame column describing 
#'   the concentration of the solute or material to be modeled.
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
#' @slot station character. A description of the sampling station or site.
#' @slot custom ANY. Empty by default, but may be modified to store any 
#'   additional data the user wants to track.
#' @importFrom methods setClass
#' @exportClass metadata
setClass(
  "metadata",
  slots=c(
    # Column names
    constituent="character",
    flow="character",
    load.rate="character",
    dates="character",
    # Units
    conc.units="character",
    flow.units="character",
    load.units="character",
    load.rate.units="character",
    # Other
    station="character",
    custom="ANY"),
  
  prototype=list(
    constituent="",
    flow="",
    load.rate="",
    dates="",
    conc.units="",
    flow.units="",
    load.units="",
    load.rate.units="",
    station="",
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
#' \item TIME: s, d
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
#' \item TIME: s, d
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
#' \item TIME: second, sec, day, d
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
#' @param station character. A description of the sampling station or site.
#' @param custom ANY. Empty by default, but may be modified to store any 
#'   additional data the user wants to track.
#' @param validate logical. If TRUE, validObject() must pass for the object to 
#'   return.
#' @return \code{metadata} returns a new metadata object with the specified 
#'   entries.
#' @export
#' @family metadata
metadata <- function(constituent, flow, load.rate="", dates, 
                     conc.units, flow.units, load.units, load.rate.units, 
                     station="", custom=NULL, validate=TRUE) {
  
  # Create a list of non-missing args
  slotvals <- c(
    if(!missing(constituent)) list(constituent=constituent) else NULL,
    if(!missing(flow)) list(flow=flow) else NULL,
    if(!missing(load.rate)) list(load.rate=load.rate) else NULL,
    if(!missing(dates)) list(dates=dates) else NULL, 
    if(!missing(conc.units)) list(conc.units=conc.units) else NULL, 
    if(!missing(flow.units)) list(flow.units=flow.units) else NULL, 
    if(!missing(load.units)) list(load.units=load.units) else NULL, 
    if(!missing(load.rate.units)) list(load.rate.units=load.rate.units) else NULL, 
    if(!missing(station)) list(station=station) else NULL, 
    if(!missing(custom)) list(custom=custom) else NULL)
  
  # Create the object by updating a prototype object
  metadata <- do.call(updateMetadata, c(list(new("metadata")), slotvals, list(validate=validate)))
  
  return(metadata)
}



#' \code{updateMetadata} modifies an existing metadata object.
#' 
#' @rdname metadata
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
      if(names(metaargs[i]) %in% c("conc.units","flow.units","load.units","load.rate.units")) {
        # Make sure units are in an accepted form
        slot(metadata, names(metaargs[i])) <- translateFreeformToUnitted(metaargs[[i]], attach.units=FALSE)
      } else if(names(metaargs[i]) %in% slotNames) {
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
exampleMetadata <- function() {
  metadata("NO3","Q","NO3_FLUX","DATE","mg/L","cms","kg","kg/day",
           "Lamprey River @ Wiswall Dam, Durham, New Hampshire",
           list(a="anything you want goes here", b=1:10))  
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
#' @return \code{getUnits} returns the specified units as a character string.
#' @export
getUnits <- function(metadata, field=c("conc", "flow", "flux", "flux rate")) {
  
  # Standardize the field input; match.arg allows partial matching for lazy typers
  field <- match.arg.loadflex(field, c("conc", "flow", "flux", "flux rate"))
  
  # Identify the corresponding slot
  slotname <- switch(
    field,
    "conc"="conc.units",
    "flow"="flow.units",
    "flux"="load.units",
    "flux rate"="load.rate.units")
  
  # Return the units info stored in that slot
  slot(metadata, slotname)
}

#' Return miscellaneous information from a metadata object.
#' 
#' \code{getInfo} returns miscellaneous information (station, custom) about a 
#' model.
#' 
#' @rdname metadata-getters
#' @return \code{getInfo} returns the miscellaneous information specified by
#'   \code{field}.
#' @export
getInfo <- function(metadata, field=c("station", "custom")) {
  
  # Standardize the field input; match.arg allows partial matching for lazy typers
  field <- match.arg.loadflex(field, c("station", "custom"))
  
  # Identify the corresponding slot
  slotname <- field
  
  # Return the metadata stored in that slot
  slot(metadata, slotname)
}

#### Show ####

#' Display a metadata object
#' 
#' @rdname show.metadata
#' @name show.metadata
#' @param object The metadata object to be displayed
#' @importFrom methods setMethod
#' @exportMethod show
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

#### Equals ####

#' Basic equality test for two metadata objects.
#' 
#' Compares the contents of each slot using the == operator.
#' 
#' @rdname equals.metadata
#' @name equals.metadata
#' @param e1 metadata object to be compared.
#' @param e2 metadata object to be compared.
#' @return logical value indicating whether the two metadata objects have
#'   identical contents.
#' @importFrom methods setMethod
#' @exportMethod ==
#' @family metadata
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
