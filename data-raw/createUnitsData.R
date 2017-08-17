# Generate units conversion & parsing data

# Should only need to be run once on a change in the function contents.
# Generates the units data to be saved in data/valid.metadata.units, 
# data/freeform.unit.translations, data/unit.conversions, and (all three 
# combined) R/sysdata.rda. These are then saved with the package.

library(dplyr)

#### valid.metadata.units ####

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

#### freeform.unit.translations ####

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

#### unit.conversions ####

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

#### save data ####

devtools::use_data(
  valid.metadata.units, freeform.unit.translations, unit.conversions,
  overwrite=TRUE, compress='gzip')

tools::checkRdaFiles('data')

#### sysdata.rda ####

# The above save calls put the data into a user-accessible location (the data 
# folder). But the following is the important line for the functionality of 
# flowconcToFluxConversion(), translateFreeformToUnitted(), 
# validMetadataUnits(), etc.:
devtools::use_data(
  valid.metadata.units, unit.conversions, freeform.unit.translations,
  overwrite=TRUE, compress='gzip', internal=TRUE)
