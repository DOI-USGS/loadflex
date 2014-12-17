### Some basic text handling helper functions

#' Capitalize first letter of words. 
#'
#'  Convert each word in x to have an upper-case first letter and lower-case
#'  subsequent letters. Doesn't make exceptions for articles like "the" or "a".
#'  
#'  @name sentenceCase
#'  @param x a character vector to convert
.sentenceCase <- function(x) {
  # Convert x to lower case
  x <- tolower(x)
  
  # Replace the pattern (interruptor)(letter) with (interruptor)(LETTER) where
  # interruptor is punctuation, space, or the string beginning and LETTER is the
  # capitalized version of letter
  gsub("(^|[[:punct:]|[:blank:]])([[:alpha:]])", "\\1\\U\\2", x, perl=TRUE)
  
  # If there are possessives (e.g., "Bill's"), we could make the s lowercase 
  # again. But this may be unnecessary flexibility for this package.
  #gsub("([[:alpha:]])'S([[:punct:]|[:blank:]]|$)", "\\1's\\2", x, perl=TRUE)
}

#' Replace any space or spaces with a single new.space character apiece
#' 
#' @name reSpace
#' @param x string[s] to be respaced
#' @param new.space character[s] with which to replace spaces
#' @param reduce.spaces logical. Reduce multiple consecutive spaces to a single one?
#' @param old.space regular expression defining space; default includes punctuation, space, and tab
#' @return The respaced character string
.reSpace <- function(x, new.space="_", reduce.spaces=FALSE, old.space="[[:punct:]|[:blank:]]") {
  ss <- sapply(x, function(onex) {
    s <- strsplit(onex, old.space)[[1]]
    space_at_end <- grepl(old.space, substr(onex,nchar(onex),nchar(onex)))
    if(reduce.spaces) {
      s <- s[s != ""]
    }
    paste(c(s, if(space_at_end) "" else NULL), collapse=new.space)
  })
  unname(ss)
}