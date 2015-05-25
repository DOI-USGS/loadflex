### Some basic text handling helper functions

#' Capitalize first letter of words.
#' 
#' Convert each word in x to have an upper-case first letter and lower-case 
#' subsequent letters. Doesn't make exceptions for articles like "the" or "a". A
#' word is defined as continuous alpha characters, so "bill's" becomes "Bill'S"
#' and "u.s.a." becomes "U.S.A".
#' 
#' @name sentenceCase
#' @param x a character vector to convert
#' @return a character vector with each word having its first letter capitalized
#'   and all others lowercase
#' @keywords internal
#' @examples
#' loadflex:::.sentenceCase("the QUICK brown Fox jumped oVer the LaZY doG")
#' loadflex:::.sentenceCase(c("QUICK brown Fox","LaZY doG"))
#' loadflex:::.sentenceCase(c("u.s.a.", "u_s_a", "bill's", "3 bears", "2 be or not 2be"))
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
#' @keywords internal
#' @examples
#' loadflex:::.reSpace("this  \t old *!$?# mandolin", reduce.spaces=TRUE) # returns "this_old_mandolin"
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