# Estimate or make assumptions about covariance of errors in a time series of
# load estimates

#### Correlations Doc ####

#' Correlation functions in \pkg{loadflex}
#' 
#' @description
#'    
#' Correlations of residuals or prediction errors are important at several 
#' points in the process of estimating solute concentrations or fluxes. Directly
#' estimating these correlations is difficult, though sometimes possible. The 
#' functions listed here exist to help the user in (1) estimating correlations
#' from data, and (2) asserting correlation structures when empirical estimates
#' are weak or unavailable.
#' 
#' \subsection{1D Correlation Functions}{
#' 
#' These are functions that produce one or more correlation coefficients from pairs of dates.
#'
#' \itemize{
#' 
#' \item \code{\link{rhoEqualDates}}
#' 
#' \item \code{\link{rho1DayBand}}
#' 
#' }
#' 
#' }
#' 
#' \subsection{1D Correlation Function Generators}{
#' 
#' These are functions that produce functions that produce one or more correlation coefficients.
#' 
#' \itemize{
#' 
#' \item \code{\link{getRhoFirstOrderFun}}
#' 
#' }
#' 
#' }
#' 
#' \subsection{2D Correlation Functions}{
#' 
#' These are functions that produce a 2D correlation matrix from a vector of dates.
#' 
#' \itemize{
#' 
#' \item \code{\link{cormatEqualDates}}
#' 
#' \item \code{\link{cormat1DayBand}}
#' 
#' }
#' 
#' }
#' 
#' \subsection{2D Correlation Function Generators}{
#' 
#' These are functions that produce functions that produce 2D correlation matrices.
#' 
#' \itemize{
#' 
#' \item \code{\link{getCormatCustom}}
#'
#' \item \code{\link{getCormatTaoBand}}
#' 
#' \item \code{\link{getCormatFirstOrder}}
#' 
#' }
#' 
#' }
#' 
#' @docType data
#' @rdname correlations
#' @name correlations
NULL


#### 1D correlation functions ####

#' Get the assumed correlation between residuals or predictions at pairs of 
#' dates.
#' 
#' These 1D correlation functions are made available to help users explore and 
#' understand the assumptions that may be made about the correlation structure 
#' of residuals or predictions. It is possible to use these functions directly 
#' for aggregation by calling \code{\link{aggregateSolute}} with the 
#' \code{cormat.function} argument set to either 
#' \code{\link{getCormatCustom}(rhoEqualDates)} or 
#' \code{\link{getCormatCustom}(rho1DayBand)}. However, the equivalent but more 
#' efficient options are to set \code{cormat.function} to 
#' \code{\link{cormatEqualDates}} or \code{\link{cormat1DayBand}}, respectively,
#' because those \code{cormatXXX} functions produce the full correlation matrix 
#' without costly intermediate steps.
#' 
#' \code{rhoEqualDates} embodies the standard LOADEST/rloadest assumption about 
#' correlation of residuals for the purpose of aggregation. Specifically, if two
#' point predictions are on the same calendar date, then rho (the correlation of
#' their errors) is set to 1, and otherwise rho = 0.
#' 
#' @rdname correlations-1D
#' @name correlations-1D
#' @param date1 The date of the first prediction
#' @param date2 The date[s] of the second prediction[s].
#' @return A correlation coefficient or a vector of coefficients corresponding 
#'   to the pair or pairs in the rows of \code{cbind(date1, date2)}.
#' @export
rhoEqualDates <- function(date1, date2) {
  dates_equal <- as.Date(date1) == as.Date(date2)
  as.numeric(dates_equal) # same as ifelse(datesequal, 1, 0), but maybe faster
}

#' @details 
#' 
#' \code{rho1DayBand} returns 1 for pairs of date-times that are within 1 day
#' (86400 seconds) of each other, or 0 for pairs that are not. This is a comparable
#' but smoother assumption than \code{rhoEqualDates}.
#' 
#' @rdname correlations-1D
#' @export
rho1DayBand <- function(date1, date2) {
  tao <- abs(date2 - date1)
  within_a_day <- (tao < as.difftime(1, units="days"))
  as.numeric(within_a_day) # same as ifelse(within_a_day, 1, 0), but maybe faster
}
# system.time(mat <- cormatrix(rho1DayBand, dates))


#### 1D correlation function getters ####

#' Produces a function that uses a first-order autocorrelation model to estimate
#' the correlation between two dates.
#' 
#' @description
#' 
#' Produces a function of the form described in \link{correlations-1D}.
#' 
#' \code{getRhoFirstOrderFun} accepts an assumed or empirically estimated value
#' of \eqn{\rho} (\code{rho}), the correlation coefficient in the first-order
#' autocorrelation model:
#' 
#' \deqn{cor(y(t1), y(t2)) = \rho^\tau}
#' 
#' where
#' 
#' \deqn{\tau = y(t2) - y(t1)}
#' 
#' and \eqn{\tau} is in the same units as those used to estimate \eqn{\rho} 
#' (\code{rho}). To ensure that the units are the same, this function also 
#' accepts a \code{time.step} argument indicating the interval between two 
#' successive values in the time series used to estimate \eqn{\rho} 
#' (\code{rho}). The function returned by \code{getRhoFirstOrderFun} takes 
#' responsibility for matching the units of \code{date1} and \code{date2} to 
#' those of \code{rho}.
#' 
#' @param rho The coefficient of the first-order autocorrelation model, either 
#'   asserted by the user or estimated by \code{\link{estimateRho}}.
#' @param time.step difftime. The time.step of the time series used to estimate 
#'   \code{rho}. It is essential to use the right units; \code{as.difftime(1, 
#'   units="hours")} is quite different from \code{as.difftime(1, 
#'   units="days")}.
#' @return A function that accepts two date arguments and returns the predicted 
#'   correlation between those two dates. One of the date arguments may be a 
#'   vector as long as the other is scalar. This function takes the form, 
#'   \code{function(date1, date2)} with arguments as defined for 
#'   \code{\link{rhoEqualDates}} and \code{\link{rho1DayBand}}.
#' @export
getRhoFirstOrderFun <- function(rho, time.step=as.difftime(1, units="hours")) {
  function(date1, date2) {
    # Count the number of time steps, tao. Tao may be non-integer in this implementation.
    tao <- abs(as.numeric(difftime(date2, date1, units=units(time.step))))/as.numeric(time.step)
    rho^tao
  }
}




#### 2D correlation matrix function getters ####

#' Turn an autocorrelation function into a function that produces a correlation
#' matrix
#' 
#' Produces a function of the form described in \link{correlations-2D} from a 
#' function of the form described in \link{correlations-1D}.
#' 
#' For use with custom autocorrelation functions. Although it is usually faster 
#' to create your own matrix-generating function, this function will accept a 
#' simpler autocorrelation function (one of the form described in 
#' \link{correlations-1D}) and return a function that accepts a vector of dates 
#' generates a matrix of correlations for all possible pairs of those dates (a 
#' function of the form described in \link{correlations-2D}. The matrix returned
#' by that second function will be sparse if it is at least half zeros.
#' 
#' @param cor1D.function An autocorrelation function - specifically, a function 
#'   that accepts two date arguments and returns the correlation coefficient[s] 
#'   for those dates. If each date argument is a single date, the output should 
#'   be a single coefficient. If the second date argument is a vector of dates, 
#'   the output should be a vector of the same length as that dates vector.
#' @param vectorized logical. Can rho.function accept its second argument as a 
#'   vector rather than a scalar? If so, the matrix-generating function may be 
#'   faster.
#' @return A function that accepts a new vector of N dates and produces an 
#'   N-by-N correlation matrix.
#' @export
getCormatCustom <- function(cor1D.function, vectorized=FALSE) {
  if(vectorized) {
    function(dates) {
      library(Matrix)
      cor_matrix <- Matrix(0, nrow=length(dates), ncol=length(dates)) # starts sparse
      for(i in 1:length(dates)) {
        cor_matrix[i,] <- cor1D.function(dates[i], dates)
      }
      return(cor_matrix)
    }
  } else {
    function(dates) {
      library(Matrix)
      cor_matrix <- Matrix(0, nrow=length(dates), ncol=length(dates)) # starts sparse
      for(i in 1:length(dates)) {
        for(j in 1:length(dates)) {
          cor_matrix[i,j] <- cor1D.function(dates[i], dates[j])
        }
      }
      return(cor_matrix)
    }
  }
}

#' Same idea as getCormatCustom(rho1DayBand, dates) but runs faster.
#' 
#' getcormatTaoBand
#' 
#' calculate the covariance  1 if dates are within the tao band, 0 if they are
#' not Same idea as cormatrix(rho1DayBand, dates) but runs in linear instead of 
#' quadratic time - a big and much-needed improvement.
#' 
#' @param max.tao length of the covariance band, defaults to 1 day
#' @return matrix of the covariances
#' @export
getCormatTaoBand <- function(max.tao=as.difftime(1, units="days")) {
  function(dates) {
    # Faster by ~40% to do this with numbers rather than dates
    max.tao <- as.numeric(as.POSIXct(dates[1] + max.tao)) - as.numeric(as.POSIXct(dates[1]))
    dates <- as.numeric(as.POSIXct(dates))
    
    # Create a list of small matrices containing indices (col1=rowID,
    # col2=colID) where the mat Matrix should contain 1
    ids <- list()
    row_min <- row_max <- 1
    for(i in 1:length(dates)) {
      # as we move forward in time, cut out earlier times that are no longer in
      # the time window
      min_date <- dates[i] - max.tao
      while(dates[row_min] <= min_date) {
        row_min <- row_min + 1
      }
      # and encompass later times that are newly within the window
      max_date <- dates[i] + max.tao
      while(dates[row_max] < max_date) {
        row_max <- row_max + 1
        if(row_max > length(dates)) break
      }
      row_max <- row_max - 1
      # within this row, plan to assign a 1 to dates within the window
      ids[[i]] <- cbind(i,row_min:row_max)
    }
    
    # Create a sparse matrix and fill it with 1s where needed
    library(Matrix)
    mat <- Matrix(data=0, nrow=length(dates), ncol=length(dates), sparse=diff(range(dates))>max.tao)
    mat[do.call("rbind", ids)] <- 1
    mat
  }
}

#' get rho matrix first order
#' 
#' @param rho the covariance  asdefined as difference between the times divided by the time step 
#' @param time.step default is 1 hour 
#' @param max.tao don't consider covariance for values further apart then this. 
#' @return covariance defined as difference between the times divided by the time step don't calculate covariance for 
#'         values further away then max.tao.    
#' @export 
getCormatFirstOrder <- function(rho, time.step=as.difftime(1, units="hours"), max.tao=as.difftime(1, units="days")) {
  function(dates) {
    library(Matrix)
    mat <- Matrix(data=0, nrow=length(dates), ncol=length(dates), sparse=diff(range(dates))>as.difftime(1, units="days"))
    row_min <- row_max <- 1
    for(i in 1:length(dates)) {
      # as we move forward in time, cut out earlier times that are no longer in
      # the time window
      min_date <- dates[i] - max.tao
      while(dates[row_min] <= min_date) {
        row_min <- row_min + 1
      }
      # and encompass later times that are newly within the window
      max_date <- dates[i] + max.tao
      while(dates[row_max] < max_date) {
        row_max <- row_max + 1
        if(row_max > length(dates)) break
      }
      row_max <- row_max - 1
      # Within this row, assign rho^tao values to dates within the window.
      # First count the number of time steps, tao. Tao may be non-integer in this implementation.
      tao <- abs(as.numeric(difftime(dates[row_min:row_max], dates[i], units=units(time.step))))/as.numeric(time.step)
      mat[i, row_min:row_max] <- rho^tao
    }
    mat
  }
}




#### 2D correlation matrix functions ####

#' Functions that each produce an autocorrelation matrix with a specified 
#' pattern.
#' 
#' @description
#' 
#' Accepts a vector of date-times or dates and returns a correlation matrix 
#' describing the assumed correlations between all possible pairs of those 
#' dates.
#' 
#' \code{cormatEqualDates} formalizes the assumption used by LOADEST and
#' rloadest: if two date-times are on the same calendar date, the correlation
#' (rho) is 1. Otherwise, rho=0.
#' 
#' @rdname correlations-2D
#' @name correlations-2D
#' @param dates date-times (as Date, POSIXct, chron, etc.) from which the 
#'   autocorrelation matrix should be produced.
#' @return A matrix of autocorrelation coefficients for all possible pairs of 
#'   date-times in \code{dates}.
#' @export
cormatEqualDates <- function(dates) {
  library(Matrix)
  mat <- Matrix(data=0, nrow=length(dates), ncol=length(dates), sparse=diff(range(dates))>as.difftime(1, units="days"))
  datefactors <- as.numeric(as.factor(as.Date(dates)))
  for(i in datefactors) {
    one_square_side <- which(datefactors == i)
    one_square <- cbind(rep.int(one_square_side, length(one_square_side)),
                        rep(one_square_side, each=length(one_square_side)))
    mat[one_square] <- 1
  }
  mat
}


#' \code{cormat1DayBand} produces a matrix containing 1s within a 1-day band 
#' (within 24 hours preceding or 24 hours following an observation) and 0s 
#' elsewhere. This is a special case of the functions that can be generated by 
#' \code{\link{getCormatTaoBand}()}.
#' 
#' @rdname correlations-2D
#' 
#' @export
cormat1DayBand <- getCormatTaoBand(as.difftime(1, units="days"))


#' \code{cormatDiagonal} produces a matrix containing 1s ONLY along the diagonal
#' - i.e., no covariance among predictions. This function is rarely suitable for aggregation with uncertainty estimation,
#' but it's faster than the alternatives.
#' 
#' @rdname correlations-2D
#' @export
cormatDiagonal <- function(dates) {
  library(Matrix)
  Diagonal(length(dates))
}


#### Relevant text from Cohn (2005), Water Resources Research, Section 6.2: ####

# "The lag one day serial correlation in the errors typically exceeds 0.5, and 
# sometimes 0.9 (G. Schwarz, personal communication, 2003), and varies with 
# hydrological factors such as basin size, topography, and groundwater inflows, 
# as well as by constituent. Moreover, we seldom have adequate data to estimate 
# rho because estimating rho requires closely spaced data which are not well 
# suited to estimating loads. Furthermore, because the standard deviation of 
# rho-hat is approximately 1/sqrt(N), it might take 100 or more observations to 
# obtain acceptable precision; very few projects can afford to worry about it.

# "For moderate sized watersheds (on the order of 1000 square miles) it is often
# assumed that the serial correlation is 1.0 within a day and 0.0 between days.
# This overestimates the within-day correlation and underestimates the
# between-day correlation. Fixing the value of rho in this way, however,
# simplifies the computation, and, given the uncertainty in rho, such an
# assumption may be reasonable. Also, it is important to recall that while the
# uncertainty in the load estimate may be sensitive to rho, the estimate is
# insensitive to rho because calibration data are collected infrequently. In
# addition, the relative magnitude of natural variability tends to diminish as
# the time interval over which loads are computed increases."
