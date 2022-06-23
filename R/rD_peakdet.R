#' rDolphin: peakdet
#'
#' @param v a numeric vector where to search peaks.
#' @param delta numeric of length one; defining the local threshold for peak detection.
#' @param x a numeric vector the same length as v containing corresponding x-values for v.
#'
#' @details This function was forked directly from rDolphin (https://github.com/danielcanueto/rDolphin/tree/master/R). From rDolphin: 
#' 
#' A peak detection algorithm for R; finds local maxima and minima. Based on an algorithm for MATLAB by Eli Billauer (http://www.billauer.co.il/peakdet.html).
#' This function is released to the public domain; any use is allowed.
#' 
#' @return A list containing two data frames maxtab and mintab containing maxima and minima. Data frames contain two columns with indices in v (or corresponding
#' values in x if provided) and values in v.
#' 
peakdet <- function(v, delta, x = NULL){
  maxtab <- NULL
  mintab <- NULL
  
  if (is.null(x))
  {
    x <- seq_along(v)
  }
  
  if (length(v) != length(x))
  {
    stop("Input vectors v and x must have the same length")
  }
  
  if (!is.numeric(delta))
  {
    stop("Input argument delta must be numeric")
  }
  
  if (delta <= 0)
  {
    stop("Input argument delta must be positive")
  }
  
  mn <- Inf
  mx <- -Inf
  
  mnpos <- NA
  mxpos <- NA
  
  lookformax <- TRUE
  
  for(i in seq_along(v))
  {
    this <- v[i]
    
    if (this > mx)
    {
      mx <- this
      mxpos <- x[i]
    }
    
    if (this < mn)
    {
      mn <- this
      mnpos <- x[i]
    }
    
    if (lookformax)
    {
      if (this < mx - delta)
      {
        maxtab <- rbind(maxtab, data.frame(pos = mxpos, val = mx))
        
        mn <- this
        mnpos <- x[i]
        
        lookformax <- FALSE
      }
    }
    else
    {
      if (this > mn + delta)
      {
        mintab <- rbind(mintab, data.frame(pos = mnpos, val = mn))
        
        mx <- this
        mxpos <- x[i]
        
        lookformax <- TRUE
      }
    }
  }
  
  list(maxtab = maxtab, mintab = mintab)
}