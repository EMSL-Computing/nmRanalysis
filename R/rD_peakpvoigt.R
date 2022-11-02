#' rDolphin: peakpvoigt
#'
#' @param x signal fitting parameters
#' @param p chemical shift values
#'
#' @details This function is based on code that was forked directly from rDolphin (https://github.com/danielcanueto/rDolphin/tree/master/R). This is the
#' pseudovoigt lineshape function.
#'
peakpvoigt <- function(x, p){

  z   <- p - x[2]
  zz  <- z * z
  x32 <- x[3] * x[3]


  a   <- 4 * log(2)
  d   <- zz + x32
  f1  <- exp(-a * zz / x32) #f1
  f2  <- x32 / d            #f2

  aa  <- x[4] * f1
  bb  <- (1 - x[4]) * f2
  y   <- x[1] * (aa + bb) #This is the peak function

  return(y)
}
