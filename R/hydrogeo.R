#' Groundwater data presentation and interpretation.
#'
#' Contains one function, for drawing Piper (or Piper-Hill) diagrams
#' from water analyses for major ions, and a dataset from Zaporozec
#'
#' \tabular{ll}{
#' Package: \tab hydrogeo\cr
#' Type: \tab Package\cr
#' Version: \tab 0.5-1\cr
#' Date: \tab 2016-11-17\cr
#' License: \tab BSD\cr
#' LazyLoad: \tab yes\cr
#' }
#'
#' @docType package
#' @name hydrogeo
#' @author Myles English \email{myles@@rockhead.biz}
#' @keywords package hydrogeology hydrology groundwater water
#' @seealso \code{\link{piper}} and \code{\link{toPercent}} \code{\link{zaporozec}}
#' @examples
#' library(hydrogeo)
#' data(zaporozec)
#' zaporozec$CO3 <- rep(0,9) # toPercent expects CO3
#' zaporozec$Na <- rep(0,9)  # toPercent expects Na
#' z <- toPercent(zaporozec)
#' pz <- piper(z)
#' plot(pz,cex=1.5)
NULL
