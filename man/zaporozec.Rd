% --- Source file: zaporozec.Rd ---
\name{zaporozec}
\alias{zaporozec}
\docType{data}
\title{Major ions for Groundwaters reported by Zaporozec}
\description{
 This data set contains major ion analyses for three groundwaters.
}
\usage{data(zaporozec)}
\format{
  A data frame with 9 observations on the following 15 variables:
  \describe{
    \item{\code{location}}{a factor with levels \code{Tertiary, Czechoslovakia} \code{Upper Cambrian, Wisconsin} \code{Upper Cretaceous, Czechoslovakia}}
    \item{\code{K}}{a numeric vector - potassium}
    \item{\code{Mg}}{a numeric vector - magnesium}
    \item{\code{Ca}}{a numeric vector - calcium}
    \item{\code{Mn}}{a numeric vector - magnesium}
    \item{\code{Fe}}{a numeric vector - iron}
    \item{\code{Cl}}{a numeric vector - chloride}
    \item{\code{NO3}}{a numeric vector - nitrate}
    \item{\code{HCO3}}{a numeric vector - bicarbonate}
    \item{\code{SO4}}{a numeric vector - sulphate}
    \item{\code{sigma}}{a numeric vector - standard deviation}
    \item{\code{TDS}}{a numeric vector - total dissolved solids}
    \item{\code{tempC}}{a numeric vector - temperature}
    \item{\code{pH}}{a numeric vector - pH}
    \item{\code{units}}{a factor with levels \code{meq/l} \code{meq_pc} \code{mg/l}}
  }
}
%\details{
%  ~~ If necessary, more details than the __description__ above ~~
%}
\source{
 Zaporozec, ``Graphical interpretation of water quality data,'' \emph{Ground Water} 10, no. 2 (1972): pages 32--43.
}
%\references{
%  ~~ possibly secondary sources and usages ~~
%}
\examples{
data(zaporozec)
str(zaporozec)
}
\keyword{datasets}
