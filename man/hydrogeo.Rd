% --- Source file: hydrogeo-package.Rd ---
\name{hydrogeo-package}
\alias{hydrogeo-package}
\docType{package}
\title{
Groundwater data presentation and interpretation.
}
\description{
  Contains one function for drawing Piper (or Piper-Hill)
  diagrams from water analyses for major ions (hydrogeology, groundwater).
}
% \details{}
%  \tabular{ll}{
%  Package: \tab hydrogeo\cr
%  Type: \tab Package\cr
%  Version: \tab 0.0.2\cr
%  Date: \tab 2009-11-07\cr
%  License: \tab BSD\cr
%  }

\author{
Myles English

Maintainer: Myles English <myles@rockhead.biz>
}
%\references{ }

\keyword{ package }
%\seealso{}
\examples{
library(hydrogeo)

l <- list( Ca = c(43,10,73,26,32),
           Mg = c(30,50,3,14,12),
           Cl = c(24,10,12,30,43),
           SO4 = c(24,10,12,30,43),
           WaterType = c(2,2,1,2,3),
           IDs = c("A","B","C","D","E") )
l$pt.pch <- c(2,1,1,3,4)
lp<-piper(l)
plot(lp)

data(zaporozec)
zaporozec$pt.pch<-seq(3,11)
p<-piper(zaporozec)
plot(p)
legend("topright", p@IDs, col=p@pt.col, pch=p@pt.pch)
}
\keyword{package}
\keyword{hydrogeology}
\keyword{hydrology}
\keyword{groundwater}
\keyword{water}
