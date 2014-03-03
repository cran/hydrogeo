\name{piper}
\alias{piper}
\title{Function Piper}
\description{
Returns a new object of class piper.
}
\usage{
piper(d, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{d}{\code{d}}{a data.frame}
  \item{\dots}{\code{\dots}}{other arguments (TODO)}
}
\value{
Object of class piper()
}
\author{
Myles English (rockhead.biz)

Maintainer: Myles English <myles@rockhead.biz>
}
\examples{
require(hydrogeo)

l <- list( Ca = c(43,10,73,26,32),
           Mg = c(30,50,3,14,12),
           Cl = c(24,10,12,30,43),
           SO4 = c(24,10,12,30,43),
           WaterType = c(2,2,1,2,3),
           IDs = c("A","B","C","D","E") )
l$pt.pch <- c(2,1,1,3,4)
lp<-piper(l)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ functions }

