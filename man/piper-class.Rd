% --- Source file: piper-class.Rd ---
\name{piper-class}
\docType{class}
\alias{piper-class}
\alias{initialize,piper-method}
\alias{plot,piper-method}

\title{Class "piper" }
\description{Major ions as percentages.}
\section{Objects from the Class}{
  Objects can be created by calls of the form:

  \code{new("piper", size, group, colours, numbersymbols, wt.col,
    wt.pch, pt.col, pt.pch)}, or
  \code{piper(dataframe)}
}
\section{Slots}{
	 \describe{
    \item{\code{size}:}{Object of class \code{numeric} --- Length of
      the (square) plot area, defaults to 300 }
    \item{\code{Ca}:}{Object of class \code{vector} --- Calcium }
    \item{\code{Mg}:}{Object of class \code{vector} --- Magnesium }
    \item{\code{Cl}:}{Object of class \code{vector} --- Chloride }
    \item{\code{SO4}:}{Object of class \code{vector} --- Sulphate }
    \item{\code{WaterType}:}{Object of class \code{vector} --- factor
      for grouping samples }
    \item{\code{call}:}{Object of class \code{character} --- call
      that created it }
    \item{\code{group}:}{Object of class \code{vector} Another way of
      grouping other than WaterType }
    \item{\code{IDs}:}{Object of class \code{vector} of sample
      identifiers }
    \item{\code{pt.col}:}{Object of class \code{vector} of colours for
      points }
    \item{\code{pt.pch}:}{Object of class \code{vector} of symbols for
      points }
  }
}
\section{Methods}{
  \describe{
    \item{initialize}{\code{signature(.Object = "piper")}: ... }
    \item{plot}{\code{signature(x = "piper")}: ... }
	 }
}
\references{
  A. Zaporozec, ``Graphical interpretation of water quality data,''
  Ground Water 10, no. 2 (1972): 32--43.
}
\author{
Myles English

Maintainer: Myles English <myles@rockhead.biz>
}
%\note{ }
%\seealso{ }
\examples{
showClass("piper")

library(hydrogeo)

l <- list( Ca = c(43,10,73,26,32),
           Mg = c(30,50,3,14,12),
           Cl = c(24,10,12,30,43),
           SO4 = c(24,10,12,30,43),
           WaterType = c(2,2,1,2,3),
           IDs = c("A","B","C","D","E") )
l$pt.pch <- c(2,1,1,3,4)

lp<-piper(l)
plot( lp, main="Piper-Hill Diagram of Water Quality" )

p <- new("piper",l)
plot( p, main="Piper-Hill Diagram of Water Quality" )
}

\keyword{classes}
\keyword{hplot}