% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/piper.R
\docType{class}
\name{piper-class}
\alias{piper-class}
\alias{initialize,piper-method}
\alias{labelAxes,piperplot-method}
\alias{plot,piper-method}
\alias{show,piper-method}
\title{Class \code{piper}}
\usage{
\S4method{initialize}{piper}(.Object, l, ..., call = NULL, pt.col = NULL)

\S4method{labelAxes}{piperplot}(x, cex.axis = 0.35, side = -1, ...)

\S4method{plot}{piper}(x, type = "p", cex = 0.75, ...)

\S4method{show}{piper}(object)
}
\arguments{
\item{.Object}{object of class piper}

\item{l}{list of data, see 'Examples' below}

\item{...}{additional arguments, as for \code{\linkS4class{piper}}}

\item{call}{the call that asked for the new piper object}

\item{pt.col}{Object of class \code{vector} of colours for points}

\item{x}{an object of class piperplot}

\item{cex.axis}{magnification to be used for axis annotation relative
to the current setting of 'cex', see help("par")}

\item{side}{integer between 1 and 10 specifying which side to lable, the default is to label all}

\item{type}{what type of plot should be drawn, only "p" for *p*oints is useful}

\item{cex}{magnification to be used for symbols relative
to the current setting of 'cex', see help("par")}

\item{object}{an object of class piper}
}
\description{
Objects of this class are plotable as Piper-Hill diagrams.  A dataframe of
major ions as percentages can be used to initialise a piper object.
}
\section{Methods (by generic)}{
\itemize{
\item \code{initialize}: Initialiser

\item \code{labelAxes}: Label the axes

\item \code{plot}: Plot an object of class \code{piper}

\item \code{show}: Show an object of class \code{piper}
}}

\section{Slots}{

\describe{
\item{\code{Ca}}{Object of class \code{vector} --- Calcium}

\item{\code{Mg}}{Object of class \code{vector} --- Magnesium}

\item{\code{Cl}}{Object of class \code{vector} --- Chloride}

\item{\code{SO4}}{Object of class \code{vector} --- Sulphate}

\item{\code{anion.x}}{x coordinate of the point on the anion triangle (internal)}

\item{\code{anion.y}}{y coordinate of the point on the anion triangle (internal)}

\item{\code{cation.x}}{x coordinate of the point on the cation triangle (internal)}

\item{\code{cation.y}}{y coordinate of the point on the cation triangle (internal)}

\item{\code{diamond.x}}{x coordinate of the point on the diamond (internal)}

\item{\code{diamond.y}}{y coordinate of the point on the anion diamond (internal)}

\item{\code{IDs}}{Object of class \code{vector} of sample identifiers}

\item{\code{pt.col}}{Object of class \code{vector} of colours for points}

\item{\code{pt.pch}}{Object of class \code{vector} of symbols for  points}

\item{\code{call}}{Object of class \code{character} --- call that created it}
}}

\examples{
showClass("piper")

l <- list( Ca = c(43,10,73,26,32),
           Mg = c(30,50,3,14,12),
           Cl = c(24,10,12,30,43),
           SO4 = c(24,10,12,30,43))

lp <- piper(l)
plot( lp, main="Piper-Hill Diagram of Water Quality" )

# change symbols and colours to differentiate water type groups
lp@pt.pch = c(2,2,4,4,4)
lp@pt.col = c(0,1,0,1,2)

# use larger symbols
plot( lp, main="Piper-Hill Diagram of Water Quality", cex=1.4 )
}
\references{
A. Zaporozec, ``Graphical interpretation of water quality
data,'' Ground Water 10, no. 2 (1972): 32--43.
}
\author{
Myles English \email{myles@rockhead.biz}
}
\keyword{classes}
