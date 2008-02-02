\name{hydrogeo-package}
\alias{hydrogeo-package}
\alias{hydrogeo}
\docType{package}
\title{
Plots Piper-Hill diagrams of water quality (hydrogeology, groundwater).
~~ package title ~~
}
\description{
Plots Piper-Hill diagrams of water quality (hydrogeology, groundwater).
}
\details{
\tabular{ll}{
Package: \tab hydrogeo\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2008-02-02\cr
License: \tab FreeBSD\cr
}
Make a data frame ('data') of water quality data consisting of sampleid and major anions and cations and (optionally) a factor to identify groups (called WaterType).

The dataframe must have row.names=sampleids, and columns named "Ca", "Mg", "Cl", "SO4" and optionally "WaterType".

Call piper(data).

The function piper() also optionally takes named vectors "colours" and "pch" for the symbols otherwise it works it out from the groups (if supplied), and numbersymbols as logical: if TRUE it ignores pch and plots each symbol as an integer character within each group.

}
\author{
Myles English (rockhead.biz)

Maintainer: Myles English <mylesenglish@gmail.com>
}
\references{
}

\keyword{ package }
\seealso{

}
\examples{

data=as.data.frame(list("Ca"=c(43,10,73,26,32),"Mg"=c(30,50,3,14,12),Cl=c(24,10,12,30,43),"SO4"=c(24,10,12,30,43),"WaterType"=c(2,2,1,2,3)),row.names=c("A","B","C","D","E"))

piper(data)
}
