.packageName <- "hydrogeo"
# Copyright Myles English 2008-2016
# Contact: myles@rockhead.biz
# License: BSD_2_clause

#' @import methods
NULL

#' @importFrom graphics plot points Axis
NULL

#' Major ions as a percentage of total major ions
#'
#' Expects certain column names
#'
#' @param d list or data.frame with the following columns:
#' Ca, Mg, Na, K and Cl, SO4, CO3, HCO3
#' @examples
#' library(hydrogeo)
#' l <- list( Ca = c(43,10,73,26,32),
#'           Mg = c(30,50,83,14,62),
#'           Na = c(54,76,3,14,12),
#'           K = c(31,22,32,22,11),
#'           Cl = c(24,10,12,30,43),
#'           SO4 = c(24,10,12,30,43),
#' 	  CO3 = c(24,10,12,30,43),
#' 	  HCO3 = c(42,110,12,3,4),
#'           WaterType = c(2,2,1,2,3),
#'           IDs = c("A","B","C","D","E") )
#' d <- toPercent(l)
#' # check, should add up to 100%
#' z <- as.data.frame(d)
#' for(i in 1:length(z[[1]])) { print(sum(z[i,5:8])) }
#' for(i in 1:length(z[[1]])) { print(sum(z[i,1:4])) }
#' @export
toPercent <- function(d){
    totalCations <- d$Ca + d$Mg + d$Na + d$K
    d$Ca <- 100 * (d$Ca / totalCations)
    d$Mg <- 100 * (d$Mg / totalCations)
    d$Na <- 100 * (d$Na / totalCations)
    d$K <- 100 * (d$K / totalCations)
    totalAnions<- d$Cl + d$SO4 + d$CO3 + d$HCO3
    d$Cl <- 100 * (d$Cl / totalAnions)
    d$SO4 <- 100 * (d$SO4 / totalAnions)
    d$CO3 <- 100 * (d$CO3 / totalAnions)
    d$HCO3 <- 100 * (d$HCO3 / totalAnions)
    return(d)
}

#' Class \code{piperplot}
#'
#' Objects of this class are plottable as empty (i.e. no points) Piper-Hill
#' diagrams
#'
#' @slot size Object of class \code{numeric} --- Length of the (square) plot
#' area, defaults to 300
#' @slot call R call that created it
setClass("piperplot",
         slots = list(size="numeric",
            ## plotted="logical",
             call="call"),
         prototype = list(size=300)##,plotted=FALSE)
         )

#' Class \code{piper}
#'
#' Objects of this class are plotable as Piper-Hill diagrams.  A dataframe of
#' major ions as percentages can be used to initialise a piper object.
#'
#' @slot Ca Object of class \code{vector} --- Calcium
#' @slot Mg Object of class \code{vector} --- Magnesium
#' @slot Cl Object of class \code{vector} --- Chloride
#' @slot SO4 Object of class \code{vector} --- Sulphate
#' @slot WaterType Object of class \code{vector} --- factor for grouping samples
#' @slot anion.x x coordinate of the point on the anion triangle (internal)
#' @slot anion.y y coordinate of the point on the anion triangle (internal)
#' @slot cation.x x coordinate of the point on the cation triangle (internal)
#' @slot cation.y y coordinate of the point on the cation triangle (internal)
#' @slot diamond.x x coordinate of the point on the diamond (internal)
#' @slot diamond.y y coordinate of the point on the anion diamond (internal)
#' @slot group Object of class \code{vector} Another way of grouping other than WaterType
#' @slot IDs Object of class \code{vector} of sample identifiers
#' @slot pt.col Object of class \code{vector} of colours for points
#' @slot pt.pch Object of class \code{vector} of symbols for  points
#' @slot call Object of class \code{character} --- call that created it
#' @references A. Zaporozec, ``Graphical interpretation of water quality
#' data,'' Ground Water 10, no. 2 (1972): 32--43.
#' @author Myles English \email{myles@@rockhead.biz}
#' @keywords classes
#' @examples
#' showClass("piper")
#'
#' l <- list( Ca = c(43,10,73,26,32),
#'            Mg = c(30,50,3,14,12),
#'            Cl = c(24,10,12,30,43),
#'            SO4 = c(24,10,12,30,43),
#'            WaterType = c(2,2,1,2,3),
#'            IDs = c("A","B","C","D","E") )
#'
#' lp <- piper(l)
#' plot( lp, main="Piper-Hill Diagram of Water Quality" )
# \dontrun{
# x = identify( c(p@@anion.x,p@@cation.x,p@@diamond.x),
#               c(p@@anion.y,p@@cation.y,p@@diamond.y),
#               p@@IDs)
#
# Now, click on the plot near some points with the LH mouse button, press the
# RH mouse button when finished. Vector x now contains the indicies of the
# points, however there are three points for each water sample, so, to look
# up the IDs in the piper object, the indicies are modulo the number of
# samples, i.e.:
#
# p@@IDs[ x %% length(p@@IDs) ]
# }
# @export piper
setClass("piper",
         slots = list(  Ca="vector",
                        Mg="vector",
                        Cl="vector",
                        SO4="vector",
                        WaterType="vector",
                        group="vector",     # TODO: make a factor
                        IDs="vector",       # NOTE: row.names changed to IDs
                        pt.col="vector",    # colours for WaterType
                        pt.pch="vector",    # symbols for WaterType
             anion.x="vector",
             anion.y="vector",
             cation.x="vector",
             cation.y="vector",
             diamond.x="vector",
             diamond.y="vector"
                      ),
         contains = "piperplot"
         )

cationy = function (Mg, size) { c(Mg * 5 * size/1100) }
cationx = function (Ca, Mg, size) { c((5 * size/11) * (1 - (Ca/100) - (Mg/200))) }

#' @describeIn piper Initialiser
# @watertype should be the first factor
# @group - should be the second factor, to allow plotting on different plots
# when there are a lot of points but the colours and symbols need to be
# preserved between plots.
# i.e. col and pch by @WaterType
#      plot by @group
# WaterTypes is shown by colour
# Sample ID is shown by colour and pch via legend with IDs
#' @param .Object object of class piper
#' @param l list of data, see 'Examples' below
# as per \code{\linkS4class{piper}}
#' @param ... additional arguments, as for \code{\linkS4class{piper}}
#' @param call the call that asked for the new piper object
#' @param group (untested) Object of class \code{vector}, a second factor to group by, to allow plotting on different plots
#' @param wt.col (untested) Water type colours
#' @param pt.col Object of class \code{vector} of colours for points
setMethod(f="initialize",
          signature(.Object = "piper"),
          function(.Object, l, ..., call=NULL,
                   group=NULL, wt.col=NULL, pt.col=NULL)
          {
            #TODO: test these checks work
            if ( any((l$Ca + l$Mg)>100) ) {
                stop("Ca + Mg must be <= 100")
            }
            if ( any((l$Cl + l$SO4)>100) ) {
                stop("Cl + SO4 must be <= 100")
            }
            .Object@Ca <- l$Ca
            .Object@Mg <- l$Mg
            .Object@Cl <- l$Cl
            .Object@SO4 <- l$SO4

            if ( !is.null(l$pt.pch) ) {
                .Object@pt.pch <- l$pt.pch
            } else {
                .Object@pt.pch <- rep_len( 0:25, length(row.names(l)) )
            }
            if ( !is.null(l$group) ) {
                .Object@group <- group
            } else { # plot all samples on the same plot
                .Object@group <- rep( 1, times=length(l$Ca) )
            }
            if ( !is.null(l$WaterType) ) {
                .Object@WaterType <- l$WaterType
            } else { # treat each sample as an individual water type
                .Object@WaterType <- seq( 1:length(row.names(l)) )
            }
            if ( !is.null(l$IDs) ) {
                .Object@IDs <- l$IDs
            } else {
                .Object@IDs <- row.names(l)
            }
            # TODO: make this less confusing
            # make the colours and symbols for the points now
            # because it saves confusion later
            # wt.col and wt.pch should be factors (?)
            # pt.col and pt.pch should be vectors
            # pt. overides wt.
          
            # set pt.col
            if ( ! is.null( l$pt.col ) ) {        # if specified
              .Object@pt.col <- l$pt.col          # assign it
            } else {                              # else calculate it...
              wtf <- as.factor(.Object@WaterType)
              if ( ! is.null( l$wt.col ) ) {
                if ( length(l$wt.col) != length( levels(wtf) ) ) {
                  cat("ERROR: wt.col wrong length for WaterType!")
                  return(invisible())
                } else { levels(wtf) <- l$wt.col }
              } else {  # the default for pt.col
                levels( wtf ) <- seq( 1:length(levels(wtf)) )
              }
              .Object@pt.col <- as.vector( wtf )
            }

            # set pt.pch
            # input wt:      2 2 1 2 3
            # output pt.col  2 2 1 2 3
            #        pt.pch  1 2 1 3 1
            if ( ! is.null( l$pt.pch ) ) {             # if specified
              .Object@pt.pch <- l$pt.pch               # assign it
            } else {                                   # else calculate it...
              wtf <- as.factor(.Object@WaterType)
              .Object@pt.pch <- .Object@WaterType                 # initialise
              ## if ( ! is.null( l$wt.pch ) ) {
##               for ( i in levels(wtf) ) {
##                  # get subset , replace with values from vector
##                  lrp<-sum( wtf==i ) # the number of samples of that watertype
##                  pch[ wtf==i ] <- l$wt.pch[1:lrp]
##                }
##              } else {
##                # loop through levels
##                for ( i in levels(wtf) ) {
##                  lrp<-sum( wtf==i ) # the number of samples of that watertype
##                  pch[ wtf==i ] <- seq(lrp)
##                }
##              }
           ###   .Object@pt.pch <- pch                 # assign
          }
            .Object@call <- call

            callNextMethod(.Object, ...)   # to fill 'size'

            .Object@anion.x = c((6 * .Object@size/11)
                           + ((5 * .Object@size/11) * (.Object@Cl/100))
                           + (1/2) * (5 * .Object@size/11) * (.Object@SO4/100))
            .Object@anion.y = c(.Object@SO4 * 5 * .Object@size/1100)
            .Object@cation.x = cationx(.Object@Ca,.Object@Mg,.Object@size)
            .Object@cation.y = cationy(.Object@Mg,.Object@size)
            .Object@diamond.x = ( (1/2) * (.Object@cation.x + .Object@anion.x)
                            + ( (1/4) * (.Object@anion.y - .Object@cation.y) ) )
            .Object@diamond.y = ( .Object@anion.x - .Object@cation.x
                            + ( (1/2) * (.Object@anion.y + .Object@cation.y) ) )
          return(.Object)
          }
          )

#' Create a new piper object
#'
#' @param d list passed to class piper, \code{\linkS4class{piper}}
#' @param ... additional arguments, as for \code{\linkS4class{piper}}
#' @seealso \code{\link{piper-class}} and \code{\link{toPercent}}
#' @export
piper <- function(d, ...){
    new("piper", d, call=sys.call(), ...)
}

#' Create a new piperplot object
#'
#' @param size integer related to the size of the plot area
#' @param ... additional arguments, as for \code{\linkS4class{piperplot}}
#' @examples
#' library(hydrogeo)
#' p = piperPaper(size=1)
#' plot(p)
#' @export
piperPaper <- function(size=NULL, ...){
    new("piperplot", call=sys.call(), ...)
}

#' Plot the diagram area with two triangles and a diamond
#'
#' @param x object of class piperplot
#' @param axes logical saying whether to draw the axes or not, defaults to TRUE
#' @param ... further arguments to plot.default
#' @export plot
#' @importFrom graphics plot.default segments par
setMethod(
    f="plot",
    signature(x = "piperplot"),
        function(x, axes = TRUE, ...) # group=NULL,
        {
            p <- (x@size/11)
            r <- (x@size/22)

            plot.default(0, 0, type="n", axes=FALSE, lty=1, lwd=1,
                         xlim=c(0, x@size + p), ylim=c(-p, x@size),
                         frame.plot=FALSE, ann=TRUE, ylab="", xlab="", ...)
            

    # draws grid lines
    thickxf <- c(0, (10 * r), (5 * r), (12 * r), x@size, (17 * r),
                 (x@size/2), (16 * r), (x@size/2), (6 * r))
    thickxt <- c((10 * r), (5 * r), 0, x@size, (17 * r), (12 * r),
                 (16 * r), (x@size/2), (6 * r), (x@size/2))
    thickyf <- c(0, 0, (10 * r), 0, 0, (10 * r), (2 * r), (12 * r), 
                 x@size, (12 * r))
    thickyt <- c(0, (10 * r), 0, 0, (10 * r), 0, (12 * r), x@size, 
                 (12 * r), (2 * r) )
    
    xf <- c(thickxf, (2 * r), (4 * r), (6 * r), (8 * r), (14 * r), 
            (16 * r), (18 * r), (20 * r), (21 * r), (20 * r), (19 * r),
            (18 * r), (21 * r), (20 * r), (19 * r), (18 * r), (9 * r), (8 * r),
            (7 * r), (6 * r), (9 * r), (8 * r), (7 * r), (6 * r), (7 * r),
            (8 * r), (9 * r), (10 * r), (7 * r), (8 * r), (9 * r), (10 * r))
    
    xt <- c(thickxt, r, (2 * r), (3 * r), (4 * r), (13 * r), (14 * r), (15 * r),
            (16 * r), (13 * r), (14 * r), (15 * r), (16 * r), (20 * r),
            (18 * r), (16 * r), (14 * r), (8 * r), (6 * r), (4 * r), (2 * r),
            r, (2 * r), (3 * r), (4 * r), (12 * r), (13 * r), (14 * r),
            (15 * r), (12 * r), (13 * r), (14 * r), (15 * r))
    
    yf <- c(thickyf, 0, 0, 0, 0, 0, 0, 0, 0, (2 * r), (4 * r), (6 * r), (8 * r),
            (2 * r), (4 * r), (6 * r), (8 * r), (2 * r), (4 * r), (6 * r),
            (8 * r), (2 * r), (4 * r), (6 * r), (8 * r), (14 * r), (16 * r),
            (18 * r), (20 * r), (10 * r), (8 * r), (6 * r), (4 * r))
    
    yt <- c(thickyt, (2 * r), (4 * r), (6 * r), (8 * r), (2 * r), (4 * r),
            (6 * r), (8 * r), (2 * r), (4 * r), (6 * r), (8 * r), 0, 0, 0, 0, 0,
            0, 0, 0, (2 * r), (4 * r), (6 * r), (8 * r), (4 * r), (6 * r),
            (8 * r), (10 * r), (20 * r), (18 * r), (16 * r), (14 * r))

    segments(xf, yf, xt, yt, col=par("fg"), lty=1, lwd=par("lwd"))

            if ( axes ) {
                Axis(x) }

##    x@plotted <- TRUE
}
    )


#' @describeIn piperplot Add axes to a piperplot
#' @param x an object of class piperplot
#' @importFrom graphics text
setMethod(
    f="Axis",
    signature(x="piperplot"),
    function(x = NULL) #, at = NULL, ..., side, labels = NULL)
        {
    p <- (x@size/11)      # for scaling
    r <- (x@size/22)      # for scaling

    cex.axis = 0.7 # FIXME
    # add axis titles
    vfont <-  c("serif", "italic")
    
    # label bottom axes (the two horizontal axes)
    xstr <- c(5 * r, 17 * r)
    ystr <- c(-r, -r)

    text(xstr, ystr, labels=c( expression(Ca^{2+''}), expression(paste(Cl,
                               ''^{symbol('-')})) ),
         vfont=vfont, cex=cex.axis)

    # label axes parallel with a line angled at 60% from horizontal
    # in a clockwise direction
    xgh <- c(14 * r, 8 * r, 20 * r)
    ygh <- c(18 * r, 6 * r, 6 * r)

    text(xgh, ygh,
         labels=c( expression( paste(Ca^{2+''}, " ", symbol('&'), " ",
                                       Mg^{2+''})),
                     expression( paste(Na^{symbol('+')}, " ", symbol('&'), " ",
                                       K^{symbol('+')})),
                     expression( paste(SO[4],''^{2-''}))),
         srt=300, vfont=vfont, cex=cex.axis)

    # label axes parallel with a line angled at 60% (approx.) from horizontal
    # in an anti clockwise direction    
    xla <- c(14 * r, 8 * r, 2 * r)
    yla <- c(6 * r, 18 * r, 6 * r)
    
    text(xla, yla,
         labels=c( expression( paste(CO[3],''^{2-''}, " ", symbol('&'), " ",
                                 HCO[3],''^{-''})),
                     expression( paste(SO[4],''^{2-''}, " ", symbol('&'), " ",
                                 Cl, ''^{symbol('-')})),
                     expression( paste(Mg^{2+''}))),
         srt=60, vfont=vfont, cex=cex.axis)

    labelAxes(x)
   })


setGeneric (
    "labelAxes",
    function(x, cex.axis=0.1, side=-1, ...)
    standardGeneric("labelAxes")
    )

#' @describeIn piper Label the axes
#' @param x an object of class piperplot
#' @param cex.axis magnification to be used for axis annotation relative
#' to the current setting of 'cex', see help("par")
#' @param side integer between 1 and 10 specifying which side to lable, the default is to label all
#' @importFrom graphics text
setMethod(
          f="labelAxes",
          signature(x="piperplot"),
          definition=function(x, cex.axis=0.35, side=-1, ...)
    {
    p <- (x@size/11)      # for scaling
    r <- (x@size/22)      # for scaling

    #TODO: Most of these don't work

    ## Tick mark labels:
    # add axis tick mark labels: 20, 40, 60, 80 percent labels
    adj <- 0.5     # to centre text on the point
    labels <- c(20 * (1:4))  # too clever by half: 20, 40, 60, 80 percent
    vfont <- c("serif", "italic")

    # for calculating offsets from line intersections
    ofs <- function(deg) {
      dd <- tan( (deg/360) * 2 * pi )
      offset <- cex.axis * dd * (x@size/50)
      return( offset )
    }

    if(side==-1 || side==1){
    # flat labels LHS - Mg2+
    xe <- c(r * 1:4)
    ye <- c(p * 1:4) 
    text(xe, ye, labels=labels, pos=2, offset=0.1, vfont=vfont, cex=cex.axis)
}
    if(side==-1 || side==2){
    # flat labels RHS - SO42-
    xe <- c(r * 21:18)
    ye <- c(p * 1:4) 
    text(xe, ye, labels=labels, pos=4, offset=0.1, vfont=vfont, cex=cex.axis)
}
    if(side==-1  || side==3){
    # labels rotated 60degrees
    # Ca2+ & Mg2+
    srt <- 60
    delta <- ofs(srt)
    xd <- c(r * 15:12) + delta
    yd <- c(p * 7:10) + delta
    text(xd, yd, labels=labels, vfont=vfont, srt=srt, cex=cex.axis, adj=adj)
}
    if(side==-1 || side==4){
    # labels rotated 120degrees
    # Ca2+
    srt <- 120
    delta <- ofs(srt)
    xa <- c(p * 4:1) - delta
    ya <- c(0) + delta
    text(xa, ya, labels=labels, vfont=vfont, srt=srt, cex=cex.axis, adj=adj)
}

    if(side==-1 || side==5){
    # CO32- & HCO3-, triangle
    srt <- 120
    delta <- ofs(srt)
    xa <- c(r * 16:13) + delta
    ya <- c(p * 4:1) - delta
    text(xa, ya, labels=labels, vfont=vfont, srt=srt, cex=cex.axis, adj=adj)
}
    
    if(side==-1 || side==6){
    # CO32- & HCO3-, diamond
    srt <- 120
    delta <- ofs(srt)
    xa <- c(r * 15:12) - delta
    ya <- c(p * 5:2) + delta
    text(xa, ya, labels=labels, vfont=vfont, srt=srt, cex=cex.axis, adj=adj)
}
    if(side==-1 || side==7){
    # labels rotated 240degrees
    # Cl-
    srt <- 240
    delta <- ofs(srt)
    xb <- c(p * 7:10) - delta
    yb <- c(0, 0, 0, 0) - delta
    text(xb, yb, labels=labels, vfont=vfont, srt=srt, cex=cex.axis, adj=adj)
}
    if(side==-1 || side==8){
    # Na+ & K+, triangle
    srt <- 240
    delta <- ofs(srt)
    xb <- c(r * 6:9) + delta
    yb <- c(p * 4:1) + delta
    text(xb, yb, labels=labels, vfont=vfont, srt=srt, cex=cex.axis, adj=adj)
}
    if(side==-1 || side==9){
    # Na+ & K+, diamond
    srt <- 240
    delta <- ofs(srt)
    xb <- c(r * 7:10) - delta
    yb <- c(p * 5:2) - delta
    text(xb, yb, labels=labels, vfont=vfont, srt=srt, cex=cex.axis, adj=adj)
}
    if(side==-1 || side==10){
    # labels rotated 300degrees
    # SO42- & Cl-
    srt <- 300
    delta <- ofs(srt)
    xc <- c(r * 7:10) + delta
    yc <- c(p * 7:10) - delta
    text(xc, yc, labels=labels, vfont=vfont, srt=srt, cex=cex.axis, adj=adj)
}
}
  )

#' @describeIn piper Plot an object of class \code{piper}
#' @param type what type of plot should be drawn, only "p" for *p*oints is useful
#' @param cex magnification to be used for symbols relative
#' to the current setting of 'cex', see help("par")
#' @importFrom graphics points.default
#' @examples
#' lp <- piper(l)
#' plot( lp, main="Piper-Hill Diagram of Water Quality", cex=1.4 )
setMethod(
    f="plot",
    signature(x = "piper"),
    function(x, type = "p", cex=0.75, ...) # group=NULL,
        {
            ##if( ! x@plotted ){ callNextMethod() }
            callNextMethod() # calls piperplot.plot()

            if ( type == "p" ) { # i.e. 'points'
              by(x, x@WaterType,
                 function(j, ...) {
                     px <- c(j$anion.x, j$cation.x, j$diamond.x)
                     py <- c(j$anion.y, j$cation.y, j$diamond.y)
                     points.default(px, py, type="p", lty=1, lwd=1, pch=j$pt.pch,
                                    col=j$pt.col, bg=NA, cex=cex, ...)
                 },
                 x, ... )
          }
            invisible()
          }
          )

# Coercion functions
#' @export
as.data.frame.piper =
  function (x, ...)
  {
    as.data.frame.list( list( IDs=x@IDs, Ca=x@Ca, Mg=x@Mg, Cl=x@Cl,
                             SO4=x@SO4, group=x@group, WaterType=x@WaterType,
                             pt.pch=x@pt.pch, pt.col=x@pt.col,
                             anion.x=x@anion.x, anion.y=x@anion.y,
                             cation.x=x@cation.x, cation.y=x@cation.y,
                             diamond.x=x@diamond.x, diamond.y=x@diamond.y ) )
  }


# TODO: work on this

# @describeIn piper For checking the validity of an object of class \code{piper}
# @param object an object of class piper
# @name setValidity

#' @noRd
setValidity("piper",
            function(object)
            {
              n <- sort(names(object))
              f <- c("Ca", "Cl", "Mg", "SO4")
              if ( identical( intersect(n,f) , f ) ) TRUE
              else paste("ERROR: missing items: ", setdiff(f,n), sep="")

              a <- length(object[[1]])
              nms <- names(object)
              x <- 0
              for (i in object){
                x <- x + 1
                nme <- nms[x]
              
                if (length(i) != object@Ca) {
                  stop("ERROR: lengths of items differ") }
                if (nme != "IDs" && nme != "WaterType") {
                #print(nme)
                  for (j in i) {
                    #print(paste("j=",j))
                    if ( ! is.numeric(j) || j < 1)
                      stop("ERROR: there is a non-numeric or negative number")
                  }
                }
              }

              over100 <- object$row.name[ ((object$Ca + object$Mg) > 100 |
                                           (object$Cl + object$SO4) > 100) ]
              if (length(over100) != 0) {
                print(paste("ERROR: row.name == ",over100,
                       ", Either cations or anions add up to more than 100%.",
                       sep=""))
                return(invisible())      # represses echo
              }
            }
            )

#' @describeIn piper Show an object of class \code{piper}
#' @param object an object of class piper
#' @importFrom utils str
setMethod("show","piper",
          function(object){
              cat("Piper object: ")
              print(object@call)
              str(object)
          })
