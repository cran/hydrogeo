\name{piper}
\alias{piper}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Piper-Hill Diagram }
\description{
Plots Piper-Hill diagrams of water quality
}
\usage{
piper(data, group = NULL, colours = NULL, pch = NULL, numbersymbols = FALSE, X = 300, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{ Plots Piper-Hill diagrams of water quality }
  \item{group}{ A dataframe : row.names=sampleids, and columns named "Ca", "Mg", "Cl", "SO4" and optionally "WaterType". }
  \item{colours}{ Optional: vector of colours for points. }
  \item{pch}{ Optional: vector of symbols for points. }
  \item{numbersymbols}{ Logical: if TRUE then plots sequential integer characters instead of symbols within groups. }
  \item{X}{ Optional: length of the plot area defaults to 300. }
  \item{\dots}{ Optional: any other parameters (e.g. to par()). }
}
\details{
  ~~ If necessary, more details than the description above ~~
}
\value{
  NULL
}
\references{  }
\author{ Myles English }
\note{ }
\seealso{  }
\examples{

data=as.data.frame(list("Ca"=c(43,10,73,26,32),"Mg"=c(30,50,3,14,12),Cl=c(24,10,12,30,43),"SO4"=c(24,10,12,30,43),"WaterType"=c(2,2,1,2,3)),row.names=c("A","B","C","D","E"))

piper(data)

## The function is currently defined as
function (data, group = NULL, colours = NULL, pch = NULL, numbersymbols = FALSE, 
    X = 300, ...) 
{
    p <- (X/11)
    q <- (X/22)
    over100 <- data[data$Ca + data$Mg > 100 | data$Cl + data$SO4 > 
        100, ]
    if (length(over100[, 1]) != 0) {
        print("ERROR")
        print("Either cations or anions add up to more than 100\% (even with calculating the third anion 'by difference'.")
        print(over100)
        return(invisible())
    }
    if (!is.null(group)) {
        data$group <- group
    }
    else {
        if (!is.null(data$WaterType)) {
            names(data)[(names(data) == "WaterType")] <- "group"
        }
        else {
            data$group <- c(rep(1, length(data[, 1])))
        }
    }
    if (!is.null(colours)) {
        data$col <- colours
    }
    else {
        data$col <- as.factor(data$group)
        levels(data$col) <- seq(1:length(levels(data$col)))
        data$col <- as.character(data$col)
    }
    if (!is.null(pch)) {
        data$pch <- pch
    }
    else {
        data$pch <- c(1)
        for (i in levels(as.factor(data$group))) {
            j <- data$group == i
            data$pch[j] <- seq(1:length(j[j == TRUE]))
            if (numbersymbols == TRUE) 
                data$pch <- as.character(data$pch)
        }
    }
    samp <- paste(1:nrow(data), sep = "")
    old.par <- par(no.readonly = TRUE)
    on.exit(par(old.par))
    par(omi = c(0, 0, 1, 0), mar = c(1, 0, 1, 0))
    plotpaper <- function(X, ...) {
        plot(0, 0, type = "n", axes = FALSE, lty = 1, lwd = 1, 
            cex = TRUE, xlim = c(0, X + p), ylim = c(-p, X), 
            frame.plot = FALSE, ann = TRUE, ylab = "", xlab = "", 
            ...)
        thickxf <- c(0, (5 * p), (5 * q), (6 * p), X, (17 * q), 
            (X/2), (8 * p), (X/2), (3 * p))
        thickxt <- c((5 * p), (5 * q), 0, X, (17 * q), (6 * p), 
            (8 * p), (X/2), (3 * p), (X/2))
        thickyf <- c(0, 0, (5 * p), 0, 0, (5 * p), p, (6 * p), 
            X, (6 * p))
        thickyt <- c(0, (5 * p), 0, 0, (5 * p), 0, (6 * p), X, 
            (6 * p), p)
        xf <- c(thickxf, p, (2 * p), (3 * p), (4 * p), (7 * p), 
            (8 * p), (9 * p), (10 * p), (21 * q), (20 * q), (19 * 
                q), (18 * q), (21 * q), (20 * q), (19 * q), (18 * 
                q), (9 * q), (8 * q), (7 * q), (6 * q), (9 * 
                q), (8 * q), (7 * q), (6 * q), (7 * q), (8 * 
                q), (9 * q), (10 * q), (7 * q), (8 * q), (9 * 
                q), (10 * q))
        xt <- c(thickxt, q, (2 * q), (3 * q), (4 * q), (13 * 
            q), (14 * q), (15 * q), (16 * q), (13 * q), (14 * 
            q), (15 * q), (16 * q), (10 * p), (9 * p), (8 * p), 
            (7 * p), (4 * p), (3 * p), (2 * p), p, q, (2 * q), 
            (3 * q), (4 * q), (12 * q), (13 * q), (14 * q), (15 * 
                q), (12 * q), (13 * q), (14 * q), (15 * q))
        yf <- c(thickyf, 0, 0, 0, 0, 0, 0, 0, 0, p, (2 * p), 
            (3 * p), (4 * p), p, (2 * p), (3 * p), (4 * p), p, 
            (2 * p), (3 * p), (4 * p), p, (2 * p), (3 * p), (4 * 
                p), (7 * p), (8 * p), (9 * p), (10 * p), (5 * 
                p), (4 * p), (3 * p), (2 * p))
        yt <- c(thickyt, p, (2 * p), (3 * p), (4 * p), p, (2 * 
            p), (3 * p), (4 * p), p, (2 * p), (3 * p), (4 * p), 
            0, 0, 0, 0, 0, 0, 0, 0, p, (2 * p), (3 * p), (4 * 
                p), (2 * p), (3 * p), (4 * p), (5 * p), (10 * 
                p), (9 * p), (8 * p), (7 * p))
        segments(xf, yf, xt, yt, col = par("fg"), lty = 1, lwd = par("lwd"))
        xstr <- c(5 * q, 17 * q)
        ystr <- c(-q, -q)
        text(xstr, ystr, labels = c("Ca2+", "Cl-"), adj = NULL, 
            pos = NULL, offset = 0, vfont = c("serif", "italic"), 
            cex = 0.7, col = NULL, font = NULL, xpd = NULL)
        xgh <- c(14 * q, 4 * p, 20 * q)
        ygh <- c(9 * p, 3 * p, 3 * p)
        text(xgh, ygh, labels = c("Ca2+ & Mg2+", "Na+ & K+", 
            "SO42-"), adj = NULL, srt = 300, pos = NULL, offset = 0, 
            vfont = c("serif", "italic"), cex = 0.7, col = NULL, 
            font = NULL, xpd = NULL)
        xla <- c(7 * p, 8 * q, 2 * q)
        yla <- c(3 * p, 18 * q, 3 * p)
        text(xla, yla, labels = c("CO32- & HCO32-", "SO42- & Cl-", 
            "Mg2+"), adj = NULL, srt = 60, pos = NULL, offset = 0, 
            vfont = c("serif", "italic"), cex = 0.7, col = NULL, 
            font = NULL, xpd = NULL)
        t <- q/4
        ylabs <- c(p * 0:11, p * 5:0, p * 6:1, 0, 0, 0, 0, 0, 
            0)
        xlabs <- c(q * 0:11, q * 5:10, q * 6:11, p * 5:0, q * 
            22:11, q * 17:12, q * 16:11, p * 6:11)
        xa <- c(p * 4:1 + t, q * 16:13 - t, q * 15:12 + t)
        ya <- c(-t, -t, -t, -t, p * 4:1 + t, p * 5:2 - t)
        text(xa, ya, labels = c(20 * (1:4)), adj = NULL, pos = NULL, 
            offset = 0, vfont = c("serif", "italic"), srt = 120, 
            cex = 0.5, col = NULL, font = NULL, xpd = NULL)
        xb <- c(p * 7:10 - t, q * 6:9 + t, q * 7:10 - t)
        yb <- c(-t, -t, -t, -t, p * 4:1 + t, p * 5:2 - t)
        text(xb, yb, labels = c(20 * (1:4)), adj = NULL, pos = NULL, 
            offset = 0, vfont = c("serif", "italic"), srt = 240, 
            cex = 0.5, col = NULL, font = NULL, xpd = NULL)
        xc <- c(q * 7:10 - t)
        yc <- c(p * 7:10 + t)
        text(xc, yc, labels = c(20 * (1:4)), adj = NULL, pos = NULL, 
            offset = 0, vfont = c("serif", "italic"), srt = c(300), 
            cex = 0.5, col = NULL, font = NULL, xpd = NULL)
        xd <- c(q * 15:12 + t)
        yd <- c(p * 7:10 + t)
        text(xd, yd, labels = c(20 * (1:4)), adj = NULL, pos = NULL, 
            offset = 0, vfont = c("serif", "italic"), srt = c(60), 
            cex = 0.5, col = NULL, font = NULL, xpd = NULL)
        xe <- c((q * 1:4 - t), (q * 21:18 + t))
        ye <- c(p * 1:4 + t)
        text(xe, ye, labels = c(20 * (1:4)), adj = NULL, pos = NULL, 
            offset = 0, vfont = c("serif", "italic"), srt = NULL, 
            cex = 0.5, col = NULL, font = NULL, xpd = NULL)
    }
    plotpoints <- function(d, X, ...) {
        Ca <- as.numeric(d$Ca)
        Mg <- as.numeric(d$Mg)
        Cl <- as.numeric(d$Cl)
        SO4 <- as.numeric(d$SO4)
        cationy <- c(Mg * 5 * X/1100)
        cationx <- c((5 * X/11) * (1 - (Ca/100) - (Mg/200)))
        aniony <- c(SO4 * 5 * X/1100)
        anionx <- c((6 * X/11) + ((5 * X/11) * (Cl/100)) + (1/2) * 
            (5 * X/11) * (SO4/100))
        projx <- ((1/2) * (cationx + anionx) + ((1/4) * (aniony - 
            cationy)))
        projy <- (anionx - cationx + ((1/2) * (aniony + cationy)))
        px <- c(anionx, cationx, projx)
        py <- c(aniony, cationy, projy)
        points(px, py, type = "p", lty = 1, lwd = 1, pch = d$pch, 
            col = d$col, cex = 0.75)
    }
    ylegend <- X
    plotlegend <- function(dd, X, ...) {
        xid <- c(X + p)
        yid <- c(ylegend - (X/30) * 1:nrow(dd))
        text(xid, yid, labels = row.names(dd), vfont = c("serif", 
            "plain"), cex = 0.7)
        if (is.character(dd$pch)) {
            text(xid - (X/15), yid, labels = c(1:nrow(dd)), vfont = c("serif", 
                "plain"), cex = 0.7)
        }
        else {
            points(rep(xid - (X/15), length(yid)), yid, pch = dd$pch, 
                col = dd$col)
        }
        return(min(yid))
    }
    plotpaper(X, ...)
    by(data, data$group, plotpoints, X, ...)
    for (dg in levels(as.factor(data$group))) {
        ylegend <- plotlegend(data[data$group == dg, ], X, ...)
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{  }
\keyword{  }% __ONLY ONE__ keyword per line
