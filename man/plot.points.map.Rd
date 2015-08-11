\name{plot.points.map}
\alias{plot.points.map}
\title{ Coloured symbols on a map. }
\description{
  Plot symbols (typically colour-filled circles - smarties) 
  on a geographic map. The position of a plot symbol is specified by its
  coordinates, using a geographical coordinate system supported by geocors. 
  The colour of a symbol is
  determined by a numeric value (different for each symbol) using a colour 
  coding similar to that in a filled countour plot. 
}
\usage{
plot.points.map(vals, lon, lat, breaks, nbreaks = 12, 
           col = "cool2warm.colors", 
           na.col = "grey", pch = 19, cex.pch = 1, 
           plot.fun = "plot.map", add = FALSE, ...)
}
\arguments{
  \item{vals}{ A data.frame or a matrix with at least three columns. Two columns
         for symbol coordinates and one column for symbol values.
         Coordinates values are in a geographic coordinate system 
         that is supported by package \pkg{geocors} and the corresponding
         columns need to be appropriately named. The value column
         determines the plot colour of the symbols together with \code{breaks}
         and \code{col}. If more 
         than three columns are provided the first non-coordinate column is
         assumed to be the symbol value column and any further
         columns are neglected. \code{vals} needs be accompanied by 
         attributes about the geographic coordinate system. 
         An older version of this function is still supported, where \code{vals} 
         is a numeric vector with symbol values and arguments \code{lon} and 
         \code{lat} are regular longitude and latitude coordinates. This
         input version is however discouraged. }
  \item{lon}{ Only for compatibility with an older version of the function: 
  	     A vector with geographic longitudes of plot symbols. }
  \item{lat}{ Only for compatibility with an older version of the function: 
  	     A vector with geographic latitudes of plot symbols.  }
  \item{breaks}{ The breaks to use for the colour coding. If missing 
         \code{breaks} is defined internally using approximately \code{nbreaks} 
         levels. }
  \item{nbreaks}{  Approximate number of breaks to use if \code{breaks} is not
  	     provided. }
  \item{col}{ A vector with colours (one more than breaks). Alternatively,
         the name of a color function can be specified. In that case 
         \code{breaks+1} colors will be selected from that colour function. }
  \item{na.col}{ The color to use for depicting NAs in symbol values. }
  \item{pch}{ The character or symbol to use. This is the same as the
         argument in \code{points()}. Default is a full circle (smarties). }
  \item{cex.pch}{ The amplification factor for the symbol. Usually this is
  	     a single value, but it can
         be a vector (same length as rows in \code{val}),
         if each symbol should be plotted with a different size. }
  \item{plot.fun}{ The plot function to use for preparing the geographic 
         map. This function is called to prepare the underlying geographic map.}
  \item{add}{ Whether the symbols shall be added on a previously 
         generated map. }
  \item{\dots}{ Additional arguments to the function specified in
  	     \code{plot.fun}. Here, the 
         projection could be specified, or the type of map outlines 
         that should be drawn. }
}
\details{
  
}
\value{
  Just a plot.
}
\references{  }
\author{ Christoph Frei }
\note{ }

\seealso{ \code{\link{plot.map}}, \code{\link{plot.swiss.map}}, 
          \code{\link{plot.poly.map}} }
\examples{
# Example 1  (Filled circle symbols on a Swiss map)
data(swiss.rain)
plot.points.map(vals=swiss.rain,breaks=c(2,5,10,20,40,60,80,100,150),
                plot.fun="plot.swiss.map",
                col=rainbow10(),legend=TRUE,leg.space=0.14)

# Example 2 (Filled circle symbols on an arbitrary map)
data(swiss.rain)
plot.points.map(vals=swiss.rain,
                breaks=c(2,5,10,20,40,60,80,100,150),col=rainbow10(),
                plot.fun="plot.map",cex.pch=0.5,
                lonlim=c(2,18),latlim=c(43,50),  # map borders
                xmaplim=c(0,20),ymaplim=c(42,52),  # mapoutlines
                legend=TRUE,leg.space=0.16,
                mapdat="worldHires",thin.map=4)

# Example 3 (Symbols with variable symbol size)
data(swiss.rain)
size.fun <- function(x) {ifelse(x>100,1,0.5)}
sizes <- sapply(swiss.rain[,"20050822"],FUN=size.fun)
plot.points.map(vals=swiss.rain,
                breaks=c(2,5,10,20,40,60,80,100,150),
                plot.fun="plot.swiss.map",
                cex.pch=sizes,
                col=rainbow10(),legend=TRUE,leg.space=0.14)

}
\keyword{ hplot }% at least one, from doc/KEYWORDS
