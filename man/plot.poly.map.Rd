\name{plot.poly.map}
\alias{plot.poly.map}
\title{ Polygon graphs on a map. }
\description{
  Plot a sequence of closed polygons on a map where the fill colour is
  determined from a numerical value associated to the polygon.
}
\usage{
plot.poly.map(vals, polys, lonlim=c(6.0,10.4), latlim=c(45.85,47.8),
        breaks, nbreaks = 12, col = "cool2warm.colors", 
        na.col = "grey", poly.border = "grey70", poly.lwd = 1, 
        plot.fun = "plot.map", add = FALSE, legend = TRUE, mapdat = "world",
        thin.map=0, map.col = "black", map.lwd = 1.5, clip = FALSE, ...)
}
\arguments{
  \item{vals}{ A vector with the values to depict as coloured polygons. }
  \item{polys}{ A list with polygon coordinates. Each list element is a
           data.frame with two columns giving the x and y coordinates 
           in the geographic coordinate system used. \code{polys} has
           attached attributes (the grid.type and grid.pars components) 
           that specify the geographic coordinate
           system. }
  \item{lonlim, latlim}{ Defines the lon/lat window to be plotted. 
           Defaults appropriate for Switzerland. }
  \item{breaks}{ The breaks to use for the color coding. If missing 
         breaks is set internally using approximately \code{nbreaks} 
         levels. }
  \item{nbreaks}{  Approximate number of breaks to use. }
  \item{col}{ A vector with colors (one more than breaks). Alternatively,
         the name of a color function can be specified. In that case the
         number of colors to select is the length of breaks plus 1. }
  \item{na.col}{ The color to use for NAs in \code{val}. }
  \item{poly.border}{ The colour to use for outlining the polygons. }
  \item{poly.lwd}{ The line width to use for outlining the polygons. }
  \item{plot.fun}{ The plot function to use for preparing the geographic 
         map. E.g. for plotting on a swiss map \code{plot.swiss.map}}
  \item{add}{ Whether the polygons shall just be added on a previously 
         generated map.  }
  \item{mapdat}{ The type of map outlines that should be drawn. See also 
         the help packes of \code{plot.map} and \code{plot.swiss.map}. }
  \item{legend}{ If a legend shall be plotted alongside. }
  \item{thin.map}{ Number of iterations to thin out map outlines. }
  \item{map.col}{ Colour to use for map outlines (borders, continents). }
  \item{map.lwd}{ Line width to use for map outlines. }
  \item{clip}{ Whether to clip away areas outside Switzerland. Only 
         active if \code{plot.fun="plot.swiss.map"} }
  \item{\dots}{ Additional arguments to \code{plot.fun}. Here the 
         projection could be specified.  }
}
\details{
  
}
\value{
  Just a plot.
}
\references{ }
\author{ Christoph Frei }
\note{  }

\seealso{ \code{\link{plot.map}}, \code{\link{plot.swiss.map}}, 
          \code{\link{plot.points.map}} }
\examples{
data(swiss.catch.expl)
area <- swiss.catch.expl$area
polys <- swiss.catch.expl$polygon
X11(width=8,height=5.5)
plot.poly.map(vals=area, polys=polys, breaks=c(50,100,150,200,300,400), 
        plot.fun = "plot.swiss.map", clip = TRUE, 
        mapdat = "swiss:border&lakes&rivers")
dev.off()

}
\keyword{ hplot }% at least one, from doc/KEYWORDS
