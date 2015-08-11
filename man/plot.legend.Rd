\name{plot.legend}
\alias{plot.legend}
\title{ Color legend for plot.map }
\description{
  Produces a color legend accompaning an image plot or a filled 
  contour plot from plot.map. This is usually not called by the user
  directly but by plot.map. See also description of \code{plot.map} 
}
\usage{
plot.legend(col, breaks, 
            density = NULL, angle = 45, slwd = par("lwd"), 
            cex.leg = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{col}{ A vector of colors. }
  \item{breaks}{ A vector of breaks (length is one smaller 
         than \code{length(col)} }
  \item{density}{ Density of shading lines (see \code{plot.map}) }
  \item{angle}{ Angel of shading lines  }
  \item{slwd}{ Line width of shading lines }
  \item{cex.leg}{ Magnification of legend annotations }
}
\details{
 
}
\value{
  A legend plot, no numerical object.
}
\references{  }
\author{ Christoph Frei }
\note{ }

\seealso{ \code{\link{plot.map}} }
\examples{
}
\keyword{ hplot }% at least one, from doc/KEYWORDS
