\name{clip.swiss}
\alias{clip.swiss}
\title{ Clip areas outside swiss border in a plot }
\description{
  Areas outside the border of Switzerland/Liechtenstein are clipped
  from a plot, i.e. wiped from the plot.
}
\usage{
clip.swiss(col = "white")
}
\arguments{
  \item{col}{ The color with witch the area outside should be coloured. }
}
\details{
   This is usually invoked by the user only via option \code{clip}
   in \code{plot.swiss.map}. In order to use clip.swiss a plot needs
   to be made beforehand. Actually the function draws a colour filled
   polygon around Switzerland and along the plot boundaries.
}
\value{
  No values returned, just the plot modified.
}
\references{  }
\author{ Christoph Frei }
\note{  }
\seealso{ \code{\link{plot.swiss.map}} }
\examples{
}
\keyword{ hplot }% at least one, from doc/KEYWORDS
