\name{pretty.contours}
\alias{pretty.contours}
\title{ Pretty contours levels }
\description{
  This is essentially the same like built-in function \code{pretty}
  but it cuts off the first and last element of the output of \code{pretty}
  so that the outermost colors are occupied too in a filled contour plot 
  or an image plot.
}
\usage{
pretty.contours(...)
}
\arguments{
  \item{...}{ All arguments are passed on to \code{pretty} }
}
\details{
  
}
\value{
  A vector of pretty levels to be used as contour levels/breaks.
}
\references{  }
\author{ Christoph Frei }
\note{  }

\seealso{ \code{\link{pretty}} }
\examples{
}
\keyword{ dplot }% at least one, from doc/KEYWORDS
