\name{rainbow10}
\alias{rainbow10}
\alias{rainbow9}
\alias{prism.colors}
\title{ Color tables added to built-in R tables }
\description{
  Variants of the built-in \code{rainbow} for better printer
  contrast. With fix number of colors (9 and 10). \code{prism.colors}
  is a rainbow like color table (11 colors) but with adjustments to make it
  suitable for precipitation mapping. 
}
\usage{
rainbow9()
rainbow10()
}
\arguments{
}
\details{
  Colors selected manually from \code{\link{rainbow}}.
}
\value{
  An array of colors that can be used in color plotting.
}
\references{ The color table prism.colors is from the PRISM Climate
    Group of the Oregon State University. 
    http://prism.oregonstate.edu
}
\author{ Christoph Frei }
\note{  }
\seealso{ \code{\link{rainbow}}, 
          \code{\link{heat.colors}},
          \code{\link{cool.colors}},
          \code{\link{cm.colors}},
          \code{\link{terrain.colors}},
          \code{\link{topo.colors}} }
\examples{
demo.palette(n=10,ch.col=c("rainbow10()"))
demo.palette(n=9,ch.col=c("rainbow9()"))
demo.palette(n=11,ch.col=c("prism.colors()"))
}
\keyword{ color }% at least one, from doc/KEYWORDS
