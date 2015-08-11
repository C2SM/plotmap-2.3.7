\name{cmsaf.col}
\alias{cmsaf.col}
\title{ Color table from cmsaf }
\description{
  Color tables used in cmsaf. Has a total of 256 colors from dark blue to red.
}
\usage{
cmsaf.col()
}
\arguments{
}
\details{
   Color codes provided by Bruno Duerr
}
\value{
  An array of colors that can be used in color plotting.
}
\references{ }
\author{ Christoph Frei }
\note{  }
\seealso{ \code{\link{rainbow}}, 
          \code{\link{heat.colors}},
          \code{\link{cool.colors}},
          \code{\link{warm2cool.colors}},
          \code{\link{cm.colors}},
          \code{\link{terrain.colors}},
          \code{\link{topo.colors}},
          \code{\link{rainbow10}},
          \code{\link{rainbow9}} }
\examples{
demo.palette(n=256,ch.col=c("cmsaf.col()"))

}
\keyword{ color }% at least one, from doc/KEYWORDS
