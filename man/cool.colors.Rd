\name{cool.colors}
\alias{cool.colors}
\alias{cool2warm.colors}
\alias{warm2cool.colors}
\title{ Color tables added to built-in R tables }
\description{
  Color tables with continous colors. \code{cool.colors} is like
  \code{warm.colors} but a sequence from light blue to full blue. 
  \code{warm2cool.colors} is a combination of \code{cool.colors} and
  \code{warm.colors} from full red to full blue. For an odd number
  the central value is white. \code{cool2warm.colors} is simply the
  reverse of \code{warm2cool.colors}.
}
\usage{
cool.colors(n)
cool2warm.colors(n, middle="#FFFFFF")
warm2cool.colors(n, middle="#FFFFFF")
}
\arguments{
  \item{n}{ The number of colors to be returned. }
  \item{middle}{ The central color to be used if \code{n} is odd. 
                 Default is white.}
}
\details{

}
\value{
  An array of colors that can be used in color plotting.
}
\references{ Implemented in analogy to built-in R color tables. 
             See e.g. \code{\link{rainbow}}. }
\author{ Christoph Frei }
\note{  }
\seealso{ \code{\link{rainbow}}, 
          \code{\link{heat.colors}},
          \code{\link{cm.colors}},
          \code{\link{terrain.colors}},
          \code{\link{topo.colors}},
          \code{\link{wb.prec.col}},
          \code{\link{rainbow10}},
          \code{\link{rainbow9}} }
\examples{
demo.palette(n=16,ch.col=c("rainbow(n, start=.7, end=.1)", 
                       "heat.colors(n)",
                       "cool.colors(n)", 
                       "warm2cool.colors(n)", 
                       "topo.colors(n)", 
                       "terrain.colors(n)", 
                       "cm.colors(n)"))

}
\keyword{ color }% at least one, from doc/KEYWORDS
