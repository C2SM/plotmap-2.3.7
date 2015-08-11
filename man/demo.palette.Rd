\name{demo.palette}
\alias{demo.palette}
\title{ Plot example for color tables. }
\description{
  Produces a sample plot showing colors of color tables. 
}
\usage{
demo.palette(n, border = if (n<32) "light gray" else NA, 
             main = paste("color palettes;  n=",n),
             ch.col = c("rainbow(n, start=.7, end=.1)", "heat.colors(n)",
                      "terrain.colors(n)", "topo.colors(n)", "cm.colors(n)") )
}
\arguments{
  \item{n}{ The number of colors to use in each table. }
  \item{border}{ The color of the box borders. }
  \item{main}{ The plot title. }
  \item{ch.col}{ A vector of strings, specifying the color tables to be 
         plotted. }
}
\details{
  This function was taken from an R built-in help page. 
}
\value{
  A plot of the color tables.
}
\references{  }
\author{ Christoph Frei }
\note{  }

\seealso{ \code{\link{plot.map}} }
\examples{
demo.palette(15)
demo.palette(10,ch.col=c("rainbow10()","warm2cool.colors(10)"))
}
\keyword{ color }% at least one, from doc/KEYWORDS
