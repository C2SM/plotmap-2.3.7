\name{continents}
\alias{continents}
\docType{data}
\title{ Continents' outline of the globe. }
\description{
  A high resolution dataset with geographic longitude / latitude of the 
  the sea shore for the entire globe. Continents, islands etc. For plotting
  on a map.
}
\usage{data(continents)}
\format{
  A list with two components
  $ x: geographic longitude in degrees
  $ y: geographic latitude in degrees
  An interrupt of the line is indicated with an intermittent NA.
  Suitable for use with \code{lines}.
}
\details{
  This dataset is used in \code{plot.map} when \code{mapdat}="continents"
}
\source{
  Data taken from package clim.pact
}
\references{
  Ask Rasmus Benestad.
}
\examples{
data(continents)
}
\keyword{datasets}
