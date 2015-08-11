\name{swiss.rain}
\alias{swiss.rain}
\docType{data}
\title{ Example dataset for plotting points on a map. }
\description{
  A dataframe with precipitation measurements from swiss rain gauges
  for 2005.08.22. 
}
\usage{data(swiss.rain)}
\format{
  A data frame with 482 observations on the following 5 variables.
  \describe{
    \item{\code{lon}}{geographic longitude (degrees)}
    \item{\code{lat}}{geographic latitude (degrees)}
    \item{\code{elev}}{elevation in m.a.s.l}
    \item{\code{20050822}}{accumulated daily precip in mm}
    \item{\code{name}}{names of the stations}
  }
}
\details{
  The data object also has an attribute ("grid.type") attached, which is
  necessary for working with \code{plot.points.map}.
}
\source{
  Data from MeteoSwiss and SLF
}
\references{
  
}
\examples{
data(swiss.rain)
}
\keyword{datasets}
