\name{at.dem}
\alias{at.dem}
\docType{data}
\title{ 1-km DEM for Austria }
\description{
  A 1-km DEM for Austria, derived from SRTM.
}
\usage{
  data(at.dem)
}
\format{
  A matrix with terrain elevation in meters on a grid in Austrian coordinates.
  Attributes of the matrix specify the coordinate system and grid point
  coordinates.

}
\details{

}
\source{
  Obtained from Johann Hiebl (ZAMG).
}
\references{
  
}
\examples{
data(at.dem)
attributes(at.dem)
image(at.dem)

}
\keyword{datasets}
