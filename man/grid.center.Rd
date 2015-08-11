\name{grid.center}
\alias{grid.center}
\title{ Central location of a grid }
\description{
  Determines for a regular grid the centre, i.e. the location that is 
  obtained when averaging separately over the x and the y coordinates of the
  grid. The center can be returned in a coordinate system different from 
  that of the grid.
}
\usage{
grid.center(fld, to.type="lonlat", ...)
}
\arguments{
  \item{fld}{ The grid with pertinent attributes on the coordinate system. }
  \item{to.type}{ The name of the target coordinate system in which the 
                  center location is requested. }
  \item{...}{ An additional \code{to.pars} argument if the target coordinate
              system requires parameters. This is just passed over to function
              \code{geocors.trafo}. }
}
\details{
   This function is suitable to determine a suitable orientation parameter 
   when plotting a field with stereographic projection.
}
\value{
  A named list with the center's x and y coordinates (list elements 1 and 2) 
  using names for the coordinates defined in package \code{geocors}. The
  list carries attributes of the coordinate system and parameters.  
}
\references{  }
\author{ Christoph Frei }
\note{  }
\seealso{ }
\examples{
data(at.dem)
grid.center(at.dem,to.type="lonlat")
}
\keyword{ hplot }% at least one, from doc/KEYWORDS
