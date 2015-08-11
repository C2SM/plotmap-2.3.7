\name{window.lims}
\alias{window.lims}
\title{ Window limits for map outlines }
\description{
  For a window in a geographic coordinate system to be plotted determine a 
  window in longitude-latitude coordinates (or projection coordinates)
  comprising the entire plot window. This function is used by internal
  functions to determine the default plot-window. 
}
\usage{
window.lims(xlim, ylim, grid.type = "lonlat", grid.pars = list(), 
            out = "proj", projection, parameters, orientation, npts = 100)
}
\arguments{
  \item{xlim}{ A two element vector giving xlims of a field to be plotted.
      Elements are in units of a geographic coordinate system specified
      in argument \code{grid.type}. }
  \item{ylim}{ Dito for y. }
  \item{grid.type}{ Coordinate system. }
  \item{grid.pars}{ Additional parameters of the coordinate system if needed. }
  \item{out}{ Output requested: If \code{out="proj"} the output is given 
      in projection coordinates. If \code{out="lonlat"} the output is given 
      in longitude-latitude coordinates. }
  \item{projection}{ The projection. }
  \item{parameters}{ The parameters of the projection. }
  \item{orientation}{ The orientation of the projection. }
  \item{npts}{ The numbers of points for sequencing the window.}
}
\details{
  Internal use
}
\value{
  A two element list with:
  \item{xlim }{ A two-element vector with the window limits in longitude or
     projection x coordinate. }
  \item{comp2 }{ A two-element vector with the window limits in latitude or
     projection y coordinate. }
}
\references{  }
\author{ Christoph Frei }
\note{ 
}
\seealso{  }
\examples{
}
\keyword{ manip }
