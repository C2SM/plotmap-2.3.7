\name{in.polygon.pm}
\alias{in.polygon.pm}
\title{Locate a point within/outside a polygon}
\description{
  Determine if a point in 2D space is located within, on the border
  or outside a polygon.  
}
\usage{
in.polygon.pm(pt, poly, output="logical", method="crossings")
}

\arguments{
  \item{pt}{a 2-element vector with the point's x,y coordinates}
  \item{poly}{a 2-column matrix with the polygons x and y coordinates in
              column 1 and 2 respectively. The number of rows must be 
              longer than 2. Otherwise the function returns a warning 
              but is not interrupted.}
  \item{output}{a string specifying the mode of the ouput. Possible 
              options are \code{"logical"} (TRUE : pt is within, FALSE: outside,
              NA: on the border of the polygon) or \code{"numeric"} (1 : pt is 
              within, -1: outside, 0: on the border of the polygon).}
  \item{method}{a string specifying the method of calculation. Possible 
              options are \code{"angles"} or \code{"crossings"} (see
              details).}
}
\details{
  Two different algorithms are implemented:

  If \code{method="angles"} then the
  function sums up angles that result if an imaginary observer 
  standing at point \code{pt} is watching along the sides of the polygon
  \code{poly}. This algorithm uses vector products in 2D to calculate
  the partial angles. If the total angle is \code{2*pi} then the point
  is inside the polygon.

  If \code{method="crossings"} then the
  function counts the number of times, the polygon crosses the positive
  side of the x-Axis (if the point is at the coordinate origin). If the
  number count is odd, the point is inside the polygon.

  Method \code{method="crossings"} is almost twice as fast and therefore
  is set as the default.
}

\value{
  If \code{output}="logical": TRUE (within), FALSE (outside)
  NA (on border)

  If \code{output}="numeric": 1 (within), -1 (outside), 0 (on border)
}

\author{ Christoph Frei }

\seealso{}
\examples{

# Square polygon
polgn <- matrix(c(c(0,1,1,0),c(0,0,1,1)),byrow=FALSE,ncol=2)

in.polygon.pm(pt=c(0.3,0.1),poly=polgn,output="logical")   # is inside

in.polygon.pm(pt=c(0.3,0),poly=polgn,output="logical")    # is on border

in.polygon.pm(pt=c(0.3,-6),poly=polgn,output="logical")    # is outside

}
\keyword{ math }% at least one, from doc/KEYWORDS
