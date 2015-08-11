\name{contourSlices}
\alias{contourSlices}
\title{ Calculate contour lines and return them as stacked polygons. }
\description{
  This is the core of filled contour plotting in plot.map. Calculate contour 
  lines and stack them in appropriate order for subsequent filled colored 
  plotting. 
}
\usage{
contourSlices(x = seq(0, 1, len = nrow(z)), 
              y = seq(0, 1, len = ncol(z)), z, nlevels = 10, 
              breaks = pretty.contours(range(z, na.rm = TRUE), n=nlevels) )
}
\arguments{
  \item{x}{ An array with grid points in x direction. 
            Same length as rows of \code{z}}
  \item{y}{ An array with grid points in x direction. 
            Same length as columns of \code{z} }
  \item{z}{ A matrix with the values to contour. }
  \item{nlevels}{ Number of desired contour levels. }
  \item{breaks}{ Breaks to use as contour levels. }
}
\details{
  This function is primarily used by plot.map for the case of filled
  contours. It is not meant to be used by the user directly. Contour
  lines are determined using R's built-in \code{coutoursLines}.
}
\value{
  A list of list-objects for each closed contour. Individual list
  objects have the following components:
  \item{level }{The level of the contour. Corresponding to one value
        from breaks. It is possible to have more than one contour line
        per level.}
  \item{x }{The x-coordinates of the closed contour line (polygon).}
  \item{y }{The y-coordinates of the closed contour line (polygon).}
  \item{hill }{TRUE if the inner portion of the polygon
        is larger than \code{level} otherwise FALSE.}
  \item{stack.level }{An integer giving the order to be used in plotting
        color filled polygons. A \code{stack.level} of 1 should be drawn 
        first, etc.. It is possible that several polygons have the same
        \code{stack.level}. Among these the plot order does not matter.}
}
\references{  }
\author{ Christoph Frei }
\note{  }

\seealso{ \code{\link{plot.map}} }
\examples{
xx <- seq(-2.5,4.5,length=100)
yy <- seq(-2.5,4.5,length=100)
xmat <- matrix(rep(xx,times=length(yy)),ncol=length(xx),
               nrow=length(yy),byrow=TRUE)
ymat <- matrix(rep(yy,times=length(xx)),ncol=length(xx),
               nrow=length(yy),byrow=FALSE)
zz <- cos(sqrt(xmat^2+ymat^2))
conts <- contourSlices(x=xx,y=yy,z=zz,breaks=c(-0.7,-0.3,0.0,0.3,0.7))

}
\keyword{ dplot }% at least one, from doc/KEYWORDS
