\name{plain.map}
\alias{plain.map}
\title{ Map outlines on a map }
\description{
  Draw a plain map of a geographic window with map outlines.
  It is recommended to invoke this function via function \code{plot.map}, 
  with \code{ptype="plain"} which provides more flexibility.

}
\usage{
plain.map(x = NULL, y = NULL, 
     grid.type = "lonlat", grid.pars = list(), 
     lonlim, latlim, 
     projection = "", parameters = NULL, orientation = NULL, 
     mapdat = "world", xmaplim, ymaplim, thin.map = 0, 
     add.grid = TRUE, box = TRUE, ...)
}
\arguments{
  \item{x}{ Vector of x-locations. If specified the geographic window is 
            defined such that the window defined by \code{c(range(x),range(y))}
            is fully displayed. }
  \item{y}{ Vector of x-locations. }
  \item{grid.type}{ A character string with the name of the coordinate
            system used for \code{x} and \code{y}. }
  \item{grid.pars}{ A list with additional parameters of the coordinate
            system used for \code{x} and \code{y}. An empty list if no
            additional parameters are needed. }
  \item{lonlim, latlim}{ Two element numeric vectors. 
            Define the lon/lat window to be plotted. 
            If specified the plot window is chosen such 
            that the entire longitude-latitude section defined by
            \code{lonlim} and \code{latlim}) is 
            comprised. If not specified the window is chosen 
            to display the entire field, i.e. such that min/max of 
            \code{x} and \code{y} are within the plot.}
  \item{projection}{ One of the projections provided by mapproject. 
            E.g. \code{projection}="stereographic" . 
            If an empty string is given (the default), the projection 
            used in the last application of mapproject will be used. 
            This is useful for plotting on an existing map. (Note: The 
            last projection settings are stored in variable 
            \code{.Last.projection}. A list of possible projections 
            is given in the help to \code{mapproject}. }
  \item{parameters}{ The orientation parameter of mapproject. (See 
            help to \code{mapproject}). (Used rarely.) }
  \item{orientation}{ The orientation parameter of the projection. (See 
            help to \code{mapproject}). For Europe as a whole a 
            reasonable projection setting is 
            \code{projection} = "stereographic", 
            \code{orientation}=c(45,0,7.5). The latter places the pole 
            of projection to latitude 45 N and longitude 7.5 E. }
  \item{mapdat}{ Name of a dataset of boundaries to be used as the 
            underlying map. 
            \code{mapdat="world"} coarse resolution national boundaries,
            \code{mapdat="continents"} continent outlines only,
            \code{mapdat="worldHires"} high res national boundaries,
            \code{mapdat="none"} no map outlines. }
  \item{xmaplim, ymaplim}{ A lon/lat window from which map outlines 
            (i.e. national boundaries etc.) shall
            be plotted. By default (i.e. \code{xmaplim} and \code{ymaplim}
            not specified) only those parts of a map outline are drawn
            which are in the section of the field (defined by x,y). 
            This does not necessarily comprise all the map data of 
            the projection 
            window. (E.g. when \code{lonlim, latlim} are larger than the
            range of \code{x, y}.) Then it is useful to define a larger
            window for the map outlines by setting
            e.g. \code{xmaplim=c(-180,180)}, \code{ymaplim=c(0,90)} 
            i.e. the whole hemisphere. The disadvantage of this is that
            it is slow if invoked with \code{madat="worldHires"}. }
  \item{thin.map}{ Number of iterations to thin out national boundaries
            or continents specified by \code{mapdat}. It is useful when
            \code{mapdat="worldHires"} or \code{mapdat="continents"}. If 
            \code{thin.map>0} the line segments for the map outline are 
            diluted. \code{thin.map = 1} means that every second point 
            is omitted. \code{thin.map = 2} means that this thinning 
            out is applied twice, etc. Thinning is useful to prevent 
            excessively large Postscript files just because of high-
            resolution national boundaries or continents.
            \code{thin.map = 0} (the default) means no thinning. }
  \item{add.grid}{ Add grid-lines for longitude, latitude circles. This just
            executes function \code{add.map.grid} with default settings. If a
            more flexible access to \code{add.map.grid} is 
            required, the function should be called separately, i.e. after
            the call to plot.map functions. }
  \item{box}{ Whether to draw a box around the plot. }
  \item{\dots}{ Accepts additional arguments from \code{plot.map} that are
            not used in \code{contour.map} but in other functions that
            are called from \code{plot.map}. }
}
\details{
  
}
\value{
  No output, only a plot. }
\references{  }
\author{ Christoph Frei }
\note{ 
}
\seealso{ \code{\link{add.map.outlines}} }
\examples{
}
\keyword{ hplot }
