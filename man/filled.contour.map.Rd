\name{filled.contour.map}
\alias{filled.contour.map}
\title{ Filled contour plot on a map }
\description{
  Generates a filled contour plot of a 2d field on a specified map projection.
  It is recommended to invoke this function via function \code{plot.map}, 
  with \code{ptype="filled.contour"} which provides more flexibility.
}
\usage{
filled.contour.map(x, y, z, grid.type = "lonlat", grid.pars = list(),
          lonlim, latlim, 
          projection = "", parameters = NULL, orientation = NULL, 
          mapdat = "world", xmaplim, ymaplim, thin.map = 0, 
          nbreaks = 10, 
          breaks = pretty.contours(range(z,finite=TRUE), n=nbreaks), 
          col = heat.colors(length(breaks)-1), na.col = "grey80",
          density = NULL, angle = 45, slwd = par("lwd"),  
          add.grid = TRUE, box = TRUE, ...)
}
\arguments{
  \item{x}{ Vector of equidistant x-locations (grid point centres) in a
            geographic coordinate system. }
  \item{y}{ Vector of equidistant y-locations (grid point centres) in a 
            geographic coordinate system. }
  \item{z}{ A matrix with the field values to be displayed. Columns of
            the matrix are running over longitude (x), 
            rows over latitude (y). }
  \item{grid.type}{ A character string with the name of the coordinate
            system used for \code{x} and \code{y}.  }
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
  \item{nbreaks}{ Approximate number of colour breaks to use. (See also
            \code{breaks}.) }
  \item{breaks}{ A numeric vector with the colour breaks.
            If not specified explicitly these are
            set automatically using pretty levels so that the total
            number of breaks is about \code{nbreaks}. }
  \item{col}{ Colors to be used for the pixel plot. The number
            of colors must be ONE SMALLER than the number of breaks. }
  \item{na.col}{ The color to use for areas with missing data (i.e. NA in z) }
  \item{density}{ The density of shading lines, in lines per inch.  A
            vector of densities for each slice (i.e. number of breaks 
            minus one). If \code{density=NULL} no shading lines are drawn
            at all. Use NA for vector elements in \code{density} when the 
            respective slice should not be shaded. The color of the shading
            lines is taken from \code{col}. See also \code{polygon}.}
  \item{angle}{ The angle of shading lines, if shading is desired. One number
            (valid for all slices) or a vector with angles for each slice.
            See \code{density} and help of \code{polygon} }
  \item{slwd}{ The line width of the shading lines. }
  \item{add.grid}{ Add grid-lines for longitude, latitude circles. This just
            executes function \code{add.map.grid} with default settings. If a
            more flexible access to \code{add.map.grid} is 
            required, the function should be called separately, i.e. after
            the call to plot.map functions. }
  \item{box}{ Draw a box around the plot. }
  \item{\dots}{ Accepts additional arguments from \code{plot.map} that are
            not used in \code{image.map} but in other functions that
            are called from \code{plot.map}. }
}
\details{
  Contour lines are calculated on regular grid in x and y coordinates using
  the built-in function \code{countour.Lines}. Filling of contours is done
  by filling polygons. The calculation of contour lines and establishment
  of closed polygons ready for filling is done in function 
  \code{contourSlices}.
}
\value{
  A plot is produced. No data is returned from the function.
}
\references{  }
\author{ Christoph Frei }
\note{  }
\seealso{ \code{\link{plot.map}}, \code{\link{contourSlices}}}
\examples{
# See examples in plot.map()
}
\keyword{ hplot }% at least one, from doc/KEYWORDS
