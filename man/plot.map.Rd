\name{plot.map}
\alias{plot.map}
\title{ Contour and image plot on a map }
\description{
  Generates a contour plot, a filled contour plot or an image plot 
  (coloured pixels) on a geographic map with user defined projection.
  This is the general function of this package which distributes 
  operations to other functions. Below is a complete list of available
  arguments, including those that are just passed on to the other
  functions.
}
\usage{
plot.map(fld, ptype = "image",
         legend = ((ptype=="image") | (ptype=="filled.contour")),
         leg.space = 0.18, cex.leg = 1.0,
         nbreaks=12, nlevels = nbreaks,
         breaks, levels,
         col = ifelse(ptype=="contour",par("fg"),"cool2warm.colors"),
         density = NULL, angle = 45, slwd = par("lwd"),
         mar.leg = c(3,0,2,4), mar.plot = c(3,2,2,1),
         ...)
}
\arguments{
  \item{fld}{ The field to depict. A matrix with attributes specifying
           the geographical coordinate system. (See details.)
           If this argument is missing or if \code{fld=NULL} a plain map
           is drawn and, if requested by \code{legend=TRUE}, also 
           a color legend for subsequent plotting of symbols, 
           text, etc. (See also \code{ptype="plain"}.) }
  \item{ptype}{ The type of plot to produce. A character string. One of
           "image" (image plot), "contour" (contour lines plot), 
           "filled.contour" (filled contour plot), "plain" (plain map). 
           Note that a filled contour
           plot can take quite long (a few minutes) if the field is complex.}
  \item{legend}{ TRUE if a color legend should be drawn. FALSE otherwise. }
  \item{leg.space}{ The fraction of the plot width to use for the legend. }
  \item{cex.leg}{ Magnification of legend annotations. }
  \item{lonlim, latlim}{ Numeric vectors with two elements. 
            Defines the lon/lat window to be plotted. 
            If specified the window of the projection is chosen such 
            that the entire window (\code{lonlim}, \code{latlim}) is 
            comprised in the plot. If not specified the window is chosen 
            to display the entire domain of \code{fld}, i.e. the window
            defined by the coordinate attributes \code{fld}. }
  \item{projection}{ The map projection. A character string. Supported
            projections are those in package \pkg{mapproj}.
            E.g. \code{projection="stereographic"} . 
            If an empty string is given (the default), the projection 
            used in the last application of \code{mapproject} will be used. 
            This is useful for plotting on an existing map without recalling
            the projection explicitly. (The 
            last projection settings are stored in variable 
            \code{.Last.projection}. A list of possible projections 
            is given in the help to \code{\link[mapproj]{mapproject}}. 
            Default is 
            \code{projection=""}}
  \item{parameters}{ The orientation parameter of \code{mapproject}. (See 
            help to \code{\link[mapproj]{mapproject}}). (Used rarely.) }
  \item{orientation}{ The orientation parameter of \code{mapproject}. (See 
            help to \code{\link[mapproj]{mapproject}}). For Europe as a whole a 
            reasonable projection setting is 
            \code{projection="stereographic"}, 
            \code{orientation=c(45,0,7.5)}. The latter places the pole 
            of projection to latitude 45 N and longitude 7.5 E. }
  \item{mapdat}{ A character string specifying a dataset of map outlines to 
            be drawn as the underlying map. 
            \code{mapdat="world"} coarse resolution national 
            boundaries (default),
            \code{mapdat="continents"} continent outlines only
            \code{mapdat="worldHires"} high res national boundaries
            \code{mapdat="none"} no map outlines. For more information
            on map outlines see \code{\link[maps]{map}}. }
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
  \item{nbreaks, nlevels}{ Approximate number of colour breaks/ contour levels
            to use. (See also \code{breaks} and \code{levels}.) }
  \item{breaks, levels}{ A numeric vector with the colour breaks /
            contour levels. For image plots or filled contour plots, 
            \code{breaks} need to be one element shorter than the length 
            of \code{col}.
            If not specified explicitly these are
            set automatically using pretty levels so that the total
            number of breaks/levels is about \code{nbreaks}/\code{nlevels}. 
            Arguments \code{breaks} and \code{levels} are used as
            synonyms, but usually inputs are made to \code{breaks} when 
            code{ptype="image"} or \code{ptype="filled.contour"}, and to
            \code{levels} when code{ptype="contour"}. Note that \code{breaks}
            need to be specified when \code{fld=NULL} or when \code{fld} is
            missing and a legend should be plotted anyway with the plain map. }
  \item{col}{ Colors used for the contour slices. When 
            \code{ptype="contour"}, \code{col} specifies the color of 
            the contour
            lines. It is cyclically completed (or truncated) to have the
            same length like \code{levels}. When \code{ptype="image"} or 
            \code{ptype="filled.contour"} then \code{col} can be the name 
            of a color table
            function (as character string) or a vector of colors the length
            of which must be ONE LARGER than the length of \code{breaks}. 
            In this
            case the first and last colors are used for field values 
            smaller/larger 
            than the first/last elements of \code{breaks}.}
  \item{na.col}{ The color to use for areas with missing data (i.e. NA in 
            \code{fld}).
            Useful values are "white" or "grey80" (default). }
  \item{lty, lwd}{ Only relevant for \code{ptype="contour"}. 
            Arrays with line types and 
            widths of contour lines. Cyclic completion until number of 
            items is the number of levels (\code{length(levels)}).}
  \item{density}{ Only for \code{ptype="filled.contour"}. 
            The density of shading lines, in lines per inch.  A
            vector of densities for each slice (i.e. number of breaks 
            minus one). If \code{density=NULL} no shading lines are drawn
            at all. Use NA for vector elements in \code{density} when the 
            respective slice should not be shaded. The color of the shading
            lines is taken from \code{col}. See also \code{polygon}.}
  \item{angle}{ Only for ptype="filled.contour". 
            The angle of shading lines, if shading is desired. One number
            (valid for all slices) or a vector with angles for each slice.
            See \code{density} and help of \code{polygon} }
  \item{slwd}{ Only for \code{ptype="filled.contour"}. The line width of the 
            shading lines. }
  \item{mar.leg}{ The margin settings (see \code{par(mar)}) to be used for 
            the color legend. Changes to the default setting are needed, 
            e.g. when a larger legend font size (see \code{cex.leg})
            should be used. }
  \item{mar.plot}{ The margin settings (see \code{par(mar)}) to be used for 
            the plot area. }
  \item{add.grid}{ Add grid-lines for longitude, latitude circles. This just
            executes function \code{add.map.grid} with default settings. If a
            more flexible access to \code{add.map.grid} is 
            required, the function should be called separately, i.e. after
            the call to plot.map functions. }
  \item{add}{ If TRUE add to the existing plot. If FALSE generate a new plot. 
            \code{add=TRUE} can only be selected for a contour plot.}
  \item{box}{ Draw a box around the plot. }
  \item{\dots}{ Additional arguments (all of which are described here but
            not in the usage section) that will be passed on to the 
            individual functions called (e.g. \code{filled.contour.map},
            \code{image.map}) from \code{plot.map}. }
}
\details{

  ** Input field format **

  The input field \code{fld} is a regular grid defined on a geographical
  coordinate system. "Regular" means equidistant rows and equidistant
  columns. 

  Several geographical coordinate systems are supported, such as
  longitude-latitude grids, lon-lat grids with a rotated pole and swiss
  coordinates. Package \pkg{geocors} deals with the different coordinate
  systems and provides transformation functions between them. The details
  section of function \code{\link[geocors]{geocors.trafo}} lists currently
  supported coordinate systems. Extensions to package \pkg{geocors} become
  directly available for \code{plot.map}, so that additional systems can
  easily be added without touching package \pkg{plotmap}.

  \code{plot.map} expects information on the coordinate system of the 
  input field in the attributes of \code{fld}. 
  Package \pkg{geocors} comprises several
  useful functions to set, read and copy grid attributes to a data object. 
  (See \code{\link[geocors]{put.grid.atts}}.)

  The following attributes need to be specified:

  "grid.type" : A character string specifying the name of the coordinate
  system. (See details of \code{\link[geocors]{geocors.trafo}}.) E.g.
  \code{attr(fld,"grid.type") <- "lonlat"} declares \code{fld} to be defined
  on a regular longitude-latitude grid. 

  "grid.pars" : A list with additional parameters needed to unequivocally
  define the coordinate system. E.g. \code{attr(fld,"grid.pars") <- 
  list(plon=-170.0,plat=30)} together with 
  \code{attr(fld,"grid.type") <- "rotpol"} defines a grid in rotated pole
  lon-lat coordinates with a pole position at 170 degress W and 30 degrees
  N. If no additional parameters are needed to define the coordinate system
  (e.g. for a grid.type of "lonlat"), the grid.pars attribute is an empty
  list: \code{attr(fld,"grid.pars") <- list()}.

  Two additional attributes are needed to define the grid point's x and 
  y coordinates in the coordinate system used. (Each a numeric vector,
  one with length equal to \code{dim(fld)[1]} and the second 
  with length \code{dim(fld)[2]}. The names of these attributes
  vary between different grid types. For grid type \code{"lonlat"} the names
  are \code{"lon"} and \code{"lat"}. 


  ** Calculation of contours and image pixels **

  Contour lines and polygons for image pixels are calculated in the coordinate
  system in which \code{fld} is defined. (I.e. on the regular grid.) 
  For contours this is done using
  the built-in function \code{countour.Lines}. Rectangles for polygon pixels
  and line segments for contours are then transformed into coordinates of
  the desired map projection.

  Filled contours are drawn with filled polygons determined from contour lines 
  and closed along the window boundary. Special programming was necessary
  to determine in which order the polygons shall be drawn. Plotting a 
  filled.contour plot can therefore take quite long if the input field
  is complex and/or if many contour levels are requested.


  ** Adding more items after plot.map **

  It is possible to add further graphics elements (text, symbols, etc.)
  into a plot produced by \code{plot.map}. Functions \code{text}, 
  \code{points} and \code{lines} can be used for this purpose. However,
  the position of additional elements have to be determined by using
  function \code{\link[mapproj]{mapproject}}. (Note: Adding further
  graphic elements is possible because the legend is plotted first.)

}
\value{
  A plot is produced. No data is returned from the function.
}
\references{  }
\author{ Christoph Frei }
\note{  }

\seealso{ \code{\link{plot.legend}}, \code{\link{add.map.grid}} }
\examples{

# (A) An image plot of a field with rotated coordinates over Europe
data(expl2)
breaks <- c(0.5,0.67,0.8,0.9,0.95,1.05,1.11,1.25,1.5,2.0)
plot.map(fld=expl2,ptype="image",mapdat="continents",
         projection="stereographic",
         orientation=c(45,0,7.5),
         breaks=breaks,na.col="grey",
         xmaplim=c(-180,180),ymaplim=c(0,90))   # map from north hemis

# (B) The same field as filled contour (this may take a bit longer)
# and with national borders (default for mapdat).
plot.map(fld=expl2,ptype="filled.contour",
         projection="stereographic",
         orientation=c(45,0,7.5),
         breaks=breaks,na.col="grey",
         xmaplim=c(-180,180),ymaplim=c(0,90))   # map from north hemis

# (C) plot an empty map for the Alps with a legend of predefined colors.
X11(width=8,height=5.5)
plot.map(lonlim=c(2,17.5),latlim=c(42.5,49.0),
         projection="stereographic",
         mapdat="worldHires",
         orientation=c(47,0,7.5),legend=TRUE,leg.space=0.12,cex.leg=2,
         breaks=1:4,col=c("darkgreen","green","yellow","orange","red"),
         xmaplim=c(1,19),ymaplim=c(42,50),add.grid=FALSE)
dev.off()

# (D) A section of the field using continent outlines and 
# overplot with colored contour lines
data(expl3); data(expl4); data(expl5)
breaks <- c(0.5,0.67,0.8,0.9,0.95,1.05,1.11,1.25,1.5,2.0)
plot.map(fld=expl3,ptype="image",
         projection="stereographic",
         orientation=c(45,0,7.5),
         breaks=breaks,col="warm2cool.colors",
         na.col="grey",mapdat="continents",
         lonlim=c(10,30),latlim=c(30,50))
plot.map(fld=expl4,ptype="contour",
         projection="",
         mapdat="continents",
         col="blue",levels=c(0.5),
         legend=FALSE,add=TRUE,lwd=3,lty=1)
plot.map(fld=expl5,ptype="contour",
         projection="",
         mapdat="continents",
         col="red",levels=c(0.5),
         legend=FALSE,add=TRUE,lwd=3,lty=1)

# (E) Plotting a global field 
data(expl7)
plot.map(fld=expl7,projection="rectangular",
         mapdat="continents",thin.map=3,leg.space=0.13,
         parameters=45,ptype = "filled.contour",
         breaks=c(-8,-6,-4,-3,-2,-1,0,1,2,3,4,6,8),
         orientation=NULL)

# (F) Plotting a field given in Swiss coordinates
data(expl8) 
plot.map(fld=expl8,ptype="filled.contour",
         breaks=c(0.1,0.9,2,4,6,8,10,12,14),col="rainbow",
         projection="stereographic",orientation=c(47.0,0.0,8.0),
         legend=TRUE,leg.space=0.13,
         mapdat="swiss:border&rivers&lakes",add.grid=FALSE)

# See also additional examples under plot.swiss.map !

}
\keyword{ hplot }% at least one, from doc/KEYWORDS
