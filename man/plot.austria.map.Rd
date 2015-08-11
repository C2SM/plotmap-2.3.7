\name{plot.austria.map}
\alias{plot.austria.map}
\title{ Contour and image plot on a map over Austria }
\description{
  Generates a contour plot, a filled contour plot or an image plot 
  (colored pixels) on a map over Austria. 
  The user can specify the projection. This function is calling plot.alps.map
  (and eventually plot.map)  
  with appropriate options for Austria. The function uses a  
  geographic dataset also used for the Alps. }

\usage{
plot.austria.map(fld,
         lonlim = c(9.5,17.0), latlim = c(46.3,48.8),
         mapdat="alps:borders&lakes&rivers",
         map.lwd=1.2,map.col="black",map.lwd2=0.4,map.col2=map.col,
         projection="stereographic",orientation=c(47.5,0.0,13.0),
         leg.space=0.14,box=TRUE,clip=FALSE,add.grid=FALSE,
         thin.map=0,thin.map2=2,...)
}
\arguments{
  All arguments of \code{plot.map} (See detailed description under 
  \code{plot.map}). Below we only list main arguments, additional 
  arguments (not availalbe in \code{plot.map}) and arguments with 
  a different default setting than in \code{plot.map}:
  \item{lonlim, latlim}{ Defines the lon/lat window to be plotted. 
            Defaults are appropriate for whole of Austria. }
  \item{mapdat}{ Appart from the allowed values for \code{plot.map}, 
            \code{plot.austria.map} also accepts:
            "alps:borders&lakes&rivers",
            "alps:borders",
            "alps:rivers",
            "alps:lakes", etc. }
  \item{map.lwd, map.col}{ Line width and color of border outlines. These
            arguments are only supported in \code{plot.alps.map} and 
            \code{plot.austria.map}. } 
  \item{map.lwd2, map.col2}{ Line width and color of river and lake outlines. 
            These arguments are only supported in \code{plot.alps.map} and 
            \code{plot.austria.map}. } 
  \item{box}{ Whether a box should be drawn.} 
  \item{clip}{ Clipping is not yet implemented for the 
            Alpine region and Austria.} 
  \item{thin.map}{ Whether to dilute line segments of national borders. 
            See \code{\link{add.map.outlines}}.} 
  \item{thin.map2}{ Whether to dilute line segments of rivers and lakes.} 
  \item{\dots}{ Additional arguments passed onward to \code{\link{plot.map}}.}

}
\details{
  The datasets for the map outlines are stored in the package.}
\value{
  A plot is produced. No data is returned from the function.
}
\references{  }
\author{ Christoph Frei }
\note{  }

\seealso{ \code{\link{alps.borders}}, \code{\link{alps.rivers}},
          \code{\link{alps.lakes}}, \code{\link{plot.map}} }
\examples{

# (A) Prepare an empty map of Austria without legend. 
#     For later plotting of symbols etc.
plot.austria.map(legend=FALSE,map.lwd2=0.7,map.col2="blue")

# (B) Prepare a station map (empty map with position of the stations)
data(expl10)
plot.austria.map(legend=FALSE,map.lwd2=0.7,map.col2="blue")
# calculate projection coordinates
procoo <- mapproject(list(x=expl10[,2],y=expl10[,3]))
points(x = procoo$x, y = procoo$y, pch = 8, col = "red", cex = 0.1)

# (C) Plot expl9 as an image plot
data(expl9)
asp <- 0.56
X11(width=8,height=asp*8)
plot.austria.map(expl9,breaks=c(1,1.75,2.5,3.25,4,4.75,5.5,6.25,7),
               col=rainbow10(),mapdat="alps:borders")
dev.off()

# (D) Alternative to (B) using plot.points.map()
data(expl10)
stats <- data.frame(expl10[,-c(1)],val=1)
attr(stats,"grid.type") <- "lonlat"
plot.points.map(vals=stats,breaks=c(0),col=c("red","red"),
                plot.fun="plot.austria.map",cex.pch=0.2,
                map.lwd2=0.7)

# (E) Plotting a high-resolution DEM for Austria
# this takes long to calculate
\dontrun{
data(at.dem)
asp <- 0.56
X11(width=8,height=asp*8)
#postscript(file="at.dem.ps",width=8,height=asp*8)
plot.austria.map(fld=at.dem, ptype="filled.contour", 
           breaks=c(400,800,1200,1600,2000,2500,3000), col="terrain.colors")
dev.off()
}

}
\keyword{ hplot }% at least one, from doc/KEYWORDS
