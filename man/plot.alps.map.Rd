\name{plot.alps.map}
\alias{plot.alps.map}
\title{ Contour and image plot on a map covering the alpine region (2E-17E, 43N-49N) }
\description{
  Generates a contour plot, a filled contour plot or an image plot 
  (colored pixels) on a map covering the alpine region (2E-17E, 43N-49N). 
  The user can specify the projection. This function is calling plot.map 
  with the appropriate options for an alpine map, but uses a different 
  geographic dataset. }

\usage{
plot.alps.map(fld,
         lonlim=c(2.4,16.8), latlim = c(43.2,48.85),
         mapdat="alps:borders&lakes&rivers",
         map.lwd=1.2,map.col="black",map.lwd2=0.4,map.col2=map.col,
         projection="stereographic",orientation=c(46.0,0.0,7.5),
         leg.space=0.14,box=TRUE,clip=FALSE,add.grid=FALSE,
         thin.map=0,thin.map2=2,...)
}
\arguments{
  All arguments of \code{plot.map} (See detailed description under 
  \code{plot.map}). Below we only list main arguments, additional 
  arguments (not availalbe in \code{plot.map}) and arguments with 
  a different default setting than in \code{plot.map}:
  \item{lonlim, latlim}{ Defines the lon/lat window to be plotted. 
            Defaults are appropriate for domain of the Alps. }
  \item{mapdat}{ Appart from the allowed values for \code{plot.map}, 
            \code{plot.alps.map} also accepts:
            "alps:borders&lakes&rivers",
            "alps:borders",
            "alps:rivers",
            "alps:lakes", etc. }
  \item{map.lwd, map.col}{ Line width and color of border outlines. These
            arguments are only supported in \code{plot.alps.map}. } 
  \item{map.lwd2, map.col2}{ Line width and color of river and lake outlines. 
            These arguments are only supported in \code{plot.alps.map}. } 
  \item{box}{ Whether a box should be drawn.} 
  \item{clip}{ Clipping is not yet implemented for the Alpine region.} 
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
\author{ Francesco Isotta }
\note{  }

\seealso{ \code{\link{alps.borders}}, \code{\link{alps.rivers}},
          \code{\link{alps.lakes}}, \code{\link{plot.map}} }
\examples{

# (A) Prepare an empty map of the alpine region without legend. 
#     For later plotting of symbols etc.
plot.alps.map(legend=FALSE,map.lwd2=0.7,map.col2="blue")

# (B) Prepare a station map (empty map with position of the stations)
data(expl10)
plot.alps.map(legend=FALSE,map.lwd2=0.7,map.col2="blue")
# calculate projection coordinates
procoo <- mapproject(list(x=expl10[,2],y=expl10[,3]))
points(x = procoo$x, y = procoo$y, pch = 8, col = "red", cex = 0.1)

# (C) Plot expl9 as an image plot
data(expl9)
asp <- 0.6
X11(width=8,height=asp*8)
plot.alps.map(expl9,breaks=c(1,1.75,2.5,3.25,4,4.75,5.5,6.25,7),
               col=rainbow10(),mapdat="alps:borders")
dev.off()

# (D) Alternative to (B) using plot.points.map()
data(expl10)
stats <- data.frame(expl10[,-c(1)],val=1)
attr(stats,"grid.type") <- "lonlat"
plot.points.map(vals=stats,breaks=c(0),col=c("red","red"),
                plot.fun="plot.alps.map",cex.pch=0.2,
                map.lwd2=0.7)


}
\keyword{ hplot }% at least one, from doc/KEYWORDS
