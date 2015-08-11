\name{plot.swiss.map}
\alias{plot.swiss.map}
\title{ Contour and image plot on a Swiss map }
\description{
  Generates a contour plot, a filled contour plot or an image plot 
  (colored pixels) on a swiss map. The user can specify the projection.
  This function is calling plot.map with the appropriate options for
  a Swiss map, but uses a different geographic dataset. It is also
  possible to clip a field outside the swiss border.
}
\usage{
plot.swiss.map(fld,
         lonlim=c(6.0,10.4),latlim=c(45.85,47.8),
         mapdat="swiss:border&lakes&rivers",
         map.lwd=1,map.col="black",map.lwd2=map.lwd,map.col2=map.col,
         projection="stereographic",orientation=c(47.0,0.0,8.0),
         leg.space=0.12,box=FALSE,clip=TRUE,add.grid=FALSE,
         thin.map=0,thin.map2=thin.map,...)
}
\arguments{
  All arguments of \code{plot.map} (See detailed description under 
  \code{plot.map}). Below we only list main arguments, additional 
  arguments (not availalbe in \code{plot.map}) and arguments with 
  a different default setting than in \code{plot.map}:
  \item{lonlim, latlim}{ Defines the lon/lat window to be plotted. 
            Defaults are appropriate for domain of Switzerland. }
  \item{mapdat}{ Appart from the allowed values for \code{plot.map}, 
            \code{plot.swiss.map} also accepts:
            "swiss:border&lakes&rivers",
            "swiss:border",
            "swiss:rivers",
            "swiss:lakes", etc. }
  \item{map.lwd, map.col}{ Line width and color of border outlines. These
            arguments are only supported in \code{plot.swiss.map}. } 
  \item{map.lwd2, map.col2}{ Line width and color of river and lake outlines. 
            These arguments are only supported in \code{plot.swiss.map}. } 
  \item{clip}{ Whether to clip areas outside Switzerland.} 
  \item{thin.map}{ Whether to dilute line segments of national borders.
            See \code{\link{add.map.outlines}}.} 
  \item{thin.map2}{ Whether to dilute line segments of rivers and lakes.} 
  \item{\dots}{ Additional arguments passed onward to \code{plot.map}}

}
\details{
  The datasets for the map outlines are stored in the package.}
\value{
  A plot is produced. No data is returned from the function.
}
\references{  }
\author{ Christoph Frei }
\note{  }

\seealso{ \code{\link{swiss.border}},\code{\link{swiss.rivers}},
          \code{\link{swiss.lakes}} }
\examples{

# (A) Prepare an empty map of Switzerland without legend. 
#     For later plotting of symbols etc.
plot.swiss.map(legend=FALSE)

# (B) Prepare an empty map of Switzerland with a color legend. 
#     For later plotting of symbols etc.
X11(width=8,height=5.5)
plot.swiss.map(breaks=1:10,col="cool2warm.colors",legend=TRUE,
               mapdat="swiss:border&lakes")
dev.off()

# (C) Plot expl1 as an image plot
data(expl1)
X11(width=8,height=5.5)
plot.swiss.map(expl1,breaks=c(20,40,60,80,100,140,180,220,260),
               col=rainbow10())
dev.off()

# (D) Plot expl1 as a filled contour plot  (takes a bit long)
X11(width=8,height=5.5)
plot.swiss.map(expl1,ptype="filled.contour",
               breaks=c(20,40,60,80,100,140,180,220,260),
               col=rainbow10(),map.lwd=1.5)
dev.off()

# (E) Plot expl1 as a contour plot  
X11(width=8,height=5.5)
plot.swiss.map(expl1,ptype="contour",
               levels=c(20,40,60,80,100,140,180,220,260),
               col="rainbow",lwd=2,map.lwd=1.2)
dev.off()

# (F) Plot expl6 using shading of countour slices  
data(expl6)
X11(width=8,height=5.5)
plot.swiss.map(expl6,ptype="filled.contour",
               breaks=c(10,50,90,110,175,250),
               col=wb.prec.col(),leg.space=0.15,
               density=c(18,15,13,NA,NA,NA,NA),slwd=3,angle=90)
dev.off()

# (G) Plot expl8 - a radar field given in Swiss coordinates 
data(expl8)
X11(width=8,height=5.5)
plot.swiss.map(expl8,ptype="filled.contour",clip=FALSE,
               breaks=c(0.1,0.9,2,4,6,8,10,12,14),col="rainbow")
dev.off()

}
\keyword{ hplot }% at least one, from doc/KEYWORDS
