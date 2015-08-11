`plot.poly.map` <- 
function(vals,polys,
         lonlim=c(6.0,10.4),latlim=c(45.85,47.8),
         breaks,nbreaks=12,
         col="cool2warm.colors",na.col="grey",
         poly.border="grey70",poly.lwd=1,
         plot.fun="plot.map",add=FALSE,legend=TRUE,
         mapdat="world",thin.map=0,
         map.col="black",map.lwd=1.5,clip=FALSE,...) {

    # define the base plot function
    pfnam <- plot.fun
    plot.fun <- match.fun(plot.fun)

    # define breaks if not specified
    if (missing(breaks)) {breaks <- NULL}
    if (is.null(breaks)) {
       breaks <- c(pretty.contours(range(vals,finite=TRUE),n=nbreaks))
    }

    # define color scale if it is specified as function
    if (exists(col)) {
            colfun <- get(col)
            col <- colfun(length(breaks) + 1)
    }

    # checks
    if (length(vals) != length(polys)) {
      stop("Length of vals and polys incompatible")
    }
    if (length(breaks) != (length(col)-1)) {
      stop("Length of colors must be length of breaks plus 1")
    }

    # determine array of cols 
    val2col <- function(val,col.miss="white") {
        if (is.na(val)) {
            return(col.miss)
        }
        hh <- which(val <= c(breaks, val))[1]
        col[hh]
    }
    zcols <- sapply(vals,FUN=val2col,col.miss=na.col)

    # make a base plot if not already there
    # this is also used to draw the legend
    if (!add) {
      plot.fun(breaks=breaks,col=col,legend=legend,
               mapdat="none",lonlim=lonlim,latlim=latlim,...)
    }

    # plot polygons
    from.type <- get.grid.atts(polys,"grid.type")[[1]]
    from.pars <- get.grid.atts(polys,"grid.pars")
    for (ii in (1:length(vals))) {
        # Transform into lon-lat coordinates
        gg <- geocors.trafo(polys[[ii]],
                      from.type=from.type,from.pars=from.pars,
                      to.type="lonlat")
        # Transform into projection coordinates
        gg <- mapproject(list(x=gg[,1],y=gg[,2]))
        polygon(x=gg$x,y=gg$y,col=zcols[ii],
                border=poly.border,lwd=poly.lwd)
    }

    # map-elements and clipping
    if ((pfnam == "plot.swiss.map") &  clip) {
       clip.swiss()
    }
    add.map.outlines(mapdat=mapdat,lonlim.map=lonlim,latlim.map=latlim,
                     thin.map=thin.map,col=map.col,lwd=map.lwd)

}
