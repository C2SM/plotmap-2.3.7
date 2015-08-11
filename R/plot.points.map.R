`plot.points.map` <- 
function(vals,lon,lat,breaks,nbreaks=12,
         col="cool2warm.colors",na.col="grey",
         pch=19,cex.pch=1,plot.fun="plot.map",add=FALSE, ...) {

    # old input or new input?
    if (!missing(lon) & !missing(lat)) {
       input <- "old"
    } else { input <- "new" }

    # check input consistency and convert
    # after this section vals is a vector with the values
    # and coords is a data.frame or matrix with two columns
    # and attributes for the coordinate system.
    if (input == "old") {
       if (("data.frame" %in% class(vals)) | ("matrix" %in% class(vals))) {
         if (dim(vals)[2] == 1) { vals <- as.vector(vals[,1]) }
         else { stop("vals should be a vector") }
       }
       if ((length(vals) != length(lon)) | (length(vals) != length(lat))) {
          stop("Unequal length in vals, lon and lat.") 
       }
       coords <- data.frame(lon=lon,lat=lat)
       coords <- put.grid.atts(coords,grid.type="lonlat")
    }
    if (input == "new") {
       if (!("data.frame" %in% class(vals)) & !("matrix" %in% class(vals))) {
          stop("vals should be a data.frame or a matrix")
       }
       grid.type=get.grid.atts(vals,"grid.type")[[1]]
       cornams <- grid.att.names(grid.type=grid.type,what="grid.cors")
       valnam <- names(vals)[!(names(vals) %in% cornams)][[1]]
       coords <- copy.grid.atts(vals,vals[,cornams])
       vals <- vals[,valnam]
       rm(grid.type)
    }

    # define the base plot function
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
    if (length(breaks) != (length(col)-1)) {
      stop("Length of colors must be length of breaks plus 1")
    }

    # calculate color vector
    r.breaks <- c(min(vals,na.rm=TRUE)-1,breaks,max(vals,na.rm=TRUE)+1)
    col.sel <- function(XX) {
                   if (is.na(XX)) {
                     return(na.col)
                   } else {
                     return(col[which(XX<=r.breaks)[1]-1])
                   }
               }
    p.col <- sapply(vals,FUN=col.sel)

    # make a base plot if not already there
    # this is also used to draw the legend
    if (!add) {
      plot.fun(breaks=breaks,col=col,na.col="white",...)
    }

    # calculate lon/lat coordinates of points
    gg <- geocors.trafo(coords,to.type="lonlat")

    # calculate projection coordinates
    gg <- mapproject(list(x=gg[,1],y=gg[,2]))

    # plot points
    points(x = gg$x, y = gg$y, pch = pch, cex = cex.pch, col = p.col)

}
