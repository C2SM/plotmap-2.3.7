`image.map` <-
function(x,y,z,grid.type="lonlat",grid.pars=list(),
         lonlim,latlim,
         projection="",parameters=NULL,orientation=NULL,
         mapdat="world",xmaplim,ymaplim,thin.map=0,
         nbreaks = 10,
         breaks=pretty.contours(range(z,finite=TRUE), n=nbreaks),
         col=heat.colors(length(breaks)-1), na.col="grey80",
         add.grid=TRUE,box=TRUE,...) {
# -------------------------------------------------------------------
# 

     # check breaks
     if (length(breaks) != length(col)+1) {
          stop("Length of col must be length of breaks minus 1")
     }


     # PREPARATIONS
     # ---------------------------------------

     # transform missing color into color code
     rgb.miss <- col2rgb(na.col)
     col.miss <- rgb(rgb.miss[1],rgb.miss[2],rgb.miss[3],
                     maxColorValue = 255)
     rm(rgb.miss)
     
     # useful quantities
     dx <- diff(x)[1]
     dy <- diff(y)[1]
     nx <- length(x)
     ny <- length(y)

     # define plotting window in projection coordinates
     # and initialize projection   --> proj.win
     # case 1 : if lonlim and latlim are specified take this lon-lat window
     if (!missing(lonlim) & !missing(latlim)) {
       proj.win <- window.lims(xlim=lonlim,ylim=latlim,
                               grid.type="lonlat",grid.pars=list(),
                               out="proj",
                               projection=projection,parameters=parameters,
                               orientation=orientation)
     # case 2 : otherwize take the window defined by the field
     } else {
       proj.win <- window.lims(xlim=range(x),ylim=range(y),
                               grid.type=grid.type,grid.pars=grid.pars,
                               out="proj",
                               projection=projection,parameters=parameters,
                               orientation=orientation)
     }


     # define the map window (i.e. the space from where to include map outlines)
     # in lon/lat coordinates    --> map.win
     # case 1 : if xmaplim and ymaplim are specified
     if (!missing(xmaplim) & !missing(ymaplim)) {
       map.win <- list(xlim=xmaplim,ylim=ymaplim)
     } else {
       # case 2 : general case when mapwindow shall be inferred from input
       # field and/or the lonlim,latlim specification
       map.win <- window.lims(xlim=range(x),ylim=range(y),
                               grid.type=grid.type,grid.pars=grid.pars,
                               out="lonlat")
       if (!missing(lonlim) & !missing(latlim)) {
       	 # if lonlim, latlim are specified take the outer-most area that
       	 # includes the field and the lonlim/latlim specifications
         map.win <- list(xlim=range(c(map.win$xlim,lonlim),finite=TRUE),
                         ylim=range(c(map.win$ylim,latlim),finite=TRUE))
       }
     }
     

     # TRANSFORMATION OF PIXEL CORNERS
     # ---------------------------------------

     # pixel corners as vectors
     xx.left <- as.vector(matrix(x-dx/2,nrow=nx,ncol=ny,byrow=FALSE))
     xx.right <- as.vector(matrix(x+dx/2,nrow=nx,ncol=ny,byrow=FALSE))
     yy.low <- as.vector(matrix(y-dy/2,nrow=nx,ncol=ny,byrow=TRUE))
     yy.up <- as.vector(matrix(y+dy/2,nrow=nx,ncol=ny,byrow=TRUE))
     
     # pixel corners in lonlat as list
     xy.ll <- geocors.trafo(x=xx.left,y=yy.low,
                            from.type=grid.type,from.pars=grid.pars,
                            to.type="lonlat",to.pars=list())
     xy.lr <- geocors.trafo(x=xx.right,y=yy.low,
                            from.type=grid.type,from.pars=grid.pars,
                            to.type="lonlat",to.pars=list())
     xy.ul <- geocors.trafo(x=xx.left,y=yy.up,
                            from.type=grid.type,from.pars=grid.pars,
                            to.type="lonlat",to.pars=list())
     xy.ur <- geocors.trafo(x=xx.right,y=yy.up,
                            from.type=grid.type,from.pars=grid.pars,
                            to.type="lonlat",to.pars=list())
     rm(xx.left,xx.right,yy.low,yy.up)

     # pixel corners in projection coordinates as list
     xy.ll <- mapproject(list(x=xy.ll[[1]],y=xy.ll[[2]]))
     xy.lr <- mapproject(list(x=xy.lr[[1]],y=xy.lr[[2]]))
     xy.ul <- mapproject(list(x=xy.ul[[1]],y=xy.ul[[2]]))
     xy.ur <- mapproject(list(x=xy.ur[[1]],y=xy.ur[[2]]))
          
     # rearrange pixel coordinates into matrices for x/y coords
     xx <- cbind(xy.ll$x,xy.lr$x,xy.ur$x,xy.ul$x)
     yy <- cbind(xy.ll$y,xy.lr$y,xy.ur$y,xy.ul$y)
     rm(xy.ll,xy.lr,xy.ul,xy.ur)



     # DETERMINE COLOURS FOR EACH PIXEL
     # ---------------------------------------
     # could this be programmed more efficiently ???
     # shouldn't we apply the same convention between breaks and col like in plot.map itself
     val2col <- function(val) {
                   if (is.na(val)) {return(col.miss)}
                   hh <- which(val <= c(breaks,val))[1]
                   if (hh==(length(breaks)+1)) {return(col.miss)}
                   if (hh==1) {return(col.miss)}
                   col[hh-1]
     }
     zcols <- sapply(z,FUN=val2col)


     # AND HERE WE GO PLOTTING
     # ----------------------------------------------

     # prepare the plot with the frame
     plot(x=c(1),y=c(1),type="n",axes=FALSE,
          xlim=proj.win$xlim,ylim=proj.win$ylim,xlab="",ylab="")

     # plot the colored pixel polygons
     poly.fun <- function(i) { polygon(x=xx[i,],y=yy[i,],
     	                       col=zcols[i],border=zcols[i],xpd=FALSE)
     	                       return() }
     trash <- sapply(1:dim(xx)[1],FUN=poly.fun)
     rm(trash)

     # add the mapoutlines on top
     add.map.outlines(mapdat=mapdat,
                      lonlim.map=map.win$xlim,latlim.map=map.win$ylim,
                      thin.map=thin.map)

     # add grid-lines for lon and lat circles
     if (add.grid) {
        delta.draw <- min(diff(map.win$xlim)/200,diff(map.win$ylim)/200)
        add.map.grid(delta.draw=delta.draw)
        rm(delta.draw)
     }
     
     # add a box
     if (box) {box()}

}

