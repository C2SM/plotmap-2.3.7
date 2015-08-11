`plain.map` <-
function(x=NULL,y=NULL,grid.type="lonlat",grid.pars=list(),
         lonlim,latlim,
         projection="",parameters=NULL,orientation=NULL,
         mapdat="world",xmaplim,ymaplim,thin.map=0,
         add.grid=TRUE,box=TRUE,...) {
# -------------------------------------------------------------------

     # complete x, y if they are missing
     miss.xy <- (is.null(x) | is.null(y))
     miss.lim <- (missing(lonlim) | missing(latlim))
     if (miss.xy & miss.lim) {
        stop("Coordinates and window lims may not both miss.")
     }
     npts <- 100
     if (miss.xy) {
        x <- seq(lonlim[1],lonlim[2],length=npts)
        y <- seq(latlim[1],latlim[2],length=npts)
        grid.type <- "lonlat"
        grid.pars <- list()
     }
     
     # PREPARATIONS
     # ---------------------------------------

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
       	 # includes the field and the lonlim/latlim specifications
         map.win <- list(xlim=range(c(map.win$xlim,lonlim),finite=TRUE),
                         ylim=range(c(map.win$ylim,latlim),finite=TRUE))
       }
     }
     

     # AND HERE WE GO PLOTTING   
     # ----------------------------------------------

     # prepare the plot with the frame. 
     plot(x=c(1),y=c(1),type="n",axes=FALSE,
         xlim=proj.win$xlim,ylim=proj.win$ylim,xlab="",ylab="")

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

