`contour.map` <-
function(x,y,z,grid.type="lonlat",grid.pars=list(),
         lonlim,latlim,
         projection="",parameters=NULL,orientation=NULL,
         mapdat="world",xmaplim,ymaplim,thin.map=0,
         nlevels = 10, 
         levels=pretty.contours(range(z,finite=TRUE), n=nlevels),
         col=par("fg"),lty=par("lty"),lwd=par("lwd"),
         add=FALSE,add.grid=TRUE,box=TRUE,...) {
# -------------------------------------------------------------------

     # warning if adding countours on previous plot with possibly 
     # different projection. 
     if (add & (projection != "")) {
       warning("Adding contours with a new projection?",immediate=TRUE)
     }
     
     
     # PREPARATIONS
     # ---------------------------------------

     # construct arrays for color, line type and line width by cyclic
     # completion
     nlev <- length(levels)
     lcols <- rep(col,length=nlev)
     ltys <- rep(lty,length=nlev)
     lwds <- rep(lwd,length=nlev)

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
     

     # CONTOURS IN USER COORDINATES
     # ---------------------------------------
     # ************************
     conts <- contourLines(x=x,y=y,z=z,levels=levels)
     # ************************


     # TRANSFORM CONTOURS INTO PROJECTION COORDINATES 
     # ---------------------------------------

     # collect level of each contour into vector
     if (length(conts) > 0) {
       cont.level <- sapply(conts,FUN=function(x) {x$level})
     }
     
     # rearrange list for use in geocors.trafo
     cnams <- grid.att.names(grid.type,what="grid.cors")
     if (length(conts) > 0) {
       conts <- lapply(conts,FUN=function(x) {
       	                         hh <- x[c("x","y")]; names(hh) <- cnams; hh})
     }
  
     # transform contour lines into lonlat coordinates
     if (grid.type != "lonlat") {
     if (length(conts) > 0) {
       for (k in 1:length(conts)) {
         conts[[k]] <- geocors.trafo(x=conts[[k]],
                             from.type=grid.type,from.pars=grid.pars,
                             to.type="lonlat")
       }
     }
     }

     # rearrange list for use in mapproject
     if (length(conts) > 0) {
       conts <- lapply(conts,FUN=function(x) {
                                 hh <- x[c(1,2)]; names(hh) <- c("x","y"); hh})
     }

     # transform contour lines into projection coordinates
     if (length(conts) > 0) {
       for (k in 1:length(conts)) {
         conts[[k]] <- mapproject(x=conts[[k]])
       }
     }
     
     
     # AND HERE WE GO PLOTTING   
     # ----------------------------------------------

     # prepare the plot with the frame. 
     # If add=TRUE no preparation is needed.
     if (!add) {
       plot(x=c(1),y=c(1),type="n",axes=FALSE,
           xlim=proj.win$xlim,ylim=proj.win$ylim,xlab="",ylab="")
     }

     # plot countour lines
     if (length(conts) > 0) {
     for (k in 1:length(conts)) {
        ii <- which(cont.level[k] == levels)[1]
        lines(x=conts[[k]]$x,y=conts[[k]]$y,lwd=lwds[ii],lty=ltys[ii],
              col=lcols[ii],xpd=FALSE)
     }
     }

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

