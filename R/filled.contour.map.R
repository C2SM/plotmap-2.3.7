`filled.contour.map` <-
function(x,y,z,grid.type="lonlat",grid.pars=list(),
         lonlim,latlim,
         projection="",parameters=NULL,orientation=NULL,
         mapdat="world",xmaplim,ymaplim,thin.map=0,
         nbreaks = 10, 
         breaks = pretty.contours(range(z,finite=TRUE), n=nbreaks), 
         col=heat.colors(length(breaks)-1), na.col="grey80",
         density=NULL,angle=45,slwd=par("lwd"),
         add.grid=TRUE,box=TRUE,...) {
# -------------------------------------------------------------------

     # PREPARATIONS FOR COLOURS AND BREAKS
     # ---------------------------------------

     # check breaks
     if (length(breaks) != length(col)+1) {
          stop("Length of col must be length of breaks minus 1")
     }

     # prepare for correct miss treatment
     z.range <- diff(range(z,finite=TRUE))
     z.min <- min(z,na.rm=TRUE)
     z.max <- max(z,na.rm=TRUE)
     # check appropriate setting of breaks
     if ((breaks[1] >= z.min) | (breaks[length(breaks)] <= z.max)) {
       stop("Inappropriate setting of breaks.")
     }
     # fill with NA replacements
     if (z.range==0) {z.range <- 1} 
     nafill <- min(z.min,min(breaks))-2*z.range
     z[is.na(z)] <- nafill
     # add one level to breaks which is the range for missings
     breaks <- c(nafill-0.1*z.range,breaks)
     col <- c(na.col,col)
     if (!is.null(density)) {
       density <- c(NA,rep(density,length=length(breaks)-2))
     }

     # transform missing color into color code
     rgb.miss <- col2rgb(na.col)
     col.miss <- rgb(rgb.miss[1],rgb.miss[2],rgb.miss[3],
                     maxColorValue = 255)
     rm(rgb.miss)

     # determine arrays for color, shading and shading-line width
     nbreaks <- length(breaks)
     if (nbreaks != length(col)+1) {
          stop("Length of col must be length of breaks minus 1")
     }
     cols <- c(col.miss,col,col.miss)
     if (is.null(density)) {
        dens <- c(NA,NULL,NA)
     } else {
        dens <- c(NA,rep(density,length=nbreaks),NA)
     }
     lwds <- c(par("lwd"),rep(slwd,length=nbreaks),par("lwd"))
     angs <- c(45,rep(angle,length=nbreaks),45)



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


     # CONTOURS IN USER COORDINATES
     # ---------------------------------------
     # ************************
     conts <- contourSlices(x=x,y=y,z=z,breaks=breaks)
     # ************************
     
     
     # TRANSFORM CONTOURS INTO PROJECTION COORDINATES 
     # ---------------------------------------
     
     # collect accompaning info on stacking and level of each contour
     if (length(conts) > 0) {
       cont.level <- sapply(conts,FUN=function(x) {x$level})
       stack.level <- sapply(conts,FUN=function(x) {x$stack.level})
       cont.hill <- sapply(conts,FUN=function(x) {x$hill})
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
     

     # AND HERE WE GO PLOTTING
     # ----------------------------------------------

     # prepare the plot with the frame. 
     plot(x=c(1),y=c(1),type="n",axes=FALSE,
          xlim=proj.win$xlim,ylim=proj.win$ylim,xlab="",ylab="")

     # plot countour slices as filled polygons
     if (length(conts) > 0) {
     for (stnr in min(stack.level):max(stack.level)) {
        cl <- which(stnr == stack.level)
        for (k in cl) {
           ll <- conts[[k]]
           hh <- which(cont.level[k] == breaks)[1]
           llcol <- cols[hh+cont.hill[k]]
           llden <- dens[hh+cont.hill[k]]
           lllwd <- lwds[hh+cont.hill[k]]
           llang <- angs[hh+cont.hill[k]]
           if (!is.na(llden) & !is.null(llden)) {
           if (llden > 0) {      
           # overplot with white polygon before plotting stripes
           # is necessary because polygons are transparent when shaded.
              polygon(x=ll$x,y=ll$y,col="white",
                   border=NA,xpd=FALSE)
           }}
           polygon(x=ll$x,y=ll$y,col=llcol,
                   density=llden,lwd=lllwd,angle=llang,
                   border=NA,xpd=FALSE)
        }
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



