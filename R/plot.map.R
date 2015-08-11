`plot.map` <-
function(fld,ptype="image",
         legend=((ptype=="image") | (ptype=="filled.contour")),
         leg.space=0.18,cex.leg=1.0,
         nbreaks=12,nlevels=nbreaks,
         breaks,levels,
         col=ifelse(ptype=="contour",par("fg"),"cool2warm.colors"),
         density=NULL,angle=45,slwd=par("lwd"),
         mar.leg=c(3,0,2,4),mar.plot=c(3,2,2,1),
         ...) {
# -------------------------------------------------------------------
# for additional parameters see individual functions.

     # checks
     if (missing(fld)) {
        fld <- NULL
        ptype <- "plain"
     }
     if (is.null(fld)) {
        ptype <- "plain"
     }
     if ((ptype == "contour") & legend) {
        warning("Legend should be omitted for contour plot")
        legend <- FALSE
     }

     # COLOUR AND BREAKS HANDLING
     # -----------------------------------------------------
     # set defaults for breaks and levels if not specified
     # The plot.map function does all the color and levels/breaks handling
     # because this is needed by the legend which is drawn outside the 
     # elementary functions contour.map, image.map, filled.contour.map
     if (missing(breaks)) {breaks <- NULL}
     if (is.null(breaks) & !missing(levels)) {breaks <- levels}
     if (missing(levels)) {levels <- NULL}
     if (is.null(levels) & !missing(breaks)) {levels <- breaks}
     if (is.null(breaks) & !is.null(fld)) {
        breaks <- pretty.contours(range(fld,finite=TRUE),n=nbreaks)
     }
     if (is.null(levels) & !is.null(fld)) {
        levels <- pretty.contours(range(fld,finite=TRUE),n=nlevels)
     }
     
     # print out breaks, levels
     if (ptype == "contour") {
        message("-- plot.map contour levels used : ", paste(levels,collapse=", "))
     }
     if (ptype %in% c("image","filled.contour")) {
        message("-- plot.map colour breaks used : ", paste(breaks,collapse=", "))
     }

     # determine col for contour plot if col is a colour table function
     if ((ptype=="contour") & (!is.null(levels))) {
       if (exists(col)) {
            # This happens when the color table is specified as function
            # name and number of colors should be chosen automatically 
            # to fit breaks/levels.
          colfun <- get(col)
          col <- colfun(length(levels))
       }
       # if col is a vector with colours it will be passes on to
       # contour.map.
     }

     # determine/modify breaks and colors 
     if (!is.null(breaks)) {
     if (legend | (ptype=="image") | (ptype=="filled.contour")) {

       # complement breaks to include whole value range of fld
       # This is necessary for correct treatment of missings
       # a) case when fld is missing or entire field is NA
       if (is.null(fld)) { special <- TRUE 
       } else { special <- (sum(is.na(fld)) == prod(dim(fld))) }
#       if ((is.null(fld)) | (sum(is.na(fld)) == prod(dim(fld)))) {  
       if (special) {  
          zrange <- diff(range(breaks))
          if (zrange == 0) { zrange <- 1 }    # necessary for length(breaks) == 1
          minbreak <- min(breaks)-zrange/100
          maxbreak <- max(breaks)+zrange/100
          breaks <- c(minbreak,breaks,maxbreak)
          rm(zrange,minbreak,maxbreak)
       } else {                                     # regular case
       # b) general case
          zmax <- max(fld,na.rm=TRUE); zmin <- min(fld,na.rm=TRUE)
          zrange <- zmax-zmin
          brange <- diff(range(breaks))
          zrange <- max(zrange,brange)
          if (zrange == 0) { zrange <- 1 }    # necessary for length(breaks) == 1
          minbreak <- min(c(zmin,breaks))-zrange/100
          maxbreak <- max(c(zmax,breaks))+zrange/100
          breaks <- c(minbreak,breaks,maxbreak)
          rm(zmax,zmin,zrange,brange,minbreak,maxbreak)
       }
       rm(special)

       # determine col for contour plot if col is a colour table function
       if (exists(col)) {
            # This happens when the color table is specified as function
            # name and number of colors should be chosen automatically 
            # to fit breaks/levels.
          colfun <- get(col)
          col <- colfun(length(breaks)-1)
       } 
       if (length(breaks) != length(col)+1) {
          stop("Length of colours must be length of breaks plus 1")
       }

     }}
     

     # GRID COORDINATES, TYPE, PARAMETERS
     # -----------------------------------------------------
     if (!is.null(fld)) {
     	
        # ckeck dimensionality of fld
        if (length(dim(fld)) != 2) { stop("fld must have 2 dimensions. ") }
        
        # get grid.type
        grid.type=try(get.grid.atts(fld,what="grid.type")$grid.type,silent=TRUE)
        if (!is.character(grid.type) | (class(grid.type) == "try-error")) { 
           stop("Inadequate or missing attribute \"grid.type\" in \"fld\".") 
        }
        
        # get grid coordinates
        cor.nams <- grid.att.names(grid.type,what="grid.cors")
        grid.cors=try(get.grid.atts(fld,what="grid.cors"))
        if ((class(grid.cors) == "try-error") | (any(names(grid.cors) != cor.nams))) { 
           stop("Inadequate or missing attributes for grid coordinates in \"fld\".")
        }
        x <- grid.cors[[1]]
        y <- grid.cors[[2]]

        # get grid parameters
        par.nams <- grid.att.names(grid.type,what="grid.pars")
        grid.pars=try(get.grid.atts(fld,what="grid.pars"))
        if (class(grid.pars) == "try-error") { 
           stop("Inadequate or missing attributes for grid parameters in \"fld\".")
        }
     } else {
        
        # defaults for ptype="plain"
     	x <- NULL
     	y <- NULL
     	grid.type <- "lonlat"
     	grid.pars <- list()
     	
     }


     # PLOTTING
     # -----------------------------------------------------
     
     # prepare the layout of the plot
     if (legend) {layout(matrix(c(2,1),ncol=2),widths=c(1,leg.space))}

     # plot the color legend
     if (legend) {
       par(mar=mar.leg)
       plot.legend(col=col,
                   breaks=breaks[-c(1,length(breaks))],
                   density=density,angle=angle,slwd=slwd,
                   cex.leg=cex.leg)
     }

     # do the plot
     if (ptype == "image") {
       # plot the pixel plot
       if (legend) {par(mar=mar.plot)}
       image.map(x=x,y=y,z=fld,grid.type=grid.type,grid.pars=grid.pars,
                 col=col,breaks=breaks,...)
     } else if (ptype == "contour") {
       # plot the contour plot
       if (legend) {par(mar=mar.plot)}
       contour.map(x=x,y=y,z=fld,grid.type=grid.type,grid.pars=grid.pars,
                   col=col,levels=levels,...)
     } else if (ptype == "filled.contour") {
       # plot the filled contour plot
       if (legend) {par(mar=mar.plot)}
       filled.contour.map(x=x,y=y,z=fld,grid.type=grid.type,grid.pars=grid.pars,
               col=col,breaks=breaks,
               density=density,angle=angle,slwd=slwd,...)
     } else if (ptype == "plain") {
       # plot the filled contour plot
       if (legend) {par(mar=mar.plot)}
       plain.map(x=x,y=y,grid.type=grid.type,grid.pars=grid.pars,...)
     } else {
       stop("Invalid entry for argument ptype:",ptype)
     }
}

