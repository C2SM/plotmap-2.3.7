`add.map.outlines` <- 
function(mapdat,lonlim.map,latlim.map,thin.map=0,thin.map2=thin.map,
     col="black",lwd=1,col2=col,lwd2=lwd) {
     if(mapdat == "none") { return() }
     
     # define a function for multiple vector appends
     multi.append <- function(x,values,after) {
        # append a sequence of values after indexes
     	     if ((length(after) == 0) | is.null(after)) {return(x)}
     	     inc <- length(values)
     	     after.exp <- after+(seq(1:length(after))-1) * (inc)
     	     for (kk in after.exp) {x <- append(x,values=values,after=kk)}
     	     x
     }
     # test 
     #a <- c(3,4,NA,7,8,12,13,14,15,19,20,30)
     #multi.append(a,value=c(43,44),after=c(5,9,11))
     #multi.append(a,value=NA,after=c(5,9,11))

     # get the map outlines from the desired map window and transform those 
     # into projection coordinates. Finally draw the lines
     mapdats <- strsplit(mapdat,split=":")[[1]]
     if (mapdat == "continents") {
       # continents must be read from external file. Are not available
       # in maps package, data was extracted from package clim.pact
       data(continents)
       mapcors <- mapproject(x=continents)
     } else if (mapdats[1] == "swiss") {
       # case for detailed map outlines of Switzerland.
       # are read from external file
       mapdats <- strsplit(mapdats[2],split="&")[[1]]
       if (all(!(c("border","rivers","lakes") %in% mapdats))) {
         stop("Inappropriate specification of mapdat.")
       }
       dd <- data.frame(name=c(NA),type=c(NA),chx=c(NA),chy=c(NA))
       if ("border" %in% mapdats) {
               data(swiss.border)
               dd <- rbind(dd,swiss.border)}
       dd <- put.grid.atts(dd[,c(3,4)],grid.type="swisscors")
       gg <- geocors.trafo(dd,to.type="lonlat")
       mapcors <- mapproject(list(x=gg[,"lon"],y=gg[,"lat"]))
       if ( ("rivers" %in% mapdats) || ("lakes" %in% mapdats) ) {
          dd2 <- data.frame(name=c(NA),type=c(NA),chx=c(NA),chy=c(NA))
          if ("rivers" %in% mapdats) {
               data(swiss.rivers)
               dd2 <- rbind(dd2,swiss.rivers)}
          if ("lakes" %in% mapdats) {
               data(swiss.lakes)
               dd2 <- rbind(dd2,swiss.lakes)}
          dd2 <- put.grid.atts(dd2[,c(3,4)],grid.type="swisscors")
          gg2 <- geocors.trafo(dd2,to.type="lonlat")
          mapcors2 <- mapproject(list(x=gg2[,"lon"],y=gg2[,"lat"]))
       }
     # added by F.Isotta 8.2010 (EURO4M domaine)
     } else if (mapdats[1] == "alps") {
       mapdats <- strsplit(mapdats[2],split="&")[[1]]
       if (all(!(c("border","borders","rivers","lakes") %in% mapdats))) {
         stop("Inappropriate specification of mapdat.")
       }
       dd <- data.frame(name=c(NA),type=c(NA),lon=c(NA),lat=c(NA))
       if (("borders" %in% mapdats) || ("border" %in% mapdats)) {
               data(alps.borders)
               dd <- rbind(dd,alps.borders)}
       dd <- put.grid.atts(dd[,c(3,4)],grid.type="lonlat")
       mapcors <- mapproject(list(x=dd[,"lon"],y=dd[,"lat"]))
       if ( ("rivers" %in% mapdats) || ("lakes" %in% mapdats) ) {
          dd2 <- data.frame(name=c(NA),type=c(NA),lon=c(NA),lat=c(NA))
          if ("rivers" %in% mapdats) {
               data(alps.rivers)
               dd2 <- rbind(dd2,alps.rivers)}
          if ("lakes" %in% mapdats) {
               data(alps.lakes)
               dd2 <- rbind(dd2,alps.lakes)}
          dd2 <- put.grid.atts(dd2[,c(3,4)],grid.type="lonlat")
          mapcors2 <- mapproject(list(x=dd2[,"lon"],y=dd2[,"lat"]))
       }
     } else if ((mapdat == "worldHires") | (mapdat == "oldWorld")) {
       # worldHires is read from map in packages map and mapdata
       # oldWorld is old boundaries from mapdata
       if (mapdat == "oldWorld") { mapdat <- NULL }
       mapcors <- map(mapdat,xlim=lonlim.map, ylim=latlim.map,
                      plot=FALSE)
       mapcors <- mapproject(list(x=mapcors$x,y=mapcors$y))
     } else {
       # new world boundaries is now default
       data(world.nations)
       mapcors <- mapproject(list(x=world.nations$border[,"lon"],
                                  y=world.nations$border[,"lat"]))
     }
     rm(mapdats)
     
     # thin the map if required 
     # this should be programmed better (takes very long) !!!!!!!!!
     do.thin.map <- function(mapcors,thin.map=0) {
         if (thin.map > 0) {
         for (tt in 1:thin.map) {
            llll <- length(mapcors$x)
            keep <- rep(c(TRUE,FALSE),length=llll)              # dilute
            keep[(is.na(mapcors$x) | is.na(mapcors$y))] <- TRUE # keep line breaks
            keep[llll] <- TRUE    # keep last
            keep[c(is.na(mapcors$x[-1]),TRUE)] <- TRUE  # keep last of a line segment
            keep[c(TRUE,is.na(mapcors$x[-llll]))] <- TRUE  # keep first of a line segment
            mapcors <- list(x=mapcors$x[keep],y=mapcors$y[keep])
         }
         rm(keep,llll)
         }
         mapcors
     }
     mapcors <- do.thin.map(mapcors=mapcors,thin.map=thin.map)
     if (exists("mapcors2")) {
         mapcors2 <- do.thin.map(mapcors=mapcors2,thin.map=thin.map2)
     }
     
     # if line segments wrap around (e.g. when plotting entire world) introduce 
     # line breaks by inserting NA values into line segment vector   
     max.len <- 0.333
     usr <- par("usr")
     critx <- max.len*diff(usr[c(1,2)]); crity <- max.len*diff(usr[c(3,4)]); 
     after <- which((abs(diff(mapcors$x)) > critx) | (abs(diff(mapcors$y)) > crity))
     mapcors$x <- multi.append(mapcors$x,values=NA,after=after)
     mapcors$y <- multi.append(mapcors$y,values=NA,after=after)
     if (exists("mapcors2")) {
        after <- which((abs(diff(mapcors2$x)) > critx) | (abs(diff(mapcors2$y)) > crity))
        mapcors2$x <- multi.append(mapcors2$x,values=NA,after=after)
        mapcors2$y <- multi.append(mapcors2$y,values=NA,after=after)
     }
     
     # draw outlines to the plot
     if (!is.null(mapcors) & (length(mapcors$x)>1)) {
        lines(x=mapcors$x,y=mapcors$y,xpd=FALSE,col=col,lwd=lwd)
     }
     if (exists("mapcors2")) {
        if (!is.null(mapcors2) & (length(mapcors2$x)>1)) {
           lines(x=mapcors2$x,y=mapcors2$y,xpd=FALSE,col=col2,lwd=lwd2)
        }
     }

}
