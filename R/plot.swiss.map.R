`plot.swiss.map` <-
function(fld,
         lonlim=c(6.0,10.4),latlim=c(45.85,47.8),
         mapdat="swiss:border&lakes&rivers",
         map.lwd=1,map.col="black",map.lwd2=map.lwd,map.col2=map.col,
         projection="stereographic",orientation=c(47.0,0.0,8.0),
         leg.space=0.12,box=FALSE,clip=TRUE,add.grid=FALSE,
         thin.map=0,thin.map2=thin.map,...) {
# -------------------------------------------------------------------
# Plot a map of Switzerland.

# Individual elements could be selected from the swiss border, rivers 
# and lakes dataset. This is not implemented yet. 

      # make the desired plot without map outlines and grids
      plot.map(fld=fld,lonlim=lonlim,latlim=latlim,
               mapdat="none",
               projection=projection,orientation=orientation,
               leg.space=leg.space,box=box,add.grid=FALSE,...)

      # clip field at the border if requested
      if (clip) {
        clip.swiss()
      }

      # add grid if requested
      if (add.grid) {
        usr <- par("usr")
        map.win <- list(xlim=usr[c(1,2)],ylim=usr[c(3,4)])
        delta.draw <- min(diff(map.win$xlim)/200,diff(map.win$ylim)/200)
        add.map.grid(delta.draw=delta.draw)
        rm(delta.draw)
      }

      # add map outlines
      add.map.outlines(mapdat=mapdat,lonlim.map=lonlim,latlim.map=latlim,
                       thin.map=thin.map,thin.map2=thin.map2,
                       lwd=map.lwd,col=map.col,
                       lwd2=map.lwd2,col2=map.col2)

}