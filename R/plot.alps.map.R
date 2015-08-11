`plot.alps.map` <-
function(fld,
         lonlim = c(2.4,16.8), latlim = c(43.2,48.85),
         mapdat="alps:borders&lakes&rivers",
         map.lwd=1.2,map.col="black",map.lwd2=0.4,map.col2=map.col,
         projection="stereographic",orientation=c(46.0,0.0,7.5),
         leg.space=0.14,box=TRUE,clip=FALSE,add.grid=FALSE,
         thin.map=0,thin.map2=2,...) {
# -------------------------------------------------------------------
# Plot a map of the region 2E-17E,43N-47N (EURO4M).
# F.Isotta modification of the original code of C. Frei (plot.swiss.map.R)

# Individual elements could be selected from the swiss border, rivers 
# and lakes dataset. This is not implemented yet. 

      # make the desired plot without map outlines and grids
      plot.map(fld=fld,lonlim=lonlim,latlim=latlim,
               mapdat="none",
               projection=projection,orientation=orientation,
               leg.space=leg.space,box=box,add.grid=FALSE,...)

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

