`plot.austria.map` <-
function(fld,
         lonlim = c(9.5,17.0), latlim = c(46.3,48.8),
         mapdat="alps:borders&lakes&rivers",
         map.lwd=1.2,map.col="black",map.lwd2=0.4,map.col2=map.col,
         projection="stereographic",orientation=c(47.5,0.0,13.0),
         leg.space=0.14,box=TRUE,clip=FALSE,add.grid=FALSE,
         thin.map=0,thin.map2=2,...) {
# -------------------------------------------------------------------
# Plot a map of Austria.

   plot.alps.map(fld=fld,
         lonlim = lonlim, latlim = latlim,
         mapdat=mapdat,
         map.lwd=map.lwd,map.col=map.col,map.lwd2=map.lwd2,map.col2=map.col2,
         projection=projection,orientation=orientation,
         leg.space=leg.space,box=box,clip=clip,add.grid=add.grid,
         thin.map=thin.map,thin.map2=thin.map2,...)

}

