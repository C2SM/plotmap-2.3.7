grid.center <- 
function(fld, to.type="lonlat", ...) {
     # get grid coordinates and type
     gc <- get.grid.atts(fld,what="grid.cors")
     gt <- get.grid.atts(fld,what="grid.type")[[1]]
     # average over x/y coordinates individually (whatever coordinate system used)
     cen <- lapply(gc,FUN=median)
     # transform into target coordinate system
     cen.lonlat <- geocors.trafo(cen,from.type=gt,to.type=to.type,...)
     cen.lonlat
}
