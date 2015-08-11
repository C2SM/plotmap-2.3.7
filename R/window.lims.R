`window.lims` <- 
function(xlim,ylim,grid.type="lonlat",grid.pars=list(),out="proj",
         projection,parameters,orientation,npts=100) {

   # define a rectangular frame  (matrix with two columns) in user coordinates
   # --> fram.user
   xx <- seq(xlim[1],xlim[2],length=npts)
   yy <- seq(ylim[1],ylim[2],length=npts)
   fram.user <- matrix(rep(0,2*2*(npts+npts)),ncol=2)
   fram.user[,1] <- c(xx,rep(xx[npts],npts),rev(xx),rep(xx[1],npts))
   fram.user[,2] <- c(rep(yy[1],npts),yy,rep(yy[npts],npts),rev(yy))

   # convert frame into longitude latitude
   fram.lonlat <- geocors.trafo(x=fram.user,
                       from.type=grid.type,from.pars=grid.pars,
                       to.type="lonlat",to.pars=list())
                       
   # convert frame into projection coordinates --> xplim, yplim
   if (out == "lonlat") {
      ll <- list(x=fram.lonlat[,1],y=fram.lonlat[,2])
   }
   if (out == "proj") {
      ll <- mapproject(x=list(x=fram.lonlat[,1],y=fram.lonlat[,2]),
                       projection=projection,
                       parameters=parameters,orientation=orientation)
   }
   xplim <- range(ll$x,finite=TRUE)
   yplim <- range(ll$y,finite=TRUE)
   
   # cleanup
   rm(ll,fram.user,fram.lonlat,xx,yy)

   # return result
   list(xlim=xplim,ylim=yplim)
}
