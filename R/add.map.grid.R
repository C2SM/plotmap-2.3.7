`add.map.grid` <- 
function(lon.grds = seq(-180, 180, by = 10), 
         lat.grds = seq( -80, +80, by = 10),
         delta.draw=45/200,
         col="black",lwd=0.5,lty=3) {

     for (i in lon.grds) {
        ygr <- seq(-90,90,by=delta.draw)
        grd <- mapproject(list(x=rep(i,length(ygr)),y=ygr))
        lines(x=grd$x,y=grd$y,lty=lty,lwd=lwd,xpd=FALSE)
     }
     for (i in lat.grds) {
        xgr <- seq(-180,180,by=delta.draw)
        grd <- mapproject(list(x=xgr,y=rep(i,length(xgr))))
        lines(x=grd$x,y=grd$y,lty=lty,lwd=lwd,xpd=FALSE)
     }

}



