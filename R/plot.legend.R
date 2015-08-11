`plot.legend` <- 
# -------------------------------------------------------------------------
function(col,breaks,
         density=NULL,angle=45,slwd=par("lwd"),
         cex.leg=1) {
# -------------------------------------------------------------------------

# plots a color bar legend for an image.map or filled.contour.map plot. 

     nbrk <- length(breaks)
     ncol <- length(col)

     # check
     if (ncol != (nbrk+1)) {
        stop("Length of col must be length of breaks plus 1")
     }

     # arrays of color, shading, shading line-width and angles 
     if (is.null(density)) {
        dens <- NULL
     } else {
        dens <- rep(density,length=ncol)
     }
     lwds <- rep(slwd,length=ncol)
     angs <- rep(angle,length=ncol)

     # original version - does not work with shadings
     image(x=c(1),y=seq(1,(ncol+1))-0.5,z=matrix(seq(1,(ncol+1))-0.5,nrow=1),
             col=col,breaks=seq(1,(ncol+1)),
             ylim=c(1,(ncol+1)),axes=FALSE,xlab="",ylab="")
     # overplot with polygons to ensure shading if required
     for (k in 1:ncol) {
        polygon(x=c(0,2,2,0,0),y=c(k,k,k+1,k+1,k),
                col="white",border=NA,xpd=FALSE)
        polygon(x=c(0,2,2,0,0),y=c(k,k,k+1,k+1,k),
                col=col[k],density=dens[k],
                lwd=lwds[k],angle=angs[k],border=NA,xpd=FALSE)
     }

     # add break labels
     axis(4,lwd=0,at=seq(2,ncol),labels=breaks,
            las=1,tick=FALSE,cex.axis=cex.leg)
     box()

}


