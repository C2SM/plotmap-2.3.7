`in.polygon.pm` <- 
# -------------------------------------------------------------------------
function(pt,poly,output="logical",method="crossings") {
# -------------------------------------------------------------------------

# determines whether a point is within / on the border or outside a polygon.

# check method
if ( !(method %in% c("angles","crossings")) ) {
   stop("method ", method, " not implemented in in.polygon.pm().")
}

# output function
out <- function(value,output) {
          if (output == "numeric") { 
             return(value) 
          }
          if (output == "logical") {
             switch(as.character(value),
                 "1" = return(TRUE),
                 "0" = return(NA),
                "-1" = return(FALSE)
             )
           }
}

# check that there are at least 3 unique points of the polygon
warn <- FALSE
if (length(poly[,1]) < 3) { 
      warn <- TRUE
} else {
      npx <- length(unique(poly[,1]))
      npy <- length(unique(poly[,2]))
      warn <- !((npx > 2) | (npy > 2) | ((npx==2) & (npy==2)))
}
if (warn) {
    warning("Strange input for polygon in in.polygon.pm!")
}

# -----------------------------------------------------------------------------
# Method of adding angles
# -----------------------------------------------------------------------------
if (method == "angles") {
	
   # calculate distances to all poly-corners
   ax <- poly[,1]-pt[1]
   ay <- poly[,2]-pt[2]
   ll <- sqrt(ax^2+ay^2)

   # case when point coincides with one of the poly-corners
   medll <- median(ll,na.rm=TRUE)
   if (min(ll)<0.000000001*medll) {return(out(0,output))}

   # normalize vectors from point to poly-corners
   ax <- ax/ll
   ay <- ay/ll

   # produce cycled vectors b
   bx <- c(ax[-1],ax[1])
   by <- c(ay[-1],ay[1])

   # calculate triangel's angles
   qq <- ax*by-ay*bx
   qq[qq>1] <- 1; qq[qq<(-1)] <- (-1)   # necessary because of numerical errors
   pp <- ax*bx+ay*by
   chi <- asin(qq)
   qq <- sign(qq)
   pp <- sign(pp)
   angls <- qq*(pi*(1-pp)/2+qq*pp*chi)
   ang.tot <- abs(sum(angls))

   # on the line ?
   if ((min(abs(angls)) < 0.000001) & 
       (abs(ang.tot - pi) < 0.000001)) {
         return(out(0,output))
   }

   # within or not
   if (ang.tot > pi) { 
        return(out(1,output)) 
   } else { 
        return(out(-1,output)) 
   }

}

# -----------------------------------------------------------------------------
# Method of counting x-axis crossings right to the point (even or odd)
# -----------------------------------------------------------------------------
if (method == "crossings") {

   is.odd <- function(x) { ((x %% 2) > 0) }

   # shift coordinates
   ax <- poly[,1]-pt[1]
   ay <- poly[,2]-pt[2]

   # check if obviously outside (increases performance on average)
   if (max(ax) < 0) { return(out(-1,output)) }
   if (max(ay) < 0) { return(out(-1,output)) }
   if (min(ax) > 0) { return(out(-1,output)) }
   if (min(ay) > 0) { return(out(-1,output)) }
   
   # check if pt is on a polygon point and return if so
   pt.is.poly.pt <- any( (ax == 0) & (ay == 0) ) 
   if (pt.is.poly.pt) { return(out(0,output)) }
   
   # check if any polygon point is on x-axix and rotate if so
   if (any(ay == 0)) {
      ang.inc <- pi/500
      rot.mat <- matrix(c( cos(ang.inc),sin(ang.inc),
                          -sin(ang.inc),cos(ang.inc) ),
                       nrow=2,ncol=2,byrow=TRUE)
      hh <- matrix(c(ax,ay),ncol=2,byrow=FALSE)
      cnt <- 0
      while ( any(ay==0) & (cnt < 5) ) {
         cnt <- cnt+1
         hh <- t( rot.mat %*% t(hh) )
         ax <- hh[,1]
         ay <- hh[,2]
      }
      if ( cnt >= 5 ) { 
         stop("Could not appropriately rotate polygon in in. polygon.pm") 
      }
      rm(cnt,ang.inc,rot.mat,hh)
   }
   
   # determine polygon sides that cross x-axis
   ixax <- which(abs(diff(c(ay,ay[1]) > 0)) > 0.5)
   
   # directly return if no crossing at all (most frequent case)
   if (length(ixax) == 0) { return(out(-1,output)) }
   
   # check if even number of crossings - otherwize stop
   if ( is.odd(length(ixax)) ) {
      stop("Something went really wrong in in.polygon.pm()")
   }
   
   # define end points of polygon lines
   nn <- length(ax)
   ixax.e <- ((ixax %% nn) + 1)
   
   # extract beginning and end points of crossing polygon lines
   aax <- ax[ixax]; aay <- ay[ixax]
   bbx <- ax[ixax.e]; bby <- ay[ixax.e]
   
   # determine x-coordinate of crossings
   xx <- aax - aay*(bbx-aax)/(bby-aay)
   
   # if point is on polygon
   if (any(xx == 0)) { return(out(0,output))}
   
   # within or not
   if ( is.odd(sum(xx>0)) ) { 
        return(out(1,output)) 
   } else { 
        return(out(-1,output)) 
   }

}

}



