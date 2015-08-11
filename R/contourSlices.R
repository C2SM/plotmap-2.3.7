`contourSlices` <- 
# -------------------------------------------------------------------------
function(x = seq(0, 1, len = nrow(z)),
         y = seq(0, 1, len = ncol(z)),
         z, 
         nlevels = 10,
         breaks = pretty.contours(range(z, na.rm=TRUE), n=nlevels)) {
# -------------------------------------------------------------------------

# calculates a stack of polygons for a color-filled contour plot.

# check
if (sum(diff(breaks)<0)>0) {
     stop("Some strange setting for breaks.")
}

#save(file= "contourSlices-test.rda",list=c("x","y","z","nlevels","breaks"))
#stop()
#load(file="contourSlices-test.rda")

# add borders with values outside plot range around the entire field 
# this is to ensure that contourLines returns closed contours
# define a value to be used for border
nafill <- breaks[1]-100*diff(breaks[c(1,length(breaks))])
nx <- length(x)
ny <- length(y)
dx1 <- diff(x[c(1,2)]); dx2 <- diff(x[c(nx-1,nx)])
dy1 <- diff(y[c(1,2)]); dy2 <- diff(y[c(ny-1,ny)])
xw <- c(x[1]-dx1,x,x[nx]+dx2)
yw <- c(y[1]-dy1,y,y[ny]+dy2)
zw <- z
zw <- rbind(c(nafill),zw,c(nafill))
zw <- cbind(c(nafill),zw,c(nafill))

# fill NAs with values well outside the contouring range
zw[is.na(zw)] <- nafill

# calculate contours
conts <- contourLines(x=xw,y=yw,z=zw,levels=breaks)

# delete contours that are single points
line.is.point <- function(line) {
              (length(unique(line$x))==1) & (length(unique(line$y))==1)
}
is.point <- lapply(conts,FUN=line.is.point)
conts <- conts[is.point==FALSE]

# determine whether inner part of contour is hill / valley
ncont <- length(conts)
hill <- rep(FALSE,ncont)
for (k in 1:ncont) {
   contk <- conts[[k]]

   # search the closest 4 grid points near point 1 of contour k
   # this also works with irregular spacings in x and y directions
   ddx <- contk$x[1] - xw
   ix1 <- rev(which(ddx > 0))[1]
   ix2 <- which(ddx < 0)[1]
   ddy <- contk$y[1] - yw
   iy1 <- rev(which(ddy > 0))[1]
   iy2 <- which(ddy < 0)[1]

   # assemble the closest points (x,y coordinates) in a matrix (x,y)
   cls.pts.ind <- matrix(NA,nrow=4,ncol=2)
   cls.pts.ind[,1] <- c(ix1,ix1,ix2,ix2)
   cls.pts.ind[,2] <- c(iy1,iy2,iy2,iy1)
   if ((ix1+1) < ix2) {
      cls.pts.ind[,1] <- c(ix1,ix1+1,ix2,ix1+1)
   } 
   if ((iy1+1) < iy2) {
      cls.pts.ind[,2] <- c(iy1+1,iy2,iy1+1,iy1)
   }
   cls.pts <- matrix(NA,nrow=4,ncol=2)
   cls.pts[,1] <- xw[cls.pts.ind[,1]]
   cls.pts[,2] <- yw[cls.pts.ind[,2]]
   cls.val <- zw[cls.pts.ind]

   # determine if points inside/outside
   inside.fun <- function(a) {
         in.polygon.pm(pt=a,
                    poly=matrix(c(contk$x,contk$y),byrow=FALSE,ncol=2),
                    output = "logical")
   }
   inside <- apply(cls.pts,FUN=inside.fun,MARGIN=c(1))

   # determine if hill or valley (by votation among the 4 selected pts.)
   votes <- (inside == (cls.val >= contk$level))
   conts[[k]]$hill <- (mean(votes,na.rm=TRUE) > 0.5)
}

# Calculate for each contour i if it is contained in contour k
# this is needed to determine the stack level for each contour.
# This part is the most CPU demanding. Investigate performance
# improvements by translating this into compiled code.
# (Note: A reformulation wit mapply() did not improve performance.)
ncont <- length(conts)
is.contained <- matrix(FALSE,ncol=ncont,nrow=ncont)
for (i in 1:ncont) {
for (k in 1:ncont) {
   if (i == k) {
       is.contained[i,k] <- FALSE
       next
   }
   conti <- conts[[i]]
   contk <- conts[[k]]

   # search a point of contour i not laying on contour k
   found <- FALSE
   ii <- 0
   nn <- length(conti$x)
   while(!found & (ii < nn)) {
      ii <- ii + 1
      found <- (sum((conti$x[ii] == contk$x) & 
                    (conti$y[ii] == contk$y)) == 0)
   }

   # calculate is.contained for one point of contour i
   nn <- length(conti$x)
   if (ii == nn) {
     # case when all points of contour i are laying on contour k
     if ((conti$hill != contk$hill) | (conti$level == contk$level)) {
       warning("Strange contours from contourLines()!")
     }
     is.contained[i,k] <- (conti$hill == (conti$level > contk$level))
   } else {
     is.contained[i,k] <- in.polygon.pm(
              pt=c(conti$x[ii],conti$y[ii]),
              poly=matrix(c(contk$x,contk$y),byrow=FALSE,ncol=2),
              output = "logical"
              )
   }
}}

# determine stack level of each contour and transfer to contour lines lists
stack.level <- apply(is.contained,FUN=sum,MARGIN=c(1))
for (k in 1:ncont) {
   conts[[k]]$stack.level <- stack.level[k]
}
rm(is.contained)

# remove working variables
rm(xw,yw,zw)

# return result
conts
}

