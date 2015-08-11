`clip.swiss` <-
function(col="white") {
# ----------------------------------------------------------------------
# clips off areas outside swiss border in an open graphics object. 

# Read border from file
data(swiss.border)

# Extract Switzerland including Liechtenstein border
idx <- swiss.border[,"name"] == "CH"
bord <- copy.grid.atts(swiss.border,swiss.border[idx,c("chx","chy")])

# Convert in lat-lon coordinates if necessary
#bord <- swiss2ll(ch.east=bord[,3],ch.north=bord[,4])
bord <- geocors.trafo(bord,to.type="lonlat")

# Remove NAs
bord <- bord[!is.na(bord[,1]),]

# Transform into projection coordinates
hh <- mapproject(list(x=bord[,"lon"],y=bord[,"lat"]))

# get plot window 
# plot window is slightly enlarged (+0.02) to ensure complete covering 
win <- par("usr")
x1 <- win[1]-0.02; x2 <- win[2]+0.02
y1 <- win[3]-0.02; y2 <- win[4]+0.02

# reorder polygon such that leftmost point is start
i.l <- which(hh$x == min(hh$x,na.rm=TRUE))[1]
hh$x <- c(hh$x[i.l:length(hh$x)],hh$x[1:i.l])
hh$y <- c(hh$y[i.l:length(hh$y)],hh$y[1:i.l])

# reorder polygon such that cycling is clockwise
i.u <- which(hh$y == max(hh$y,na.rm=TRUE))[1]
i.d <- which(hh$y == min(hh$y,na.rm=TRUE))[1]
if (i.d < i.u) {
   hh$x <- rev(hh$x); hh$y <- rev(hh$y)
}

# assemble polygons for upper and lower portions of clipping
i.l <- which(hh$x == min(hh$x,na.rm=TRUE))[1]
i.r <- which(hh$x == max(hh$x,na.rm=TRUE))[1]
p1x <- c(hh$x[i.l:i.r],x2,x2,x1,x1,hh$x[i.l])
p1y <- c(hh$y[i.l:i.r],hh$y[i.r],y2,y2,hh$y[i.l],hh$y[i.l])
p2x <- c(hh$x[i.r:length(hh$x)],x1,x1,x2,x2,hh$x[i.r])
p2y <- c(hh$y[i.r:length(hh$y)],hh$y[i.l],y1,y1,hh$y[i.r],hh$y[i.r])

# draw polygons
polygon(x=p1x,y=p1y,col=col,border=NA) 
polygon(x=p2x,y=p2y,col=col,border=NA) 

}

