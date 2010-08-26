# Author: George Wang & Robert J. Hijmans
# August 2010
# version 1
# license GPL3


distPoint2Polyline <- function(p, pline, distfun=distHaversine, ispoly=FALSE) {

	xy <- geosphere:::.pointsToMatrix(p)
	pline <- geosphere:::.pointsToMatrix(pline, poly=ispoly) 
	
	# find the closest node 
	d <- distfun(xy, pline)
	near <- which.min(d)
	
	if (ispoly) {
		a <- which(pline[-nrow(pline),] == pline[-1,])
		if (length(a) > 0) {
			pline <- pline[-a, ,drop=FALSE]
		}

		d <- distfun(xy, poly)
		near <- which.min(d)
		if (near == 1) {
			seg1 <- poly[c(nrow(poly)-1, near), ,drop=FALSE]
			seg2 <- poly[c(near,(near+1)), ,drop=FALSE]
		} else if (near == nrow(poly)) {
			seg1 <- poly[c((near-1),near), ,drop=FALSE]
			seg2 <- poly[c(near, 2), ,drop=FALSE]	
		} else {
			seg1 <- poly[c((near-1),near), ,drop=FALSE]
			seg2 <- poly[c(near,(near+1)), ,drop=FALSE]
		}	
	} else {
		# should not have consecutive points that are equal
		a <- which(pline[-nrow(pline),] == pline[-1,])
		if (length(a) > 0) {
			pline <- pline[-a, ,drop=FALSE]
		}

		if (nrow(pline) <= 2 ) { # could also be one in a degenerate case
			seg1 <- pline
			seg2 <- pline
		} else if (near == 1) {
			seg1 <- seg2 <- pline[c(near, near+1), ,drop=FALSE]
		} else if (near == nrow(pline)) {
			seg1 <- seg2 <- pline[c((near-1),near), ,drop=FALSE]
		} else {
			seg1 <- pline[c((near-1),near), ,drop=FALSE]
			seg2 <- pline[c(near,(near+1)), ,drop=FALSE]
		}
	}
	
	seg1length  <- distfun(seg1[1,], seg1[2,])
	seg2length  <- distfun(seg2[1,], seg2[2,])
	
	trackdist1 <- alongTrackDistance(seg1[1,], seg1[2,], xy)
	trackdist2 <- alongTrackDistance(seg2[1,], seg2[2,], xy)
	
	#if the track is longer than the segment, it's not on the segment.
	trackdist1[which(trackdist1 >= seg1length)] <- NA 
	trackdist2[which(trackdist2 >= seg2length)] <- NA 

	bear1 <- bearing(seg1[1,], seg1[2,])
	bear2 <- bearing(seg2[1,], seg2[2,])
	
	trackpoint1  <- destPoint(seg1[1,], bear1, trackdist1)
	trackpoint2  <- destPoint(seg2[1,], bear2, trackdist2)
	
    tpdist1 <- distfun(trackpoint1, xy)
    tpdist2 <- distfun(trackpoint2, xy)
    
	dist <- c(d[near], tpdist1, tpdist2)
	segmins <- which.min(dist)
	nearpoint <- rbind(pline[near,], trackpoint1, trackpoint2)[segmins,]
	dist <- c(min(dist, na.rm=TRUE), nearpoint)
	names(dist) <- c("dist","lon","lat")
	return(dist)
}

 #poly <- rbind(c(-180,-20), c(-140,55), c(10, 0), c(-140,-60), c(-180,-20))
 #pnt <- cbind(-150,0)
 #d = point2PolyLine(pnt, poly)
 #plot( makePoly(poly), type='l')
 #points(poly)
 #points(pnt, col='blue', pch=20)
 #points(d[2], d[3], col='red', pch='x')
 
 
 
 

