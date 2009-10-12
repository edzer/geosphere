# author of original JavaScript code: Chris Vennes
# (c) 2002-2009 Chris Veness
# http://www.movable-type.co.uk/scripts/latlong.html
# Licence: LGPL, without any warranty express or implied

# Port to R by Robert Hijmans
# October 2009
# version 0.1
# license GPL3

crossTrackDistance <- function(p1, p2, p3, r=6378137) {
	toRad <- pi / 180 
	p1 <- pointsToMatrix(p1) * toRad
	p2 <- pointsToMatrix(p2) * toRad
	p3 <- pointsToMatrix(p3) * toRad
	compareDim(p1, p2, p3)
	
    d13 <- distVincentySphere(p1, p3)
	b13 <- bearing(p1, p3)
	b12 <- bearing(p1, p2)
	
	dxt <- asin(sin(d13/r)*sin(b13-b12)) * r
	return(dxt)
}


alongTrackDistance <- function(p1, p2, p3, r=6378137) {
	toRad <- pi / 180 
	p1 <- pointsToMatrix(p1) * toRad
	p2 <- pointsToMatrix(p2) * toRad
	p3 <- pointsToMatrix(p3) * toRad
	compareDim(p1, p2, p3)

    d13 <- distVincentySphere(p1, p3)
	dxt <- crossTrackDistance(p1, p2, p3, r)
	
	dat <- acos(cos(d13/r)/cos(dxt/r)) * r
	return(dat)
}

