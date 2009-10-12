# Author: Robert J. Hijmans
# October 2009
# version 0.1
# license GPL3


isAntipodal <- function(p1, p2) {
	toRad <- pi / 180 
	p1 <- pointsToMatrix(p1) * toRad
	p2 <- pointsToMatrix(p2) * toRad
	return( isTRUE(all.equal(p1[,2]+p2[,2], 0)) & isTRUE(all.equal(abs(p1[,1]-p2[,1]), pi)) ) 
}


antipode <- function(p) {
	toRad <- pi / 180 
	p <- pointsToMatrix(p) * toRad
	lon <- -p[,1]
	lat <- -pi + p[,2]
	return( cbind(lon,lat) / toRad )
}

