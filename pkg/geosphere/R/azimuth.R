# Author: Robert J. Hijmans
# Date :  March 2010
# Version 1.0
# Licence GPL v3


azimuth <- function(p1, p2) {
	toRad <- pi / 180 
	p1 <- .pointsToMatrix(p1) * toRad
	p2 <- .pointsToMatrix(p2) * toRad
	p <- cbind(p1[,1], p1[,2], p2[,1], p2[,2])	
	p1 <- p[,1:2,drop=FALSE]
	p2 <- p[,3:4,drop=FALSE]

	dLon = p2[,1] - p1[,1] 
    y = sin(dLon)  * cos(p2[,2]) 
    x = cos(p1[,2]) * sin(p2[,2]) - sin(p1[,2]) * cos(p2[,2]) * cos(dLon) 
    azm = atan2(y, x) / toRad
    azm[azm < 0] <-  360 + azm[azm < 0] 
	return(azm)
}


