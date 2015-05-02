# Author: Robert J. Hijmans
# Date :  May 2015
# Licence GPL v3


distGeo <- function(p1, p2) {
	p1 <- .pointsToMatrix(p1) 
	p2 <- .pointsToMatrix(p2) 
	x  <- cbind(p1[,1], p1[,2], p2[,1], p2[,2])
	r <- .Call("inversegeodesic", as.double(x[,1]), as.double(x[,2]), as.double(x[,3]), as.double(x[,4]), PACKAGE='geosphere')
	r <- matrix(r, ncol=3, byrow=TRUE)
	r[, 1]
}

