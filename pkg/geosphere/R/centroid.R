# Author: Robert J. Hijmans
# April 2010
# version 0.1
# license GPL3

# See http://local.wasp.uwa.edu.au/~pbourke/geometry/polyarea/

.basiccentroid <- function(p) {
	p2 = rbind(p[-1,], p[1,])
	P = p[,1] * p2[,2] - p2[,1] * p[,2]
	area6 <- 6 * sum(P) / 2
    lon <- sum((p[,1] + p2[,1]) * P)
    lat <- sum((p[,2] + p2[,2]) * P)
	return(cbind(lon, lat) / area6 )
}

if (!isGeneric("centroid")) {
	setGeneric("centroid", function(x, ...)
		standardGeneric("centroid"))
}	

setMethod("centroid", signature(x='matrix'), 
function(x) {
	x <- .pointsToMatrix(x)
	x <- mercator(x, r=1)
	cenM <- .basiccentroid(x)
	cenG <- mercator(cenM, r=1, inverse=TRUE)
	return(cenG)
}
)
