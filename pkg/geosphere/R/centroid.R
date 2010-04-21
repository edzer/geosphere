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
	dif1 = max(x[,1]) - min(x[,1])
	x2 = x
	x2[x2[,1] < 0, 1] <- x2[x2[,1] < 0, 1] + 360
	dif2 = max(x2[,1]) - min(x2[,1]) + 0.1
	rotate = FALSE
	if (dif2 < dif1) {
		rotate = TRUE
		x2[,1] = x2[,1] - 180
		x = x2
	}
	x <- mercator(x, r=1)
	cenM <- .basiccentroid(x)
	cenG <- mercator(cenM, r=1, inverse=TRUE)
	if (rotate) {
		cenG[,1] = cenG[,1] + 180
		cenG[cenG[,1] > 180,1] = cenG[cenG[,1] > 180,1] - 360
	}
	rownames(cenG) = NULL
	return(cenG)
}
)



setMethod("centroid", signature(x='SpatialPolygons'), 
function(x) {
	x = x@polygons
	n = length(x)
	res = matrix(nrow=n, ncol=2)
	for (i in 1:n) {
		parts = length(x[[i]]@Polygons )
		parea = sapply(x[[i]]@Polygons, function(y){slot(y, "area")} )
		hole = sapply(x[[i]]@Polygons, function(y){slot(y, "hole")} )
		parea[hole] = -1
		j = which.max(parea)
		crd = x[[i]]@Polygons[[j]]@coords
		res[i,] = centroid(crd)
	}
	return(res)
} )

