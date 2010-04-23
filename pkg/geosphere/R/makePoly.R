# author Robert Hijmans
# April 2010
# version 0.1
# license GPL


makePoly <- function(p, interval=10000, r=6378137, sp=FALSE) {
	p <- .pointsToMatrix(p)
	if (nrow(p) < 3) {
		stop('cannot make a polygon (insufficent number of vertices)')
	}
	if (! isTRUE(all.equal(p[1,], p[nrow(p),]))) {
		p <- rbind(p, p[1,])
	}
	res <-NULL
	for (i in 1:(nrow(p)-1)) {
		d <- distCosine(p[i,], p[i+1,], r=r)
		n <- ceiling(d / interval)
		pts <- gcIntermediate(p[i,],p[i+1,], n)
		pts <- rbind(p[i,], pts, p[i+1,])
		res <- rbind(res, pts)
	}
	if (sp) {
		if (! require(sp) ) {
			stop("you need to install the 'sp' package to have the result returned as an sp object (or use sp=FALSE)")
		}
		res <- SpatialPolygons(list(Polygons(list(Polygon(res)), 1)))		
	}
	return(res)
}
 
