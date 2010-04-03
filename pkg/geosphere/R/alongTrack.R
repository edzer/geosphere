# based on code by Ed Williams
# licence GPL
# http://williams.best.vwh.net/avform.htm#XTE

# Port to R by Robert Hijmans
# October 2009
# version 0.1
# license GPL3

alongTrackDistance <- function(p1, p2, p3, r=6378137) {
	toRad <- pi / 180 
	p1 <- .pointsToMatrix(p1) * toRad
	p2 <- .pointsToMatrix(p2) * toRad
	p3 <- .pointsToMatrix(p3) * toRad
	p <- cbind(p1[,1], p1[,2], p2[,1], p2[,2], p3[,1], p3[,2])
	p1 <- p[,1:2]
	p2 <- p[,3:4]
	p3 <- p[,5:6]
	
	tc <- bearing(p1, p2) * toRad
	tcp <- bearing(p1, p3) * toRad
    dp <- distCosine(p1, p3, r=1)
	xtr <- asin(sin(tcp-tc) * sin(dp))

# +1/-1 for ahead/behind [lat1,lon1]
	direction <- sign(cos(tc - tcp))  
	dist <- direction * acos(cos(dp) / cos(xtr)) * r
	
	if (is.vector(dist)) { dist <- matrix(dist) }
	colnames(dist) <- 'distance'
	
	return(dist)
}
