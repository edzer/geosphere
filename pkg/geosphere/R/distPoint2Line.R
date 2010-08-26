# Author: George Wang & Robert J. Hijmans
# August 2010
# version 1
# license GPL3


distPoint2Line <- function(p, line, distfun=distHaversine) {

	p <- geosphere:::.pointsToMatrix(p)
	line <- geosphere:::.pointsToMatrix(line) 
	line1 <- line[-nrow(line), ,drop=FALSE]
	line2 <- line[-1, ,drop=FALSE]
	seglength  <- distfun(line1, line2)
	
	res <- matrix(nrow=nrow(p), ncol=3)
	colnames(res) <- c("distance","lon","lat")
	
	for (i in 1:nrow(p)) {
		xy <- p[i,]
# the crossTrackDistance is the shortest distance of a point to a great circle
		crossdist <- abs(crossTrackDistance(line1, line2, xy))
		
# the alongTrackDistance is the length of the path along the great circle to the point of intersection
# there are two, depending on which node you start
# we want to use the min, but the max needs te be < segment lenght
		trackdist1 <- alongTrackDistance(line1, line2, xy)
		trackdist2 <- alongTrackDistance(line2, line1, xy)
		mintrackdist <- pmin(trackdist1, trackdist2)
		maxtrackdist <- pmax(trackdist1, trackdist2)
		crossdist[maxtrackdist >= seglength] <- NA 
		
# if the crossdist is NA, we use the distance to the nodes
		nodedist <- distfun(xy, line)
		
		warnopt = getOption('warn')
	 	options('warn'=-1) 		
		distmin1 <- min(nodedist, na.rm=TRUE)
		distmin2 <- min(crossdist, na.rm=TRUE)
		options('warn'= warnopt) 
		
		if (distmin1 <= distmin2) {
			j <- which.min(distmin1)
			res[i,] <- c(distmin1, line[j,])
		} else {
			j <- which.min(crossdist)
			# if else to determine from which node to start
			if (trackdist1[j] < trackdist2[j]) {
				bear <- bearing(line1[j,], line2[j,])
				pt <- destPoint(line1[j,], bear, mintrackdist[j])
				res[i,] <- c(crossdist[j], pt)
			} else {
				bear <- bearing(line2[j,], line1[j,])
				pt <- destPoint(line2[j,], bear, mintrackdist[j])
				res[i,] <- c(crossdist[j], pt)	
			}
		}
	}
	return(res)
}

 #poly <- rbind(c(-180,-20), c(-140,55), c(10, 0), c(-140,-60), c(-180,-20))
 #pnt <- rbind(c(-150,0), c(-75,0), c(-70,-10), c(-80,20), c(-100,-50), c(-100,-60), c(-100,-40), c(-100,-20), c(-100,-10), c(-100,0))
 #d = distPoint2Line(pnt, poly)
 #plot( makePoly(poly), type='l')
 #points(poly)
 #points(pnt, col='blue', pch=20)
 #points(d[,2], d[,3], col='red', pch='x')
 #for (i in 1:nrow(d)) lines(gcIntermediate(pnt[i,], d[i,2:3], 10), lwd=2)

 

