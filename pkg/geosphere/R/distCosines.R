# Author: Robert J. Hijmans
# Date :  June 2008
# Licence GPL v3

# distance based on law of cosines
# http://en.wikipedia.org/wiki/Great_circle_distance

distCosine <- function(p1, p2, r=6378137) {
	p1 <- geosphere:::.pointsToMatrix(p1) 
	p2 <- geosphere:::.pointsToMatrix(p2) 
	pp  <- cbind(p1[,1], p1[,2], p2[,1], p2[,2], as.vector(r))

	# remove identical points to avoid errors due to floating point math
	i <- apply((p1 - p2) < .Machine$double.eps ^ 0.5, 1, all)
	p <- pp[!i, ,drop=FALSE]
	r <- rep(0, nrow(pp))
	if (nrow(p) > 0) {
		p[,1:4] <- p[,1:4] * pi / 180 
		r[i] <- acos( sin(p[,2]) * sin(p[,4]) + cos(p[,2]) * cos(p[,4]) * cos(p[,1]-p[,3]) ) * p[,5]
	}
	r
}

 
#distCosine(c(-69.652220,7.348464),c(-69.652220,7.348464))
#distCosine(c(-1,1),c(-1,1))
#distCosine(c(-1,1.1),c(-1,1.1))
#distCosine(c(-1,1.2),c(-1,1.2))

#	n <- nrow(p)
#	d <- vector("double", n)
#	d <- .C('distance', as.integer(n), as.double(p[,1]), as.double(p[,2]), as.double(p[,3]), as.double(p[,4]), as.double(p[,5]), as.integer(1), d)[[8]]
#	return(d)
