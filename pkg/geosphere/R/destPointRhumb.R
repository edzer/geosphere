# author of original JavaScript code: Chris Vennes
# (c) 2002-2009 Chris Veness
# http://www.movable-type.co.uk/scripts/latlong.html
# Licence: LGPL, without any warranty express or implied

# see http://williams.best.vwh.net/avform.htm#Rhumb
# for the original formulae

# Port to R by Robert Hijmans
# October 2009
# version 0.1
# license GPL3


destPointRhumb <- function(p, brng, d, r=6378137) {

	brng <- as.vector(brng)
	d <- as.vector(d)
	r <- as.vector(r)
	p <- .pointsToMatrix(p)
	p <- cbind(p, brng, d, r)
	
	lon <- p[,1]
	lat <- p[,2]
	lat[lat==90|lat==-90] <- NA
	brng <- p[,3]
	d <- p[,4]
	r <- p[,5]
	
	toRad <- pi / 180 
	toDeg <- 1 / toRad

	d <- d/r  #// d <- angular distance covered on earth's surface (ie in radians)
	lat1 <- lat * toRad
	lon1 <- lon * toRad
	brng <- brng * toRad

	lat2 <- lat1 + d * cos(brng)
	dLat <- lat2-lat1
	dPhi <- log(tan(lat2/2+pi/4)/tan(lat1/2+pi/4))
	
	i <- abs(dLat) > 1e-10 
	q <- vector(length=length(i))
	q[i] <- dLat/dPhi 
	q[!i] <- cos(lat1)

	dLon <- d * sin(brng) / q
	
#// check for some daft bugger going past the pole
	i <- (abs(lat2) > pi/2) & lat2 > 0
	lat2[i] <- pi-lat2[i]
	i <- (abs(lat2) > pi/2) & lat2 <= 0
	lat2[i] <- (pi-lat2[i])
  
	lon2 <- (lon1+dLon+pi)%%(2*pi) - pi
 
	res <- cbind(lon2, lat2) * toDeg
	colnames(res) <- c('lon', 'lat')
	return(res)
}

