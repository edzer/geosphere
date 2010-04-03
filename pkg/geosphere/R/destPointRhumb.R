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
	p <- .pointsToMatrix(p)
	p <- cbind(p, brng, d)
	
	lon <- p[,1]
	lat <- p[,2]
	brng <- p[,3]
	d <- p[,4]
	
	toRad <- pi / 180 
	toDeg <- 1 / toRad

	d <- d/r  #// d <- angular distance covered on earth's surface
	lat1 <- lat * toRad
	lon1 <- lon * toRad
	brng <- brng * toRad

	lat2 <- lat1 + d*cos(brng)
	dLat <- lat2-lat1
	dPhi <- log(tan(lat2/2+pi/4)/tan(lat1/2+pi/4))
	if(abs(dLat) > 1e-10) { q <- dLat/dPhi } else { q <- cos(lat1) }
	dLon <- d*sin(brng)/q
	
  #// check for some daft bugger going past the pole
	i <- (abs(lat2) > pi/2) & lat2 > 0
	lat2[i] <- pi-lat2[i]
	i <- (abs(lat2) > pi/2) & lat2 <= 0
	lat2[i] <- -1*(pi-lat2[i])
  
	lon2 <- (lon1+dLon+pi)%%(2*pi - pi)
 
	if (is.nan(lat2) || is.nan(lon2)) return(NULL)
	res <- cbind(lon2, lat2) * toDeg
	colnames(res) <- c('lon', 'lat')
	return(res)
}

