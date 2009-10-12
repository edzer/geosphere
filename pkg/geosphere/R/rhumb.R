# author of original JavaScript code: Chris Vennes
# (c) 2002-2009 Chris Veness
# http://www.movable-type.co.uk/scripts/latlong.html
# Licence: LGPL, without any warranty express or implied

# Port to R by Robert Hijmans
# October 2009
# version 0.1
# license GPL3


distRhumb <- function(p1, p2, r=6378137) {
#* calculate distance, bearing, destination point on rhumb line
#*   see http:#//williams.best.vwh.net/avform.htm#Rhumb
	toRad <- pi / 180 
	p1 <- pointsToMatrix(p1) * toRad
	p2 <- pointsToMatrix(p2) * toRad
  
    compareDim(p1, p2)
  
	lon1 <- p1[,1]
	lat1 <- p1[,2]
	lon2 <- p2[,1]
	lat2 <- p2[,2]

	dLat <- (lat2-lat1) 
	dLon <- abs(lon2-lon1)
	dPhi <- log(tan(lat2/2 + pi/4)/tan(lat1/2 + pi/4))

	q <- vector(length=length(dLat))
	i <- abs(dLat) > 1e-10 
	q[i] <- dLat/dPhi
	q[-i]  <- cos(lat1) 
	
  #// if dLon over 180° take shorter rhumb across 180° meridian:
	dLon[dLon > pi] <- 2*pi - dLon[dLon > pi]  

	d <- sqrt(dLat*dLat + q*q*dLon*dLon) 
	return(d * r)
}

# You are welcome to re-use these scripts [under a LGPL license, without any warranty express or implied] 
# provided solely that you retain my copyright notice and a link to this page.
# If you would like to show your appreciation, I would most gratefully accept donations.
# If you have any queries or find any problems, contact me at ku.oc.epyt-elbavom@oeg-stpircs.
# (c) 2002-2009 Chris Veness

# Port to R by Robert Hijmans

brngRhumb <- function(p1, p2) {
# source http://www.movable-type.co.uk/scripts/latlong.html
# (c) 2002-2009 Chris Veness

	toRad <- pi / 180 
	p1 <- pointsToMatrix(p1) * toRad
	p2 <- pointsToMatrix(p2) * toRad
    compareDim(p1, p2)
  
	lon1 <- p1[,1]
	lat1 <- p1[,2]
	lon2 <- p2[,1]
	lat2 <- p2[,2]

	dLon <- (lon2-lon1)
	dPhi <- log(tan(lat2/2 + pi/4)/tan(lat1/2+pi/4))
	i <- (abs(dLon) > pi)
	j <- i && dLon > 0
	dLon[j] <- -(2*pi-dLon[j])
	j <- i && dLon <= 0
	dLon[j] <- dLon[j] <- (2*pi+dLon[j])
	
	b <- atan2(dLon, dPhi)
	b <- b / toRad
	b <- (b+360) %% 360
	names(b) <- NULL
	return(b)
}


destPointRhumb <- function(p, brng, dist, r=6378137) {
# source http://www.movable-type.co.uk/scripts/latlong.html
# (c) 2002-2009 Chris Veness

	p <- pointsToMatrix(p)
	lon <- p[,1]
	lat <- p[,2]

	toRad <- pi / 180 
	toDeg <- 1 / toRad

	d <- dist/r  #// d <- angular distance covered on earth's surface
	lat1 <- lat * toRad
	lon1 <- lon * toRad
	brng <- brng * toRad

	lat2 <- lat1 + d*cos(brng)
	dLat <- lat2-lat1
	dPhi <- log(tan(lat2/2+pi/4)/tan(lat1/2+pi/4))
	if(abs(dLat) > 1e-10) { q <- dLat/dPhi } else { q <- cos(lat1) }
	dLon <- d*sin(brng)/q
  #// check for some daft bugger going past the pole
	if (abs(lat2) > pi/2) {
		if (lat2 >0) {
			lat2 <- pi-lat2
		} else {
			lat2 <- -(pi-lat2)
		}
	}
  
	lon2 <- (lon1+dLon+pi)%%(2*pi - pi)
 
	if (is.nan(lat2) || is.nan(lon2)) return(NULL)
	res <- cbind(lon2, lat2) * toDeg
	colnames(res) <- c('lon', 'lat')
	return(res)
}
