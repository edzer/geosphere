# author of original JavaScript code: Chris Vennes
# (c) 2002-2009 Chris Veness
# http://www.movable-type.co.uk/scripts/latlong.html
# Licence: LGPL, without any warranty express or implied

# Port to R by Robert Hijmans
# October 2009
# version 0.1
# License GPL3


midPoint <- function(p1, p2) {
#* calculate midpoint of great circle line between p1 & p2.
#*   see http:#//mathforum.org/library/drmath/view/51822.html for derivation
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

	Bx <- cos(lat2) * cos(dLon)
	By <- cos(lat2) * sin(dLon)

	lat3 <- atan2(sin(lat1)+sin(lat2), sqrt((cos(lat1)+Bx)*(cos(lat1)+Bx) + By*By ) )
	lon3 <- lon1 + atan2(By, cos(lat1) + Bx)

	lon3[is.nan(lon3)] <- NA
	lat3[is.nan(lat3)] <- NA
	
	res <- cbind(lon3, lat3) / toRad
	colnames(res) <- c('lon', 'lat')
	return(res)
}

destPoint <- function(p, brng, d, r=6378137) {
#* calculate destination point given start point, initial bearing (deg) and distance (km)
#*   see http:#//williams.best.vwh.net/avform.htm#LL
# source http://www.movable-type.co.uk/scripts/latlong.html
# (c) 2002-2009 Chris Veness
	toRad <- pi / 180 

	p <- pointsToMatrix(p) * toRad
	lon1 <- p[,1] 
	lat1 <- p[,2]

	brng <- brng * toRad

	lat2 <- asin( sin(lat1)*cos(d/r) + cos(lat1)*sin(d/r)*cos(brng) )
	lon2 <- lon1 + atan2(sin(brng)*sin(d/r)*cos(lat1), cos(d/r)-sin(lat1)*sin(lat2))
	lon2 <- (lon2+pi)%%(2*pi) - pi  #// normalise to -180...+180
	lon2[is.nan(lon2)] <- NA
	lat2[is.nan(lat2)] <- NA

	res <- cbind(lon2, lat2) / toRad
	colnames(res) <- c('lon', 'lat')
	return(res)
}



polePoint <- function(lat, brng) {
# ‘Clairaut’s formula’ : the maximum latitude of a great circle path, given a bearing and latitude on the great circle
# source http://www.movable-type.co.uk/scripts/latlong.html
# (c) 2002-2009 Chris Veness
	toRad <- pi / 180 
	
	latMax <- acos(abs(sin(brng * toRad) * cos(lat * toRad)))
	latMax <- latMax / toRad 
	return(latMax)
}


