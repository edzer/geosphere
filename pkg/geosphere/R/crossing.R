# author Robert Hijmans
# October 2009
# version 0.1
# license GPL3


crossingParallels <- function(p1, p2, lat) {
# longitudes at which a given great circle crosses a given parallel
# source: http://williams.best.vwh.net/avform.htm

	toRad <- pi / 180 
	p1 <- pointsToMatrix(p1) * toRad
	p2 <- pointsToMatrix(p2) * toRad

	compareDim(p1, p2)
	
	
	lon1 <- p1[,1]
	lat1 <- p1[,2]
	lon2 <- p2[,1]
	lat2 <- p2[,2]
	lat3 <- lat * toRad
	
	l12 = lon1-lon2
	A <- sin(lat1)*cos(lat2)*cos(lat3)*sin(l12)
	B <- sin(lat1)*cos(lat2)*cos(lat3)*cos(l12) - cos(lat1)*sin(lat2)*cos(lat3)
	C <-  cos(lat1)*cos(lat2)*sin(lat3)*sin(l12)
	lon <- atan2(B,A)   
	
	lon3 <- matrix(NA, nrow=length(lon1), ncol=2)
	
	i <- !(abs(C) > sqrt(A^2 + B^2))
	lon3[-i,] <- NA

	dlon = acos(C/sqrt(A^2+B^2))
	lon3[i,1] <- (lon1[i]+dlon[i]+lon[i]+pi %% 2*pi)-pi
	lon3[i,2] <- (lon1[i]-dlon[i]+lon[i]+pi %% 2*pi)-pi
	
	lon3 <- lon3  / toRad
	return(lon3)
}

