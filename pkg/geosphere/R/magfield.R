# Author: Robert J. Hijmans
# License GPL3
# Version 1 June 2011


magDec <- function(p, date='2005-06-30', model='IGRF2005', h=0) { 

	models <- c('IGRF90','WMM85','WMM90','WMM95','IGRF95','WMM2000','IGRF2000','WMM2005','IGRF2005')
	m <- which(model == models)
	if (length(m) != length(model)) {
		stop('unknown model. Should be one of: ', models) 
	}

	julday <- as.integer(julian(as.Date(date), -2440588))

	p <- .pointsToMatrix(p) 
	d <- cbind(p[,1], p[,2], h/1000, julday, m)

	n <- nrow(d)
	r <- as.double(vector("double", 5*n))

	r <- .C("magfield", n, as.double(d[,1]), as.double(d[,2]), as.double(d[,3]), as.integer(d[,4]), as.integer(d[,5]), r, NAOK=TRUE, PACKAGE='geosphere')[[7]] 

	r <- matrix(r, nrow=n, byrow=TRUE)
	colnames(r) = c('declination', 'dip', 'Bx', 'By', 'Bz')
	r
}

