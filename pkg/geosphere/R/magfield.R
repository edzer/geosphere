# Author: Robert J. Hijmans
# License GPL3
# Version 1 June 2011


magDec <- function(p, date='2005-06-30', model='IGRF2005', h=0) { 
	models <- c('IGRF90','WMM85','WMM90','WMM95','IGRF95','WMM2000','IGRF2000','WMM2005','IGRF2005')
	m <- which(model[1] == models)
	if (length(m) == 0) { stop('unknown model. Should be one of: ', models) }
	h=h[1]
	date <- as.Date(date[1])
	year <- as.integer(format(date, "%Y"))
	stopifnot(year > 1949 | year < 2050) 
	yy <- as.integer(format(date, "%y"))
	mm <- as.integer(format(date, "%m"))
	dd <- as.integer(format(date, "%d"))

	p <- .pointsToMatrix(p) 
	n <- nrow(p)
	
	r <- as.double(vector("double",5*n))
	r <- .C("magfield", n, as.double(p[,1]), as.double(p[,2]), as.double(h/1000), dd, mm, yy, m, r, NAOK=TRUE, PACKAGE='geosphere')[[9]] 
	r <- matrix(r, nrow=n, byrow=TRUE)
	colnames(r) = c('declination', 'dip', 'Bx', 'By', 'Bz')
	r
}

