# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date : April 2010
# Version 1
# Licence GPL v3



if (!isGeneric("span")) {
	setGeneric("span", function(x, ...)
		standardGeneric("span"))
}	


setMethod("span", signature(x='matrix'), 
function(x, nbands='fixed', n=100, res=0.1, fun, r=6378137, ...) {
	if (!require(sp)) {stop('you need to install the "sp" package to use this function')}

	x <- SpatialPolygons(list(Polygons(list(Polygon(x)), 1)))
	if (missing(fun)) {
		p <- span(x, nbands=nbands, n=n, res=res, r=r, ...) 
	} else {	
		p <- span(x, nbands=nbands, n=n, res=res, fun=fun, r=r, ...) 
	}
	return(p)
} )



setMethod("span", signature(x='SpatialPolygons'), 
function(x, nbands='fixed', n=100, res=0.1, fun, r=6378137, ...) {

	if (!require(sp)) {stop('you need to install the "sp" package to use this function')}
	if (!require(raster)) {stop('you need to install the "raster" package to use this function')}
	
	if (! nbands %in% c('fixed', 'variable')) {
		stop('bandwidth should be "fixed" or "variable"')
	}
	
	if (nbands == 'fixed') {
		n = max(n, 1)
	} else {
		if (res <= 0) {
			stop('res should be larger than zero')
		}
	}

	npol <- length(x@polygons)
	lon <- list()
	lat <- list()
	
	for (i in 1:npol) {
		pp <- x[i,]
		rs <- raster(pp)
		if (nbands == 'fixed') {
			rowcol(rs) <- c(n, n)
		} else {
			res(rs) <- res
		}
				
		yfr <- yFromRow(rs, 1:nrow(rs))
		xd <- distHaversine(cbind(0,yfr), cbind(xres(rs),yfr), r=r)
		yd <- distHaversine(cbind(0,0), cbind(0,yres(rs)), r=r)
		
		rs <- polygonsToRaster(pp, rs, silent=TRUE)
		rs <- getValues(rs, format='matrix')
		lat[[i]] <- as.vector(apply(rs, 1, sum, na.rm=TRUE) * yd)
		lon[[i]] <- as.vector(apply(rs, 2, sum, na.rm=TRUE) * xd)
	}

	if (! missing(fun)) {
		lon = sapply(lon, fun)
		lat = sapply(lat, fun)
		return(cbind(lon, lat))
	} else {
		return(c(lon=lon, lat=lat))
	}
}
)

