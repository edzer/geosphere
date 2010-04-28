# Author: Robert Hijmans
# based on example in Software for Data Analysis by John Chambers (pp 250-1), but adjusted to follow great circles, rather than straight (2D) lines.



.plotArrows <- function(p, line, fraction, length, ...) {

	if (fraction >= 1) {
		lines(line, ...)
	} else {
		dist <- distHaversine(p[-nrow(p),], p[-1,]) * (1 - fraction)
		bearing <- bearing(p[-nrow(p),], p[-1,])
		p0 <- destPoint(p[-nrow(p),], bearing, dist)
		for (i in 1:nrow(p0)) {
			line = .makeSinglePoly(rbind(p0[i,], p[i+1,]))
			lines(line)
		}
	}
		
	bearing = finalBearing(p[-nrow(p),], p[-1,])
	bearing = (bearing + 180) %% 360
	pp = destPoint(p[-1,], bearing, 10000)
	x0 <- pp[,1]
	y0 <- pp[,2]
	x1 <- p[,1][-1]
	y1 <- p[,2][-1]
#	delta = sqrt(mean((x1-x0)^2 + (y1-y0)^2, na.rm=TRUE))
#	delta = delta * (par("pin")[1] / diff(range(x, na.rm=TRUE)))
	arrows(x0, y0, x1, y1, code=2, length=length, ...)
}


plotPoly <- function(p, arrows=FALSE, fraction=0.9, length=0.15, ...) {
	if (! arrows ) {
		if (inherits(p, 'Spatial')) {
			plot(p, ...)
		} else {
			plot(p, type='l', ...)
		}
	} else {
		if (inherits(p, 'Spatial')) {
			plot(bbox(p), asp=1, type='n')
			p = p@polygons
			n = length(p)
			for (i in 1:n) {
				parts = length(p[[i]]@Polygons )
				sumarea = 0
				for (j in 1:parts) {
					pp =  p[[i]]@Polygons[[j]]@coords 
					line = .makeSinglePoly(pp)
					.plotArrows(pp, line, fraction, length, ...)
				}
			}
		} else {
			line = .makeSinglePoly(p)
			plot(line, asp=1, type='n')
			.plotArrows(p, line=line, fraction, length, ...)
		}
	}
}

