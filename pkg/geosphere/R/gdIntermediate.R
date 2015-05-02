# author Robert Hijmans
# May 2015
# version 0.1
# license GPL


.gdinterm <- function(p1, p2, n, a, f) {

		if (antipodal(p1, p2)) {
			return(matrix(Inf, ncol=n, nrow=n))
		}
		if (isTRUE(all.equal(p1, p2))) {
			return(cbind(rep(p1[,1], n), rep(p1[,2], n) ))
		}
	
		db <- .Call("inversegeodesic", as.double(p1[,1]), as.double(p1[,2]), as.double(p2[,1]), as.double(p2[,2]), as.double(a), as.double(f), PACKAGE='geosphere')

		steps <- (db[1]/(n+1)) * 1:n
		p <- cbind(p1[,1], p1[,2], db[2], steps)	
		destPoint(p[,1:2], p[,3], steps)
}



gdIntermediate <- function( p1, p2, n=50, breakAtDateLine=FALSE, addStartEnd=FALSE, sp=FALSE, sepNA=FALSE, a=6378137, f=1/298.257223563) {
	p1 <- .pointsToMatrix(p1)
	p2 <- .pointsToMatrix(p2)
	p <- cbind(p1[,1], p1[,2], p2[,1], p2[,2], as.vector(n), a, f)
	res <- list()

	n <- round(n)
	stopifnot(n > 0)
	
	for (i in 1:nrow(p)) {
		x <- .gdinterm(p[i,1:2,drop=FALSE], p[i,3:4,drop=FALSE], p[i,5], p[i,6], p[i,7])
		if (addStartEnd) {
			x <- rbind(p[i,1:2,drop=FALSE], x, p[i,3:4,drop=FALSE])
		}			

		if (breakAtDateLine) {
			r <- range(x[,1]) 
			r <- r[2] - r[1]
			if (r > 200) {
				dif <- abs(x[-nrow(x),1] - x[-1,1])
				tr <- which(dif==max(dif))
				x1 <- x[1:tr, ,drop=FALSE]
				x2 <- x[(tr+1):nrow(x), ,drop=FALSE]
				if (x1[tr,1] < 0) { 
					x1[tr,1] <- -180 
					x2[1,1] <- 180
				} else { 
					x1[tr,1] <- 180 
					x2[1,1] <- -180 
				}
				if (nrow(x1) <= 1) {
					res[[i]] <- x2
				} else if (nrow(x2) <= 1) {
					res[[i]] <- x1
				} else {
					res[[i]] <- list(x1, x2)
				}
			} else {
				res[[i]] <- x
			}
		} else {
			res[[i]] <- x
		}
	}
	
	if (sp) {
		for (i in 1:length(res)) {
			if (! is.list(res[[i]])) {
				res[[i]] <- Lines( list( Line (res[[i]])), ID=as.character(i)) 	
			} else {
				res[[i]] <- Lines( list( Line (res[[i]][[1]]), Line(res[[i]][[2]])), ID=as.character(i))
			}
		}
		
		res <- SpatialLines(res, CRS("+proj=longlat +ellps=WGS84"))
		
	} else if (nrow(p) == 1 ) {
	
		res <- res[[1]]
		
	} else if (sepNA) {
		r <- res[[1]]
		for (i in 2:length(res)) { 
			r <- rbind(r, c(NA,NA), res[[i]]) 
		}
		return(r)
	}
	
	return(res)
}


