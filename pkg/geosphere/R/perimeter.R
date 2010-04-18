# Based on code by Jason_Steven
# http://forum.worldwindcentral.com/showthread.php?p=69704

# R implementation by Robert Hijmans

perimeter <- function(xy, r=6378137) {
	xy2 = rbind(xy[-1,], xy[1,])
	d = distCosine(xy, xy2, r=r)
	return(sum(d))
}

