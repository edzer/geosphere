# Robert Hijmans
# April 2010
# version 1
# License GPL3

distm <- function(x, fun=distCosine) {
	n = nrow(x)
	dm = matrix(ncol=n, nrow=n)
	dm[cbind(1:n, 1:n)] = 0
	for (i in 1:(n-1)) {
		j = (i+1):n
		dm[i,j] = fun(x[i,], x[j,])
	}
	return(dm)
}

