
distHorizon <- function(h, r=6378137) {
	b = 0.8279
	sqrt( 2 * r * h / b ) 
}

