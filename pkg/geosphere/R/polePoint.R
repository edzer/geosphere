# author of original JavaScript code: Chris Vennes
# (c) 2002-2009 Chris Veness
# http://www.movable-type.co.uk/scripts/latlong.html
# Licence: LGPL, without any warranty express or implied

# Much of the above is based on formulae by Ed Williams
# http://williams.best.vwh.net/avform.htm

# Port to R by Robert Hijmans
# October 2009
# version 0.1
# License GPL3




polePoint <- function(lat, brng) {
# ‘Clairaut’s formula’ : the maximum latitude of a great circle path, given a bearing and latitude on the great circle
# source http://www.movable-type.co.uk/scripts/latlong.html
# (c) 2002-2009 Chris Veness
	toRad <- pi / 180 
	
	latMax <- acos(abs(sin(brng * toRad) * cos(lat * toRad)))
	latMax <- latMax / toRad 
	return(latMax)
}


