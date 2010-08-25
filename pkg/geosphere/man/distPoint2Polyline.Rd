\name{distPoint2Polyline}
\Rdversion{1.1}

\alias{distPoint2Polyline}

\title{
Distance between points and polylines or (the border of) polygons.
}

\description{
The shortest distance between points and polylines or polygons. 
}

\usage{distPoint2Polyline(p, pline, distfun=distHaversine, ispoly=FALSE)
}

\arguments{
  \item{p}{longitude/latitude of point(s). Can be a vector of two numbers, a matrix of 2 columns (first one is longitude, second is latitude) or a SpatialPoints* object}
  \item{pline}{as above}
  \item{distfun}{A distance function, \link[geosphere]{distHaversine}}  
  \item{ispoly}{Is \code{pline} a polygon?}  
}


\value{
Distance and lon/lat of nearst point. Distance is in the same unit as \code{r} in the \code{distfun}(default is meters)
}


\author{
George Wang and Robert Hijmans
}


\examples{
poly <- rbind(c(-180,-20), c(-140,55), c(10, 0), c(-140,-60), c(-180,-20))
pnt <- cbind(-150,0)
d = distPoint2Polyline(pnt, poly)
plot( makePoly(poly), type='l')
points(poly)
points(pnt, col='blue', pch=20)
points(d[2], d[3], col='red', pch='x')
}

\keyword{ spatial }


