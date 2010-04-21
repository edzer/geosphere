\name{span}

\alias{centroid}
\alias{centroid,matrix-method}
\alias{centroid,SpatialPolygons-method}
  
\title{centroid of polygons}

\description{
Compute the centroid of longitude/latitude polygons. Unlike other functions in this package, there is no spherical trigonomery involved in the implementation of this function. Instead, yhe function projects the polygon to the (conformal) Mercator coordinate reference system, computes the centroid, and then inversely projects it to longitude and latitude. This approach fails for polygons that include one of the poles. The function should work for polygons that cross the -180/180 meridian (date line), as long as they do not also cross the zero (Greenwich) meridian.
}

\usage{
centroid(x, ...)
}

\arguments{
\item{x}{a 2-column matrix (longitude/latitude)}
\item{...}{Additional arguments. None implemented}
}

\note{
For multi-part polygons, the centroid of the largest part is returned.
}	

\value{
A matrix (longitude/latitude)
}

\author{Robert J. Hijmans }

\examples{
pol <- rbind(c(-180,-20), c(-160,5), c(-60, 0), c(-160,-60), c(-180,-20))
centroid(pol) 
}

\keyword{methods}
\keyword{spatial}
