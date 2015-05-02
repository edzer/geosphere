\name{distGeo}
\Rdversion{1.1}

\alias{distGeo}

\title{
Distance on an ellipsoid (the geodesic)
}

\description{
Highly accurate estimate of the shortest distance between two points on an ellipsoid (default is WGS84 ellipsoid).
The shortest path between two points on an ellipsoid is called the geodesic.
}

\usage{
distGeo(p1, p2, a=6378137, f=1/298.257223563)
}

\arguments{
 \item{p1}{longitude/latitude of point(s). Can be a vector of two numbers, a matrix of 2 columns (first column is longitude, second column is latitude) or a SpatialPoints* object}
 \item{p2}{as above}
 \item{a}{major (equatorial) radius of the ellipsoid. The default value is for WGS84 }
 \item{f}{ellipsoid flattening. The default value is for WGS84 } 
}


\value{
Vector of distances in meters
}

\author{
This function calls GeographicLib code by C.F.F. Karney
}

\references{
C.F.F. Karney, 2013. Algorithms for geodesics, J. Geodesy 87: 43-55. \url{https://dx.doi.org/10.1007/s00190-012-0578-z}. Addenda: \url{http://geographiclib.sf.net/geod-addenda.html}. Also see \url{http://geographiclib.sourceforge.net/}
}


\seealso{
\code{\link[geosphere]{distCosine}, \link[geosphere]{distHaversine}, \link[geosphere]{distVincentySphere}, \link[geosphere]{distVincentyEllipsoid}, \link{distMeeus}}
}

\examples{
distGeo(c(0,0),c(90,90))
}

\keyword{ spatial }
