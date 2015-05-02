\name{distGeo}
\Rdversion{1.1}

\alias{distGeo}

\title{
Distance on an ellipsoid
}

\description{
High precision estimate of the shortest distance between two points on an ellipsoid (default is WGS84 ellipsoid).
}

\usage{
distGeo(p1, p2, a=6378137, f=1/298.257223563)
}

\arguments{
  \item{p1}{longitude/latitude of point(s). Can be a vector of two numbers, a matrix of 2 columns (first one is longitude, second is latitude) or a SpatialPoints* object}
  \item{p2}{as above}
 \item{a}{major (equatorial) radius of the ellipsoid. The default value is for WGS84 }
 \item{f}{ellipsoid flattening. The default value is for WGS84 } 
}


\value{
Vector of distances in the same unit as \code{r} (default is meters)
}

\references{
\url{http://geographiclib.sourceforge.net/}
}

\author{
Robert Hijmans
}

\seealso{
\code{\link[geosphere]{distGeo}, \link[geosphere]{distHaversine}, \link[geosphere]{distVincentySphere}, \link[geosphere]{distVincentyEllipsoid}, \link{distMeeus}}
}

\examples{
distGeo(c(0,0),c(90,90))
}

\keyword{ spatial }
