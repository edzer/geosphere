\name{intermediate}
\Rdversion{1.1}

\alias{gcIntermediate}
\alias{gdIntermediate}

\title{
Intermediate points on a great circle (sphere) or on a geodesic (ellipsoid)
}

\description{
Get intermediate points (way points) between the two locations with longitude/latitude coordinates. gdIntermediate uses an ellipsoid (default is WGS84) and is fast and accurate. gcIntermediate is based on a spherical model of the earth and internally uses \code{\link{distCosine}}. 
}

\usage{
gdIntermediate(p1, p2, n=50, breakAtDateLine=FALSE, addStartEnd=FALSE, sp=FALSE, sepNA, a=6378137, f=1/298.257223563) 

gcIntermediate(p1, p2, n=50, breakAtDateLine=FALSE, addStartEnd=FALSE, sp=FALSE, sepNA) 

}

\arguments{
  \item{p1}{longitude/latitude of a single point, in degrees. This can be a vector of two numbers, a matrix of 2 columns (first one is longitude, second is latitude) or a SpatialPoints* object}
  \item{p2}{as for \code{p1}}
  \item{n}{integer. The desired number of intermediate points}
  \item{breakAtDateLine}{logical. Return two matrices if the dateline is crossed?}
  \item{addStartEnd}{logical. Add p1 and p2 to the result?}
  \item{sp}{logical. Return a SpatialLines object?}
  \item{sepNA}{logical. Rather than as a list, return the values as a two column matrix with lines seperated by a row of NA values? (for use in 'plot')}
  \item{a}{major (equatorial) radius of the ellipsoid. The default value is for WGS84 }
  \item{f}{ellipsoid flattening. The default value is for WGS84 } 
}  


\value{
matrix or list with intermediate points
}

\references{
Geodesic: \url{http://geographiclib.sourceforge.net/}

Great circle: \url{http://williams.best.vwh.net/avform.htm#Intermediate}
}

\author{
Robert Hijmans based on code by Ed Williams (great circle) and using GeographicLib code by C.F.F. Karney (geodesic)
}


\examples{
gdIntermediate(c(5,52), c(-120,37), n=6, addStartEnd=TRUE)
gcIntermediate(c(5,52), c(-120,37), n=6, addStartEnd=TRUE)
}

\keyword{ spatial }
