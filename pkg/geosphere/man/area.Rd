\name{area}
\Rdversion{1.1}

 
\alias{area}
\alias{area,matrix-method}
\alias{area,data.frame-method}
\alias{area,SpatialPolygons-method}


\title{
Compute the area of a polygon
}

\description{
Compute the area of a polygon on a sphere. Polygons should not self-intersect.
}

\usage{
area(x, ...)
}

\arguments{
 \item{x}{Longitude/latitude of the points forming a polygon; Must be a matrix of 2 columns (first one is longitude, second is latitude) or a SpatialPolygons* object}
 \item{...}{Additional arguemnts. One implemented \code{r=6378137}, the radius of the earth; default = 6378137 m}  
}

\value{
Area. Unit is the square of the unit of r.
}

\author{
Robert J. Hijmans. Based on code by Jason_Steven (\url{http://forum.worldwindcentral.com/showthread.php?p=69704})
}

\references{
Bevis, M. and G. Cambareri, 1987. Computing the Area of a Spherical Polygon of Arbitrary Shape. Mathematical Geology 19: 335-346
}


\examples{
xy <- rbind(c(-180,-20), c(-140,55), c(10, 0), c(-140,-60), c(-180,-20))
area(xy)
}

\keyword{ spatial }

