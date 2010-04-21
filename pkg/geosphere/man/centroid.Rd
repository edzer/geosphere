\name{span}

\alias{centroid}
\alias{centroid,matrix-method}
  
\title{centroid of polygons}

\description{
Compute the centroid of longitude/latitude polygons. The centroid is computed by first projecting the polygon to the (conformal) Mercator coordinate reference system, computing the centroid, and then inversely projecting to geographic coordinates.
}

\usage{
centroid(x, ...)
}

\arguments{
\item{x}{a 2-column matrix (longitude/latitude)}
\item{...}{Additional arguments. None implemented}
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
