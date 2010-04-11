\name{azimuth}

\Rdversion{1.1}

\alias{azimuth}

\title{
Azimuth
}

\description{
Azimuth (direction) between two points.
}

\usage{
azimuth(p1, p2)
}

\arguments{
  \item{p1}{longitude/latitude of point(s). Can be a vector of two numbers, a matrix of 2 columns (first one is longitude, second is latitude) or a SpatialPoints* object}
  \item{p2}{as above}
}


\value{
Azimuth in degrees
}


\author{
Robert J. Hijmans
}

\examples{
azimuth(c(0,0),c(90,90))
}

\keyword{ spatial }

