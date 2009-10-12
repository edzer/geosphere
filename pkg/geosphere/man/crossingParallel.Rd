\name{crossingParallels}
\Rdversion{1.1}

\alias{crossingParallels}

\title{
Crossing parellels
}

\description{
Longitudes at which a given great circle crosses a given parallel (latitude)
}

\usage{
crossingParallels(p1, p2, lat) 
}

\arguments{
  \item{p1}{longitude/latitude of point(s); can be a vector of two numbers, a matrix of 2 columns (first one is longitude, second is latitude) or a spatialPoints* object}
  \item{p2}{as above. Should have same length as p1, or a single point (or vice versa when p1 is a single point}
  \item{lat}{a latitude}
}


\value{
two points (longitudes) 
}

\references{
\url{http://williams.best.vwh.net/avform.htm#Intersection}
}

\author{
Robert Hijmans based on code by Ed Williams
}


\examples{
crossingParallels(c(5,52), c(-120,37), 40)
}

\keyword{ spatial }
