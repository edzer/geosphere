\name{latOnGreatCircle}
\Rdversion{1.1}

\alias{latOnGreatCircle}

\title{
Latitude on a Great Circle
}

\description{
Get a latiude (given a longitude) on a great circle
}

\usage{
latOnGreatCircle(p1, p2, lon) 
}

\arguments{
  \item{p1}{Longitude/latitude of a single point; can be a vector of two numbers, a matrix of 2 columns (first one is longitude, second is latitude) or a spatialPoints* object}
  \item{p2}{As above}
  \item{lon}{Longitude}
}


\value{
a matrix of points
}

\references{
\url{http://williams.best.vwh.net/avform.htm#Int}
}

\author{
Robert Hijmans based on a formula by Ed Williams
}


\examples{
latOnGreatCircle(c(5,52), c(-120,37), lon=-120)
}

\keyword{ spatial }
