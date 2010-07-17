\name{randomCoordinates}
\Rdversion{1.1}

\alias{randomCoordinates}

\title{
Random coordinates
}

\description{
Random coordinates on a sphere. This sampling is 'uniform' in the sense that the probability that a point is drawn from any region is equal to the area of that region divided by the area of the entire sphere. This would not happen if you took a random uniform sample of longitude and latitude, as the sample would be biased towards the poles.
}

\usage{
randomCoordinates(n)
}

\arguments{
  \item{n}{Sample size (number of points (coordinate pairs)) }
}

\value{
Matrix of lon/lat coordiantes
}

\references{
based on suggstions by Michael Orion, \url{http://sci.tech-archive.net/Archive/sci.math/2005-09/msg04691.html}
}

\author{
Robert Hijmans
}

\examples{
randomCoordinates(3)
}

\keyword{ spatial }
