\name{polePoint}
\Rdversion{1.1}

\alias{polePoint}


\title{
Highest latitude on a great circle
}

\description{
Given a latitude and an initial bearing, what is the polar-most point that will be reached when following a great circle? Computed with Clairaut's formula. 
}

\usage{
polePoint(lat, brng)
}

\arguments{
  \item{lat}{latitude of point(s)}
  \item{brng}{bearing}
}


\value{
A pair of coordinates (longitude/latitude)
}

\references{
\url{http://williams.best.vwh.net/ftp/avsig/avform.txt}

\url{http://www.movable-type.co.uk/scripts/latlong.html}
}

\author{
Chris Veness; ported to R by Robert Hijmans
}

\seealso{
\code{\link[geosphere]{gcLat}, \link[geosphere]{gcLon}}
}


\examples{
polePoint(c(5,52),30)
}

\keyword{ spatial  }

