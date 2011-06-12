\name{magneticDeclination}

\Rdversion{1.1}

\alias{magDec}

\title{
Magnetic declination
}

\description{
Magnetic declination (or variation) is the angle between magnetic north (the direction the north end of a compass needle points) and true north. The declination is positive when the magnetic north is east of true north (more on \href{http://en.wikipedia.org/wiki/Magnetic_declination}{wikipedia}). }

\usage{
magDec(p, date='2005-06-30', model='IGRF2005', h=0)
}

\arguments{
  \item{p}{longitude/latitude of point(s). Can be a vector of two numbers, a matrix of 2 columns (first one is longitude, second is latitude) or a SpatialPoints* object}
  \item{date}{A single date between '1950-01-01' and '2049-12-31' (character, Date, or anything that works for \link{as.Date}) }
  \item{model}{Character. One of 'IGRF90', 'WMM85', 'WMM90', 'WMM95', 'IGRF95', 'WMM2000', 'IGRF2000', 'WMM2005', 'IGRF2005'}
  \item{h}{The height above the spheroid in meters }
  
}

\value{
A matrix with for each point the magnetic declination, dip angle (the angle the field lines make with the horizontal. As this approaches 90 degrees the magnetic compass becomes unuseable), and three components of the earth's magenetic field in nanotesla (nT): N and E components (Bx and By) in the horizon plane of the WGS84 ellipsoid, and the downward component(Bz).
}


\references{
Ed Williams' \href{http://williams.best.vwh.net/index.html}{website}
}

\author{
C code by Ed Williams. Bridge to R by Robert Hijmans
}

\examples{
magDec(rbind(c(-12,-5),c(50,5)), date='2001-05-04')
}

\keyword{ spatial }
