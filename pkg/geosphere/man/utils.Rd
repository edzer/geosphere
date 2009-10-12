\name{utilities}
\Rdversion{1.1}

\alias{pointsToMatrix}
\alias{compareDim}

\title{
Internal utility functions
}

\description{
pointsToMatrix is a simple helper function. Point input to all other functions is processed by this function to coerce points into a two column matrix of longitude / latitude. 

compareDim compares two or three points objects to check if they have the same length or have lenght 1 (point).
}

\usage{
pointsToMatrix(p)
compareDim(p1,p2,p3)
}

\arguments{
  \item{p}{a vector of two numbers, a matrix of 2 columns (first one is longitude, second is latitude) or a spatialPoints* object}
  \item{p1}{a matrix of 2 columns}
  \item{p2}{a matrix of 2 columns}
  \item{p3}{a matrix of 2 columns}
}


\value{
pointsToMatrix: a matrix with two columns representing longitude and latitude.

compareDim: \code{TRUE} (invisible) or an error 
}


\author{
Robert Hijmans and Jacob van Etten
}


\keyword{ spatial  }

