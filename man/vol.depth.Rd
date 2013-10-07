\name{vol.depth}
\alias{vol.depth}

\title{Dummy hypsographic curve for depth averaging}
\description{
  This function is used as a simple function for depth-weighted 
  (instead of volumetric) averaging of concentration (or temperature etc.) data in lakes.
}
\usage{
vol.depth(z, level)
}

\arguments{
  \item{z}{vector of water depths}
  \item{level}{water level of the lake surface (m NN or m above ground)}
}

\value{
  the vector \code{z} is returned without change.
}

\seealso{\code{\link{vertmean}}, \code{\link{vol.tsb}}}

\keyword{misc}

