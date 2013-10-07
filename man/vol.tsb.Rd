\name{vol.tsb}
\alias{vol.tsb}

\title{Hypsographic volume curve of Bautzen Reservoir}
\description{
  The hypsographic curve returns the water volume as a function of the water level.
}
\usage{
vol.tsb(z, level)
}

\arguments{
  \item{z}{vector of water depths}
  \item{level}{water level of the lake surface (m NN, base level is 154m NN)}

}

\value{
  A vector of volumes corresponding to z.
}

\seealso{\code{\link{vertmean}}}

\keyword{misc}

