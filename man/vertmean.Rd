\name{vertmean}
\alias{vertmean}

\title{Vertical Means of Water Bodies (Lakes and Reservoirs)}
\description{
  Calculate vertical mean values which respect different depths of layers and 
  lake morphometry.
}
\usage{
vertmean(depth, vari, level, top, bot, vol)
}

\arguments{
  \item{depth}{sorted vector of sampled depths}
  \item{vari}{measurements corresponding to \code{depth} (concentration, temperature, \dots)}
  \item{level}{Surface water level (above ground or m NN, depending on hypsographic function used}
  \item{top}{top water level of requested layer}
  \item{bot}{bottom water level of requested layer}
  \item{vol}{hypsographic function to be used (e.g. \code{vol.depth})}
}

\value{
  volumetric average value of the requested layer between depths \code{top} and \code{bottom}.
}

\seealso{\code{\link{vol.tsb}}, \code{\link{vol.depth}}}
\examples{

## Plot of hypsographic function
plot(x<-seq(0,155,0.01),y<-vol.tsb(0,x),xlim=c(154,max(x)))

## Test Data
level <- 167.5

depth <- c(0, 1, 6, 7, 8, 9, 10, 11)
vari  <- c(18.8, 17, 12.1, 11.9, 11.8, 11.7, 11.4, 11.4)

vertmean(depth, vari, level, top=0, bot=4, vol.depth)
vertmean(depth, vari, level, top=4, bot=13.5, vol.depth)

vertmean(depth, vari, level, top=0, bot=4, vol.tsb)
vertmean(depth, vari, level, top=4, bot=13.5, vol.tsb)

vertmean(depth, vari, level, top=0, bot=3.7, vol.depth)
vertmean(depth, vari, level, top=3.7, bot=13.5, vol.depth)

vertmean(depth, vari, level, top=0, bot=3.7, vol.tsb)
vertmean(depth, vari, level, top=3.7, bot=13.5, vol.tsb)

}
\keyword{misc}

