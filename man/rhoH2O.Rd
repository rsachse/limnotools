\name{rhoH2O}
\alias{rhoH2O}
\title{Density of water}
\description{
  Density of water in \eqn{g/cm^3}{g/cm^3}
}
\usage{
rhoH2O(T, p = 0, S = 0, method = c("Chen", "Fofonoff"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{T}{temperature in degree centigrade}
  \item{p}{pressure in bar}
  \item{S}{salinity in PSU}
  \item{method}{Chen for fresh water systems, Fofonoff for marine systems}
}
\details{
  Bitte noch Definitionsbereiche hinzufügen!!
}
\value{
  Vector with density values.
}
\references{ 
Chen, Ch.-T. and F.J. Millero (1986). Precise thermodynamic properties of natural 
waters covering only the limnological range. Limnol. Oceanogr. 31 No. 3, 657 - 662.

Fofonoff N.P. and R.C. Millard (1983). Algorithms for Computation of Fundamental 
Properties of Seawater. UNESCO technical papers in marine science, 44, 53 pp.

%Paul, L. (1985). Das thermische Regime der Talsperre Saidenbach und einige 
%Beziehungen zwischen abiotischen und biotischen Komponenten. Dissertation, 
%TU Dresden, Fakultät Bau-, Wasser- und Forstwesen. 84 pp.
}

\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{
rhoH2O(4)
T <- 0:30
plot(T, rhoH2O(T))
S <- 0:40
plot(S, rhoH2O(T=4, S=S, method="Fofonoff"))
lines(S, rhoH2O(T=4, S=S, method="Chen"))

}
\keyword{ misc }
