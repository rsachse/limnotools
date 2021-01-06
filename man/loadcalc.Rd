\name{loadcalc}
\alias{loadcalc}

\title{Load calculation methods for dissolved and suspended matter in rivers}
\description{
  This function provides several load calculation routines known from the literature.
  \bold{Note: the code is experimental, several methods are not yet documented and
  not all methods are thoroughly tested!!!}
}
\usage{
loadcalc(c, Q, Qy, doy=NULL, method = "2", year = 364, ncl = 15, logclass = TRUE)
}

\arguments{
  \item{c}{vector with measured concentrations of dissolved substances}
  \item{Q}{vector of discharge measurements according to the concentration data}
  \item{Qy}{discharge measurements for the whole year}
  \item{doy}{day of year (or index of measurements), needed for method "4"}
  \item{method}{selects the load calculation algorithm}
  \item{year}{length of the year, maybe 364, 365 or 360}
  \item{ncl}{number of classes (for method "harned"}
  \item{logclass}{specifies if discharge classes for method "harned" should be based
    on dividing logarithmic (TRUE) or linear (FALSE) scale.}
}
\details{
  We and others made extensive tests and suggest methods \code{"2"} 
  (discharge weighted mean) or \code{"harned"} (method with discharge classes).
}
\value{
  The total load for the whole year (or period if specified ).
}
\references{
Coats R, Lju FJ, Goldman CR (2002). A Monte Carlo test of load calculation methods, Lake
tahoe basin, California-Nevada. Journal of American Water Resources Association, 38(3),
719-730.

Dann MS, Lynch JA, Corbett ES (1986). Comparison of Methods for Estimating Sulfate
Export from a Forested Watershed. J. Environ. Qual., 15(2), 140-145.

Harned DA, Daniell CCI, Crawford J (1981). Methods of discharge compensation as an aid
to the evaluation of water quality trends. Water Resources Research, 17(5), 1389-1400.

Hilden M (2003). Ermittlung von Stoff-Frachten in Fließgewässern. Probenahmestrategien
und Berechnungsverfahren. Konzepte und Strategien, Länderarbeitsgemeinschaft Wasser
(LAWA).

Keller M, Hilden M, Joost M (1997). Vergleich von Schätzmethoden für jährliche Stofffrachten
am Beispiel des IKSR-Meßprogrammes 1995. Bundesministerium für Umwelt, Naturschutz
und Reaktorsicherheit, Maßnahme Nr. 4, Weitergehende Bewertung von Gewässergütedaten,
JAP-BfG, Nr 413 (BfG-1078), Bundesanstalt für Gewässerkunde.


Schleppi P, Waldner PA, Fritschi B (2006). Accuracy and precision of different sampling
strategies and flux integration methods for runoff water: comparisons based on measurements
of the electrical conductivity. Hydrological Processes, 20, 395-410.

Swistock BR, Edwards PJ,Wood F, Dewalle DR (1997). Comparison of methods for calculating
annual solute exports from six forested Appalachian watersheds. Hydrological Processes, 11,
655-669.

Zweynert U, Behrendt H, Zweynert M (2004). Berechnung jährlicher Stoffrachten in 
Fließgewässern - Ein neuer Ansatz im Vergleich mit etablierten Methoden. 
In: Deutsche Gesellschaft für Limnologie (DGL) - Tagungsbericht 2003 (Köln), 
Bd. II, S. 643-648.

}

\examples{
## daily discharge meaures for one year
Qy <- rlnorm(365, meanlog=3, sdlog=1)
## sampling dates
t  <- floor(seq(1, 365, length=20))
## discharges and concentrations
Q  <- Qy[t]
c  <- runif(1:20, min=0, max=10)

loadcalc(c, Q, Qy, method="2")

## Method with discharge classes (e.g. 15 (default) or 10)
loadcalc(c, Q, Qy, method="harned")
loadcalc(c, Q, Qy, ncl=10, method="harned")
loadcalc(c, Q, Qy, method="harned", logclass=FALSE)

## average weighted concentrations
loadcalc(c, Q, Qy, method="2")/sum(Qy)
loadcalc(c, Q, Qy, method="harned")/sum(Qy)

## average non-weighted concentration
mean(c)
}

\keyword{ misc }

