O2sat <- function(Tc=T-273.15, T=Tc+273.15, NN=0, S=0, method=c("APHA", "QUAL2E", "Weiss", "Paul")) {
   log10 <- function(x) log(x)/log(10)
   method <- match.arg(method)
   switch(method,
     # American Public Health Association
     APHA = exp(-139.34411+(157570.1/T)-(66423080/T^2)+(12438000000/T^3)-(862194900000/T^4)),
     # source: QUAL2E
     # http://www.ecs.umass.edu/cee/reckhow/courses/577/577l12/ppframe.htm
     QUAL2E = exp(-139.34 + (1.5757 * 100000/T) - (6.6423 * 10000000/T^2) +
             (1.2438 * 10000000000/T^3) - (8.6219 * 100000000000/T^4) -
             S * (1.7674 * 0.01 - (10.754/T) + (2.1407 * 1000/T^2))),
     # Weiss, R. (1970). "The solubility of nitrogen, oxygen, and argon 
     # in water and seawater". Deep-Sea Res. 17: 721-35.
     Weiss = 1.426 * exp(-173.4292 + 249.6339 * 100 / T + 143.3483 * log(T / 100) - 21.8492 * T / 100 +
           S * (-0.033096 + 0.014259 * T / 100 + - 0.001700 * (T / 100)^2)),
     # Paul, L. approximation that respects heigth above see level
     Paul = (1012-0.12 * NN)/1013*(14.674-13.644*log10(1+Tc/12.8))
   )
}


   
