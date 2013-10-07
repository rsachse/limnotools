############################################################
## Frachtberechnungsverfahren, implementiert von S. Jachner
## ergaenzt und weiterentwickelt von ThPe
############################################################

loadcalc <- function(c, Q, Qy, doy=NULL, method="2", year=364, ncl=15, logclass=TRUE) {

  ## Angenommen, die gemessene Konzentration ist für den gesamten
  ## Fließquerschnitt repräsentativ, so ergibt sich der Transport T
  ## als Produkt von Abfluss und Konzentration.
  ## Aufgrund der Probenahmesequenzen ist es üblich ein Messjahr von
  ## 52 Wochen, also von 364 Tagen zu verwenden.
  loadreference <- function(c, Q) {
    Transport <- c * Q
    Transport <- Transport[1:364]
    sum(as.numeric(Transport))
  }
  
  ## Methode 1: Standardverfahren: Schätzung der Jahresfracht Bestimmung
  ## der Transporte durch Multiplikation der gemessenen Konzentrationen
  ## mit den entsprechenden Tagesabflüssen. Angenommen diese Transporte
  ## bilden eine repräsentative Stichprobe des Transportes, so kann man
  ## die Jahresfracht Fquer schätzen.
  load1 <- function(c, Q, year=364) {
    N <- length(c)
    year / N * sum(c * Q)
  }
  
  ## Methode 2 FvonQ, abflusskorrigierte Standardmethode Angleichung des
  ## mittleren Transportes mit einem Korrekturfaktor an das
  ## Abflussgeschehen des Messjahres FvonQ ergibt sich als Fquer von
  ## Methode 1, welches mit dem Verhältnis von mittlerem Jahresabfluss
  ## Qquer zu mittlerem beprobten Abfluss QM multipliziert wird.
  load2 <- function(c, Q, Qy, year=364) {
    N <- length(c)
    Fquer <- year / N * sum(c * Q)
    Qquer <- mean(Qy)
    QM <- mean(Q)
    Fquer * Qquer / QM
  }
  
  ## neuer abflussreduzierter Ansatz (ABR-Methode)
  ## nach Zweynert, U. and Behrendt, H. and Zweynert, M., 2002
  load2new <- function(c, Q, Qy, year=364) {
    Qy <- Qy[1:year]
    Qschranke <- mean(Qy) + sqrt(var(Qy))
    Qred <- ifelse(Q >= Qschranke, Qschranke, Q)  
    N <- length(c)
    Fquer <- year / N * sum(c * Qred)
    Qquer <- mean(Qy)
    QredM <- mean(Qred)
    Fquer * Qquer / QredM
  }
  
  ## Methode 3: CQquer, Methode der Jahresmittelwerte Annahme: c(t) und
  ## Q(t) schwanken nur unwesentlich um ihre Jahresmittelwerte cquer und
  ## Qquer; CQquer = 364 * cquer * Qquer
  load3 <- function(c, Q, year=364) {
    l <- length(c)
    cquer <- (1/l) * sum(as.numeric(c))
    Q <- Q[1:364]
    Qquer <- (1/year) * sum(as.numeric(Q))
    year * cquer * Qquer
  }
  
  ## Methode 4: Flin, Interpolationsmethode Man nimmt an, dass die
  ## Konzentrationswerte zwischen zwei Messungen nur wenig schwanken. So
  ## kann durch eine Interpolation zwischen den Messwerten auf die
  ## Konzentration an allen 364 Tagen geschlossen werden.
  load4 <- function(c, Q, doy, year=364) {
    y <- approx(doy, c, n=1:year)$y
    Q <- Q[1:year]
    sum(y * Q)
  }
  
  ## Methode 5: F_QC, Q-C-Regressions-Methode Man geht davon aus, dass
  ## die Konzentrationen c mit dem Abfluss Q korrelieren.  Die
  ## Identifikation der Konzentrations-Abflussbeziehung erfolgt durch
  ## eine Regression der gemessenen N Wertepaare (c(ti), (Q(ti)). Dabei
  ## können unterschiedliche Regressionstypen verwendet werden. Dabei
  ## können unterschiedliche Regressionstypen verwendet werden.
  
  ## Methode 5a: c(Q) = a0 + a1 * exp(-a2 * Q)
  load5a <- function(c, Q, Qy, year=364) {
    f5a <- function(x, a0, a1, a2) {a0 + a1 * exp(-a2 * x)}
    
    mydata <- list(x=Q, y=c)
    reg <- lm(log(c)~Q)
    a1 <- coef(reg)[1]
    a2 <- coef(reg)[2] 
    pstart <- list(a0=1, a1=exp(a1), a2=-a2)
    
    aFit <- nls(y ~ f5a(x, a0, a1, a2), data=mydata, start=pstart)
    Qy <- Qy[1:year]
    a0 <- coef(aFit)[1]
    a1 <- coef(aFit)[2]
    a2 <- coef(aFit)[3]
    y <- a0 + a1 * exp(-a2 * Qy)
    sum(y * Qy)
  }
  
  ## Methode 5b: c(Q) = a0 + a1 * Q
  ## f5b <- function(x, a0, a1) {a0 + a1 * x}
  load5b <- function(c, Q, Qy, year=364) {
    reg <- lm(c~Q)
    Qy <- Qy[1:year]
    y1 <- predict(reg, list(Q=Qy))
    sum(y1 * Qy)
  }
  
  ## Methode 5c: c(Q) = a0 + a1 * Q + a2 * Q^2
  ## f5c <- function(x, a0, a1, a2) {a0 + a1 * x + a2 * x^2}
  load5c <- function(c, Q, Qy, year=364) {
    reg <- lm(c~1+Q+I(Q^2)) 
    Qy <- Qy[1:year]
    y1 <- predict(reg, list(Q=Qy))
    sum(y1 * Qy)
  }
  
  ## weitere Methode 5 = Methode 6a
  ## Transformation: log(c(Q)) = a0 + a1 * log(Q)
  ## f5 <- function(x, a0, a1) {exp(a0 + a1 * log(Q))}
  ## das entspricht auch der Gleichung c(Q)=exp(a0) * Q^(a1)
  load5 <- function(c, Q, Qy, year=364) {
    reg <- lm(log(c)~log(Q))
    Qy <- Qy[1:year]
    a0 <- coef(reg)[1]
    a1 <- coef(reg)[2]
    y1 <- exp(a0) * Qy^(a1)
    sum(y1 * Qy)
  }
  
  ## Methode 6: F_QT, Q-T-Regressions-Methode Es wird eine Korrelation
  ## von Transport und Abfluss angenommen. Da sich der Transport als
  ## Produkt aus Konzentration und Abfluss ergibt, ist eine Abhängigkeit
  ## schon implizit vorhanden. Insbesondere wenn die zeitliche Variation
  ## der Konzentrationswerte c(ti) nicht sehr groß ist, lässt sich eine
  ## Transport-Abflussbeziehung der Form T(Q(t))=c(t)*Q(t)
  ## identifizieren. Für die Regression können unterschiedliche
  ## Kurventypen genutzt werden.
  
  ## Methode 6a
  ## f <- function(x, a0, a1) {a0*x^a1}
  load6a <- function(c, Q, Qy, year=364) {
    Transport <- Q * c
    reg <- lm(log(Transport)~log(Q))
    Qy <- Qy[1:364]
    a0 <- exp(coef(reg)[1])
    a1 <- coef(reg)[2]
    y1 <- a0 * Qy^a1
    sum(y1)
  }
  
  ## Methode 6c
  ## f <- function(x, a0, a1) {a0 + a1 * x}
  load6c <- function(c, Q, Qy, year=364) {
    Transport <- Q * c
    reg <- lm(Transport~Q)
    Qy <- Qy[1:year]
    y1 <- predict(reg, list(Q=Qy))
    sum(y1)
  }
  
  ## Klasseneinteilungsmethode nach Harned et al., 1981
  loadharned <- function(c, Q, Qy, ncl=15, logclass=TRUE) {
    ## log oder kein log
    trans <- function(Q, logclass) {   
      if (logclass) Q <-log(Q)
      Q
    }
    ## Teil 2: Klasseneinteilung und zusammenfassen der Klassen
    ## 2.1: provisorische Klassengrenzen
    minQ <- min(trans(Qy, logclass))
    maxQ <- max(trans(Qy, logclass))
  
    cl   <- seq(minQ, maxQ, length=ncl+1)
    qcl  <- cut(trans(Qy, logclass), breaks=cl, labels=FALSE, include.lowest=TRUE)
    tqcl <- as.data.frame(table(qcl))
    xxx  <- data.frame(no = 1:ncl,
                       ll = cl[-(ncl+1)],
                       ul = cl[-1]
                      )
                 
    sample.qcl  <- cut(trans(Q, logclass), breaks=cl, labels=FALSE, include.lowest=TRUE)
    sample.tqcl <- as.data.frame(table(sample.qcl))
    xxx$f <- tqcl$Freq[match(1:ncl, sample.tqcl$sample.qcl)]
    xxx$f[is.na(xxx$f)] <- 0    
    unwichtig <- unique(c(xxx$ll[xxx$f==0], xxx$ul[xxx$f==0]))
  
    ## 2.2: neue Klassengrenzen
    cl <- cl[!(1:(ncl+1) %in% match(unwichtig,cl))]
  
    ## A1: erweitern bis zu Randklassen
    ## A2: waere Teilung einer Klasse vor Erweiterung
    cl  <- unique(c(minQ, cl, maxQ))
    ncl <- length(cl)-1
  
    ## Teil 3: Konzentrationen zuordnen und Fracht berechnen
    qcl  <- cut(trans(Qy, logclass), breaks=cl, labels=FALSE, include.lowest=TRUE)
    tqcl <- as.data.frame(table(qcl))
    xcl  <- data.frame(no = 1:ncl,
                       ll = cl[-(ncl+1)],
                       ul = cl[-1]
                       )
  
    xcl$qsum  <- as.numeric(lapply(split(Qy, cut(trans(Qy, logclass), cl, labels=FALSE, include.lowest=TRUE)), sum))
    xcl$cmean <- as.numeric(lapply(split(c, cut(trans(Q, logclass), cl, labels=FALSE, include.lowest=TRUE)), mean))
  
    ## /sum ... mittlere Konzentration
    cc <- sum(xcl$qsum * xcl$cmean) #/sum(xcl$qsum)
  }
  
  ###################### MAIN ########################
  
  if (method == "reference") L<-loadreference(c, Q)
  if (method == "1")        L<-load1(c, Q, year=364)
  if (method == "2")        L<-load2(c, Q, Qy, year=364)
  if (method == "2new")     L<-load2new(c, Q, Qy, year=364)
  if (method == "3")        L<-load3(c, Q, year=364)
  if (method == "4")        L<-load4(c, Q, doy, year=364)
  if (method == "5a")       L<-load5a(c, Q, Qy, year=364)
  if (method == "5b")       L<-load5b(c, Q, Qy, year=364)
  if (method == "5c")       L<-load5c(c, Q, Qy, year=364)
  if (method == "5")        L<-load5(c, Q, Qy, year=364)
  if (method == "6a")       L<-load6a(c, Q, Qy, year=364)
  if (method == "6c")       L<-load6c(c, Q, Qy, year=364)
  if (method == "harned")   L<-loadharned(c, Q, Qy, ncl, logclass)

  return(L)
}
