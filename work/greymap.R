##################################################
# greymap.R
# erzeugt graue Hintergrundbalken in Abhängigkeit
# von einem Datenvektor
# ThPe, 11.09.2000, Weitergabe erlaubt
##################################################
#Parameter

#maximaler Grauwert (schwarz=1)
colmax <- 0.4
colmin <- 0.1
nbars <- 50

#Zuerst brauchen wir ein paar Daten
#
# erstmal eine Kurve fuer den Vordergrund
x <- seq(1,10,length=101)
y <- sin(x)

#dann die Daten fuer den grauen Hintergrund
#  Testdaten als Gradient oder als
#  gleichverteilte Zufallszahlen
zdata <-10
ygrey <- seq(-1,1,length=zdata)
zgrey <- seq(0,1, length=zdata)
#zgrey <- runif(y,min=0,max=1)

#Jetzt ermitteln wir die Koordinatengrenzen
xmin <- min(x)
xmax <- max(x)
ymin <- min(y, ygrey)
zmax <- max(zgrey)

#Anzahl der Werte
ngrey <- length(ygrey)

#und nun erzeugen wir die Ecken fuer die gefaerbten Boxen
xl<-seq(0,0,length=ngrey)
xr<-seq(100,100,length=ngrey)
yu<- ygrey - (ygrey[2]-ygrey[1])/2
yo<- ygrey + (ygrey[2]-ygrey[1])/2

# und deren Grauintensitaet
colintens <- 1 - (zgrey / zmax * (colmax-colmin) + colmin)

# Jetzt noch das Ganze darstellen
#   Zunaechst einen leeren Plot
plot(x,y,type="n")

#   dann die Graubalken
rect(xl,yu,xr,yo,border=gray(colintens),col=gray(colintens))

#   und zum Schluss die Vordergrundgrafik
lines(x,y)

