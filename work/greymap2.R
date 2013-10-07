##################################################
# greymap2.R
# erzeugt graue Hintergrundbalken in Abhängigkeit
# von einem Datenvektor
# ThPe, 11.09.2000, 
##################################################


## Parameter fuer das Verfahren
## maximaler Grauwert (schwarz=1)
colmax <- 0.4  # Dunkelgrau
colmin <- 0.1  # Hellgrau
nbars <- 100   # Feinheit der Balken


## Zuerst brauchen wir ein paar Daten ...
## ... erstmal eine Kurve fuer den Vordergrund
x <- seq(1,10,length=101)
y <- sin(x)

## ...dann die Daten fuer den grauen Hintergrund
##  Testdaten als Gradient (seq) oder als
#  gleichverteilte Zufallszahlen (runiq)
zdata <- 10
ygrey <- seq(-1, 1, length=zdata)
#zgrey <- seq( 0, 1, length=zdata)
zgrey <- runif(ygrey,min=0,max=1)


## Nun ermitteln wir die Koordinatengrenzen
xmin <- min(x)
xmax <- max(x)
ymin <- min(y, ygrey)
ymax <- max(y, ygrey)
zmax <- max(zgrey)

## Konstruktion von horizontalen grauen Balken
## zuerst die "Ecken" fuer die gefaerbten Boxen
xl<-seq(0,0,length=nbars)
xr<-seq(100,100,length=nbars)
classwidth <- (ymax - ymin)/nbars
ymean <- seq(ymin, ymax, length=nbars)
yu<- ymean - classwidth/2
yo<- ymean + classwidth/2

## Ermittlung der Grauintensitaet
## Beachte: approx macht lineare Interpolation
##  - gibt Liste mit den Spalten x und y zurueck
##  - hiervon wird nur y gebraucht.
zinterpol <- approx(ygrey, zgrey, ymean, method="linear")
colintens <- 1 -  (zinterpol$y / zmax * (colmax-colmin) + colmin)

## Jetzt noch das Ganze darstellen
##   Zunaechst einen leeren Plot
plot(x,y,type="n")

##   dann die Graubalken
rect(xl,yu,xr,yo,border=gray(colintens),col=gray(colintens))

## Alternativ: Bunt, z.B. "rainbow" oder
##   cm.colors, heat.colors, topo.colors, terrain.colors
# ncolors <- 500
# colorvec <- rainbow(ncolors, s = 1, v = 1, start = 0, end = max(1,ncolors - 1)/ncolors, gamma = 1)
# color <- colorvec[(ncolors-1) * (1-zinterpol$y / zmax)+1]
# rect(xl,yu,xr,yo,border=color,col=color)


##   und zum Schluss die Vordergrundgrafik
lines(x,y)

