#----------------------------------------------------------------------
# Programm, dass die Isoplethen der TSQ (von RWTH Aachen digitalisiert)
# einliest und in 2D linear interpoliert
# Achtung!!!
# ThPe 2001
#----------------------------------------------------------------------

rm(list=ls())
gc()

#--------------------------------------------------
# gleichmaessige Verteilung von Punkten auf einer
# in 2D geschlossenen Kurve
#--------------------------------------------------
interpol2D <- function (x, y, n) {
    
    dx <- c(x[1]-x[length(x)],diff(x))
    dy <- c(y[1]-y[length(y)],diff(y))
    d  <- (dx^2 + dy^2)^0.5
    cd <- cumsum(d)

    order <- seq(0, 1, length=length(x))
    cdnew <- seq(0,max(cd), length=n)

    ynew <-  approx(cd, y, cdnew)$y
    xnew <-  approx(cd, x, cdnew)$y
    list("x"=xnew,"y"=ynew)
}


skip     <- 0
iso      <- list(NULL)
niso     <- scan(file="Isolinien.txt", what=double(0),nmax=1)
skip     <- skip + 1
i        <- 1
colmax <- 0.4  # Dunkelgrau
colmin <- 0.1  # Hellgrau
zmax <- 64
zmin <- 53

xyz <- NULL


for (i in 1:1) {
    #########################################
    headline <- scan(file="Isolinien.txt", what="char",nmax=5,skip=skip)
    headline
    ndata    <- as.double(headline[1])
    level    <- as.double(headline[2])
    id_iso   <- as.double(headline[5])
    skip <- skip + 1
    tmp     <- scan(file="Isolinien.txt", what=double(0),nmax=ndata*2, skip=skip)
    skip <- skip + ndata
    tmp2 <- matrix(tmp, 2)       # => tmp2[1,] = x

    # xyz-Matrix erzeugen
    z <- rep(level,ndata)
    xyz <- cbind(xyz,rbind(tmp2,z)) # xyz[1,] = x 2=y, 3=z
    
    # --- Test: Rand anfuegen, damit "Talsperre nicht auslaeuft"
    z <- rep(level+1, ndata)
    xyz <- cbind(xyz,rbind(tmp2,z)) # xyz[1,] = x 2=y, 3=z
    

    # --- Umrisse fuer Grafik merken
    if (i==1) {      
      x1 <- xyz[1,]
      y1 <- xyz[2,]
      n1 <- ndata
      plot(x1,y1,col="blue",lwd=4)
    }
    colintens <- (level-zmin) / (zmax-zmin) * (colmax-colmin) + colmin
    #zmax <- max(level, zmax)
    #zmin <- min(level, zmin) Nur die Einleseroutine!!!

    #lines(tmp2[1,], tmp2[2,], col=colorvec[i])
    #polygon(tmp2[1,], tmp2[2,], col=colorvec[i])
    
}


xy <- interpol2D(x1,y1,250)

plot(xy$x,xy$y,type="b")
lines(x1,y1,col="blue")








