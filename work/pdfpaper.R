 postscript("d:/pdf.ps")

#Laserdrucker-Version
fcol<-"black"
bcol<-"gray"

#Version fuer Farbdrucker
#col<-"red"
#col<-"green"

x<-seq(0,1,length=101)
y<-seq(-4,4,length=31)

oldpar <- par()
par(mar=c(5, 8, 4, 2) + 0.1)

gy  <- qnorm(py<-c(0.02,0.05,
                   0.1,0.2,0.5,
                   1,2,5,
                   10,20,30,40,50,60,70,80,90,
                   95,98,99,
                   99.5,99.8,99.9,
                   99.95,99.98)/100)
gy2 <- qnorm(c(seq(0.02,0.1,0.01),
               seq(0.1 ,0.2,0.05),
               seq(0.2 ,1  ,0.1),
               seq(1   ,3  ,0.2),
               seq(3   ,5  ,0.5),
               seq(5   ,10 ,1),
               seq(10  ,90 ,2),
               seq(90  ,95 ,1),
               seq(95  ,97 ,0.5),
               seq(97  ,99 ,0.2),
               seq(99  ,99.8,0.1),
               seq(99.8,99.9,0.05),
               seq(99.9,99.98,0.01))/100)

                   
plot(c(min(x),max(x)),c(-4,4),type="n",xlab="x",ylab="",axes=FALSE)

axis(2,gy,py*100,las=1)
axis(1,seq(0,1,0.05))

for (i in 1:length(gy2)) {
  lines(c(min(x),max(x)),rep(gy2[i],2),col=bcol)
}


for (i in 1:length(gy)) {
  lines(c(min(x),max(x)),rep(gy[i],2),col=fcol)
}

for (xx in x) lines(c(xx,xx),c(min(y),max(y)),col=bcol)

for (xx in 0:10/10) lines(c(xx,xx),c(min(y),max(y)),col=fcol)


dev.off()






