x <- round((1:10)* 14 + runif(10, min=-5, max=5))

y <- rnorm(x, mean=50, sd=10)

tbarplot <- function(x, y, wbox = min(diff(x))*0.9, ofs=0.5, ...) {
  barplot(y, wbox, space = c(x[1]-wbox*ofs, (diff(x)-wbox ))/wbox, ...)
}


tbarplot(x,y, ylim=c(0, 60))
points(x,rep(0, length(x)), pch=2, col="red")
axis(1)
box()