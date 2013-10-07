lagged <- function(x, k) {
  L <- length(x+k)
  z <- matrix(NA, nrow = L + k - 1, ncol = k)
  for (i in 1:k) {
    z[(1:L) + (i - 1), i] <- x
  }
  return(z)
}


x <- rep(1, 30)

x <- lagged(x, 5)

xx <- rowSums(x)

wf <- function(x, r) {
  w <- exp(x * r)
  w/sum(w)
}

z <- t(t(x) * wf(1:5, -0.1))
