"vol.tsb" <-
function(z, level) {
  ## Grund bei 154m NN
  zz  <- level - 154 - z
  ## bei sehr kleiner Tiefe lineare Interpolation
  vol <- ifelse(zz < 0.5,
    approx(c(0, 0.5), c(0, 18020.74), zz)$y,
    ## Polynom nach U. Miersch
    vol <- 44331 - 202176 * zz + 363281 * zz^2 - 150342 * zz^3 + 47275.8 * zz^4 -
          6791.125 * zz^5 + 503.9106 * zz^6 - 18.95684 * zz^7 + 0.2871075 * zz^8
  )
  vol
}

