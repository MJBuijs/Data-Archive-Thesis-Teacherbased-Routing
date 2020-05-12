######
# F7. Theta_comp()
######

## ---------------------------------------------------------------------------------------------------------------------------------
# Comparison of theta's 
Theta_comp <- function(true, est) {
  a <- 2 * t(true) %*% est
  b <- t(true) %*% true + t(est) %*% est
  return(a / b)
}
