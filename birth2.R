ptormore <- function(k) {
  return(p <- 1 - choose(365, 365 - k) * factorial(k) / 365 ^ k)
}
k<-ptormore(k=30)
k
