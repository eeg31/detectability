#' @include load_default.R

#negative log likelihood of a beta binomial distribution
dbetabinom <- function(pos, size, mode, var, tol=1e-3) {
  
  if(mode < tol) mode <- tol
  if(mode > 1 - tol) mode <- 1 - tol
  
  alpha <- (mode)*var
  beta <- (1 - mode)*var
  
  out <- extraDistr::dbbinom(pos, size = size, alpha = alpha, beta = beta, log=FALSE) 
  out <- max(out, minprob)
  
  return(-log(out))
}
