#' @include load_default.R

get_beta <- function(mode, var, pn=default_sample, tol=1e-3, ...){
  
  if(mode < tol) mode <- tol
  if(mode > 1 - tol) mode <- 1 - tol
  
  alpha <- (mode)*var
  beta <- (1 - mode)*var
  
  return(qbeta(seq(1/(2*pn), 1-1/(2*pn), length.out=pn), shape1=alpha, shape2=beta))
  
}