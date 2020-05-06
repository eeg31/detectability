#' @include get_thetas.R

plot_comps <- function(...){
  context <- get_context(...)
  
  comps <- get_thetas(context$mean, return_comps = TRUE)
  
  
}