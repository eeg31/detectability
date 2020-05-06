#' @include get_context.R

get_syndrome <- function(...){

  update_context <- get_context(...)
  inner_syndrome(context=update_context, ...)
}
