#' @include load_default.R
#' @include get_beta.R
#' @include get_denoms.R

inner_syndrome <- function(x_name = 'all',
                     n = default_sample,
                     context = NULL,
                     mode_matrix = default_mode, # a matrix with named rows (syndromes) and columns (symptoms)
                     var_matrix = default_var, # a matrix matched to mode_matrix
                     incidence = NULL,
                     thetas = NULL,
                     ...
                     ){
  if(!is.null(context)) {
    syndromes <- context$syndromes
    return(syndromes)
  } else {
    syndrome_names <- rownames(mode_matrix)
    symptom_names <- colnames(mode_matrix)
    
    # reorder prior incidences
    incidence <- incidence[syndrome_names]

    #output distributions in correct format
    syndromes <- list()
  }
  
  if(x_name=='all') {
    x_name <- syndrome_names
  } 
  
  denoms <- get_denoms(mode_matrix, incidence)

  syndromes <- list()
  syndromes <- lapply(x_name, function(i){
    tibble(symptom=symptom_names) %>%
    mutate(mode=purrr::map_dbl(symptom, function(j) mode_matrix[i, j]),
           var=purrr::map_dbl(symptom, function(j) var_matrix[i, j]),
           sample=purrr::map2(mode, var, function(x,y) get_beta(mode=x, var=y)),
           denom=denoms)
          
  })
  names(syndromes) <- syndrome_names
  
  if(is.null(thetas)) thetas <- get_thetas(syndromes, prob=incidence, ...)
  
  syndromes <- lapply(syndromes, function(i) {
                                            i$theta <- thetas
                                            i})
  #return named list of distributions (with n members)
  return(syndromes)
}
