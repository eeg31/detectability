#' @include load_default.R
#' @include inner_syndrome.R
#' @include get_denoms.R
#' @include get_thetas.R

check_priors <- function(p){
  p[p < minprob] <- minprob
  p[p > 1-minprob] <- 1 - minprob
  return(p)
}


get_context <- function(mode_matrix=default_mode,
                    var_matrix=default_var,
                    syndrome_names=NULL,
                    symptom_names=NULL,
                    symptoms_considered='all',
                    location=NULL,
                    priors=NULL,
                    spill_scalar=1,
                    ... 
                  ){
  if(symptoms_considered != 'all') {
    mode_matrix <- mode_matrix[, symptoms_considered]
    var_matrix <- var_matrix[, symptoms_considered]
  }
  
  if(is.null(syndrome_names)) syndrome_names <- rownames(mode_matrix)
  if(is.null(symptom_names)) symptom_names <- colnames(mode_matrix)

  new_df <- matrix(character(length(syndrome_names)*length(symptom_names)),
                   ncol=length(symptom_names),
                   dimnames = list(syndrome_names, symptom_names))

  #get denoms

  x <- location
  if(!is.null(x) & is.null(priors)) {

    # if a location is named (and priors not given), obtain incidences from data frame
    incidences <- filter(default_incidence, location==x)
    syndrome_names <- syndrome_names[syndrome_names %in% incidences$syndrome]
                  
    priors <- incidences$incidence
    names(priors) <- incidences$syndrome
    priors <- priors[syndrome_names]
  } else {
    # if no location, input incidence must be either a named vector or a data frame
    # with columns 'syndrome' and 'incidence'
    if(is.null(priors)) {
      #TODO: fix this
      
      print('Warning: name a location. Using SSA default incidences')
      priors <- filter(default_incidence, location=='SSA')$incidence
      names(priors) <- filter(default_incidence, location=='SSA')$syndrome
    }
    
    if(mode(priors)=='numeric'){
      syndrome_names <- syndrome_names[syndrome_names %in% names(priors)]
      
      priors <- priors[syndrome_names]
    } else {
      #throw error: incidence improperly specified
    }
  }
  
  priors[to_scale] <- priors[to_scale]*spill_scalar 
  priors <- priors/sum(priors[!is.na(priors)])
  priors <- check_priors(priors)
  priors <- priors/sum(priors[!is.na(priors)])
  
  mode_matrix <- mode_matrix[syndrome_names,]
  denoms <- get_denoms(mode_matrix, priors)
  
  mode_matrix <- sapply(1:ncol(mode_matrix), function(i) {
    x <- as.numeric(mode_matrix[,i])
    x <- case_when(as.numeric(x) > 1 - minprob ~ 1 - minprob,
              as.numeric(x) < minprob ~ minprob,
              x=="" | x=='NA' ~ as.numeric(NA),
              TRUE~as.numeric(x))
    
    mean_x <- mean(x, na.rm=T)
    case_when(is.na(x) ~ mean_x,
              TRUE~x)
  })
  colnames(mode_matrix) <- symptom_names
  rownames(mode_matrix) <- syndrome_names
  
  var_matrix <- var_matrix[syndrome_names,symptom_names]
  priors[is.na(priors)] <- 0
  
  #convert mode and vars to distributions (inner_syndrome)
  syndromes <- inner_syndrome(x_name='all', incidence=priors, 
                              mode_matrix=mode_matrix, var_matrix=var_matrix, ...)

  thetas <- get_thetas(syndrome_list = syndromes,
                      prob=priors, 
                      ...)
  
  return(list(denoms = denoms,
              priors = priors,
              syndromes = syndromes,
              mode = mode_matrix,
              symptom_names=symptom_names,
              syndrome_names=syndrome_names,
              theta=thetas
  ))
}
