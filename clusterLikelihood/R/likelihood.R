#' @include get_context.R
#' @include dbetabinom.R

#TODO: include denominators and priors

AICc <- function(nLog, k, n){
  out <- 2*k + 2*nLog
  out <- out + (2*k^2 + 2*k)/(n - k - 1)
  return(as.numeric(out))
}


likelihood <- function(ll_data, name=NULL, context=NULL, symptoms_considered='all', ...){
  if(is.null(context)) {
    print('warning: no context specified to likelihood function; will be recalculated')
    context <- get_context(...)
  }

  syndrome_names <- context$syndrome_names
  if(name=='all') name <- syndrome_names
  if(is.null(name)) {
    print('no syndrome specified: getting all likelihoods')
    name <- syndrome_names
  }
  denoms <- context$denoms
  
  ll_data <- ll_data[, colnames(ll_data) %in% context$symptom_names]
  priors <- context$priors[syndrome_names]
  to_fill <- as_tibble(expand.grid(symptom=colnames(ll_data), syndrome=syndrome_names)) %>%
             mutate_if(is.factor, as.character) %>%
             mutate(mode=purrr::map2_dbl(syndrome, symptom, function(x, y) filter(context$syndromes[[x]], symptom==y)$mode),
                    var=purrr::map2_dbl(syndrome, symptom, function(x, y) filter(context$syndromes[[x]], symptom==y)$var),
                    prior=purrr::map_dbl(syndrome, function(y) priors[y]),
                    denom=purrr::map_dbl(symptom, function(y) denoms[y]),
                    theta=purrr::map2_dbl(syndrome, symptom, function(x, y) filter(context$syndromes[[x]], symptom==y)$theta),
                    pos=purrr::map_dbl(symptom, function(i){
                      sum(ll_data[,i], na.rm=TRUE)
                    }),
                    total=purrr::map_dbl(symptom, function(i){
                      sum(!is.na(ll_data[,i]))
                    })) %>%
              filter(total>0) %>%
              mutate(lik=purrr::map_dbl(1:nrow(.), function(i){
                      dbetabinom(pos=pos[i], size=total[i], mode=mode[i], var=var[i]) * theta[i]
                    }))
  
  liks <- sapply(syndrome_names, function(i){
          s <- filter(to_fill, syndrome==i)
          sum(s$lik)
  }) 
  liks <- liks - log(priors[syndrome_names])
  liks <- exp(-liks)/sum(exp(-liks), na.rm=T)
  liks <- liks[which(syndrome_names %in% name)]
  names(liks) <- name
  
  if(any(is.na(liks) | is.infinite(liks) | is.nan(liks))) {
    print(paste("NA likelihood: syndrome ", name))
  }
  
  return(liks)
}
