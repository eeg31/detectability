#' @include get_context.R
#' @include get_syndrome.R
#' @include get_linelist.R


make_dummy_outbreak <- function(syndrome, n){

  ll <- as_tibble(expand.grid(case=1:n, symptom=as.character(unique(syndrome$symptom)))) %>%
        left_join(syndrome, by='symptom') %>%
        mutate(res=purrr::map_int(sample, function(i){
                  rbinom(1, prob=sample(i, 1), size=1)
          })
        ) %>%
        dplyr::select(-c('sample', 'mode', 'var')) %>%
        tidyr::pivot_wider(id_cols=case, names_from=symptom, values_from=res) %>%
        dplyr::select(-'case')

  return(ll)
}

make_dummy_from_mean <- function(syndrome_mean, #a vector of mean symptom rates
                                 n, ...){
  
  s_df <- data.frame(symptom=names(syndrome_mean),
                     mean=as.numeric(syndrome_mean[1,])) %>%
          filter(!is.na(mean))

  ll <- expand.grid(case=1:n, symptom=s_df$symptom) %>%
        left_join(s_df) %>%
        mutate(res=purrr::map_int(mean, function(i){
          rbinom(1, prob=i, size=1)
        })
        ) %>%
        tidyr::pivot_wider(id_cols=case, names_from=symptom, values_from=res) %>%
        dplyr::select(-'case')
  
  return(ll)
}

sim_outbreak <- function(ref_name=NULL, 
                         nsample=1, maxsize=10, 
                         minsize=1,
                         def_name='all symptoms',
                         only.ref=FALSE,
                         keep_dummy=FALSE, 
                         linelist=NULL,
                         symps_considered='all',
                         ...){
  
  if(is.null(ref_name) & is.null(linelist)) {
    print('either supply a linelist or a syndrome name to simulate')
    return(NA)
  } 
  
  
  update_context <- get_context(...)

  priors <- update_context$priors

  if(ref_name=='all') return(sim_all(names=update_context$syndrome_names,
                              nsample, maxsize, 
                              minsize,
                              def_name,
                              only.ref,
                              keep_dummy, 
                              linelist,
                              symps_considered, ...))
  
  if(symps_considered=='all') {
    symps_considered <- update_context$symptom_names
  } else if(symps_considered=='minimal') {
    symps_considered <- c('fever', 'death', 'any haemorrhage/bleeding', 'jaundice', 'hiccups')
  } else if (symps_considered=='typical' & !is.null(ref_name)){
    mean_symps <- update_context$mode[ref_name,]
    symp_diff <- mean_symps - update_context$denoms
    symps_considered <- names(mean_symps)[symp_diff > 0]
  } 
  
  if(!is.null(linelist) & !is.list(linelist)){
    if(nrow(linelist)>0) linelist <- linelist[,colnames(linelist) %in% symps_considered]
    sim_data <- list(get_linelist(linelist, context=update_context))
  } else if(is.list(linelist)){
    sim_data <- lapply(linelist, function(i) get_linelist(i[,colnames(i) %in% symps_considered], context=update_context))
  } else if(is.null(linelist)) {
    ref_syndrome <- filter(update_context$syndromes[[ref_name]], symptom %in% symps_considered)
    sim_data <- lapply(1:nsample, function(i) make_dummy_outbreak(syndrome=ref_syndrome, n=maxsize)) 
    sim_data <- lapply(sim_data, get_linelist, context=update_context)
  } else {
    sim_data <- get_linelist(linelist, update_context)
  }

  out_df <- tibble(size=minsize:maxsize) %>%
            mutate(info=def_name,
                   sims=purrr::map(size, function(i){
                       probs <- sapply(1:nsample, function(k){
                           y <- sim_data[[k]][1:i,]
                           likelihood(ll_data=y, name='all', context=update_context, ...)
                       })
                   })) %>%
             right_join(., as_tibble(expand.grid(size=minsize:maxsize, syndrome=names(priors))), by='size') %>%
             mutate(sims = purrr::map2(sims, syndrome, function(x, y) x[y,]),
                    prob = purrr::map_dbl(sims, function(i) median(i, na.rm=TRUE)),
                    min = purrr::map_dbl(sims, function(i) min(i, na.rm=TRUE)),
                    max = purrr::map_dbl(sims, function(i) max(i, na.rm=TRUE)),
                    min75 = purrr::map_dbl(sims, function(i) quantile(i, probs=0.25, na.rm=TRUE)),
                    max75 = purrr::map_dbl(sims, function(i) quantile(i, probs=0.75, na.rm=TRUE)),
                    min95 = purrr::map_dbl(sims, function(i) quantile(i, probs=0.025, na.rm=TRUE)),
                    max95 = purrr::map_dbl(sims, function(i) quantile(i, probs=0.975, na.rm=TRUE))
                    ) %>%
            dplyr::select(-'sims')
    
  if(only.ref) out_df <- filter(out_df, syndrome==ref_name)
  if(keep_dummy) return(list(result=out_df, linelist=sim_data))
  return(out_df)
}


sim_all <- function(names,
                    .nsample, .maxsize, 
                    .minsize,
                    .def_name,
                    .only.ref,
                    .keep_dummy, 
                    .linelist,
                    .symps_considered, ...){
  
  all_out <- lapply(names, function(x) {
                                    out <- sim_outbreak(ref_name=x, .nsample, .maxsize, 
                                                        .minsize,
                                                        .def_name,
                                                        .only.ref,
                                                        .keep_dummy, 
                                                        .linelist,
                                                        .symps_considered, ...)
                                    print(x)
                                    return(out)
                                    })
  names(all_out) <- names
  return(all_out)
}

