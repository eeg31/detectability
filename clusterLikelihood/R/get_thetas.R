get_thetas <- function(syndrome_list, prob, nsim=1e3, w=0, return_sims=FALSE, ...){
  
  if(w==0) return(1)
  
  syndrome_names <- names(prob)
  symptom_names <- syndrome_list[[1]]$symptom

  sims <- tibble(syndrome=rep(syndrome_names, round(prob*nsim))) %>%
          mutate(id=1:nrow(.)) %>%
          left_join(expand.grid(syndrome=syndrome_names, 
                                symptom=syndrome_list[[1]]$symptom)) %>%
          mutate(n=purrr::map2_dbl(syndrome, symptom, function(x,y) sample(syndrome_list[[x]]$sample[[y]], 1))) %>%
          tidyr::pivot_wider(id_cols='id', names_from='symptom', values_from='n') %>%
          select(-'id') %>%
          data.matrix
  dists <- tsne::tsne(t(sims), k=1, max_iter = 100)
  dists <- abs(dists)/max(abs(dists))
  names(dists) <- symptom_names
  
  #theta <- sqrt(apply(matrix(comps$rotation[, 1:min(ncomps, ncol(comps$rotation))], ncol=min(ncomps, ncol(comps$rotation))),
  #                    1, function(x) sum(x^2)))

  dists <- dists*w + (1-w)
  
  if(return_sims) return(sims)
  return(dists)
}