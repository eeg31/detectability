#' @include load_default.R

plot_syndromes <- function(syndromes, priors=NULL, ns=1000){
  out <- list()
  
  if(!is.list(syndromes)) {
    new <- list()
    new[[1]] <- syndromes
    syndromes <- new
  }
  
  symptom_names <- syndromes[[1]]$symptom
  
  for(i in 1:length(syndromes)){
    name <- names(syndromes)[i]
    
    to_plot <- syndromes[[i]] %>%
               mutate(typical=mode>denom) %>%
               tidyr::unnest('sample') %>%
               mutate(sample=as.numeric(sample)) %>%
               filter(!is.na(sample)) 

    ordered <- unique(to_plot$symptom)[rev(order(unique(to_plot$symptom)))]
    to_plot <- to_plot %>%
               mutate(symptom=factor(symptom, ordered))

   g <- ggplot(to_plot) +
      geom_violinhalf(aes(x=symptom, y=sample, fill=typical), lwd=0, scale='width')+
      scale_y_continuous('density', limits=c(0,1))+
      scale_x_discrete('')+
      coord_flip()+
      theme_classic()+
      ggtitle(name) +
      theme(text=element_text(size=10), plot.title=element_text(size=10, hjust=1))+
      scale_fill_viridis_d(option="A", begin=0.1, end=0.9)+
     guides(fill=FALSE)
   
   out[[i]] <- print(g)
  }
  
  ordered <- symptom_names[order(symptom_names)]
  symptom_names <- factor(symptom_names, ordered)
  
  if(!is.null(priors)){
    
    to_plot <- as_tibble(expand.grid(syndrome=rep(names(priors), ceiling(priors*ns)),
                                  symptom=symptom_names)) %>%
               mutate(sample=purrr::map2_dbl(syndrome, symptom, function(i, j){
                      vals <- filter(syndromes[[i]], symptom==j)$sample[[1]]
                      sample(vals, 1)
               })
               )
             
    
    ordered <- unique(to_plot$symptom)[rev(order(unique(to_plot$symptom)))]
    to_plot <- to_plot %>%
      mutate(symptom=factor(symptom, ordered))
    
    g <- ggplot(to_plot) +
      geom_violinhalf(aes(x=symptom, y=sample), lwd=0, fill='grey50', scale='width')+
      scale_y_continuous('density', limits=c(0,1))+
      scale_x_discrete('')+
      coord_flip()+
      theme_classic()+
      ggtitle("overall")+
      theme(text=element_text(size=10), plot.title=element_text(size=10, hjust=1))
    
    out[[i]] <- print(g)
    
  }
  
  return(out)
}
