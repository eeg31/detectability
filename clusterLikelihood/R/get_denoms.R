get_denoms <- function(symptom.mat, prob, verbose=FALSE){
  prob[prob < minprob] <- minprob
  prob[prob > 1 - minprob] <- 1 - minprob
  
  denoms <- numeric(ncol(symptom.mat))
  outprobs <- numeric(ncol(symptom.mat))
  
  for(i in 1:length(denoms)){
    
    symp.avail <- which(!is.na(symptom.mat[,i]) & symptom.mat[,i]!="" & symptom.mat[,i]!='NA')
    if(length(symp.avail) < nrow(symptom.mat) & verbose) {
      print(paste('symptom ', colnames(symptom.mat)[i],
                  ' not available for all diseases',sep=""))
    }

    symp.NI <- grepl("NI", symptom.mat[,i])
    if(sum(symp.NI)>0) symptom.mat[symp.NI,i] <- minprob
    
    #numerator: incidence of symptom among all diseases
    total <- as.numeric(symptom.mat[symp.avail,i])
    denoms[i] <- sum(prob[symp.avail]*total)/sum(prob[symp.avail])
  }
  
  denoms[denoms < minprob] <- minprob
  names(denoms) <- colnames(symptom.mat)
  return(denoms)
}