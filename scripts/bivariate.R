library(classInt)
library(raster)
library(rgdal)
library(dismo)
library(XML)
library(maps)
library(sp)
library(viridis)
require(reshape2)

colmat<-function(nquantiles=50, upperleft='cyan', upperright='purple', bottomleft='beige', bottomright='brown1'){
    my.data<-seq(0,1,.01)
    my.class<-classIntervals(my.data,n=nquantiles,style="quantile")
    my.pal.1<-findColours(my.class,c(upperleft,bottomleft))
    my.pal.2<-findColours(my.class,c(upperright, bottomright))
    col.matrix<-matrix(nrow = 101, ncol = 101, NA)
    for(i in 1:101){
      my.col<-c(paste(my.pal.1[i]),paste(my.pal.2[i]))
      col.matrix[102-i,]<-findColours(my.class,my.col)
    }
      
    for(i in 1:101){
      col.temp<-col.matrix[i-1,]
    }
    
    seqs<-seq(0,100,(100/nquantiles))
    seqs[1]<-1
    col.matrix<-col.matrix[c(seqs), c(seqs)]
    
    return(col.matrix)
}



biv_legend <- function(mat, xlab="x label", ylab="y label", xlimits=c(0,1), ylimits=c(0,1), ...){
  
  n <- nrow(mat)
  
  colnames(mat) <- sapply(1:ncol(mat), function(y) paste('Y', y, sep='-'))
  rownames(mat) <- sapply(1:nrow(mat), function(x) paste('X', x, sep='-'))
  mat <- as_tibble(mat, rownames='X') %>% 
         tidyr::pivot_longer(cols=starts_with('Y')) %>%
         mutate(X=gsub('X-', '', X),
                name=gsub('Y-', '', name),
                X=as.numeric(X),
                name=as.numeric(name))
  
  leg <- ggplot(mat)+
    geom_raster(aes(x=X, y=name, fill=value))+
    scale_fill_identity()+
    theme_classic()+
    scale_x_continuous(xlab, breaks=seq(1, n-1, length.out = 5), 
                       labels=round(seq(xlimits[1], xlimits[2], length.out=5), 2))+
    scale_y_continuous(ylab, breaks=seq(1, n-1, length.out = 5), 
                       labels=seq(ylimits[1], ylimits[2], length.out=5), 2)
  
  return(leg)
  
}
  



bivariate.map<-function(df, nquantiles=50, ...){

  col.matrix <- colmat(nquantiles)

  df$Xquantile <- NA
  df$Yquantile <- NA
  
  Ybrks <- with(df, quantile(unique(df$ebola.prob), na.rm=TRUE, probs = c(seq(0,1,1/nquantiles))))
  Xbrks <- with(df, quantile(unique(df$EVDrisknorm), na.rm=TRUE, probs = c(seq(0,1,1/nquantiles))))
  df$Yquantile <- cut(df$ebola.prob, breaks = Ybrks, labels = 2:length(Ybrks),include.lowest = TRUE)
  df$Xquantile <- cut(df$EVDrisknorm, breaks = Xbrks, labels = 2:length(Xbrks),include.lowest = TRUE)
  
  cols <- numeric(nrow(df))
  for(i in 1:length(cols)){
    a <- df$Xquantile[i]
    b <- df$Yquantile[i]
    cols[i] <- col.matrix[b,a]
  }
  df$color <- cols
  
  legend <- biv_legend(col.matrix, ylimits=c(0,1),
                                   xlimits=c(min(df$ebola.prob, na.rm=T), max(df$ebola.prob, na.rm=T)), 
                       ...)
  
  return(list(map=df, legend=legend))
}
  