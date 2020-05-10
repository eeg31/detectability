coords2country <- function(points) {  
  
  points <- data.frame(lat=points$x, long=points$y)
  countriesSP <- getMap(resolution='low')
  pointsSP = SpatialPoints(points, proj4string=CRS(proj4string(countriesSP)))  
  
  indices = over(pointsSP, countriesSP)
  
  indices$ISO3 
}