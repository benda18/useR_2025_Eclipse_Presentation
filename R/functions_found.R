library(renv)
library(ggplot2)
library(swephR)

rm(list=ls());cat('\f')
# "The real treasure was the functions we made along the way" - Anon

convertGeodedicLatLonToITRFXYZ <- function(lon      = -78.9202362421599, 
                                           lat      = 36.04764254394491, 
                                           h.meters = 0){
  #Algorithm from Explanatory Supplement to the Astronomical Almanac 3rd ed. P294
  a    <- 6378136.6
  f    <- 1/298.25642
  C    <- sqrt(((cos(lat)^2) + ((1.0-f)^2) * (sin(lat)^2)))
  S    <- ((1-f)^2) *C
  h    <- h.meters
  r    <- array()
  r[1] <- (a*C+h) * cos(lat) * cos(lon)
  r[2] <- (a*C+h) * cos(lat) * sin(lon)
  r[3] <- (a*S+h) * sin(lat)
  
  names(r) <- c("x", "y", "z")
  
  return(r)
}

r12 <- convertGeodedicLatLonToITRFXYZ() 

plot(x = r12["x"], y = r12["y"])




