#Version 1.0; First written: 26/07/2020
#Authors: I.H. Salgado-Ugarte & V.M. Saito-Quezada
#Revised 16/03/2025
bootsam <- function(y, crbw, std, nr) {
source("c:/Rprogs/bootsam.R")
bsam <- replicate(nr,sample(((1+(crbw/std)^2)^-.5)*(y+crbw*rnorm(length(y))),replace=TRUE))
}
