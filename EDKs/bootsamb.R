#Version 1.0; First written: 26/07/2020
#Authors: I.H. Salgado-Ugarte & V.M. Saito-Quezada
#Revised 16/03/2025
bootsamb <- function(y, crbw, nr) {
source("c:/Rprogs/bootsam.R")
std <- sd(y)
bsam <- replicate(nr,sample(((1+(crbw/std)^2)^-.5)*(y+crbw*rnorm(length(y))),replace=TRUE))
}
