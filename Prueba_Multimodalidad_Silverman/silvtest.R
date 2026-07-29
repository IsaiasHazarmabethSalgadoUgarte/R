#Version 1.0; First written: 26/07/2020
#Authors: I.H. Salgado-Ugarte & V.M. Saito-Quezada
#Revised 16/03/2025
silvtest <- function(y, crbw, m, nuri=1, nurf, cnm, list="FALSE", plot="TRUE") {
source("c:/Rprogs/silvtest.R")
countr <- 1
bw <- crbw
k <- 6
nr <- nurf-nuri+1
snaf<-0
while (countr <= nr) {
   dres <- warpdenpy(sort(y[,countr]),bw,m,k,plot)
   bwc <- bw
   n<-c(1:length(dres$den))
   difvar<-dres$den[n+1]-dres$den[n]
   inmo<-rep(0,length(dres$den))
   inmo[(difvar[n]>=0 & difvar[n+1]<0)]=1
   numo<-sum(inmo)
   na<-rep(0,length(dres$den))
   #if(numo[n]>cnm) {na[n]=1} 
   na[(numo[n]>cnm)]=1
   sna<-sum(na)
   if (list=="TRUE") cat("Estimation number = ", countr, "  Number of modes = ", trunc(numo,2),"\n")
   snaf<-snaf+sna   
   countr<-countr+1
   }  
cat(" ","\n")
cat("Critical number of modes = ",cnm, "\n")
cat(" ","\n")
cat("Pvalue = ", snaf, "/ ",nr, "=" , snaf/nr, "\n") 
}
