#Versión 1.0; First written: 15/07/2020
#Revised: 07/04/2024; 17/03/2025
#Authors: I.H. Salgado-Ugarte & V.M. Saito-Quezada
numodes <- function(x,y) {
source("c:/Rprogs/numodes.R")
n<-c(1:length(x))
difvar<-x[n+1]-x[n]
inmo<-rep(0,length(x))
inmo[(difvar[n]>=0 & difvar[n+1]<0)]=1
numo<-sum(inmo)
inter<-y[2]-y[1]
modes<-y[inmo==1]+inter
cat("____________________________________________________________","\n")    
cat("Number of modes = ", trunc(numo,2),"\n")
cat("____________________________________________________________","\n")
cat("Modes in density/frequency estimation","\n")
cat("____________________________________________________________","\n")
i <- 1
while (i < numo+1) {
   cat(" Mode ( ", trunc(i,2), " ) = ", round(modes[i],4),"\n")
   i <- i + 1
   }
cat("____________________________________________________________","\n")
return(modes)
}
