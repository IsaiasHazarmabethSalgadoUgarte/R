#Versión 1.0; First written: 15/07/2020
#Revised: 07/04/2024; 17/03/2025
#Authors: I.H. Salgado-Ugarte & V.M. Saito-Quezada
nuamodes <- function(x,y) {
source("c:/Rprogs/nuamodes.R")
n<-c(1:length(x))
difvar<-x[n+1]-x[n]
inamo<-rep(0,length(x))
inamo[(difvar[n]<=0 & difvar[n+1]>0)]=1

inamo[1]=0
inamo[length(x)-1]=0

nuamo=sum(inamo)
inter<-y[2]-y[1]
amodes<-y[inamo==1]+inter
cat("____________________________________________________________","\n")    
cat("Number of antimodes = ", trunc(nuamo,2),"\n")
cat("____________________________________________________________","\n")
cat("Antimodes in density/frequency estimation","\n")
cat("____________________________________________________________","\n")
i <- 1
while (i < nuamo+1) {
   cat(" Antimode ( ", trunc(i,2), " ) = ", round(amodes[i],4),"\n")
   i <- i + 1
   }
cat("____________________________________________________________","\n")
return(amodes)
}
