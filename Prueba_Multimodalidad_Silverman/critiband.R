#Version 1.0; First written: 26/07/2020
#Revised: 28/04/2021; 16/03/2025
#Authors: I.H. Salgado-Ugarte & V.M. Saito-Quezada
critiband <- function(y, bwh, bwl, st, m, plot="TRUE") {
source("c:/Rprogs/critiband.R")
countr <- 1
nr <- round((bwh-bwl)/st)
nr
bwc <- bwh
k <- 6
bw <- bwc

bwi <- numeric(nr)
numoi <- numeric(nr)

cat("Estimation number ","  Bandwidth ", "  Number of modes ","\n")

while (countr <= nr+1) {
   k <- 6
   dres <- warpdenpy(sort(y),bw,m,k,plot)
   bwc <- bw
   n<-c(1:length(dres$den))
   difvar<-dres$den[n+1]-dres$den[n]
   inmo<-rep(0,length(dres$den))
   inmo[(difvar[n]>=0 & difvar[n+1]<0)]=1
   numo=sum(inmo)
   cat("       ", countr, "               ", bwc, "            ", trunc(numo,2),"\n")

   bwi[countr] <- bw
   numoi[countr] <- numo
   cbw <- data.frame(cbind(numoi,bwi))
   #cat(numoi[countr],bwi[countr])

   bw <- bwc-st
   countr=countr+1   
   }

cbwo <- cbw[order(cbw$numoi),]
lnumo <- c(NA,cbwo$numoi[1:(length(cbwo$numoi)-1)])
lbw <- c(NA,cbwo$bwi[1:(length(cbwo$bwi)-1)])
dlnm <- cbwo$numoi-lnumo
critb <-data.frame(cbind(cbwo$numoi[dlnm!=0]-1,lbw[dlnm!=0]))
names(critb)<- c("NuMo", "CBW")
critb <- na.omit(critb)
critb

}
