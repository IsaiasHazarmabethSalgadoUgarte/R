#Versi�n 1.0; First written: 03/07/2020
#Authors: I.H. Salgado-Ugarte & V.M. Saito-Quezada
#Colaborators (Phyton): N. Plascencia-D�az & M.M. Salgado-Saito
#Revised 16/03/2025
bcvwarpy <- function(y, delta, kercode, mstart, mend, osb) {
source("c:/Rprogs/bcvwarpy.R")
write.table(y, file="_data2.raw", row.names=FALSE, col.names=FALSE)
write.table(c(delta,kercode,mstart,mend), file="_inpval.raw", row.names=FALSE, col.names=FALSE)
iv <- matrix(c(delta, kercode, mstart, mend),1,4)
write.table(iv, file="_inpval.raw", row.names=FALSE, col.names=FALSE)
py_run_file("bcvwarpy.py")
bcvres <- read.table("resfile.csv",header=TRUE, sep=",", row.names=1)
bcvress <- bcvres[with(bcvres, order(bcvres$BCV)),]
maxx<-max(mend,osb)

if(kercode==1) kfun <- "Quartic" else
if(kercode==2) kfun <- "Triweight"

plot(BCV~M, data=bcvres, log="x", xlim=c(mstart, maxx),type="l",
ylab="BCV-value",xlab="M-value",
main=paste("Biased Cross Validation (WARP), delta =", delta,",",kfun," Kernel"))
abline(v=osb, col="red")
bcvr<-data.frame(bcvres)
bcvr2<-data.frame(bcvress)
cat("-----------------------------------------------------------------------","\n")
cat("Biased Cross-validation for WARPing density estimation,",kfun,"Kernel","\n") 
cat("-----------------------------------------------------------------------","\n")
cat("Biased Cv-score                 M-value                    Bandwidth","\n")

spa1 <- rep(" ",10)
spa2 <- rep(" ",11)
for (i in 1:5) cat(" ",format(bcvr2[i,1],nsmall=10),spa1,format(bcvr2[i,2],digits=1),spa2,format(bcvr2[i,3],nsmall=4),"\n")
bcvr<-data.frame(bcvr)
#return(bcvr2)
}