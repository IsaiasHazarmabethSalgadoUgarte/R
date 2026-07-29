#Versi�n 1.0; First written: 03/07/2020
#Authors: I.H. Salgado-Ugarte & V.M. Saito-Quezada
#Colaborators (Phyton): N. Plascencia-D�az & M.M. Salgado-Saito
#Revised 16/03/2025
l2cvwarpy <- function(y, delta, kercode, mstart, mend, osb) {
source("c:/Rprogs/l2cvwarpy.R")
write.table(y, file="_data2.raw", row.names=FALSE, col.names=FALSE)
write.table(c(delta,kercode,mstart,mend), file="_inpval.raw", row.names=FALSE, col.names=FALSE)
iv <- matrix(c(delta, kercode, mstart, mend),1,4)
write.table(iv, file="_inpval.raw", row.names=FALSE, col.names=FALSE)
py_run_file("l2cvwarpy.py")
l2cvres <- read.table("resfile.csv",header=TRUE, sep=",", row.names=1)
l2cvress <- l2cvres[with(l2cvres, order(l2cvres$CV)),]
maxx<-max(mend,osb)

if(kercode==1) kfun <- "Uniform" else
if(kercode==2) kfun <- "Triangular" else
if(kercode==3) kfun <- "Epanechnikov" else
if(kercode==4) kfun <- "Quartic" else
if(kercode==5) kfun <- "Triweight" else
if(kercode==6) kfun <- "Gaussian"

plot(CV~M, data=l2cvres, log="x", xlim=c(mstart, maxx),type="l",
ylab="CV-value",xlab="M-value",
main=paste("L2 Cross Validation (WARP), delta =", delta,",",kfun," Kernel"))
abline(v=osb, col="red")
l2cvr<-data.frame(l2cvres)
l2cvr2<-data.frame(l2cvress)
cat("----------------------------------------------------------------------------------","\n")
cat("Least Squares Cross-validation for WARPing density estimation,",kfun,"Kernel","\n") 
cat("----------------------------------------------------------------------------------","\n")
cat("Biased Cv-score                 M-value                    Bandwidth","\n")

spa1 <- rep(" ",9)
spa2 <- rep(" ",12)
for (i in 1:5) cat(" ",format(l2cvr2[i,1],nsmall=10),spa1,format(l2cvr2[i,2],digits=1),spa2,format(l2cvr2[i,3],nsmall=4),"\n")
l2cvr<-data.frame(l2cvr)

#return(l2cvress)
}
