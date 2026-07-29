#Versión 1.0; First written: 11/07/2020
#Revised: 02/05/2021
#Authors: I.H. Salgado-Ugarte & V.M. Saito-Quezada
#Colaborators (Phyton): N.I. Plascencia-Díaz & M.M. Salgado-Saito

gwarprpy <- function(y, x, delta, selector, kercode, mstart, mend, bound=0.1) {
source("c:/Rprogs/gwarprpy.R")

write.table(cbind(x,y), file="_data2.raw", row.names=FALSE, col.names=FALSE, sep="\   ")
write.table(c(delta,kercode,mstart,mend), file="_inpval.raw", row.names=FALSE, col.names=FALSE)
iv <- matrix(c(delta, selector, kercode, mstart, mend, bound),1,6)
write.table(iv, file="_inpval.raw", row.names=FALSE, col.names=FALSE)

py_run_file("gwarpreg.py")

gwarpres <- read.table("resfile.csv",header=TRUE, sep=",", row.names=1)
gwarpress <- gwarpres[with(gwarpres, order(gwarpres$C)),]

if(selector==1) selector <- "Shibata" else
if(selector==2) selector <- "GCV" else
if(selector==3) selector <- "Akaike" else
if(selector==4) selector <- "FPE" else
if(selector==5) selector <- "Rice's T"

if(kercode==1) kfun <- "Uniform" else
if(kercode==2) kfun <- "Triangular" else
if(kercode==3) kfun <- "Epanechnikov" else
if(kercode==4) kfun <- "Quartic" else
if(kercode==5) kfun <- "Triweight" else
if(kercode==6) kfun <- "Gaussian"

plot(C~M, data=gwarpres, type="l", ylab="CV-value",xlab="M-value",
main=paste("G(M), delta =", delta,",  PF = ", selector,", Kernel = ", kfun))

gwarpres<-data.frame(gwarpres)
gwarpress<-data.frame(gwarpress)

cat("----------------------------------------------------------------------------------","\n")
cat("G(M) CV (WARP) N-W Regression , PF = ",selector,", ",kfun,"Kernel, ","Delta = ",delta,"\n") 
cat("----------------------------------------------------------------------------------","\n")
cat("      CV-score                 M-value                    Bandwidth","\n")

spa1 <- rep(" ",9)
spa2 <- rep(" ",12)

for (i in 1:5) cat(" ",format(gwarpress[i,1],nsmall=10),spa1,format(gwarpress[i,2],digits=0),spa2,format(gwarpress[i,3],nsmall=4),"\n")
#gwarpres<-data.frame(gwarpres)

gwarpr<-data.frame(gwarpres)
#return(gwarpress)
}
