#Versión 1.0; First written: 03/07/2020
#Authors: I.H. Salgado-Ugarte & V.M. Saito-Quezada
#Colaborators (Phyton): N. Plascencia-Díaz & M.M. Salgado-Saito
l2cvwarpy2 <- function(y, delta, kercode, mstart, mend, osb) {
source("c:/Rprogs/l2cvwarpy2.R")
write.table(y, file="_data2.raw", row.names=FALSE, col.names=FALSE)
write.table(c(delta,kercode,mstart,mend), file="_inpval.raw", row.names=FALSE, col.names=FALSE)
iv <- matrix(c(delta, kercode, mstart, mend),1,4)
write.table(iv, file="_inpval.raw", row.names=FALSE, col.names=FALSE)
py_run_file("l2cvwarpy.py")
l2cvres <- read.table("resfile.csv",header=TRUE, sep=",", row.names=1)
l2cvress <- l2cvres[with(l2cvres, order(l2cvres$CV)),]
plot(CV~M, data=l2cvres, log="x", xlim=c(mstart, mend),type="l",
ylab="CV-value",xlab="M-value",
main=paste("L2 Cross Validation (WARP), delta =", delta,", Kernel = ", kercode))
abline(v=osb)
l2cvr<-data.frame(l2cvres)
return(l2cvress)
}
