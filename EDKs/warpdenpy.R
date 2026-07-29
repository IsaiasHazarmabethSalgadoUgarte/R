#Version 1.0; First written: 15/07/2020
#Revised 07/08/2020; 09/04/2021; 13/02/2022; 16/03/2025
#Authors: I.H. Salgado-Ugarte & V.M. Saito-Quezada
#Colaborators (Phyton): N.I. Plascencia-Diaz & M.M. Salgado-Saito
warpdenpy <- function(y, bw, mval, kercode, plot="TRUE", gt="poly", ...) {
source("c:/Rprogs/warpdenpy.R")
write.table(sort(y), file="_data2.raw", row.names=FALSE, col.names=FALSE)
write.table(c(bw, mval, kercode), file="_inpval.raw", row.names=FALSE, col.names=FALSE)
iv <- matrix(c(bw, mval, kercode),1,3)
write.table(iv, file="_inpval.raw", row.names=FALSE, col.names=FALSE)
py_run_file("warping.py")
warpres <- read.table("resfile.csv",header=TRUE, sep=",", row.names=1)
file.remove(c("_data2.raw","_inpval.raw","resfile.csv"))
if (plot=="TRUE") {
if (gt=="poly") {
plot(den~mid, data=warpres, type="l", ylab="Density",xlab="Midpoints",
main=paste("WARPing density (poly), bw =", format(bw,nsmall=4) ,", M = ", mval,", Kernel = ", kercode), ...)
} else
if (gt=="step")  {
   inter<-warpres$mid[2]-warpres$mid[1]
   lowcut<-warpres$mid-(inter/2)
   plot(den~lowcut, data=warpres, type="s", ylab="Density",xlab="Midpoints",
   main=paste("WARPing density (step), bw =", format(bw,nsmall=4) ,", M = ", mval,", Kernel = ", kercode), ...)
   }
}
warpres<-data.frame(warpres)
#return(warpres)
}
