#Version 1: 03/10/2020; revised: 04/02/2021; 26/04/2021
#Original version:
#11/09/98 by Isaías Hazarmabeth Salgado-Ugarte
#R version by I.H. Salgado-Ugarte & V.M. Saito-Quezada
#Revised 16/03/2025
#Laboratorio de Biometria y Biologia Pesuqra
#Batalla 5 de mayo S/N esq. Fuerte de Loreto
#Ejército de Oriente, Iztapalapa, 09230, CDMX
#Mexico City; e-mail: isalgado@unam.mx
#As the Stata version, This program calculates logaritmic differences and draw the Bhathacharya's
#plot using the observation's numbers as plotting symbols in order to 
#define the points definning negatively sloped lines, which represent
#individual gaussian components.
#This routine is a based on the new version integrating two previous simple Stata programs
#diflogen.ado and bhatplot.ado.
bhatgauc <- function(f, mp, mi=1,mf=length(mp),gc) {
source("c:/Rprogs/bhatgauc.R")
index <- c(1:length(mp))
logf <- log(f)
laglogf <- logf[index+1]
diflogf <- laglogf-logf
#plot(diflogf[mi:mf]~mp[mi:mf], type="n")
#text(mp,diflogf,index)
#abline(h=0)
gauco <- lm(diflogf[mi:mf]~mp[mi:mf])
a <- coefficients(gauco)[1]
b <- coefficients(gauco)[2]
sumgc <- summary(gauco)
R2 <- sumgc$r.squared
AdjR2 <- sumgc$adj.r.squared
wd <- mp[2]-mp[1]
mean <- -1*a/b+wd/2
sd <- (wd/(-1*b))^.5
gaup <- wd*(1/(sd*sqrt(2*pi)))*exp(-.5*((mp-mean)/sd)^2)
sumgp <- sum(gaup[mi:mf])
sumf <- sum(f[mi:mf])
n <- round(sumf/sumgp)
cat("R-square = ", round(R2,4), "Adj R-square = ", round(AdjR2,4),"\n")
cat("p-value = ", round(pf(sumgc$fstatistic[1L], sumgc$fstatistic[2L], sumgc$fstatistic[3L], lower.tail = FALSE),4),"\n")
cat("Mean = ", round(mean,4),"\n")
cat("s.d. = ", round(sd, 4),"\n")
cat("Component size = ", as.integer(n),"\n")
gauc <- n*gaup
fmax <- max(f)+(.1*max(f))
plot(f~mp, ylim=c(0,fmax))
lines(f~mp)
lines(spline(gauc~mp, n=200 , method="natural"), col="red")
gc<-data.frame(gauc)
#return(gauc)
}