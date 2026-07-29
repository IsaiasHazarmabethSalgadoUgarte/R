#Version 1: 03/10/2020
#Original version:
#11/09/98 by Isaías Hazarmabeth Salgado-Ugarte
#R version by I.H. Salgado-Ugarte & V.M. Saito-Quezada
#Revised 16/03/2025
#Laboratorio de Biometria y Biologia Pesquera
#Batalla 5 de mayo S/N esq. Fuerte de Loreto
#Ejército de Oriente, Iztapalapa, 09230, CDMX
#Mexico City; e-mail: isalgado@unam.mx
#As the Stata version, this program calculates logaritmic differences and draw the Bhathacharya's
#plot using the observation's numbers as plotting symbols in order to 
#define the points defining negatively sloped lines, which represent
#individual gaussian components.
#This routine is a based on the new version integrating two previous simple Stata programs
#diflogen.ado and bhatplot.ado.
bhattaplt <- function(f, mp,mi=1,mf=length(mp)) {
source("c:/Rprogs/bhattaplt.R")
index <- c(1:length(mp))
logf <- log(f)
laglogf <- logf[index+1]
diflogf <- laglogf-logf
plot(diflogf[mi:mf]~mp[mi:mf], type="n")
text(mp,diflogf,index)
abline(h=0)
}