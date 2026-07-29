#Version 1: 26/02/2013
#Original version:
#12/07/93 by Isaías Hazarmabeth Salgado-Ugarte
#Updated version by I.H. Salgado-Ugarte & V.M. Saito-Quezada
#Revised 16/03/2025
#R version by Vladimir Asiaín Ramírez
#Laboratorio de Biometria y Biologia Pesquera
#Batalla 5 de mayo S/N esq. Fuerte de Loreto
#Ejército de Oriente, Iztapalapa, 09230, CDMX
#Mexico City; e-mail: isalgado@unam.mx
#This program calculates a density trace of a  
#series of values according to the Cosine Weight function.
#Based on the procedure described in Chambers, et al. (1983)
#considering 50 equally spaced points
cosdetra <- function(D,B)
{
A<<-sort(D)
nuobs<-length(A)
maxval<-max(A)
minval<-min(A)
range<-maxval-minval
inter<-range/49
midval<-cumsum(rep(inter,nuobs))+minval-inter
A1<-as.data.frame(as.matrix(A))
u<-data.frame(A)
for(i in 1:50)u[i]<-(midval[i]-A1)/B
u1<-u<0.5 & u>-0.5
wu<-1+cos(2*pi*(u*u1))
sums<-cumsum(wu*u1)
fwy<-(1/(nuobs*B))*sums[nuobs,]
dtrace<<-as.numeric(fwy)
midpt<<-midval[1:50]
plot(midpt,dtrace,type="o")
}