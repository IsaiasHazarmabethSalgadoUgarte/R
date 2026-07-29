#Version 1: 10/04/2021
#Original version:
#20/05/93 by Isaías Hazarmabeth Salgado-Ugarte
#R Version adapted from kerngaus1.1 by Vladimir Asiaín Ramírez
#Updated version by I.H. Salgado-Ugarte & V.M. Saito-Quezada
#Revised 16/03/2025
#Laboratorio de Biometría y Biología Pesquera
#Batalla 5 de mayo S/N esq. Fuerte de Loreto
#Ejército de Oriente, Iztapalapa, 09230, CDMX
#Mexico City; e-mail: isalgado@unam.mx
#This program calculates the variable bandwidth kernel density estimator of a  
#series of values according to the gaussian weight function and the
#adaptive procedure described in Fox (1990) modified from Silverman(1986) but
#only uses an uniformly spaced number of points (50 by default) to finish 
#graphing the results
varwike2<-function(D,B,C=50)
{
A<<-D
nuobs<-length(A)
A1<-as.data.frame(as.matrix(A))
z<-data.frame(A)
for(i in 1:nuobs)z[i]<-(A[i]-A1)/B
z1<-z<2.5 & z>-2.5
kz<-(1/(sqrt(2*pi)))*exp(-.5*(z*z1)^2)
sums<-data.frame(A)
for(i in 1:nuobs)sums[i]<-cumsum(kz[,i]*z1[,i])
fkx<-(1/(nuobs*B))*sums[nuobs,]
fkx1<-as.numeric(fkx)
lnfkx<-log(fkx1)
lnfg<-mean(lnfkx)
fg<-exp(lnfg)
winfac<-sqrt(fg/fkx1)

maxval<-max(A)+B+(max(A)-min(A))*.1
minval<-min(A)-B-(max(A)-min(A))*.1
range<-maxval-minval
inter<-range/(C-1)
midv<-cumsum(rep(inter,C))+minval+inter/2

z2<-data.frame(A)

for(i in 1:C)z2[i]<-(midv[i]-A1)/(winfac*B)

z3<-z2<2.5 & z2>-2.5
kz2<-(1/(sqrt(2*pi)))*exp(-.5*(z2*z3)^2)

sums2<-data.frame(A)

for(i in 1:C)sums2[i]<-cumsum((kz2[,i]*z3[,i])/winfac)
fkx2<-(1/(nuobs*B))*sums2[nuobs,]
den<<-as.numeric(fkx2)
midp<<-midv[1:C]
plot(midp,den,type="l")
vwr<-data.frame(midp,den)
}