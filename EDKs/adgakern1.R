#Version 1: 26/02/2013
#Original version:
#20/05/93 by Isaías Hazarmabeth Salgado-Ugarte
#Updated version by I.H. Salgado-Ugarte & V.M. Saito-Quezada
#R version by Vladimir Asiaín Ramírez
#Laboratorio de Biometria y Biologia Pesquera
#Batalla 5 de mayo S/N esq. Fuerte de Loreto
#Ejército de Oriente, Iztapalapa, 09230, CDMX
#Mexico City; e-mail: isalgado@unam.mx
#This program calculates an adaptive kernel density estimator of a  
#series of values according to the gaussian weight function and the
#adaptive the procedure described in Fox, (1990) and Silverman(1986)
adgakern1<-function(D,B)
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
z2<-data.frame(A)
for(i in 1:nuobs)z2[i]<-(A[i]-A1)/(winfac*B)
z3<-z2<2.5 & z2>-2.5
kz2<-(1/(sqrt(2*pi)))*exp(-.5*(z2*z3)^2)
sums2<-data.frame(A)
for(i in 1:nuobs)sums2[i]<-cumsum((kz2[,i]*z3[,i])/winfac)
fkx2<-(1/(nuobs*B))*sums2[nuobs,]
dtrace<<-as.numeric(fkx2)
plot(A,dtrace,type="o")
}