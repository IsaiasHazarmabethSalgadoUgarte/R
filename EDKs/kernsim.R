#Version 1: 26/02/2013
#Original version:
#20/05/93 by Isaías Hazarmabeth Salgado-Ugarte
#Updated version by I.H. Salgado-Ugarte & V.M. Saito-Quezada
#R version by Vladimir Asiaín Ramírez
#Laboratorio de Biometria y Biologia Pesquera
#Batalla 5 de mayo S/N esq. Fuerte de Loreto
#Ejército de Oriente, Iztapalapa, 09230, CDMX
#Mexico City; e-mail: isalgado@unam.mx
#This program calculates a simple density estimator of a  
#series of values according to a rectangular weight function.
#Based on the procedure described in Fox, (1990)
#but considering only 50 equally spaced points
#last correction 2/05/2011 by IHSU
kernsim<-function(D,B)
{
A<<-D
nuobs<-length(A)
maxval<-max(A)+B
minval<-min(A)-B
range<-maxval-minval
inter<-range/49
midval<-cumsum(rep(inter,nuobs))+minval-3*inter/2
A1<-as.data.frame(as.matrix(A))
z<-data.frame(A)
for(i in 1:50)z[i]<-(midval[i]-A1)/B
z1<-z<1 & z>-1
sums<-data.frame(A)
for(i in 1:50)sums[i]<-cumsum(0.5*z1[,i])
fkx<-(1/(nuobs*B))*sums[nuobs,]
dtrace<<-as.numeric(fkx)
midpt<<-midval[1:50]
plot(midpt,dtrace,type="o")
}