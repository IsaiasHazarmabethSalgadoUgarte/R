#Version 1: 26/02/2013
#Original version:
#19/05/93 by Isaías Hazarmabeth Salgado-Ugarte
#Updated version by I.H. Salgado-Ugarte & V.M. Saito-Quezada
#R version by Vladimir Asiaín Ramírez
#Revised 16/03/2025
#Laboratorio de Biometria y Biologia Pesquera
#Batalla 5 de mayo S/N esq. Fuerte de Loreto
#Ejército de Oriente, Iztapalapa, 09230, CDMX
#Mexico City; e-mail: isalgado@unam.mx
#This program calculates a simple density estimator of a  
#series of values according to Epanechnikov's weight function.
#It is based on the procedure described in Fox, (1990)
#but considering only 50 equally spaced points
#as suggested by Chambers, et al. (1983)
kernepa<-function(D,B)
{
A<<-D
nuobs<-length(A)
maxval<-max(A)+B
minval<-min(A)-B
range<-maxval-minval
inter<-range/50
midval<-cumsum(rep(inter,50))+minval+inter/2
A1<-as.data.frame(as.matrix(A))
z<-data.frame(A)
for(i in 1:50)z[i]<-(midval[i]-A1)/B
z1<-z<=sqrt(5) & z>=-sqrt(5)
kz<-(3/(4*sqrt(5)))*(1-((z*z1)^2/5))
sums<-data.frame(A)
for(i in 1:50)sums[i]<-cumsum(kz[,i]*z1[,i])
fkx<-(1/(nuobs*B))*sums[nuobs,]
dtrace<<-as.numeric(fkx)
midpt<<-midval[1:50]
plot(midpt,dtrace,type="o")
}