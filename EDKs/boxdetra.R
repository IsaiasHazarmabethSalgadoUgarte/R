#Version 1: 26/02/2013
#Original version:
#13/05/93 by Isaías Hazarmabeth Salgado-Ugarte
#Updated version by I.H. Salgado-Ugarte & V.M. Saito-Quezada
#Revised 16/03/2025
#R version by Vladimir Asiaín Ramírez
#Laboratorio de Biometria y Biologia Pesquera
#Batalla 5 de mayo S/N esq. Fuerte de Loreto
#Ejército de Oriente, Iztapalapa, 09230, CDMX
#Mexico City; e-mail: isalgado@unam.mx
#This program calculates a density trace of a series of ordered 
#values according to the Boxcar Weight function.
#Based on the procedure described in Chambers et al. (1983)
boxdetra<-function(D,B)
{
A<<-sort(D)
nuobs<-length(A)
lowcut<-A-B/2
uppcut<-A+B/2
trace<-0
for(i in 1:nuobs)trace[i]<-sum(A>=lowcut[i] & A<uppcut[i])/(B*nuobs)
trace->>dtrace
plot(A,dtrace, type="s")
}
