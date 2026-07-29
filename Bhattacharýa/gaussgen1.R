#Version 1: 29/01/2021
#Original version:
#11/09/98 by Isaías Hazarmabeth Salgado-Ugarte
#R version by I.H. Salgado-Ugarte & V.M. Saito-Quezada
#Revised 16/03/2025
gaussgen1<-function(midpoivar, mean, sd, freq){
  if(is.numeric(midpoivar)==FALSE | is.numeric(mean)==FALSE | is.numeric(sd)==FALSE | is.numeric(freq)==FALSE){
    stop("syntax: gaussgen midpoivar meanval sdval freqval comgaufre")
  }
  width<-midpoivar[2]-midpoivar[1]
  gauprob<-width*(1/(sd*sqrt(2*pi)))*exp(-.5*((midpoivar-mean)/sd)^2)
  gaucom=freq*gauprob
  return(gaucom)
}