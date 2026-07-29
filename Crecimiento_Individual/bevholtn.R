#Version 1: 07/11/2020
#Original version:
#11/09/98 by Isaías Hazarmabeth Salgado-Ugarte
#Updated version by I.H. Salgado-Ugarte & V.M. Saito-Quezada
#R version by Leonardo Chacón Ramírez, 28/08/2021
#Revised 17/03/2025
#Laboratorio de Biometria y Biologia Pesquera
#Batalla 5 de mayo S/N esq. Fuerte de Loreto
#Ejército de Oriente, Iztapalapa, 09230, CDMX
#Mexico City; e-mail: isalgado@unam.mx
#This program estimates K and t_0 (for a given L_inf) parameters of the 
#von Bertalanffy growth function using mean size (length) at age data 
#according to the procedure proposed by Beverton and Holt (1957). This ado 
#file draws the ln(L_inf - l_t) - age graph and provides a table with the
#estimated K and t_0 parameters in addition to the estimated von 
#Bertalanffy growth function.

bevholtn<-function(msize,agevar,linf,title=NULL,xlab="Age",ylab="Ln(L_\u221e - l_t)",graph=TRUE,legend=FALSE){
source("c:/Rprogs/bevholtn.R")
  if(length(msize)!=length(agevar)){
    stop("msize and agevar are not the same size")
  }
  if(length(msize)==0){
    stop("msize argument equals zero")
  }
  liv<-linf
  if(liv==0){
    stop("you must provide the Linfinite value")
  }
  touse<-rep(1,length(msize))
  options(warn=-1)
  difvar=log(liv-msize)
  options(warn=0)
  a<-1
  indvar<-NULL
  depvar<-NULL
  for (i in 1:length(msize)) {
    if(is.na(difvar[i])==FALSE && is.infinite(difvar[i])==FALSE){
      indvar[a]<-agevar[i]
      depvar[a]<-difvar[i]
      a<-a+1
    }
  }
  regresion<-lm(depvar~indvar)
  difest<-NULL
  for (i in 1:length(agevar)) {
    difest[i]<-regresion$coefficients[1]+regresion$coefficients[2]*agevar[i]
  }
  b<-regresion$coefficients[2]
  a<-regresion$coefficients[1]
  if(b>=0){
    stop("The slope of the line of the observed points is >=0")
  }
  kval=b*-1
  tzero= (a-log(liv))/kval
  
  cat("Estimation of K and t_0 values by the Beverton-Holt Method\n")
  for(i in 1:60){cat("-")}
  cat("\n")
  cat("Intercept =",round(a,4),"Slope =",round(b,4),sep = "   ","\n")
  r2<-cor(indvar,depvar)^2
  r2Adj<-1-((length(indvar)-1)/(length(indvar)-1-1))*(1-r2)
  cat("R-square =",round(r2 ,4),"Adj R-square =",round(r2Adj,4),sep = "   ","\n")
  cat("K         =",round(kval,4),"    ","t_0          =",round(tzero,4),"\n",sep ="   ")
  for(i in 1:60){cat("-")}
  cat("\n")
  cat("Estimated von Bertalanffy Growth Function\n")
  for(i in 1:60){cat("-")}
  cat("\n")
  
  if( tzero<0){
    tzplus=tzero*-1
    cat("l_t = ",round(liv,4),"* (1-exp(",round(b,4),"*(t +",round(tzplus,4),")))","\n")
  }else{
    cat("l_t = ",round(liv,4),"* (1-exp(",round(b,4),"*(t -",round(tzero,4),")))","\n")
  }
  for(i in 1:60){cat("-")}
  cat("\n")
  
  if(graph==TRUE){
    if(is.null(title)==TRUE){
      kvlab<-as.character(round(kval,4))
      tzlab<-as.character(round(tzero,4))#########
      title<-paste('Beverton-Holt graph, K = ',kvlab,', t_0 = ',tzlab)
    }else{
      if(is.character(title)==FALSE){
        stop("The title is not a string")
      }
    }
    if(is.character(xlab)==FALSE){
      stop("xlab is not a string")
    }
    if(is.character(ylab)==FALSE){
      stop("ylab is not a string")
    }
    plot(agevar,difest,main = title,type = "l",ylab = ylab,xlab = xlab)
    points(difvar)
    grid()
    if(legend==TRUE){
      legend(x="bottom",legend = c("Estimated line","Observed points"),pch =c(15,1))
    }
  }
  l<-liv*(1-exp(b*(agevar-tzero)))
  #return(l)
}