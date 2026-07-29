#First written 16/09/98; Revised 26/10/2001
#Authors: Salgado-Ugarte I.H. & V.M. Saito-Quezada
#This program estimates the Linfinite parameter of the von
#Bertalanffy growth function by means of the Ford-Walford method
#Ford(1933); Walford (1946) using mean length at age values.
#R version assisted by: A. García-Martínez & L. Chacón-Ramírez
#Written 29/04/2020; revised 17/03/2025
#libreria tidyverse,stats,graphics
fordwaln<-function(x,title=NULL,xlab="Size at age (t)",ylab="Size at age t+1",graph=TRUE,legend=FALSE){
#source("c:/Rprogs/bandw.R")
  if(is.numeric(x)==FALSE && length(x)<=1){
    stop("La variable insertada no es numï¿½rica o tiene una longitud igual o menor a 1")
  }
  llagest<-NULL
  msize<-x
  nrow<-length(msize)+2
  n<-1:length(msize)
  lagmsi<-msize[2:length(msize)]
  lmsize<-c(0,msize)
  llagm= lagmsi
  regresion<-lm(llagm~lmsize[2:(nrow-2)])

  if(regresion$coefficients[2]>=1){
    print("The slope of the line of the observed points is >=1")
    break
  }
  for(i in 1:length(lmsize)){
    llagest[i]<-regresion$coefficients[1]+regresion$coefficients[2]*lmsize[i]
  }
  llagest<-c(llagest,regresion$coefficients[1]/(1-regresion$coefficients[2]))
  lmsize<-c(lmsize,regresion$coefficients[1]/(1-regresion$coefficients[2]))
  linf<-lmsize[length(lmsize)]
  kavl=log(regresion$coefficients[2])*-1
  
  cat("Estimation of L_inf and K values by the Ford-Walford Method\n")
  for(i in 1:60){cat("-")}
  cat("\n")
  cat("Intercept =",round(regresion$coefficients[1],4),"slope =",round(regresion$coefficients[2],4),sep = "   ","\n")
  r2<-cor(lmsize[2:(nrow-2)],llagm)^2
  r2Adj<-1-((nrow-3-1)/(nrow-3-1-1))*(1-r2)
  cat("R-square =",round(r2 ,4),"Adj R-square =",round(r2Adj,4),sep = "   ","\n")
  cat("L_inf =",round(linf,4),"k =",round(kavl,4),sep = "         ","\n")
  for(i in 1:60){cat("-")}
  cat("\n")
  if(graph==TRUE){
    if(is.null(title)==TRUE){
      p1<-as.character(round(linf,4))
      p2<-as.character(round(kavl,4))
      title<-paste('Ford Walford graph, L\u221e =',p1,' K =',p2) #modified by IHSU 20/10/2020
    }else{
      if(is.character(title)==FALSE){
        stop("El tï¿½tulo no es tipo caracter")
      }
    }
    #xlab<-"Size at age (t)"
    #ylab<-"Size at age t+1"
    if(is.character(xlab)==FALSE){
      stop("xlab no es tipo caracter")
    }
    if(is.character(ylab)==FALSE){
      stop("ylab no es tipo caracter")
    }
    plot(lmsize,lmsize,main = title,xlab = xlab,ylab = ylab,type="l",col="green")
    lines(lmsize,llagest,col="red")
    points(x=lmsize[2:(nrow-2)],llagm)
    grid()
    if(legend==TRUE){
      legend(x="bottom",legend = c("Recta de los Valores","Recta de las Estimaciones"),fill = c("green","red"))
    }
  }
  
}
