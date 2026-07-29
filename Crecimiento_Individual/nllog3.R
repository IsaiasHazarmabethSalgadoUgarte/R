#Logistic Growth Function Estimator (three parameters)
#R version 10/03/2021 by Salgado-Ugarte, I.H. & V.M. Saito-Quezada
#revised 17/03/2025
nllog3 <- function(ml,age){
  
  if(is.numeric(ml)==FALSE | length(ml)<= 1 ){
    stop("First entry is not a vector or/and it is not numeric or/and length <= 1")
  }
  if(is.numeric(age)==FALSE | length(age)<= 1 ){
    stop("Second entry is not a vector or/and it is not numeric or/and length <= 1")
  }
  #if(is.numeric(sex)==FALSE | length(sex)<= 1 ){
  #  stop("Third entry is not a vector or/and it is not numeric or/and length <= 1")
  #}
  fac<-1.1
  y<-ml
  resumen<-summary(ml)
  mini<-resumen[1]
  maxi<-resumen[6]
  
  if (maxi<0) {
			b <- maxi/fac 
	}else { 
			b <- maxi*fac 
	}
  
	if (mini<0) { 
		  a <- mini*fac 
	}else { 
			a <- mini/fac 
	}
  
  y<-log((y-a)/(b-y))
  regresion<-lm(y~age)
  b2<-regresion$coefficients[2]
  b3<--1*regresion$coefficients[1]/b2
  b1<-b-a
  
  st<-Map(rep,list(Li=b1,K=b2,t0=b3))
  
  model <- nls(ml ~ Li / (1 + exp(-K * (age-t0))),start =st)
  #sumMod <- summary(model)
  
  return(model)
  #return(sumMod)
}

#largo2<-NULL
#edad2<-NULL
#sexo2<-NULL
#bandera<-FALSE
#x<-1
#for (i in 1:length(sexo)) {
#  if(is.na(largo[i]) || is.na(edad[i]) || is.na(sexo[i]) ){
#    bandera<-TRUE
#  }
#  if(bandera==FALSE){
#    largo2[x]<-largo[i]
#    edad2[x]<-edad[i]
#    sexo2[x]<-sexo[i]
#    x<-x+1
#  }
#  bandera<-FALSE
#}