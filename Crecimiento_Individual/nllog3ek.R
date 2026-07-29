nllog3ek<- function(ml,age,sex){
  
  if(is.numeric(ml)==FALSE | length(ml)<= 1 ){
    stop("First entry is not a vector or/and it is not numeric or/and length <= 1")
  }
  if(is.numeric(age)==FALSE | length(age)<= 1 ){
    stop("Second entry is not a vector or/and it is not numeric or/and length <= 1")
  }
  if(is.numeric(sex)==FALSE | length(sex)<= 1 ){
    stop("Third entry is not a vector or/and it is not numeric or/and length <= 1")
  }
  
  
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
  
  st<-Map(rep,list(Li=b1,K=b2,t0=b3),c(2,1,2))
  
  model <- nls(ml ~ Li[sex] / (1 + exp(-K * (age-t0[sex]))),start =st)
  #sumMod <- summary(model)
  
  return(model)
  #return(sumMod)
}