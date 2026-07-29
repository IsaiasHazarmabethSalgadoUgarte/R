#This program estimate the logistic function with two parameters.
#It can be used to estimate size at age or median maturity size
#R version 04/03/2021 by Salgado-Ugarte, I.H. & V.M. Saito-Quezada
#Revised  17/03/2025

nllog2 <- function(ml,age, r=1, Lm=14){ #"ye,equis"
  
  if(is.numeric(ml)==FALSE | length(ml)<= 1 ){
    stop("First entry is not a vector or/and it is not numeric or/and length <= 1")
  }
  if(is.numeric(age)==FALSE | length(age)<= 1 ){
    stop("Second entry is not a vector or/and it is not numeric or/and length <= 1")
  }
  if(is.numeric(r)==FALSE | is.numeric(Lm)==FALSE){
    stop("Start values are not numeric")
  }
  
  st<-list(r=r,Lm=Lm)
  
  model <- nls(age ~ 1/(1 + exp(-r*(ml-Lm))),start =st)
  #sumMod <- summary(model)
  
  return(model)
  #return(sumMod)
}