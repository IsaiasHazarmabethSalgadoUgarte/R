#Version 1.0; First written: 16/03/2022
#Revised: 16/03/2025
#Authors: I.H. Salgado-Ugarte & V.M. Saito-Quezada
#R version by A. García-Martínez & L. Chacón-Ramírez
nlpfun <- function(y, x, a=1, b=1){ #"ye,equis"
  
  if(is.numeric(y)==FALSE | length(y)<= 1 ){
    stop("First entry is not a vector or/and it is not numeric or/and length <= 1")
  }
  if(is.numeric(x)==FALSE | length(x)<= 1 ){
    stop("Second entry is not a vector or/and it is not numeric or/and length <= 1")
  }
  if(is.numeric(a)==FALSE | is.numeric(b)==FALSE){
    stop("Start values are not numeric")
  }
  
  st<-list(a=a,b=b)
  
  model <- nls(y ~ a*x^b, start = st)
  #sumMod <- summary(model)
  
  return(model)
  #return(sumMod)
}