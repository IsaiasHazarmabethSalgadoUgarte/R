nlvbgfek <- function(ml,age,sex, Linf=500, Kval=1, tcero=0.1){
  
  if(is.numeric(ml)==FALSE | length(ml)<= 1 ){
    stop("First entry is not a vector or/and it is not numeric or/and length <= 1")
  }
  if(is.numeric(age)==FALSE | length(age)<= 1 ){
    stop("Second entry is not a vector or/and it is not numeric or/and length <= 1")
  }
  if(is.numeric(sex)==FALSE | length(sex)<= 1 ){
    stop("First entry is not a vector or/and it is not numeric or/and length <= 1")
  }
  if(is.numeric(Linf)==FALSE | is.numeric(Kval)==FALSE | is.numeric(tcero)==FALSE){
    stop("Start values are not numeric")
  }
  
  st<-Map(rep,list(Li=Linf,K=Kval,t0=tcero),c(2,1,2))
  
  model <- nls(ml ~ Li[sex] * (1 - exp(-K * (age-t0[sex]))),start =st)
  sumMod <- summary(model)
  
  #return(model)
  return(sumMod)
}