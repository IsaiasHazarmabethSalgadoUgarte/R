#Seasonal von Bertalanffy growth function for ages in weeks
#with two cycles according to the proposal by Haddon (2001)
#Authors: Isaías H. Salgado-Ugarte & V. Mitsui Saito-Quezada
#R version by Leonardo Chacón Ramírez & Alan García Martínez
#Date: 08/02/2021; update: 30/03/2024
#Revised 17/03/2025
nlsvbff2 <- function(ml,age,at1,at2,at3,at4,at5,at6,at7,at8,retM=FALSE){
  
  if(is.numeric(ml)==FALSE | length(ml)<= 1 ){
    stop("First entry is not a vector or/and it is not numeric or/and length <= 1")
  }
  if(is.numeric(age)==FALSE | length(age)<= 1 ){
    stop("Second entry is not a vector or/and it is not numeric or/and length <= 1")
  }
  if(is.numeric(at1)==FALSE | is.numeric(at2)==FALSE | is.numeric(at3)==FALSE | is.numeric(at4)==FALSE | is.numeric(at5)==FALSE | is.numeric(at6)==FALSE | is.numeric(at7)==FALSE | is.numeric(at8)==FALSE){
    stop("3-10 entrys are not numeric")
  }
  if(is.logical(retM)==FALSE){
    stop("eleventh entry are not logical")
  }
  Li = at1
  K = at2
  t0 = at3
  C1 = at4
  s1 = at5
  C2 = at6
  s2 = at7
  p = at8
  
  st<-list(Li=Li,K=K,t0=t0,C1=C1,s1=s1,C2=C2,s2=s2,p=p)
  
  model <- nls(ml ~ Li * (1 - exp(-1 * ((C1*sin(2*pi*(age-s1)/52))+(C2*sin(2*pi*(age-s2)/p))+(K*(age-t0))))),start = st)
  #model <- nls(ml ~ Li * (1 - exp(-K * (age-t0))),start =st)
  sumMod <- summary(model)
  if(retM==TRUE){
    return(model)
  }else{
    return(sumMod)
  }
}