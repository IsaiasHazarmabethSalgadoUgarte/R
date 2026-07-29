#Seasonal von Bertalanffy growth function according to
#Pitcher & MacDonald (1973); Pauly & Gaschütz (1979); Haddon (2001)
#Authors: Isaías H. Salgado-Ugarte & V. Mitsui Saito-Quezada
#R version by Leonardo Chacón Ramírez & Alan García Martínez
#Date: 15/02/2021; update: 30/03/2024
#Revised 17/03/2025
nlsvbffm<- function(ml,age,at1,at2,at3,at4,at5){
  
  if(is.numeric(ml)==FALSE | length(ml)<= 1 ){
    stop("First entry is not a vector or/and it is not numeric or/and length <= 1")
  }
  if(is.numeric(age)==FALSE | length(age)<= 1 ){
    stop("Second entry is not a vector or/and it is not numeric or/and length <= 1")
  }
  if(is.numeric(at1)==FALSE | is.numeric(at2)==FALSE | is.numeric(at3)==FALSE | is.numeric(at4)==FALSE | is.numeric(at5)==FALSE){
    stop("3-7 entrys are not numeric")
  }
  Li = at1
  K = at2
  t0 = at3
  C = at4
  s = at5
  
  st<-list(Li=Li,K=K,t0=t0,C=C,s=s)
  
  model <- nls(ml ~ Li * (1 - exp(-1 * ((C*sin(2*pi*(age-s)/12))+(K*(age-t0))))),start = st)
  #model <- nls(ml ~ Li * (1 - exp(-K * (age-t0))),start =st)
  sumMod <- summary(model)
  
  #return(model)
  return(sumMod)
}