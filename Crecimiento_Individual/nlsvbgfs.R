#Seasonal von Bertalanffy growth function according to
#Somers, 1988 and with annotations from García-Berthou et al. 2012
#Authors: Isaías H. Salgado-Ugarte & V. Mitsui Saito-Quezada
#R version by Leonardo Chacón Ramírez & Alan García Martínez
#Date: 17/02/2021; update: 30/03/2024
#Revised 17/03/2025
nlsvbgfs<- function(ml,age,at1,at2,at3,at4,at5){
  
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
  ts = at5
  
  st<-list(Li=Li,K=K,t0=t0,C=C,ts=ts)
  
  model <- nls(ml~Li*(1 - exp(-K*(age-t0)-(C*K)/(2*pi)*sin(2*pi*(age-ts))+(C*K)/(2*pi)*sin(2*pi*(t0-ts)) )),start = st)
 return(model)
}