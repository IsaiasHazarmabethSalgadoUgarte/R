#Richards' Median mature size calculator
#R version 14/07/2021 by Salgado-Ugarte, I.H. & V.M. Saito-Quezada
#revised 18/07/2021; 17/03/2025
#This routine permits to calculate the median size of mature
#individuals providing the parameter estimates (L, r, and s)
#from the Richards Function (four parameters) "ricf"

ricmL50 <-function(L,r,s) {
   t1 <- .5^(1-s)
   t2 <- t1-1
   t3 <- t2/(1-s)
   t4 <- log(-t3)/-r
   Lm <- t4+L
   return(Lm)
}