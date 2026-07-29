#Maturity median size function selector
#R version 14/07/2021 by Salgado-Ugarte, I.H. & V.M. Saito-Quezada
#Revised 18/07/2021; 06/11/2021; 17/03/2025
#Maturity median size function selector
#This routine permits to select non linear functions to estimate
#the median maturity size (L_50):
#- Logistic Function (two parameters) (Lysack, 1980)   "log2f"
#- Brower & Griffiths (2005) Function (two parameters) "b_g2f"
#- Gompertz (1825) Function (two parameters) "gom2f"
#- White (2002) Function (two parameters) "white"
#- Richards (1959) Function (three parameters) "ricf"

mmsfsel <- function(f) {

if(f=="log2f") {
   log2f <- function(x,r,Lm) 1/(1+exp(-r*(x-Lm)))
   } else
if(f=="b_g2f") {
   b_g2f <- function(x,r,Lm) 1/(1+exp(-(x-Lm)/r))
   } else
if(f=="gom2f") {
   gom2f <- function(x,r,L) exp(-exp(-r*(x-L)))
   } else
if(f=="white") {
   whitef <- function(x,r,Lm) 1/(1+exp(-log(19)*((x-Lm)/(r-Lm))))
   } else
if(f=="ricf") {
   function(x,s,r,L) (1-(1-s)*exp(-r*(x-L)))^(1/(1-s))
   }
}