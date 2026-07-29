#Individual Growth Function Selector
#R version 14/07/2021 by Salgado-Ugarte, I.H. & V.M. Saito-Quezada
#revised 18/07/2021; augmented 17/03/2025
#Growth function selector
#This routine permits to select individual growth functions:
#- von Bertalanffy Growth Function "vbgf"
#- Logistic Function (three parameters) "logf"
#- Gompertz Function (three parameters) "gomf"
#- Richards Function (four parameters) "ricf"
#- "Linear" von Bertalanffy Growth Function "lvb"

gfsel <- function(f) {

if(f=="vbgf") {
   vbgf <- function(t,L,k,t0) L*(1-exp(-k*(t-t0)))
   } else
if(f=="logf") {
   logf <- function(t,L,k,t0) L*(1+exp(-k*(t-t0)))^(-1)
   } else
if(f=="gomf") {
   gomf <- function(t,L,k,t0) L*exp(-exp(-k*(t-t0)))
   } else
if(f=="ricf") {
   ricf <- function(t,L,k,t0,m) L*(1-exp(-k*(t-t0)))^m
   } else
if(f=="lvb") {
   lvb <- function(t,b0,b1,k,t0) (b0+b1*t)*(1-exp(-k*(t-t0)))
   }
}