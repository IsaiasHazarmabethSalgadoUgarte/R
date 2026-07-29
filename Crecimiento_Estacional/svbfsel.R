#Seasonal von Bertalanffy growth function selector
#
#R version 14/07/2021 by Salgado-Ugarte, I.H. & V.M. Saito-Quezada
#revised 18/07/2021; 26/07/2021; 04/10/2021; 30/03/2024; 17/03/2025
#Pauly and Gaschutz function added 04/11/2025

#This routine permits to select individual seasonal growth functions:
#- Seasonal von Bertalanffy Growth Function (Pauly & Gaschutz, 1979) "svbpg"
#- Seasonal von Bertalanffy Growth Function (weeks) "svbw"
#- Seasonal von Bertalanffy Growth Function (months) "svbm"
#- Seasonal von Bertalanffy Growth Function (years) "svby"
#- Seasonal von Bertalanffy Growth Function two cycles (weeks) "svbw2"
#- Seasonal von Bertalanffy Growth Function Somers (weeks) "svbs"
#- Seasonal von Bertalanffy Growth Function Somers with WP adjustment "svbwp"


svbfsel <- function(f) {

if(f == "svbpg") {
   function(t,L,k,t0,C,ts) L*(1-exp(-(k*(t-t0)+C*2*k/pi*sin(2*pi*(t-ts)))))
   } else

if(f == "svbw") {
   function(t,L,k,t0,C,s) L*(1-exp(-1*(C*sin(2*pi*(t-s)/52)+k*(t-t0))))
   } else

if(f == "svbm") {
   function(t,L,k,t0,C,s) L*(1-exp(-1*(C*sin(2*pi*(t-s)/12)+k*(t-t0))))
   } else

if(f == "svby") {
   function(t,L,k,t0,C,s) L*(1-exp(-1*(C*sin(2*pi*(t-s)/1)+k*(t-t0))))
   } else

if(f == "svbw2") {
   function(t,L,k,t0,C1,s1,C2,s2,p) L*( 1-exp( -1*( C1*sin(2*pi*(t-s1)/52) + C2*sin(2*pi*(t-s2)/p) + k*(t-t0) ) ) )
   }	else

if(f == "svbs") {
   function(t,L,k,t0,C,ts) L*(1-exp(-k*(t-t0) - (C*k)/(2*pi)*sin(2*pi*(t-ts)) + (C*k)/(2*pi)*sin(2*pi*(t0-ts)) ))
   } else

if(f== "svbwp") {
   function(t,L,k,t0,C,WP) L*(1-exp(-k*(t-t0) - (C*k)/(2*pi)*sin(2*pi*(t-WP+.5)) + (C*k)/(2*pi)*sin(2*pi*(t0-WP+.5)) ))
   }
}