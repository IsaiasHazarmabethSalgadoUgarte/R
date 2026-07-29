vbgfno <- function(t,L1,k1,t01,L2,k2,t02,g) {
if(g==1) {
L1*(1-exp(-k1*(t - t01))) } else

{L2*(1-exp(-k2*(t - t02)))} 
}