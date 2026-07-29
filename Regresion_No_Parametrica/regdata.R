regdata<-function(n,seed) {
   source("c:/Rprogs/regdata.R")
   #.Random.seed<<-seed
   set.seed(seed)
   x<-sort(runif(n))
   m<-(sin(2*pi*x^3))^3
   e<-rnorm(n)*sqrt(0.1)
   y<-m+e
 result<-list(x=x,y=y,m=m,e=e)
 result
}
