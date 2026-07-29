#Version 1.0; First written: 30/07/2018; 
#Revised: 04/07/2020; 18/03/2025
#Authors: I.H. Salgado-Ugarte & V.M. Saito-Quezada
s4253ehtb <- function (y)
{
     h <- function(y) {
          source("c:/Rprogs/s4253ehtb.R")
          N <- length(y)
          # medians of 4 (copy and medians of 2 rules for endvalues)
          zsum <- NULL
          for (i in 3:N){ zsum[i] <- c(y[i-2]+y[i-1]+y[i]+y[i+1])}
          zmax <- NULL
          for (i in 3:N){ zmax[i] <- max(c(y[i-2],y[i-1],y[i],y[i+1]))}
          zmin <- NULL
          for (i in 3:N){ zmin[i] <- min(c(y[i-2],y[i-1],y[i],y[i+1]))}
          zval <- NULL
          for (i in 3:N){ zval[i] <- (zsum[i] - zmax[i] - zmin[i])/2}
          zval[1] <- y[1]
          zval[2] <- (zval[1]+y[2])/2
          zval[N] <- (y[N-1]+y[N])/2
          # medians of 2 (copy rule for endvalues)
          z2 <- NULL
          for (i in 2:N) { z2[i] <- (zval[i]+zval[i+1])/2 }
          z2[1] <- y[1]
          z2[N] <- y[N]
          # medians of 5 (medians of 3 and copy rules for endvalues)
          z5 <- NULL
          for (i in 3:N) { z5[i] <- median(c(z2[i-2],z2[i-1],z2[i],z2[i+1],z2[i+2]))}
          z5sum <- NULL
          for (i in 2:N){ z5sum[i] <- c(z2[i-1]+z2[i]+z2[i+1])}
          z5max <- NULL
          for (i in 2:N){ z5max[i] <- max(c(z2[i-1],z2[i],z2[i+1]))}
          z5min <- NULL
          for (i in 2:N){ z5min[i] <- min(c(z2[i-1],z2[i],z2[i+1]))}
          z5[2] <- z5sum[2]-z5max[2]-z5min[2]
          z5[N-1] <- z5sum[N-1]-z5max[N-1]-z5min[N-1]
          z5[1] <- y[1]
          z5[N] <- y[N]
          # medians of 3
          z3sum <- NULL
          for (i in 2:N) z3sum[i] <- {z5[i-1]+z5[i]+z5[i+1]}
          z3max <- NULL
          for (i in 2:N){z3max[i] <- max(c(z5[i-1],z5[i],z5[i+1]))}
          z3min <- NULL
          for (i in 2:N){z3min[i] <- min(c(z5[i-1],z5[i],z5[i+1]))}
          z3 <- NULL
          for (i in 2:N){z3[i] <- z3sum[i]-z3max[i]-z3min[i]}
          # Endpoint rule
          tempo1 <- sort(c(y[1],z3[2],3*z3[2]-2*z3[3]))          
          z3[1] <- tempo1[2]
          tempo2 <- sort(c(y[N],z3[N-1],3*z3[N-1]-2*z3[N-2]))
          z3[N] <- tempo2[2]
          # Hanning
          zh <- NULL
          for (i in 2:N) { zh[i] <- (1/4)*z3[i-1]+(1/2)*z3[i]+(1/4)*z3[i+1]}
          zh[1] <- z3[1]
          zh[N] <- z3[N]
          return(zh)
}
sm <- h(y)
rf <- (y - sm)
sm.rf <- h(rf)
smooth <- (sm.rf + sm)
return(smooth)
}
