#Version 1: 30/07/2018
#Original version (Stata):
#29/10/92 by Isaías Hazarmabeth Salgado-Ugarte
#Updated version by I.H. Salgado-Ugarte & V.M. Saito-Quezada
#04/03/2021
#Revised 18/03/2025
#Laboratorio de Biometria y Biologia Pesquera
#Batalla 5 de mayo S/N esq. Fuerte de Loreto
#Ejército de Oriente, Iztapalapa, 09230, CDMX
#Mexico City; e-mail: isalgado@unam.mx
s4253eht <- function (y)
{
    h <- function(y) {
          N <- length(y)
          z <- NULL
          # medians of 4 (copy and medians of 2 rules at ends)
          z[1] <- y[1]
          z[2] <- median(c(y[1],y[2]))
          for (i in 3:(N)){ z[i] <- median(c(y[i - 2], y[i - 1], y[i], y[i+1]))}
          z[N] <- median(c(y[N - 1], y[N]))
          # medians of 2 (copy rule at ends)
          z1 <- NULL
          for (i in 2:N) { z1[i] <- (z[i]+z[i+1])/2}
          z1[1] <- y[1]
          z1[N] <- y[N]
          # medians of 5 (copy and medians of 3 rules at ends)
          z2 <- NULL
          for (i in 3:(N-2)) { z2[i] <- median(c(z1[i-2], z1[i-1], z1[i], z1[i+1], z1[i+2]))}
          z2[2] <- median(c(z1[1],z1[2],z1[3])) 
          z2[1] <- y[1]
          z2[N-1] <- median(c(z1[N],z1[N-1],z1[N-2]))
          z2[N] <- y[N]
          # medians of 3 with Endpoints rule
          z3 <- NULL
          for (i in 2:(N-1)) {z3[i] <- median(c(z2[i-1], z2[i], z2[i+1]))}
          z3[1] <- median(c(y[1],z2[2],3*z2[2]-2*z2[3]))
          z3[N] <- median(c(y[N],z2[N-1],3*z2[N-1]-2*z2[N-2]))
          # Hanning
          z4 <- NULL
          z4[1] <- z3[1]
          z4[N] <- z3[N]
          for (i in 2:(N-1)) {z4[i] <- (1/4)*z3[i-1]+(1/2)*z3[i]+(1/4)*z3[i+1]}
          return(z4)
}
sm <- h(y)
rf <- (y - sm)
sm.rf <- h(rf)
smooth <- (sm.rf + sm)
return(smooth)
}

