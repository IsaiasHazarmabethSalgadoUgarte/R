#Function for Monte Carlo (Stochastic) Simulation of 
#population growth in a limited environment
#Version 1: 27/06/2023
#Authors: Isaías H. Salgado-Ugarte y V. Mitsui Saito-Quezada
#Laboratorio de Biometría y Biología Pesquera
#FES Zaragoza UNAM
#Based on Poole (1974) and Chiappa-Carrara, et al. (2009)
logistoc <- function(N, b, d, a1, a2, rep, seed) {
   	set.seed(seed)
	rn <- runif(rep)
	nt <- numeric(rep)
	for (i in 1:rep) {
		part1 <- b*N-a1*N^2
   		part2 <- (b + d)*N - (a1-a2)*N^2
   		Pr <- part1/part2
		  	if (Pr > rn[i]) {
    			N <- N + 1 } else
			{N <- N - 1}
		nt[i] <- N }		
t <- 1:rep
plot(t,nt)
return(nt)
}