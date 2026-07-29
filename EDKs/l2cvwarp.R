l2cvwarp <- function (y, delta, kercode, mstart, mend)
{
	# Initial Calculations
	ic <- function(y) {  
		N <- length(y)
		mnum <- mend-mstart+1
		maxval <- max(y)
		minval <- min(y)
		if(kercode=="6") {delta=delta*4}
		start <- minval-delta*(mend+0.1)
		origin <- (floor(start/delta)-0.5)*delta
		numbin <- ceiling((maxval+delta*(mend+0.1)-origin)/delta)+1
		maxnoemp <- min(N+1,numbin)

	}
	# Binning Data
	bd <- function(y) {
		for (i in 1:N) { indexi <- floor(y[i]-origin)/delta
			if(bin[indexi]==0) {nl <- 1
				frequ[nl] <- indexi
				index[nl] <- nl} else
			frequ[bin[indexi]] <- frequ[bin[indexi]]+1
		}

	# Main Loop
	for (M in mstart:mend) {
		h <- M*delta
		intsquare <- 0
		estexp <- 0
		for (z in 1:numbin) {fm[z] <- 0}
		
		if(kercode==1) {
			cm <- M/((2*M-1)*(N*h)
			for (L in 0:M-1) {kwe[L] <- cm}
		} else
		
		if(kercode==2) {
			cm <- 1/N*h
			for (L in 0:M-1) {kwe[L] <- cm*(1-L/M)}
		} else

		if(kercode==3) {
			cm <- 3*M^2/((4*M^2-1)*(N*h))
			for (L in 0:M-1) {kwe[L] <- cm*(1-(L/M)^2)}
		} else

		if(kercode==4) {
			cm <- 0.9375/((1-0.0625/M^4)*(N*h))
			for (L in 0:M-1) {kwe[L] <- cm*(1-(L/M)^2)^2}
		} else

		if(kercode==5) {
			part1 <- 1+0.14583333/M^4
			part2 <- 0.052083333/M^6
			cm <- 1.09375/((part1-part2)*(N*h)
			for (L in 0:M-1) {kwe[L] <- cm*(1-(L/M)^2)^3}
		} else

		if(kercode==6) {
			cm <- 0.3989*4/(N*h)
			for (L in 0:M-1) {kwe[L] <- cm*(exp(-8*(L/M)^2))}
		}
	# Weight Bins
		
		for (kw in 0:nl-1) {
			for (lw in 1-M:M-1) {
		fm[index[kw]+lw <- fm[index[kw]+lw]+kwe[abs(lw)]*frequ[kw]
			}
		}

		for (kl in 1:nl) {estexp <- estexp+frequ[kl]*fm[index[kl]]}
		estexp <- estexp-kwe[0]*N
		estexp <- estexp/(n-1)
		for (z in 1:numbin) {insquare <- intsquare+fm[z]^2}
		intsquare <- intsquare*delta
		cv[M-mstart+1] <- intsquare-2*estexp
		mv[M-mstart+1] <- M
		if(kercode==6) {
			hv[M-mstart+1] <- mv[M-mstart+1]*delta/4
		} else
		hv[M-mstart+1] <- mv[M-mstart+1]*delta
		}
return(c(cv, mv, hv))
}
