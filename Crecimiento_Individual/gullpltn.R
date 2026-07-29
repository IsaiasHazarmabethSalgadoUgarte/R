#Original Stata version: 16/09/98;
#First written 26/10/2020; Revised 26/10/01 ; Updated 28/04/2020
#Authors of the original program: Salgado-Ugarte I.H. y V.M. Saito-Quezada
#Authors of the R adaptation: Salgado-Ugarte I.H. y Garcia-Martinez A.
#This program, is an adaptation of an existent program on Stata that
#estimates the Linfinite and K parameters of the von Bertalanffy growth function 
#by means of the Gulland-Holt method Gulland and Holt (1959) adjusted by 
#Gulland (1969) using mean length at age values.
#Revised 28/08/2021; 17/03/2025

gullpltn<-function(VectorEntrada, title=NULL,xlab=NULL,ylab=NULL,graph=TRUE,legend=FALSE){
  
  if(is.numeric(VectorEntrada)==FALSE && length(VectorEntrada)<=1){
    stop("NO OBSERVATIONS")
    print("The input vector does not contain numeric values, or has a length less/equal to 1")
  }
  
  
  #Se le asigna en vector (que pertenece a la lista de la BD) a msize
  #Msize es un vector
  msize <- VectorEntrada
  
  #Nrows es un vector de filas(elementos en /msize), + 2, 
  nurows <- length(msize) + 2
  
  #Se le asigna del valor [2] al ultimo a lagmsi (que tambien es un vector)
  lagmsi <- msize[2:length(msize)]
  
  
  
  #Sel le agrega el valor 0 al principio de /msize a un nuevo vector lmsize
  lmsize <- NULL
  lmsize[2:(length(msize)+1)] <- msize[1:length(msize)]
  
  
  llagm <- NULL
  llagm[2:(length(lagmsi)+1)] <- lagmsi[1:length(lagmsi)]
  
  
  delsi <- llagm[1:(length(llagm)+1)] - lmsize[1:(length(lmsize))]
  
  
  if(is.na(lmsize[1]) == TRUE){
    lmsize[1] <- 0
  }
  
  regress <- lm(delsi~lmsize)
  
  delest <- NULL
  for(i in 1:(length(lmsize))){
    delest[i] <- regress$coefficients[1]+regress$coefficients[2]*lmsize[i]
  }
  
  b <- regress[["coefficients"]][["lmsize"]]
  
  if(b >= 1){
    print("The slope of the line of the observed points is >=1")
    break
  }
  kval = log(b+1)*-1
  linf = ((delest[1])/(1-exp(kval*-1)))
  #--------------------------------------------------------------------------------------------------------
  cat("Estimation of L_inf and K values by the Gulland Method\n")
  for(i in 1:60){cat("-")}
  cat("\n")
  cat("Intercept", "=", round(delest[1], 4),"    slope", "=",round(b,4), sep = " ","\n")
  
  adjlmsize <- lmsize[2:(length(lmsize)-1)]
  adjdelsi <- delsi[2:(length(delsi)-1)]
  
  rsq <- cor(adjlmsize,adjdelsi)^2
  
  AdrSq <- 1 - ((nurows-3-1)/(nurows-3-1-1)) * (1-rsq)
  
  cat("R^2", "=", round(rsq ,4),"           Adj R^2", "=",round(AdrSq, 4), sep = " ","\n")
  cat("L_inf", "=", round(linf,4),"        k", "=", round(kval,4), sep = " ","\n")
  for(i in 1:60){cat("-")}
  cat("\n")
  #-------------------------------------------------------------------------------------------------------
  if(graph==TRUE){
    
    if(is.null(title)==TRUE){
      Li <- as.character(round(linf,4))
      k <- as.character(round(kval,4))
      title <- paste('Gulland plot, Linf =', Li,' K =', k)
    }else{
      if(is.character(title)==FALSE){
        stop("Title is not character type")
      }
    }
    
    if(is.null(xlab)==TRUE){
      xlab <- paste("Size at age t")
    }else if(is.character(xlab)==FALSE){
      stop("xlab is not character typer")
    }
    
    if(is.null(ylab)==TRUE){
      ylab <- expression(paste(Delta," Size"))
    }else if(is.character(ylab)==FALSE){
      stop("ylab is not character type")
    }
    
    plotzone <- c(0,lmsize,delest,linf,adjdelsi)
    plot(c(max(plotzone),min(plotzone)),c(min(plotzone),max(plotzone)/3), main = title,xlab = xlab,ylab = ylab,type="l",col="white")
    #plot(c(lmsize,linf),c(delest,0), main = title,xlab = xlab,ylab = ylab, type="l", col="red")
    lines(c(lmsize,linf),c(delest,0), col="red")
    points(lmsize,delsi, col="blue")
    grid()
    
    if(legend==TRUE){
      legend(x="bottom",legend = c("Recta de xy","Puntos"),fill = c("red","Blue"))
    }
  }
}