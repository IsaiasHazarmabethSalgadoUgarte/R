#First written 09/11/98; version 2.00 14/06/99
#Authors: Salgado-Ugarte I.H. & V.M. Saito-Quezada
#This program estimates the Linfinite parameter of the von
#Bertalanffy growth function by means of the Gulland-Holt method
#Gulland y Holt (1959) according to Sparre et al. (1989)
#R version first written 29/10/2020 with the assistance of
#A. García-Martínez & L. Chacón-Ramírez; revised 28/08/2021
#This program, is an adaptation of an existent program on Stata
#Revised 17/03/2025
gulholtn<-function(VectorEntrada, VectorEntrada2,  Titl=NULL,xlab=NULL,ylab=NULL,graph=TRUE,legend=FALSE){
  if(is.numeric(VectorEntrada)==FALSE && length(VectorEntrada)<=1){
    stop("NO OBSERVATIONS")
    print("The input vector does not contain numeric values, or has a length less/equal to 1")
  }
  if(length(VectorEntrada)!=length(VectorEntrada2)){
    stop("Input vectors has diferent length")
  }
  
  #Nrows es un vector de filas(elementos en /msize), + 2, 
  nurows <- length(VectorEntrada) + 2
  
  #Se le asigna en vector (que pertenece a la lista de la BD) a msize
  #Msize es un vector
  msize <- VectorEntrada
  
  #Se le asigna del valor [2] al ultimo a lagmsi (que tambien es un vector)
  lagmsi <- msize[2:length(msize)]
  #print.default(lagmsi)
  
  #Sel le agrega el valor 0 al principio de /msize a un nuevo vector lmsize
  lmsize <- NULL
  for(i in 1:(length(msize)-1)){
    lmsize[i+1] <- msize[i]
  }
  #print.default(lmsize)
  
  #Se crea un nuevo vercor con los elementos de /lagmsi pero con un 0 en la pos[1]
  llagm <- NULL
  for(i in 1:length(msize)){
    llagm[i+1] <- lagmsi[i]
  }
  #print.default(llagm)
  
  if(is.na(lmsize[1]) == TRUE){
    lmsize[1] <- 0
  }
  #print.default(lmsize)
  #----------------------------------------------------------------------------------
  
  #Entrada del segundo vector sele asigna a /agev
  agev <- VectorEntrada2
  
  #Se declara nueva variable, que concatena el elem 0 a agev, en la pos[1]
  lagage <- NULL
  for(i in 1:(length(agev)-1)){
    lagage[i+1] <- agev[i]
  }
  #print.default(lagage)

  deltata <- VectorEntrada2 - lagage
  #print.default(deltata)
  
  deltas <- llagm[1:(length(llagm)-1)] - lmsize
  #print.default(deltas)
  
  ye <- deltas/deltata
  #print.default(ye)
  
  equis <- (llagm[1:(length(llagm)-1)] + lmsize)/2
  #print.default(equis)
  
  #------------------------------------------------------------------------------------
  yeadj <- NULL
  j = 1
  for (i in 1:length(ye)) {
    if(is.na(ye[i])== FALSE && is.infinite(ye[i])==FALSE){
      yeadj[j] <- ye[i]
      j <- j+1
    }
  }
  #print.default(yeadj)
  
  equisadj <- NULL
  j = 1
  for (i in 1:length(equis)) {
    if(is.na(equis[i])== FALSE && is.infinite(equis[i])==FALSE){
      equisadj[j] <- equis[i]
      j <- j+1
    }
  }
  #print.default(equisadj)
  #--------------------------------------------------------------------------------
  
  regress<-lm(yeadj ~ equisadj[1:length(yeadj)])
  #View(regress)
  
  b <- regress$coefficients[2]
  #print.default(b)
 
  if(b>=0){
    stop("The slope of the line of the observed points is >=0")
  }
  
  yest <- NULL
  for(i in 1:length(regress$fitted.values)){
    yest[i+1] <- regress$fitted.values[i]
  }
  
  ba <- regress[["coefficients"]][["(Intercept)"]]
  equis[nurows] <- ba / (b *-1)
  yest[nurows] <- 0
  #print.default(yest)
  
  linf <- ba / (b *-1)
  kval <- (b *-1)
  
  #--------------------------------------------------------------------------------------------------------
  cat("Estimation of L_inf and K values by the Gulland-Holt Method\n")
  for(i in 1:60){cat("-")}
  cat("\n")
  cat("Intercept", "=", round(ba, 4),"    slope", "=",round(b,4), sep = " ","\n")
  
  rsq <- cor(yeadj,equisadj[1:length(yeadj)])^2
  
  AdrSq <- 1 - ((nurows-3-1)/(nurows-3-1-1)) * (1-rsq)
  
  cat("R^2", "=", round(rsq ,4),"           Adj R^2", "=",round(AdrSq, 4), sep = " ","\n")
  cat("L_inf", "=", round(linf,4),"        k", "=", round(kval,4), sep = " ","\n")
  for(i in 1:60){cat("-")}
  cat("\n")
  #-------------------------------------------------------------------------------------------------------
  if(graph==TRUE){
    
    if(is.null(Titl)==TRUE){
      Li <- as.character(round(linf,4))
      k <- as.character(round(kval,4))
      Titl <- paste('Gulland-Holt graph, Linf =', Li,' K =', k)
    }else{
      if(is.character(Titl)==FALSE){   #Modificado por IHSU 6/05/23
        stop("Title is not character type")
      }
    }
    
    if(is.null(xlab)==TRUE){
      xlab <- expression(paste("Mean size by ",Delta," Age"))
    }else if(is.character(xlab)==FALSE){
      stop("xlab is not character typer")
    }
    
    if(is.null(ylab)==TRUE){
      ylab <- expression(paste(Delta," Size / ",Delta," Age"))
    }else if(is.character(ylab)==FALSE){
      stop("ylab is not character type")
    }
    
    yestadj <- NULL
    j = 1
    for (i in 1:length(yest)) {
      if(is.na(yest[i])== FALSE && is.infinite(yest[i])==FALSE){
        yestadj[j] <- yest[i]
        j <- j+1
      }
    }
    #print.default(yestadj)
    plotzone <- c(0,yeadj,yestadj,equisadj,linf,b)
    
    
    plot(c(max(equisadj),min(equisadj)),c(min(plotzone),max(plotzone)/3), main = Titl,xlab = xlab,ylab = ylab,type="l",col="white")
    lines(equisadj,yestadj[1:length(equisadj)], col="red")
    points(equisadj,yeadj, col="blue")
    grid()
    
    if(legend==TRUE){
      legend(x="bottom",legend = c("Recta de xy","Puntos"),fill = c("red","Blue"))
    }
  }
}
