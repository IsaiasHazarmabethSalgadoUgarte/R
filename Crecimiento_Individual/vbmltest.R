#Growth function comparison by maximum likelihood
#according to Roff(2006)
#Authors: Isaías H. Salgado-Ugarte & V. Mitsui Saito-Quezada
#R version by Leonardo Chacón Ramírez & Alan García Martínez
#Date: 13/01/2021; 
#Revised 17/03/2025
vbmltest <- function(lon, edad, sexo,Linf=500, Kval=1, tcero=0.1){
  if(is.numeric(lon)==FALSE | length(lon)<= 1 ){
    stop("First entry is not a vector or/and it is not numeric or/and length <= 1")
  }
  if(is.numeric(edad)==FALSE | length(edad)<= 1 ){
    stop("Second entry is not a vector or/and it is not numeric or/and length <= 1")
  }
  if(is.numeric(sexo)==FALSE | length(sexo)<= 1 ){
    stop("First entry is not a vector or/and it is not numeric or/and length <= 1")
  }
  if(is.numeric(Linf)==FALSE | is.numeric(Kval)==FALSE | is.numeric(tcero)==FALSE){
    stop("Start values are not numeric")
  }
  #--------------------------------------------------------------------------------------------------------------
  Lin_Constrains <- c("-------------","none","","-------------","Li1 = Li2","K1 = K2","t01 = t02","-------------",
                      "Li1 = Li2","","-------------","K1 = K2","","-------------","t01 = t02","","-------------")
  Models <- Lin_Constrains
  RSS <- Lin_Constrains
  EF <- Lin_Constrains
  DF <- Lin_Constrains
  P <- Lin_Constrains
  #------------------------------------------------------------------------------------------------------------------
  st<-Map(rep,list(Li=Linf,K=Kval,t0=tcero),c(2,2,2))
  model1 <- summary(nls(lon ~ Li[sexo] * (1 - exp(-K[sexo] * (edad-t0[sexo]))),start =st))
  Models[2]<-paste("l_t1=",round(model1$coefficients[1],2),"[1-exp{",-round(model1$coefficients[3],3),"(t+(",-round(model1$coefficients[5],3),"))}]")
  RSS[2]<-round(sum(model1$residuals^2),2)
  Models[3]<-paste("l_t2=",round(model1$coefficients[2],2),"[1-exp{",-round(model1$coefficients[4],3),"(t+(",-round(model1$coefficients[6],3),"))}]")
  #----------------------------------------------------------------------------------------------------------------------------------------------------
  st<-list(Li=Linf,K=Kval,t0=tcero)
  model2 <- summary(nls(lon ~ Li * (1 - exp(-K * (edad-t0))),start =st))
  Models[5]<-paste("l_tt=",round(model2$coefficients[1],2),"[1-exp{",-round(model2$coefficients[2],3),"(t+(",-round(model2$coefficients[3],3),"))}]")
  RSS[5]<-round(sum(model2$residuals^2),2)
  EF[5]<-round(((as.double(RSS[5])- as.double(RSS[2]))/(3*2-3))/(as.double(RSS[2])/(length(lon) - 3*2)),2)  #round(-length(lon)*log(as.double(RSS[2])/as.double(RSS[5])),2)
  DF[5]<-3 #df1
  DF[6]<-length(lon)-as.double(DF[5])*2 #df2
  P[5]<-round(pf(as.double(EF[5]),as.double(DF[5]),as.double(DF[6]),lower.tail = F),4)
  #-----------------------------------------------------------------------------------------------------------------------------------------------------
  st<-Map(rep,list(Li=Linf,K=Kval,t0=tcero),c(1,2,2))
  model3 <- summary(nls(lon ~ Li * (1 - exp(-K[sexo] * (edad-t0[sexo]))),start = st))
  Models[9]<-paste("l_t1=",round(model3$coefficients[1],2),"[1-exp{",-round(model3$coefficients[2],3),"(t+(",-round(model3$coefficients[4],3),"))}]")
  Models[10]<-paste("l_t2=",round(model3$coefficients[1],2),"[1-exp{",-round(model3$coefficients[3],3),"(t+(",-round(model3$coefficients[5],3),"))}]")
  RSS[9]<-round(sum(model3$residuals^2),2)
  EF[9]<-round(((as.double(RSS[9])-as.double(RSS[2]))/(as.double(DF[5])*2-(as.double(DF[5])*2-1)))/(as.double(RSS[2])/(as.double(DF[6]))),2)  #round(-length(lon)*log(as.double(RSS[2])/as.double(RSS[9])),2)
  DF[9]<-as.double(DF[5])*2-(as.double(DF[5])*2-1) #df3
  DF[10]<-as.double(DF[6]) #df2
  P[9]<-round(pf(as.double(EF[9]),as.double(DF[9]),as.double(DF[10]),lower.tail = F),4)
  #-----------------------------------------------------------------------------------------------------------------------------------------------------
  st<-Map(rep,list(Li=Linf,K=Kval,t0=tcero),c(2,1,2))
  model4 <- summary(nls(lon ~ Li[sexo] * (1 - exp(-K * (edad-t0[sexo]))),start =st))
  Models[12]<-paste("l_t1=",round(model4$coefficients[1],2),"[1-exp{",-round(model4$coefficients[3],3),"(t+(",-round(model4$coefficients[4],3),"))}]")
  Models[13]<-paste("l_t2=",round(model4$coefficients[2],2),"[1-exp{",-round(model4$coefficients[3],3),"(t+(",-round(model4$coefficients[5],3),"))}]")
  RSS[12]<-round(sum(model4$residuals^2),2)
  EF[12]<-round(((as.double(RSS[12])-as.double(RSS[2]))/(as.double(DF[9])))/(as.double(RSS[2])/(as.double(DF[10]))),2)  #round(-length(lon)*log(as.double(RSS[2])/as.double(RSS[12])),2)
  DF[12]<-as.double(DF[9])  #df3
  DF[13]<-as.double(DF[10])  #df2
  P[12]<-round(pf(as.double(EF[12]),as.double(DF[12]),as.double(DF[13]),lower.tail = F),4)
  #-----------------------------------------------------------------------------------------------------------------------------------------------------
  st<-Map(rep,list(Li=Linf,K=Kval,t0=tcero),c(2,2,1))
  model5 <- summary(nls(lon ~ Li[sexo] * (1 - exp(-K[sexo] * (edad-t0))),start = st))
  Models[15]<-paste("l_t1=",round(model5$coefficients[1],2),"[1-exp{",-round(model5$coefficients[3],3),"(t+(",-round(model5$coefficients[5],3),"))}]")
  Models[16]<-paste("l_t2=",round(model5$coefficients[2],2),"[1-exp{",-round(model5$coefficients[4],3),"(t+(",-round(model5$coefficients[5],3),"))}]")
  RSS[15]<-round(sum(model5$residuals^2),2)
  EF[15]<-round(((as.double(RSS[15])-as.double(RSS[2]))/(as.double(DF[12])))/(as.double(RSS[2])/(as.double(DF[13]))),2)  #round(-length(lon)*log(as.double(RSS[2])/as.double(RSS[15])),2)
  DF[15]<-as.double(DF[12])
  DF[16]<-as.double(DF[13])
  P[15]<-round(pf(as.double(EF[15]),as.double(DF[15]),as.double(DF[16]),lower.tail = F),4)
  
  cat("______________________________________________________________________________")
  cat("\n  Maximum likelihood F tests for two von Bertalanffy Growth Functions")
  cat("\n______________________________________________________________________________\n")
  
  
  l <- NULL
  for (i in 1:length(Lin_Constrains)) {
    l <- c("|",l)
  }
  Models[1]=Models[4]=Models[8]=Models[11]=Models[14]=Models[17]<- "--------------------------------------------"
  Models[6]=Models[7] = ""
  
  RSS[1]=RSS[4]=RSS[8]=RSS[11]=RSS[14]=RSS[17]<- "------"
  RSS[6]=RSS[7] = ""
  
  EF[1]=EF[4]=EF[8]=EF[11]=EF[14]=EF[17]<- "------"
  EF[2]=EF[6]=EF[7] <- ""
  
  DF[1]=DF[4]=DF[8]=DF[11]=DF[14]=DF[17]<- "------"
  DF[2]=DF[7] <- ""
  P[1]=P[4]=P[8]=P[11]=P[14]=P[17]<- "------"
  P[2]=P[6]=P[7] <- ""

  test.2 <- data.frame(Lin_Constrains,l,Models,RSS,EF,DF,P)
  #format(test.2, justify = "centre")
  m<-as.matrix.noquote(test.2)
  h<-rep("",17)
  rownames(m)<-h
  m
  
}