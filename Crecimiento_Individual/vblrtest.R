#Growth function comparison by likelihood ratio
#according to Kimura(1980)
#Authors: Isaías H. Salgado-Ugarte & V. Mitsui Saito-Quezada
#R version by Leonardo Chacón Ramírez & Alan García Martínez
#Date: 17/03/2021; update: 28/08/2021
#Revised 17/03/2025
vblrtest <- function(lon, edad, sexo,Linf=500, Kval=1, tcero=0.1){
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
  
  st<-Map(rep,list(Li=Linf,K=Kval,t0=tcero),c(2,2,2))
  model1 <- summary(nls(lon ~ Li[sexo] * (1 - exp(-K[sexo] * (edad-t0[sexo]))),start =st))
  label1_1<-"none"
  label1_2<-paste("l_t1=",round(model1$coefficients[1],2),"[1-exp{",-round(model1$coefficients[3],3),"(t+(",-round(model1$coefficients[5],3),"))}]")
  label1_3<-round(sum(model1$residuals^2),2)
  label2_2<-paste("l_t2=",round(model1$coefficients[2],2),"[1-exp{",-round(model1$coefficients[4],3),"(t+(",-round(model1$coefficients[6],3),"))}]")
  #----------------------------------------------------------------------------------------------------------------------------------------------------
  st<-list(Li=Linf,K=Kval,t0=tcero)
  model2 <- summary(nls(lon ~ Li * (1 - exp(-K * (edad-t0))),start =st))
  label3_1<-"Li1 = Li2"
  label4_1<-"K1  = K2"
  label5_1<-"t01 = t02"
  label3_2<-paste("l_tt=",round(model2$coefficients[1],2),"[1-exp{",-round(model2$coefficients[2],3),"(t+(",-round(model2$coefficients[3],3),"))}]")
  label3_3<-round(sum(model2$residuals^2),2)
  label3_4<-round(-length(lon)*log(label1_3/label3_3),2)
  label3_5<-3
  label3_6<-round(1-pchisq(label3_4,label3_5),3)
  #-----------------------------------------------------------------------------------------------------------------------------------------------------
  st<-Map(rep,list(Li=Linf,K=Kval,t0=tcero),c(1,2,2))
  model3 <- summary(nls(lon ~ Li * (1 - exp(-K[sexo] * (edad-t0[sexo]))),start = st))
  label6_1<-"Li1 = Li2"
  label6_2<-paste("l_t1=",round(model3$coefficients[1],2),"[1-exp{",-round(model3$coefficients[2],3),"(t+(",-round(model3$coefficients[4],3),"))}]")
  label7_2<-paste("l_t2=",round(model3$coefficients[1],2),"[1-exp{",-round(model3$coefficients[3],3),"(t+(",-round(model3$coefficients[5],3),"))}]")
  label6_3<-round(sum(model3$residuals^2),2)
  label6_4<-round(-length(lon)*log(label1_3/label6_3),2)
  label6_5<-1
  label6_6<-round(1-pchisq(label6_4,label6_5),3)
  #-----------------------------------------------------------------------------------------------------------------------------------------------------
  st<-Map(rep,list(Li=Linf,K=Kval,t0=tcero),c(2,1,2))
  model4 <- summary(nls(lon ~ Li[sexo] * (1 - exp(-K * (edad-t0[sexo]))),start =st))
  label8_1<-"K1  = K2"
  label8_2<-paste("l_t1=",round(model4$coefficients[1],2),"[1-exp{",-round(model4$coefficients[3],3),"(t+(",-round(model4$coefficients[4],3),"))}]")
  label9_2<-paste("l_t2=",round(model4$coefficients[2],2),"[1-exp{",-round(model4$coefficients[3],3),"(t+(",-round(model4$coefficients[5],3),"))}]")
  label8_3<-round(sum(model4$residuals^2),2)
  label8_4<-round(-length(lon)*log(label1_3/label8_3),2)
  label8_5<-1
  label8_6<-round(1-pchisq(label8_4,label8_5),3)
  #-----------------------------------------------------------------------------------------------------------------------------------------------------
  st<-Map(rep,list(Li=Linf,K=Kval,t0=tcero),c(2,2,1))
  model5 <- summary(nls(lon ~ Li[sexo] * (1 - exp(-K[sexo] * (edad-t0))),start = st))
  label10_1<-"t01 = t02"
  label10_2<-paste("l_t1=",round(model5$coefficients[1],2),"[1-exp{",-round(model5$coefficients[3],3),"(t+(",-round(model5$coefficients[5],3),"))}]")
  label11_2<-paste("l_t2=",round(model5$coefficients[2],2),"[1-exp{",-round(model5$coefficients[4],3),"(t+(",-round(model5$coefficients[5],3),"))}]")
  label10_3<-round(sum(model5$residuals^2),2)
  label10_4<-round(-length(lon)*log(label1_3/label10_3),2)
  label10_5<-1
  label10_6<-round(1-pchisq(label10_4,label10_5),3)
  
  cat("______________________________________________________________________________")
  cat("\nLikelihood ratio test for two von Bertalanffy Growth Functions")
  cat("\n________________________________________________________________________________\n")
  
  Lin_Constrains <- c("-------------",label1_1,"","-------------",label3_1,label4_1,label5_1,"-------------",
                      label6_1,"","-------------",label8_1,"","-------------",label10_1,"","-------------")
  l <- NULL
  for (i in 1:length(Lin_Constrains)) {
    l <- c("|",l)
  }
  Models <- c("---------------------------------------------",
              label1_2,label2_2,"---------------------------------------------",
              label3_2,"","","---------------------------------------------",
              label6_2,label7_2,"---------------------------------------------",
              label8_2,label9_2,"---------------------------------------------",
              label10_2,label11_2,"---------------------------------------------")
  RSS <- c("-------",label1_3,"","-------","",label3_3,"","-------",label6_3,"","-------",
           label8_3,"","-------",label10_3,"","-------")
  PRL <- c("------","","","------","",label3_4,"","------",label6_4,"","------",label8_4,"",
           "------",label10_4,"","------")
  DF <- c("----","","","----","",label3_5,"","----",label6_5,"","----",label8_5,"",
          "----",label10_5,"","----")
  P <- c("------","","","------","",label3_6,"","------",label6_6,"","------",label8_6,"",
         "------",label10_6,"","------")
  test.2 <- data.frame(Lin_Constrains,l,Models,RSS,PRL,DF,P)
  format(test.2, justify = "centre")
  m<-as.matrix.noquote(test.2)
  h<-rep("",17)
  rownames(m)<-h
  m
  
}