#Author (original version): Salgado-Ugarte, I.H.
#First written: 24/04/2002 (version 1.00); revised 03/05/2002
#R version 14/07/2021 by Salgado-Ugarte, I.H., V.M. Saito-Quezada, 
#A. García-Martínez & L. Chacón-Ramírez; revised 18/07/2021; 17/03/2025
#This program performs the multivariate test for von Bertalanffy 
#Growth Function comparison, considering Hotelling's T^2 statistic
#(based on Bernard, D.R. 1981. Multivariate analysis as a means of comparing
#growth in fish. Canadian Journal of Fisheries and Aquatic Sciences, 
#38; 223-236).
#sm<-matrix(c(294.74,-0.6018,-32.57,-.6018,.0013,.073,-32.57,.073,4.2296),nrow = 3,ncol = 3)
#sf<-matrix(c(1596.18,-1.1895,-91.05,-1.1895,.0009,.0713,-91.0468,.07,5.76),nrow = 3,ncol = 3)
#pm<-matrix(c(441.16,.13,-3.36),nrow = 3, ncol = 1)
#pf<-matrix(c(505.97,.09,-4.57),nrow = 3, ncol = 1)
#hotegfun(sm,sf,pm,pf,78,78,99)

hotegfun<-function(s1,s2,p1,p2,n1,n2,cl){
  if(is.matrix(s1)==FALSE || is.matrix(s2)==FALSE || is.matrix(p1)==FALSE || is.matrix(p2)==FALSE || is.numeric(n1)==FALSE || is.numeric(n2)==FALSE || is.numeric(cl)==FALSE){
    stop("Syntax is hotegfun(s1, s2, p1, p2, n1, n2, cl)")
  }

  dp<- p1-p2

  #dprime<-p1-p2

  #dp<-t(dprime)
  
  dprime<-t(dp)

  if(cl<1){
    stop("You must provide the confidence level in percentage")
  }
  s<-((n1-1)*s1+(n2-1)*s2)/(n1+n2-2)
  sinv<-solve(s)
  
  
  for (i in 1:78) {
   cat("=")
  }
  cat("\n")
  cat("Multivariate test for Growth Function comparison \n","from two populations (based on Bernard, 1981)\n")
  for (i in 1:78) {
    cat("_")
  }
  cat("\n")
  cat("         Matrix S                             Matrix S inverse \n")
  cat("   ",round(s[1,1:3],4),"\t          ",round(sinv[1,1:3],4),"\n")
  cat("   ",round(s[2,1:3],4),"\t          ",round(sinv[2,1:3],4),"\n")
  cat("   ",round(s[3,1:3],4),"\t          ",round(sinv[3,1:3],4),"\n")  
  
  for (i in 1:78) {
    cat("_")
  }
  cat("\n")
  
  a<-dim(dprime)
  cat("|")
  for (i in 1:a[2]) {
    cat(" ",round(dprime[1,i],4)," ")
  }
  cat(" |    = [P1-P2]'\n")
  for (i in 1:78) {
    cat("_")
  }
  cat("\n")
  
  t2=(n1*n2/(n1+n2))*dprime%*%sinv%*%dp
  
  
  dof=n1+n2-4
  sl = (100 - cl)/100
  t2t=(3*(n1+n2-2))/(dof)* qf(sl,3,dof,lower.tail = FALSE)
  fc = qf(sl,3,dof,lower.tail=FALSE)
  
  cat("T^2 = ",round(t2[1,1],4)," T^2_",sl,": 3,",dof," = ",round(t2t,4)," F_",sl,": 3,",dof," = ",round(fc,4),"\n")
  for (i in 1:78) {
    cat("_")
  }
  cat("\n")
  
  if(t2[1,1]>t2t){
    llLi = dp[1,1]-(((n1+n2)/(n1*n2))*(3*(n1+n2 -2))/(dof)*fc*s[1,1])^.5
    ulLi = dp[1,1]+(((n1+n2)/(n1*n2))*(3*(n1+n2 -2))/(dof)*fc*s[1,1])^.5
    fcritLi= (n1*n2*(n1+n2-4)*(dp[1,1])^2)/(3*(n1+n2)*(n1+n2-2)*s[1,1])
    
    llk = dp[2,1]-(((n1+n2)/(n1*n2))*(3*(n1+n2-2))/(dof)*fc*s[2,2])^.5
    ulk = dp[2,1]+(((n1+n2)/(n1*n2))*(3*(n1+n2-2))/(dof)*fc*s[2,2])^.5
    Fcritk= (n1*n2*(n1+n2-4)*(dp[2,1])^2)/(3*(n1+n2)*(n1+n2-2)*s[2,2])
    
    llto = dp[3,1]-(((n1+n2)/(n1*n2))*(3*(n1+n2-2))/(dof)*fc*s[3,3])^.5
    ulto = dp[3,1]+(((n1+n2)/(n1*n2))*(3*(n1+n2-2))/(dof)*fc*s[3,3])^.5
    Fcritto= (n1*n2*(n1+n2-4)*(dp[3,1])^2)/(3*(n1+n2)*(n1+n2-2)*s[3,3])
    
    con<-cl
    cat("       Confidence intervals of ",con,"%\t\t\t  Critical F\n")
    cat("   ",round(llLi,4),"<= L_inf1 - L_inf2 <=",round(ulLi,4),"\t          ",round(fcritLi,4),"\n")
    cat("   ",round(llk,4),"<=   K1   -   K2   <=",round(ulk,4),"\t          ",round(Fcritk,4),"\n")
    cat("   ",round(llto,4),"<=  t_o1  -  t_o2  <=",round(ulto,4),"\t          ",round(Fcritto,4),"\n")
    for (i in 1:78) {
      cat("=")
    }
    cat("\n")
  }else{
    cat("The growth functions are not different at ",con,"% confidence level\n")
    for (i in 1:78) {
      cat("=")
    }
    cat("\n")
  }
  
}