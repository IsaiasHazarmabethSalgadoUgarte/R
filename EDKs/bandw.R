#Version 1: 18/07/2020; revised 09/04/2021; 04/05/2023;
#30/03/2026
#Original version:
#Isaias H. Salgado-Ugarte, Makoto Shimizu and Toru Taniuchi
#University of Tokyo, Faculty of Agriculture,
#Department of Fisheries, Yayoi 1-1-1, Bunkyo-ku
#Tokyo 113, Japan.(Fax 81-3-3812-0529)
#Updated version:
#Isaias H. Salgado-Ugarte (1)(2) & V. Mitsui Saito-Quezada (1);
#Marco A. Perez-Hernandez (2)
#Revised 16/03/2025
#(1) Laboratorio de Biometria y Biologia Pesquera, 
#Facultad de Estudios Superiores "Zaragoza" Campus II
#Batalla 5 de mayo S/N esq. Fuerte de Loreto
#Ejercito de Oriente, Iztapalapa, 09230, CDMX
#Mexico City
#(2) Biologia, Universidad Autonoma Metropolitana Iztapalapa
#Av. Michoacan y la Purisima, Col. Vicentina
#Iztapalapa 09340 CDMX Mexico City.
#(Fax 55-5-773-6336; 55-5-804-4688)
#e-mail: isalgado@unam.mx
bandw <- function(x, kernel="Gaussian", ...) {
source("c:/Rprogs/bandw.R")
#x <- order(x)
nuobs <- length(x)
maxval <- max(x, ...)
minval <- min(x, ...)
sigma <- sqrt(var(x, ...))
#iqr <- quantile(x,0.75, ...) - quantile(x,0.25, ...)
fiven <- fivenum(x, ...)
fd <- fiven[4]-fiven[2]
#psigma <- iqr/1.349
psigma <- fd/1.349
ks <- 1 + log(nuobs)/log(2)
osb <- (2*nuobs)^(1/3)
fposb <- ((147/2)*nuobs)^(1/5)
hs <- 3.5*sigma*nuobs^(-1/3)
hfd <- 2*(fd)*nuobs^(-1/3)
osht <- (maxval - minval)/((2*nuobs)^(1/3))
osuv <- 3.729*sigma*nuobs^(-1/3)
osr <- 2.603*fd*nuobs^(-1/3)
fpg <- 2.15*sigma*nuobs^(-1/5)
fpos <- 2.33*sigma*nuobs^(-1/5)
hsv <- 0.9*min(sigma,psigma)*nuobs^(-1/5)
hh <- 1.06*min(sigma,psigma)*nuobs^(-1/5)
osh <- 1.144*sigma*nuobs^(-1/5)

 if(kernel=="Uniform") {hsv=hsv*1.74
     hh=hh*1.74
     osh=osh*1.74} else

 if(kernel=="Triangular") {hsv=hsv*2.432
   hh=hh*2.432
   osh=osh*2.432} else

 if(kernel=="Epanechnikov") {hsv=hsv*2.214
   hh=hh*2.214
   osh=osh*2.214} else

 if(kernel=="Biweight") {hsv=hsv*2.623
   hh=hh*2.623
   osh=osh*2.623} else

 if(kernel=="Triweight") {hsv=hsv*2.978
   hh=hh*2.978
   osh=osh*2.978} else

if(kernel=="Gaussian") {hsv=hsv*1
   hh=hh*1
   osh=osh*1} else

if(kernel=="Cosinus") {hsv=hsv*2.288
   hh=hh*2.288
   osh=osh*2.288} else

   stop("Kernel not included")

cat("____________________________________________________________","\n")
cat("Some practical number of bins and binwidth-bandwidth rules","\n")
cat("for univariate density estimation using histograms","\n")
cat("frequency polygons (FP)and kernel density estimators","\n")
cat("============================================================","\n")
#cat("Number of observations = ", round(nuobs,4),"\n")
#cat("Maximum                = ", round(maxval,4),"\n")
#cat("Minimum                = ", round(minval,4),"\n")
#cat("Sigma                  = ", round(sigma,4),"\n")
#cat("IQR                    = ", round(fd,4),"\n")
#cat("psigma                 = ", round(psigma,4),"\n")
cat("Sturges' number of bins                           = ", ceiling(ks),"\n")
cat("Oversmoothed number of bins                      <= ", floor(osb),"\n")
cat("------------------------------------------------------------","\n")
cat("FP oversmoothed number of bins                   <= ", floor(fposb),"\n")
cat("============================================================","\n")
cat("Scott's optimal Gaussian binwidth                 = ", round(hs,4),"\n")
cat("Freedman-Diaconis optimal robust binwidth         = ", round(hfd,4),"\n")
cat("Terrell-Scott's oversmoothed binwidth            >= ", round(osht,4),"\n")
cat("Oversmoothed homoscedastic binwidth              >= ", round(osuv,4),"\n")
cat("Oversmoothed robust binwidth                     >= ", round(osr,4),"\n")
cat("------------------------------------------------------------","\n")
cat("FP optimal Gaussian binwidth                      = ", round(fpg,4),"\n")
cat("FP oversmoothed binwidth                         >= ", round(fpos,4),"\n")
cat("============================================================","\n")
if(kernel=="Uniform") cat("Uniform kernel (1)","\n") else

if(kernel=="Triangular") cat("Triangular kernel (2)","\n") else

if(kernel=="Epanechnikov") cat("Epanechnikov kernel (3)","\n") else

if(kernel=="Biweight") cat("Biweight (Quartic) kernel (4)","\n") else

if(kernel=="Triweight") cat("Triweight kernel (5)","\n") else

if(kernel=="Gaussian") cat("Gaussian kernel (6)","\n") else

if(kernel=="Cosinus") cat("Cosinus kernel","\n")

cat("============================================================","\n")

cat("Silverman's optimal bandwidth                     = ", round(hsv,4),"\n") 
cat("Haerdle's better optimal bandwidth                = ", round(hh,4),"\n")
cat("Scott's oversmoothed bandwidth                    = ", round(osh,4),"\n")
cat("____________________________________________________________","\n")
}
