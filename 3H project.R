require(forecast)
require(verification)
require(SpecsVerification)
require(stats)
require(ggplot2)
require(ggfortify)

Data1<-read.csv("BA.L3.csv")
Data1

#Extracting Data
Matrixe<-function(Data){
  High<-suppressWarnings(na.omit(as.numeric(as.character(Data1$High))))
  Low<-suppressWarnings(na.omit(as.numeric(as.character(Data1$Low))))
  Open<-suppressWarnings(na.omit(as.numeric(as.character(Data1$Open))))
  Close<-suppressWarnings(na.omit(as.numeric(as.character(Data1$Close))))
  DataSet<- as.matrix(cbind(High,Low,Open,Close))
  print(DataSet)
}
Highdat <- Matrixe(Data1)[ ,"High"]#, frequency = 365)
Lowdat <- ts(Matrixe(Data1)[ ,"Low"], frequency = 365)
Opendat <- ts(Matrixe(Data1)[ ,"Open"], frequency = 365)
Closedat <- ts(Matrixe(Data1)[ ,"Close"], frequency = 365)
par(mfrow = c(1,1))
#Highdat <- Detrend(Highdat2)
#Highdat1 <- Highdat

#Splitting into train and test
#length(Highdat1) <- length(Highdat)/2

#Plotting of Data
plot(Highdat, xlab='Years', ylab = 'High', pch = ".", type="l")

par(mfrow = c(1,2))
acf(ts(diff(Highdat)),main='ACF')
pacf(ts(diff(Highdat)),main='PACF')
#8
#1328
#1414
#1499
#1580
#1664
#1751
#1833
#1917
#2004
#2085
#print(Highdat[2085])

#Auto ARIMA
ARIMAfit1 = auto.arima((Opendat),D = 1, approximation = FALSE, trace = TRUE, allowdrift = FALSE)
ARIMAfit2 = auto.arima((Highdat), approximation = FALSE, trace = TRUE, allowdrift = FALSE, D = 1)
ARIMAfit3 = auto.arima((Lowdat), approximation = FALSE, trace = TRUE, allowdrift = FALSE, D = 1)
ARIMAfit4 = auto.arima((Closedat), approximation = FALSE, trace = TRUE, allowdrift = FALSE, D = 1)

plot(fitted(ARIMAfit2))
Box.test(resid(ARIMAfit2),lag = 1, type = "Ljung-Box", fitdf = 0)

summary(ARIMAfit1)
summary(ARIMAfit2)
summary(ARIMAfit3)
summary(ARIMAfit4)

write.csv(fitted(ARIMAfit2), "ARIMAforecast.csv")
#for (i in c(1328,1414,1499,1580,1664,1751,1833,1917,2004,2085)){
#print (c(fitted(ARIMAfit1)[i],
#fitted(ARIMAfit2)[i],
#fitted(ARIMAfit3)[i],
#fitted(ARIMAfit4)[i]))
#}
plot(fitted(ARIMAfit2))
summary(ARIMAfit)

#write.table(summary(ARIMAfit), file = "table.txt", sep = ",", quote = FALSE, row.names = F)

#95% Prediction intervals
upper <- fitted(ARIMAfit2) + 1.96*sqrt(ARIMAfit2$sigma2)
lower <- fitted(ARIMAfit2) - 1.96*sqrt(ARIMAfit2$sigma2)


#Plotting with points outside of intervals and once step forecast
par(mfrow = c(1,1))
plot(Highdat, type="n", ylim=range(lower,upper) ,xlab = 'Years', ylab = 'High of BAE Systems in dollars', main = "ARIMA(0,1,0)")
polygon(c(time(Highdat),rev(time(Highdat))), c(upper,rev(lower)), 
        col=rgb(0,0,0.6,0.2), border=FALSE)
lines(Highdat)
lines(fitted(ARIMAfit2),col='red')
out <- (Highdat < lower | Highdat > upper)
length(Highdat[out])
points(time(Highdat)[out], Highdat[out], pch=19)
legend("topleft", c("Input","SARIMA(0,1,0)(0,1,0)[365]"), fill=c("black","red"))

#Out of sample predictions
prediction = predict(ARIMAfit, n.ahead = 1000) 
lines(prediction$pred,col='blue')

#Scoring 
fitvalue <- fitted(ARIMAfit1)

#for SARIMA
#fit5 <- 0
#length(fitvalue)
#for(i in 367:2497){
#  fit5[i-366] <- fitvalue[i-365]+fitvalue[i-1]-fitvalue[i-366]
#  print(fit5)
#}
#plot(fit5,pch = ".", type="l", ylab = "Mean")
#fit5
#fitte <- Closedat[367:2497]

#Average ignorance function (mean has to be a vector)
ignr <- function (verification, meanv, sd){
  ign <- 0
  for(i in 1:length(meanv)){
    ign[i]<- -log(pnorm(verification[i], mean = meanv[i], sd = sd))
    }
    return(ign)
}

#for ARIMA
fitted(ARIMAfit1)[3]
Opendat[2]
fitvalue <- fitted(ARIMAfit4)
value <- Closedat
fitte <- fitvalue
length(fitvalue)
length(fit5)
fit5 <- c(fitvalue[1],fitvalue)[1:2497]
cbind(value, fitvalue)
print(fit5)


par(mfrow =c(1,1))
plot(ignr(fitte,fit5,sqrt(ARIMAfit1$sigma2)),ylab = "Ignorance", xlab = "Days",pch = ".", type="l", main = 'ARIMA Ignorance Plot')
mean(ignr(value,fitvalue,sqrt(ARIMAfit4$sigma2)))
x <- 1:2497
ingrplot<-as.data.frame(cbind(x,ignr(value,fitvalue,sqrt(ARIMAfit4$sigma2))))
names(ingrplot)<-c("Index", "Ignorance")
ggplot()+geom_line(data = ingrplot, aes(x=Index, y=Ignorance), color="black")
ggsave("ACloseIgn.png", device = "png", dpi = 100)


#bootstrap
ignorance <- (ignr(fitte,fit5,sqrt(ARIMAfit$sigma2)))
average_ign <- (mean((sample(ignorance, 2132, replace=TRUE))))
average_list <- replicate(1000,(mean((sample((ignr(value,fitvalue,sqrt(ARIMAfit4$sigma2))), 2497, replace=TRUE)))))

plot(sort(average_list))
print((sort(average_list))[50])
print((sort(average_list))[950])


#Gaussian 
mean(Highdat)
var(Highdat)
gau<-function(n, mean, sd){
  a<-0
  for(i in 1:n){
    a[i]<-rnorm(1,mean,sd = sd)
  }
  print(a)
}
opengau <- gau(2497,mean(Opendat),sd(Opendat))
highgau <- gau(2497,mean(Highdat),sd(Highdat))
lowgau <- gau(2497,mean(Lowdat),sd(Lowdat))
closegau <- gau(2497,mean(Closedat),sd(Closedat))
gaugau<-as.data.frame(cbind(1:2497,opengau,highgau,lowgau,closegau))
gaugau
names(gaugau)<-c("Index","Open","High","Low","Close")
ggplot(data=gaugau,aes(x=Index,y=Close))+geom_line()

ignopengau<-ignr(opengau,rep(mean(Opendat),2497),sd(Opendat))
ignhighgau<-ignr(highgau,rep(mean(Highdat),2497),sd(Highdat))
ignlowgau<-ignr(lowgau,rep(mean(Lowdat),2497),sd(Lowdat))
ignclosegau<-ignr(closegau,rep(mean(Closedat),2497),sd(Closedat))
RMSE(ignopengau, Opendat)
RMSE(ignhighgau, Highdat)
RMSE(ignlowgau, Lowdat)
RMSE(ignclosegau, Closedat)

GaussIgnoravg <- replicate(1000,(mean((sample(ignclosegau, 2497, replace=TRUE)))))
print(sort(GaussIgnoravg)[50])
print(sort(GaussIgnoravg)[950])


gauign<-as.data.frame(cbind(1:2497,ignclosegau))
names(gauign)<-c("Index","Ignorance")
gauign
ggplot(data = gauign,aes(x=Index, y=Ignorance))+geom_line()



ggsave("GauCIgn.png", device = "png", dpi = 100)

plot(sample,pch = ".", type="l",ylab = "High of BAE Systems in dollars", xlab = "Days", main = "Random Samples from Gaussian Disstribution")
mean(ignr(sample,rep(mean(Highdat),2497),sd(Highdat)))
plot(ignr(sample,rep(mean(Highdat),2497),sd(Highdat)), pch=".", type = "l", xlab = "Days", ylab = "Ignorance", main = "Gaussian Distribution Ignorance Plot")
#Gaussian Bootstrap
GaussIgnor <-ignr(sample,rep(mean(Highdat),2497),sd(Highdat))
GaussIgnoravg <- replicate(1000,(mean((sample(GaussIgnor, 2497, replace=TRUE)))))
print(mean(GaussIgnoravg))
plot(sort(GaussIgnoravg))
print((sort(GaussIgnoravg))[50])
print((sort(GaussIgnoravg))[950])

RMSE = function(m, o){
  sqrt(mean((m - o)^2))
}
RMSE(Highdat, fitted(ARIMAfit2))

#Standard Kernel Dressing
SKD3samp <- function(meanv, var){
  a <- matrix(0, nrow = length(meanv), ncol = 3)
  for (i in 1:length(meanv)){
     for (j in 1:3){
        a[i,j] <- rnorm(1, meanv[i], var)
      }
    }
  
  print(a)
}
#warnings(b <- AKD3samp(c(23,32,21,3),20))

SKD <- SKD3samp(fitvalue,ARIMAfit2$sigma2)
nrow(SKD)
#par(mfrow = c(1,3))
plot(Highdat,col=1,xlim = c(4,5))
lines(ts(SKD[,1],frequency =365), pch=".", type = "l",col =2)
lines(ts(SKD[,2],frequency=365),  pch=".", type = "l", col =3)
lines(ts(SKD[,3],frequency = 365), pch=".", type = "l", col =4)
lines(fitted(ARIMAfit2),pch =".",type="l", col = 5)

kernel<-function(x){
  a <- exp(-(x)^2/2)/sqrt(2*pi)
}



sigma = 0.5
"sqrt(ARIMAfit2$sigma2)"
a=0

SKD <- function(SKD3sam,fitvalue, sigmal, test, sigma1){
#  d<- 0
  #for (j in 1:length(sigmal)){
  for (i in 1:nrow(SKD3sam)){
    a[i] <- (1/(3*(sigma1)))*(kernel((-SKD3sam[i,1]+test[i]+1)/sigma1)+kernel((-SKD3sam[i,2]+test[i]+1)/sigma1)+kernel((-SKD3sam[i,3]+test[i]+1)/sigma1))
    b[i] <- a[i]+test[i]}
    #d <- ignr(test, b,sigmal)
  return(b)}

fitvalue<- fitted(ARIMAfit1)
c <- SKD3samp(fitvalue,ARIMAfit1$sigma2)
print(c)
nrow(c)
sigmal<-seq(1, 10, 0.1)

skdsamp <- SKD(c,fitvalue,4.123,Opendat, sqrt(ARIMAfit1$sigma2))
RMSE(skdsamp, Closedat)
mean(skdsamp)
plot(skdsamp, pch=".", type="l")

x <- 1:2497
skdsam<-as.data.frame(cbind(x,skdsamp))
skdsam
names(skdsam)<-c("Index","Ignorance")
ggplot(data = skdsam, aes(x = Index,y=Ignorance))+geom_line()
  geom_line(aes(y=Ensemble), color="black")

ggsave("AKDCloseIgn.png", device = "png", dpi = 100)


sverage_list <- replicate(1000,(mean(sample(skdsamp, 2497, replace=TRUE))))
plot(sort(sverage_list))
mean(sverage_list)
print(sort(sverage_list)[50])
print(sort(sverage_list)[950])


plot(ts(SKD(c, fitvalue,3),frequency = 365),pch=".",type="l")
lines(Highdat,col =2)
mean(ignr(Highdat,SKD(c, fitvalue,3),3))

fitvalue<-fitted(ARIMAfit2)
samp<-
fSKD<- function(SKD3, fitvalue, sigma){
  a<- 0
  for(i in 1:nrow(SKD3)){
    a[i] <- (SKD3[i,1]+SKD3[i,2]+SKD3[i,3])/3
  }
  print(a)
}

sigmal

mean(ignr(Highdat,fSKD(SKD3samp(fitvalue,2),fitvalue,2),10))

plot(ts(fSKD(SKD3samp(fitvalue,1),fitvalue,2),frequency = 365),pch=".",type="l")
lines(Highdat,col =2)
a

SKD3sa<- SKD3samp(fitvalue,4)
for(i in 1:nrow(SKD3sa)){
  a[i]<- (SKD3sa[i,1]+SKD3sa[i,2]+SKD3sa[i,3])/3
}

plot(ts(fSKD(),frequency =365),pch = ".", type = "l", col=1)
lines(Highdat,col=2)
plot(ts(SKD(SKD3samp(fitvalue,ARIMAfit$sigma2),fitvalue,5),frequency = 365),pch =".", type ="l")
plot(Highdat)
par(mfrow = c(1,2))
plot(SKD3samp(fitvalue,ARIMAfit$sigma2)[,1],pch = ".", type = "l", col=1)
lines(SKD3samp(fitvalue,ARIMAfit$sigma2)[,2],pch = ".", type = "l", col=2)
lines(SKD3samp(fitvalue,ARIMAfit$sigma2)[,3],pch = ".", type = "l", col=3)
