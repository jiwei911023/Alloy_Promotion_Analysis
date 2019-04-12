##pounds nonweb
install.packages("xts")
library(xts)

sold_noweb <-ts(pounds$pounds_sold,frequency=12,start=c(2005,1),end = c(2008,5)) 
plot.ts(sold_noweb,col="red",main= "Noweb_pounds_sold")
##better
require(xts)
SOLD <- rnorm(24)
sold_noweb <- ts(pounds$pounds_sold, frequency = 12, start = c(2005, 1), end = c(2008,5), deltat = 1/6)
plot(as.xts(sold_noweb), major.format = "%Y-%m", col = "red", main = "web_pounds_sold")
#predict
install.packages("forecast")
library(zoo)
library(forecast)
poundsts <- ts(ts(pounds$pounds_sold,frequency=12,start=c(2005,1),end = c(2008,5)))
#diff
#sdiff <- diff(sold_noweb,differences = 2)
#plot.ts(sdiff)
#ACF/PACF
acf(sold_noweb,lag.max =30 ,plot=FALSE)
acf(sold_noweb,lag.max =30 )
pacf(sold_noweb,lag.max =30 ,plot=FALSE)
pacf(sold_noweb,lag.max =30 )
#arima
auto.arima(sdiff,trace=T)
#predict
parima <- arima(sold_noweb,order=c(1,2,5))
predict_pounds <- forecast(parima,h=7,level=c(99.5))
future <- forecast(predict_pounds)  
plot(future,main="Pridiction Pounds Sold")
##check
tsdiag(parima)
#ACF:residual have no significent self-related
#all p value > 0.1

#comparison
sold_web <-ts(pounds$pounds_sold,frequency=12,start=c(2005,1),end = c(2008,12))
plot(sold_web,col="red",main="Actual Pounds Sold")
abline(x=2006)
??line()
#L-junkbox
##Box.test(future$residuals,lag = 5,type = "Ljung-Box")
plot.ts(future$residuals)
plotForecastErrors <- function(forecasterrors){
  hist(forecasterrors, col="red", freq = F)
  mysd <- sd(forecasterrors)
  mynorm <- rnorm(10000, mean = 0, sd = mysd)
  myhist <- hist(mynorm, plot = F)
  points(myhist$mids, myhist$density, type="l", col="blue", lwd=2)
}
plotForecastErrors(future$residuals)
####almost the normal distribution  mean_residuals = 0

par(mfcol=c(2,1))

###pounds comprison
library(readxl)
pounds2 <- read_excel("C:/Users/54329/Desktop/Pounds2.xlsx")
View(Pounds2)
names(Pounds2) <- c("time","predict_pounds","actual_pounds")
library(ggplot2)
ggplot(data=pounds2, mapping=aes(x=actual_pounds, y=predict_pounds, group=time)) +geom_line(aes(linetype=time,color=time))+
  geom_point(aes(color=time))
ggplot(Pounds2, aes(time)) + 
  geom_line(aes(y = predict_pounds, colour = "predict_pounds")) + 
  geom_line(aes(y = actual_pounds, colour = "actual_pounds"))+ggtitle("Pounds_sold_for_EveryMonth")+theme(plot.title = element_text(lineheight=.8, face="bold"))








