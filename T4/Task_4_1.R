library(quantmod)
library(tseries)
library(timeSeries)
library(forecast)
library(xts)

#Obtaining historical stock prices prices yahoo finance from 1998 to current date
getSymbols('^BSESN',from="1998-01-01",to="2020-10-14")

class(BSESN)

Sensex<-BSESN[,4]
Sensex<-na.approx(Sensex)
par(mfrow=c(1,1))
plot(Sensex)
par(mfrow=c(1,2))
Acf(Sensex,main="ACF for Differenced Series")
Pacf(Sensex,main="PACF for Differenced Series")

#PACF<-P value ACF<-Q Value

print(adf.test(Sensex))
auto.arima(Sensex,seasonal = F)

fitA<-auto.arima(Sensex,seasonal = F)
tsdisplay(residuals(fitA),lag.max = 40,main = "(0,1,0) Model Residuals")


fitB<-arima(Sensex,order = c(0,1,17))
tsdisplay(residuals(fitB),lag.max = 40,main = "(0,1,18) Model Residuals")

fitC<-arima(Sensex,order = c(0,2,37))
tsdisplay(residuals(fitC),lag.max = 40,main = "(0,2,37) Model Residuals")


fitD<-arima(Sensex,order = c(1,1,1))
tsdisplay(residuals(fitC),lag.max = 40,main = "(1,1,1) Model Residuals")
 
par(mfrow=c(2,2))
term<-100

fcast1<-forecast(fitA,h=term)
plot(fcast1)

fcast2<-forecast(fitB,h=term)
plot(fcast2)

fcast3<-forecast(fitC,h=term)
plot(fcast3)

fcast4<-forecast(fitD,h=term)
plot(fcast4)

#Check Accuracy of the forecasts
accuracy(fcast1)
accuracy(fcast2)
accuracy(fcast3)
accuracy(fcast4)


term<-45

fcast1<-forecast(fitA,h=term)
plot(fcast1)

fcast2<-forecast(fitB,h=term)
plot(fcast2)

fcast3<-forecast(fitC,h=term)
plot(fcast3)

fcast4<-forecast(fitD,h=term)
plot(fcast4)

#Check Accuracy of the forecasts
accuracy(fcast1)
accuracy(fcast2)
accuracy(fcast3)
accuracy(fcast4)


term<-365

fcast1<-forecast(fitA,h=term)
plot(fcast1)

fcast2<-forecast(fitB,h=term)
plot(fcast2)

fcast3<-forecast(fitC,h=term)
plot(fcast3)

fcast4<-forecast(fitD,h=term)
plot(fcast4)

#Check Accuracy of the forecasts
accuracy(fcast1)
accuracy(fcast2)
accuracy(fcast3)
accuracy(fcast4)