rm(list=ls())
getwd()
setwd("C:/Users/ASUS/Desktop/Codes/Python/PROJECTS/P8-Time Series")
data=read.csv("india_shares.csv")
data
t=ts(data$Mobile,start=c(2009,1),frequency=12)
length(t)
d=data$Mobile
plot(t,main="Mobile share in Device Market",xlab="Years",ylab="Percentage of market share")
length(t)
pacf(data$Mobile)

_______________________________________________________________
#DURBIN WATSON TEST
x=1:180
fit=lm(d~x)
summary(fit)
install.packages("car")
library(car)
durbinWatsonTest(fit)
#presence of autocorrelation
________________________________________________________________
#DIVIDING INTO TRAINING AND TESTING DATASET
train=data$Mobile[1:156]
test=data$Mobile[-(1:156)]
train_data=ts(train,start=c(2009,1),frequency=12)
test_data=ts(test,start=c(2022,1),frequency=12)
plot(train_data)
_______________________________________________________________
#Differencing the data
d1=diff(train_data,differences=1)
plot(d1,main="First order differenced series",xlab="Year",ylab="% of mobile share")
Box.test(d1, lag = 1,type ="Ljung-Box")
d2=diff(train_data,differences=2)
plot(d2,main="Second order differenced series",xlab="Year",ylab="% of mobile share")
Box.test(d2, lag = 1,type ="Ljung-Box")
#1st order diff is better
install.packages("tseries")
library(tseries)
adf.test(d1)
adf.test(d2)
adf.test(train_data)
____________________________________________________________________
#ACF PACF plot
acf(train,lag.max=100,main="ACF of Train data")
pacf(train,lag.max=100,main="PACF of Train data")
acf(d1,lag.max=100,main="ACF of first order differenced series")
acf(d2,lag.max=100,main="ACF of second order differenced series")

pacf(d1,lag.max=100)
___________________________________________________________________
#MODELS
model1=arima(train_data,order=c(0,1,1))
summary(model1)
model2=arima(train_data,order=c(0,2,0))
summary(model2)
model3=arima(train_data,order=c(0,1,0))
summary(model3)
model4=arima(train_data,order=c(0,2,1),include.mean=T)
summary(model4)

#model4 is best
install.packages("forecast")
library(forecast)
auto.arima(train_data,allowmean=T)
model4$aic
_____________________________________________________________________
#FITTING
fit.values=fitted(model4)
plot(train_data,main="Plot of training dataset and fitted values",ylab="Share of Mobile")
lines(fit.values,col="red")
legend('topleft',legend=c('Original','Fitted'),col=c('black','red'),lty=1)
_____________________________________________________________________
#Test for Residuals
install.packages("randtests")
library(randtests)
res=residuals(model4)
runs.test(res)
plot(fitted(model4),res,main="Residual Plot",xlab="Fitted Values",ylab="Residuals")
acf(res,lag.max=50,main="ACF plot of Model Residuals")
_______________________________________________________________________
#FORECAST
library(forecast)
forecast1=forecast(model4,h=length(test_data),level=95)
f2=as.data.frame(forecast1)
forc=ts(f2[1],start=c(2022,1),frequency=12)
UCL=ts(f2[3],start=c(2022,1),frequency=12)
LCL=ts(f2[2],start=c(2022,1),frequency=12)

plot(test_data,main="Plot of Actual vs Forecast",ylab="Share of Mobile",ylim=c(0,150))
lines(forc,col="red")
lines(UCL,col="blue")
lines(LCL,col="green")
legend('topleft',legend=c('Original','Forecast','Upper Confidence Limit','Lower Confidence Limit'),col=c('black','red','blue','green'),lty=1)
#MAPE calc
ei=abs(forc-test_data)/test_data
mean(ei)
