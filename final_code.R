# Final Script 

# Preparation

library(tidyverse)
library(xts)
library(forecast)
library(TSstudio)
library(urca)
library(strucchange)

setwd("/Users/allie/Desktop/Universität/Master/Predictive Analytics/Final/Data")
# read data 

interest = read_csv("interest.csv")
production = read_csv("production.csv")
ex_rate = read_csv("ex_rate.csv")

ex_rate$dates = as.Date(ex_rate$dates, "%Y-%d-%m")

# convert to time series object
ex_ts = xts_to_ts(xts(ex_rate$values, order.by = ex_rate$dates), frequency = 12)

############# Exchange Rate Time Series ########################

# summary statistics 
summary(ex_rate$values)
length(ex_rate$values)

# plot 
ex_ts %>%  
  autoplot() +
  xlab("Year") +
  ylab("Exchange Rate")

# autocorrelation 
acf(ex_ts) 
pacf(ex_ts)

# decompoositon of the time series 
ex_ts %>% 
  decompose(type="multiplicative") %>% 
  autoplot() +
  xlab("Year") +
  ggtitle("Classical multiplicative decomposition of the Effective Exchange Rate")


# unit root detection & stationarity tests

# type "mu" only because it is only a drift present
summary(ur.kpss(ex_ts, type = "tau"))

# test statistik is 0.477 
# larger than 0.05 
# the data is not stationary 

# test with trend 
summary(ur.df(ex_ts,type = "trend"))

# accept the null hypothesis for tau2 (larger than 5pct level) and reject for phi1 (smaller than 5%)
# test without drift or trend
summary(ur.df(ex_ts, type = "none"))

# reject the null hypothesis 

# Differencing of tge Exchange Rate Time Series

ex_diff = ex_ts %>% 
  diff()

ex_diff %>%  
  autoplot() +
  xlab("Time") +
  ylab("Differenced Exchange Rate")

# autocorrelation
acf(ex_diff)
pacf(ex_diff)

# test of autocorrelation
Box.test(ex_diff, type = "Ljung-Box", lag = 2)
Box.test(ex_ts, type = "Ljung-Box", lag = 2)

# decomposition of time series
ex_diff %>% 
  decompose(type="multiplicative") %>% 
  autoplot() +
  xlab("Year") +
  ggtitle("Classical multiplicative decomposition of the transformed Effective Exchange Rate")

# unit root and stationarity tests 

summary(ur.kpss(ex_diff, type = "tau"))

# test statistik is 0.0515 
# larger than 0.05 
# the data is still not stationary

# test with differenced time series, and type drift
summary(ur.df(ex_diff, type = "trend"))
# trend coefficient not significant 

# testing for structural breaks 
# ex_ts
test_ex_ts = cbind(Lag0 = ex_ts,
                   Lag1 = stats::lag(ex_ts,-1))

qlr = Fstats(Lag0 ~ Lag1, data = test_ex_ts, from = 0.15)
plot(qlr)
test = sctest(qlr, type = "supF")  
test
breakpoints(qlr, alpha = 0.05)
plot(qlr, alpha = 0.1, main = "F Statistics")
lines(breakpoints(qlr))    

# ex_diff

test_ex_diff = cbind(Lag02 = ex_diff,
                     Lag12 = stats::lag(ex_diff,-1))

qlr2 = Fstats(Lag02 ~ Lag12, data = test_ex_diff, from = 0.15)
plot(qlr2)
test2 = sctest(qlr2, type = "supF")  
test2
breakpoints(qlr2, alpha = 0.05)
plot(qlr2, alpha = 0.1, main = "F Statistics")
lines(breakpoints(qlr2))    


# adjust rows for dynamic regression 
ex_ts_adj = window(ex_ts, start = c(1999,1), end = c(2020,12))
ex_adj_diff = ex_ts_adj %>% 
  diff()


############# Interest Rate ################################

# prep interest 
interest$dates = as.Date(interest$dates, "%Y-%d-%m")
int_ts = xts_to_ts(xts(interest$values, order.by = interest$dates), frequency = 12)
int_ts = window(int_ts, end = c(2020,12))

# plot
int_ts %>%  
  autoplot() +
  xlab("Year") +
  ylab("Interest Rate")

# stationarity test 
# trend 
summary(ur.df(int_ts, type = "trend")) 
# in conclusive, unit root may not be present, only accept for some coefficients
summary(ur.kpss(int_ts, type = "tau"))
# accept null hypothesis 
# data is stationary 
# results inconclusive-> we difference and test whether this improves the results


# difference
int_diff = int_ts %>% 
  diff()

# repeat tests 
summary(ur.df(int_diff, type = "trend")) 
summary(ur.kpss(int_diff, type = "tau"))
# reject the null hypothesis 
# drift
summary(ur.df(int_diff, type ="drift"))
# reject
summary(ur.df(int_diff, type = "none"))
# reject 
# we use the original time series as this is considered stationary in kpss test (and unit root inconclusive)
# unlike differenced time series 

# testing for structural breaks 
# int_ts
test_int_ts = cbind(Lag0 = int_ts,
                    Lag1 = stats::lag(int_ts,-1))

qlr = Fstats(Lag0 ~ Lag1, data = test_int_ts, from = c(1993,1))
plot(qlr)
test = sctest(qlr, type = "supF")  
test
breakpoints(qlr, alpha = 0.05)
plot(qlr, alpha = 0.1, main = "F Statistics")
lines(breakpoints(qlr))    

# int_diff

test_int_diff = cbind(Lag02 = int_diff,
                      Lag12 = stats::lag(int_diff,-1))

qlr2 = Fstats(Lag02 ~ Lag12, data = test_int_diff, from = 0.15)
plot(qlr2)
test2 = sctest(qlr2, type = "supF")  
test2
breakpoints(qlr2, alpha = 0.05)
plot(qlr2, alpha = 0.1, main = "F Statistics")
lines(breakpoints(qlr2))    

############# Industrial Production #######################

# prep production 
production$dates = as.Date(production$dates, "%Y-%d-%m")
prod_ts = xts_to_ts(xts(production$values, order.by = production$dates), frequency = 12)
prod_ts = window(prod_ts, start = c(1999,1), end = c(2020,12)) 

# plot
prod_ts %>% 
  autoplot() +
  xlab("Year") +
  ylab("Industrial Production")

# test for unit root & stationarity 
#we test for stationarity and unit root with drift,no trend 
summary(ur.df(prod_ts, type = "drift")) 
# reject null for tau2, accept for phi1 
summary(ur.df(prod_ts, type = "trend")) # test with trend
# unit root is present when considering trend 
# inconclusive, unit root may not be present, only accept for some coefficients
summary(ur.kpss(prod_ts, type = "tau"))
# larger than 5%, accept null hypothesis: data is stationary

# difference
prod_diff = prod_ts %>% 
  diff()

# test again 
summary(ur.kpss(prod_diff, type = "tau"))
# accept null, time series is stationary 

# Testing for structural breaks

# prod_ts
test_prod_ts = cbind(Lag0 = prod_ts,
                     Lag1 = stats::lag(prod_ts,-1))

qlr = Fstats(Lag0 ~ Lag1, data = test_prod_ts, from = c(1993,1))
plot(qlr)
test = sctest(qlr, type = "supF")  
test
breakpoints(qlr, alpha = 0.05)
plot(qlr, alpha = 0.1, main = "F Statistics")
lines(breakpoints(qlr))    

# prod_diff

test_prod_diff = cbind(Lag02 = prod_diff,
                       Lag12 = stats::lag(prod_diff,-1))

qlr2 = Fstats(Lag02 ~ Lag12, data = test_prod_diff, from = 0.15)
plot(qlr2)
test2 = sctest(qlr2, type = "supF")  
test2
breakpoints(qlr2, alpha = 0.05)
plot(qlr2, alpha = 0.1, main = "F Statistics")
lines(breakpoints(qlr2))  

############# Forecasts: ARIMA ############################


ex_diff_short = window(ex_diff, start = c(2014,6))
# this part of code is adjusted with the models described in Table 1. 
arima_model = Arima(ex_diff, order = c(1,0,1), seasonal = c(1,0,0))
summary(arima_model)
checkresiduals(arima_model)
arima_model = auto.arima(ex_diff)
arima_model
# returns model ARIMA(0,0,1)(1,0,0)[12] with zero mean 
checkresiduals(arima_model)
# residuals look normally distributed around zero mean
# no significant spikes in the acf plot 
# p value is very large, therefore we accept the null hypothesis, the residuals are not autocorrelated 
# so we conclude that the residuals are white noise, and the model seems to capture the dynamic in the data well

# forecast with an arima model 

# train - test split 

train = window(ex_diff_short, end = c(2020, 6))
test = window(ex_diff_short, start = c(2020,7), end = c(2020,12))
fit = Arima(train, c(0,0,1),seasonal = c(1,0,0))
fc_arima = forecast(fit, h = 9)

# plot forecast
autoplot(fc_arima) +
  autolayer(ex_diff)+
  ggtitle("Arima forecasts for the Effective Exchange Rate") +
  xlab("Year") +
  ylab("Exchange Rate (differenced)")

accuracy(fc_arima, test)


############# Forecasts: Dynamic Regression ############################

# Dynamic Regression Model
model_dr_2 = Arima(ex_adj_diff, xreg = cbind(int_diff, prod_diff), order = c(0,0,1), seasonal = c(1,0,0))
summary(model_dr_2)  

model_dr_4 =Arima(ex_adj_diff, xreg = int_diff, order = c(0,0,1), seasonal = c(1,0,0))
summary(model_dr_4) 

model_dr_6 =Arima(ex_adj_diff, xreg = prod_diff, order = c(0,0,1), seasonal = c(1,0,0))
summary(model_dr_6) 

model_dr_7 = auto.arima(ex_adj_diff, xreg = cbind(int_diff, prod_diff), ic = 'bic' )
summary(model_dr_7)

model_dr_8 = auto.arima(ex_adj_diff, xreg = int_diff, ic = 'bic' )
summary(model_dr_8)

model_dr_9 = auto.arima(ex_adj_diff, xreg = prod_diff, ic = 'bic' )
summary(model_dr_9)


checkresiduals(model_dr_2)
# no significant auto correlation, heteroskedacity, appears to be skewed to the left 
checkresiduals(model_dr_4)
# no significant auto correlation, heteroskedacity, appears to be normally distributed
checkresiduals(model_dr_6)
# no significant auto correlation, heteroskedacity, appears to be normally distributed
checkresiduals(model_dr_7)
# no significant autocorrelation, heteroskedacity, skewed to the left
checkresiduals(model_dr_8)
# no significant autocorrelation, heteroskedacity, kind of normally distributed
checkresiduals(model_dr_9)
# no significant autocorrelation, heteroskedacity, skewed to the left

# forecast with the dynamic regression model 
train = window(ex_diff_short, end = c(2020, 6))
xreg_train = window(prod_diff, start = c(2014,6), end = c(2020,6))
xreg_test = window(prod_diff,start = c(2020,6))
test = window(ex_diff_short, start = c(2020,7), end = c(2021,1))

fit = Arima(train, xreg = xreg_train, order = c(0,0,1))
fc_dr <- forecast(fit, xreg = xreg_test, h = 7)

# adjust ex_diff_short for plot 
ex_diff_short = window(ex_diff_short, end = c(2021,1))
# plot forecast
autoplot(fc_dr) +
  autolayer(ex_diff_short)+
  ggtitle("Dynamic Regression forecasts for the Effective Exchange Rate") +
  xlab("Year") +
  ylab("Exchange Rate (differenced)")

accuracy(fc_dr, test)
