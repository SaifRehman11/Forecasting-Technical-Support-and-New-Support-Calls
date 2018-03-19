library(TStools)
library(forecast)

x <- read.csv("31781792.csv", header = TRUE)
x.sc <- ts(x$supportcalls, frequency = 7)
x.nc <- ts(x$newcalls, frequency = 7)

frequency(x.sc)
frequency(x.nc)

plot(x.sc, main = "Technical Support Calls")

hist(x.sc)

#Exploring Support Calls
x.sc.cma <- cmav(x.sc, ma = 7, fill = FALSE)
plot(x.sc, col = "blue", main = "7-day Moving Average Depicting trend of Technical Support Calls")
lines(x.sc.cma, col = "red")

plot(x.sc)

summary(x.sc)
hist(x.sc)
summary(x.nc)
hist(x.nc)

seasplot(x.sc )

#x.sc.seas <- seasplot(x.sc, )

#Doing a multiplicative decomposition of the Support Calls 
decompscm <- decomp(x.sc, decomposition = "multiplicative", outplot = TRUE)

str(decompscm)
decompscm$irregular


regular_components <- decompscm$trend * decompscm$season

mmm_errors <- x.sc - regular_components

decompscm$irregular - mmm_errors

#Doing a additive decomposition of the Support Calls
decompsca <- decomp(x.sc, decomposition = "additive", outplot = TRUE)

str(decompsca)
decompsca$irregular

decompsca$trend

decompsca$season

regular_components <- decompsca$trend + decompsca$season

add_errors <- x.sc - regular_components

decompsca$irregular - add_errors


#Multiplicative
plot(decompose(x.sc, type = "m"))
#And additive seasonality
plot(decompose(x.sc, type = "a"))




############Exploring New Calls

plot(x.nc, main = "New Subscription Calls")


x.nc.cma <- cmav(x.nc, ma = 7, fill = FALSE)
plot(x.nc, col = "blue")
lines(x.nc.cma, col = "red")


seasplot(x.nc)

#x.sc.seas <- seasplot(x.sc, )

#Doing a multiplicative decomposition of the Support Calls 
decompncm <- decomp(x.nc, decomposition = "multiplicative", outplot = TRUE)

str(decompncm)
decompncm$irregular

decompncm$trend

decompncm$season

regular_components <- decompncm$trend * decompncm$season

mmm_errors <- x.nc - regular_components

decompncm$irregular - mmm_errors

#Doing a additive decomposition of the Support Calls
decompnca <- decomp(x.nc, decomposition = "additive", outplot = TRUE)

str(decompnca)
decompnca$irregular

decompnca$trend



regular_components <- decompnca$trend + decompnca$season

add_errors <- x.nc - regular_components

decompnca$irregular - mmm_errors

#Multiplicative
plot(decompose(x.nc, type = "m"))
#And additive seasonality
plot(decompose(x.nc, type = "a"))



length(x$supportcalls)


#Support Calls

#Find the total number of observations 
sclength <- length(x.sc)
#Size of train set
train_length <- 42
#And the forecasting horizon
h <- 7
#Create the training set
sc.train <- ts(x.sc[1:train_length], frequency = 7)
#Create the test set
sc.test <- ts(x.sc[(train_length+1):sclength], frequency = 7, start = 7)

#Find the total number of observations 
nclength <- length(x.nc)
#Size of train set
train_length <- 42
#And the forecasting horizon
h <- 7
#Create the training set
nc.train <- ts(x.nc[1:train_length], frequency = 7)
#Create the test set
nc.test <- ts(x.nc[(train_length+1):nclength], frequency = 7, start = 7)




#Fitting a naive

#Support Calls
naive_method <- naive(sc.train, h=h)

#naive_forecast <- forecast(naive_method, h=h)$mean

plot(x = 4, y = 20000, type = "n", main = "Naive Method Forecast for Support Calls", xlim = c(1,8), ylim = range(x.sc))
lines(sc.train)
lines(naive_method$fitted, col = "red")
lines(naive_method$mean, col = "blue")
lines(sc.test, col = "dark green")

accuracy(f = naive_method$fitted, x = sc.train)

accuracy(f = naive_method$mean, x = sc.test)



#Fitting a naive

#New Subscription Calls
naive_method <- naive(nc.train, h=h)

#naive_forecast <- forecast(naive_method, h=h)$mean

plot(x = 4, y = 20000, type = "n", main = "Naive Method Forecast for New Subscription Calls", xlim = c(1,8), ylim = range(x.nc))
lines(nc.train)
lines(naive_method$fitted, col = "red")
lines(naive_method$mean, col = "blue")
lines(nc.test, col = "dark green")

accuracy(f = naive_method$fitted, x = nc.train)

accuracy(f = naive_method$mean, x = nc.test)

x <- 7 + c(0,1/7,2/7,3/7,4/7,5/7,6/7)
x


###Doing smooth moving average

#Support Calls

SMA.sc <- ma(sc.train, order = 3, centre = FALSE)

#Firstly we get rid of NA values
SMA.sc.nona <- SMA.sc[!is.na(SMA.sc)]

SMA.sc.nonats <- ts(SMA.sc.nona, frequency = 7)
#Then form a forcast
SMA.sc.forecast <- ts(rep(SMA.sc.nona[length(SMA.sc.nona)], 7), frequency = 7, start = 7)

plot(x = 4, y = 20000, type = "n", main = "Smooth Moving Average for Support Calls", xlim = c(1,8), ylim = range(x.sc))
lines(sc.train)
lines(SMA.sc.nonats, col = "red")
lines(SMA.sc.forecast, col = "blue")
lines(sc.test, col = "dark green")

#In Sample Accuracy
#accuracy(f = naive_method$fitted, x = sc.train)
#Holdout Sample Accuracy
accuracy(f = SMA.sc.forecast, x = sc.test)

#7 order SMA
SMA.sc <- ma(sc.train, order = 7, centre = FALSE)

#Firstly we get rid of NA values
SMA.sc.nona <- SMA.sc[!is.na(SMA.sc)]

SMA.sc.nonats <- ts(SMA.sc.nona, frequency = 7)
#Then form a forcast
SMA.sc.forecast <- ts(rep(SMA.sc.nona[length(SMA.sc.nona)], 7), frequency = 7, start = 7)

plot(x = 4, y = 20000, type = "n", main = "Smooth Moving Average for Support Calls", xlim = c(1,8), ylim = range(x.sc))
lines(sc.train)
lines(SMA.sc.nonats, col = "red")
lines(SMA.sc.forecast, col = "blue")
lines(sc.test, col = "dark green")

#In Sample Accuracy
#accuracy(f = naive_method$fitted, x = sc.train)
#Holdout Sample Accuracy
accuracy(f = SMA.sc.forecast, x = sc.test)




SMA.sc.errors <- sc.test - SMA.sc.forecast

SMA.sc.ME <- mean(SMA.sc.errors)
SMA.sc.MSE <- mean(SMA.sc.errors^2)
SMA.sc.MAE <- mean(abs(SMA.sc.errors))
SMA.sc.MAPE <- 100 * mean(abs(SMA.sc.errors)/sc.test)


#Doing SMA for New Call Subscriptions


SMA.nc <- ma(nc.train, order = 3, centre = FALSE)

#Firstly we get rid of NA values
SMA.nc.nona <- SMA.nc[!is.na(SMA.nc)]


SMA.nc.nonats <- ts(SMA.nc.nona, frequency = 7)
#Then form a forcast
SMA.nc.forecast <- ts(rep(SMA.nc.nona[length(SMA.nc.nona)], 7), frequency = 7, start = 7)


plot(x = 4, y = 20000, type = "n", main = "Smooth Moving Average for New Subscription Calls", xlim = c(1,8), ylim = range(x.nc))
lines(nc.train)
lines(SMA.nc.nonats, col = "red")
lines(SMA.nc.forecast, col = "blue")
lines(nc.test, col = "dark green")



#In Sample Accuracy
#accuracy(f = naive_method$fitted, x = sc.train)
#Holdout Sample Accuracy
accuracy(f = SMA.nc.forecast, x = nc.test)



#7-order
SMA.nc <- ma(nc.train, order = 7, centre = FALSE)

#Firstly we get rid of NA values
SMA.nc.nona <- SMA.nc[!is.na(SMA.nc)]


SMA.nc.nonats <- ts(SMA.nc.nona, frequency = 7)
#Then form a forcast
SMA.nc.forecast <- ts(rep(SMA.nc.nona[length(SMA.nc.nona)], 7), frequency = 7, start = 7)


plot(x = 4, y = 20000, type = "n", main = "Smooth Moving Average(7-order) for New Subscription Calls", xlim = c(1,8), ylim = range(x.nc))
lines(nc.train)
lines(SMA.nc.nonats, col = "red")
lines(SMA.nc.forecast, col = "blue")
lines(nc.test, col = "dark green")



#In Sample Accuracy
#accuracy(f = naive_method$fitted, x = sc.train)
#Holdout Sample Accuracy
accuracy(f = SMA.nc.forecast, x = nc.test)



SMA.nc.errors <- nc.test - SMA.nc.forecast

SMA.nc.ME <- mean(SMA.nc.errors)
SMA.nc.MSE <- mean(SMA.nc.errors^2)
SMA.nc.MAE <- mean(abs(SMA.nc.errors))
SMA.nc.MAPE <- 100 * mean(abs(SMA.nc.errors)/nc.test)




#Exponential Smoothing

#Support Calls

ETS_ANN_0.15 <- ets(sc.train, model = "AAA", alpha = 0.15)
ETS_ANN_0.15
plot(ETS_ANN_0.15)

plot(x = 4, y = 20000, type = "n", main = "Exponential Smoothing for Technical Support Calls", xlim = c(1,8), ylim = range(x.sc))
lines(sc.train)
lines(ETS_ANN_0.15$fitted, col = "red")
lines(forecast(ETS_ANN_0.15, h=h)$mean, col = "blue")
lines(sc.test, col = "dark green")


accuracy(forecast(ETS_ANN_0.15, h=h)$mean, sc.test)


plot(ETS_ANN_0.15$residuals)




ets(sc.train, "MNN", alpha = 0.15)



ETS_ZZZ_opt = ets(sc.train, "ZZZ")
coef(ETS_ZZZ_opt)
plot(ETS_ZZZ_opt)
accuracy(forecast(ETS_ZZZ_opt, h=h)$mean, sc.test)


plot(x = 4, y = 20000, type = "n", main = "Exponential Smoothing for Technical Support Calls", xlim = c(1,8), ylim = range(x.sc))
lines(sc.train)
lines(ETS_ZZZ_opt$fitted, col = "red")
lines(forecast(ETS_ZZZ_opt, h=h)$mean, col = "blue")
lines(sc.test, col = "dark green")



ETS_ANN_0.15_forecast <- forecast(ETS_ANN_0.15, h=h)$mean
plot(forecast(ETS_ANN_0.15), h=h)


ES_ANN_0.15 <- es(sc.train, "ANN", persistance = 0.15, h=h)
ES_ANN_0.15

ES_ANN_opt <- es(sc.train, "ANN", h=6, silent = "a")
coef(ES_ANN_opt)

ES_ANN_opt_forecast <- forecast(ES_ANN_opt, h=h)$mean


####Add Performance measurements Later



#New Subscription Calls

ETS_AAA_0.15 <- ets(nc.train, model = "AAA", alpha = 0.15)
ETS_AAA_0.15
plot(ETS_ANN_0.15$residuals)
plot(ETS_ANN_0.15$fitted)


plot(x = 4, y = 20000, type = "n", main = "Exponential Smoothing (AAA, 0.15) for New Subscription Calls", xlim = c(1,8), ylim = range(x.nc))
lines(nc.train)
lines(ETS_AAA_0.15$fitted, col = "red")
lines(forecast(ETS_AAA_0.15, h=h)$mean, col = "blue")
lines(nc.test, col = "dark green")


accuracy(forecast(ETS_AAA_0.15, h=h)$mean, nc.test)


ETS_ZZZ_opt = ets(nc.train, "ZZZ")

plot(x = 4, y = 20000, type = "n", main = "Exponential Smoothing (ZZZ) for New Subscription Calls", xlim = c(1,8), ylim = range(x.nc))
lines(nc.train)
lines(ETS_ZZZ_opt$fitted, col = "red")
lines(forecast(ETS_ZZZ_opt, h=h)$mean, col = "blue")
lines(nc.test, col = "dark green")


accuracy(forecast(ETS_ZZZ_opt, h=h)$mean, nc.test)





ets(nc.train, "MNN", alpha = 0.15)

ETS_ANN_opt = ets(nc.train, "ANN")
coef(ETS_ANN_opt)

ETS_ANN_0.15_forecast <- forecast(ETS_ANN_0.15, h=h)$mean
plot(forecast(ETS_ANN_0.15), h=h)


ES_ANN_0.15 <- es(nc.train, "ANN", persistance = 0.15, h=h)
ES_ANN_0.15

ES_ANN_opt <- es(nc.train, "ANA", h = 7 )
lines(nc.test)
coef(ES_ANN_opt)

ES_ANN_opt_forecast <- forecast(ES_ANN_opt, h=h)$mean


####Add Performance measurements Later


###Doing rolling origin method

##Support Calls


#Set horizon and number of rolling origins
h <- 7
origins <- 4
sclength <- length(x.sc)

sc.train.length <- sclength - h - origins + 1
sc.test.length <- h + origins - 1

sc.roll.train <- ts(x.sc[1:sc.train.length], 
                         frequency = frequency(x.sc))

sc.roll.test <- x.sc[(sc.train.length+1):sclength]

sc.forecasts <- matrix(NA, nrow = origins, ncol = h)
sc.holdout <- matrix(NA, nrow = origins, ncol = h)

colnames(sc.forecasts) <- paste0("horizon",c(1:h))
rownames(sc.forecasts) <- paste0("origin", c(1:origins))

dimnames(sc.holdout) <- dimnames(sc.forecasts)

View(sc.holdout)


for(i in 1:origins) {
  #Create a ts object out of the medium noise data
  our_train_set <- ts(x.sc[1:(sc.train.length+i-1)],
                      frequency = frequency(x.sc))
  
  #Write down the holdout values from the test set
  sc.holdout[i,] <- sc.roll.test[i-1+(1:h)]
  
  #Produce forecasts and write them down
  sc.forecasts[i,] <- forecast(ets(our_train_set, "AAA"),h=h)$mean
}

#MAE for each horizon
colMeans(abs(sc.holdout - sc.forecasts))



###SES###

#Fit SES with fixed intial seed 
es_ANN_initial_1 <- es(x.sc, model = "AAA", initial = x.sc[1],
                       h=h, holdout = TRUE)

es_ANN_initial_1$accuracy

accuracy(es_ANN_initial_1$forecast, sc.test)
#Fit SES with optimized seed
es_ANN_opt <- es(x.sc, model = "ANN", h=h, holdout= TRUE)

es_ANN_opt$accuracy

accuracy(es_ANN_opt$forecast, sc.test)

#Benchmarking
#Fit SES with optimized seed 

sc.naive <- es(x.sc, model = "ANN", persistance = 1, 
                         h=h, holdout = TRUE)

sc.naive$accuracy

accuracy(sc.naive$forecast, sc.test)

##Other SES methods, Holt's method

#This is meant for data with trend so check if it is true

#Calculate Holt Method
ets_ANN <- ets(sc.train, model = "AAN")
ets_ANN

coef(ets_ANN)

forecast(ets_ANN, h=h)$mean

plot(forecast(ets_ANN, h=h))

#Calculate a Damped Holt Method 
ets_AAdn <- ets(sc.train, model = "AAN", damped = TRUE)

ets_AAdn

#Fit a holt's method , no damped trend
ets(sc.train, model = "AAN", damped = FALSE)


es_AAdn <- es(x.sc, model = "AAdN", h=h, holdout = TRUE)


##Holt-Winters

#This is meant for trend seasonal data so check if that is true


#Fit a model using ets()
ets_AAA <- ets(sc.train, model = "AAA", damped = FALSE)
#do the same thing using es():
es_AAA <- es(sc.train, model = "AAA", h=h)

ets_AAA
es_AAA



#Selecting best model based on optimization

#calculate an optimized ETS method using ets()
ets_ZZZ <- ets(sc.train, model = "ZZZ")
#Do the same thing using es()
es_ZZZ <- es(sc.train, model = "ZZZ")


accuracy(es_ZZZ$forecast, sc.test)


#Select the most appropriate non-seasonal model with ets()
ets_ZZN <- ets(sc.train, model = "ZZN")
#Do the same thing with es()
es_ZZN <- es(sc.train, model = "ZZN", silent = "a")




##New Subscription Calls


#Set horizon and number of rolling origins
h <- 7
origins <- 4
nclength <- length(x.nc)

nc.train.length <- nclength - h - origins + 1
nc.test.length <- h + origins - 1

nc.roll.train <- ts(x.nc[1:nc.train.length], 
                    frequency = frequency(x.nc))

nc.roll.test <- x.nc[(nc.train.length+1):nclength]

nc.forecasts <- matrix(NA, nrow = origins, ncol = h)
nc.holdout <- matrix(NA, nrow = origins, ncol = h)

colnames(nc.forecasts) <- paste0("horizon",c(1:h))
rownames(nc.forecasts) <- paste0("origin", c(1:origins))

dimnames(nc.holdout) <- dimnames(nc.forecasts)

View(nc.holdout)


for(i in 1:origins) {
  #Create a ts object out of the medium noise data
  our_train_set <- ts(x.nc[1:(nc.train.length+i-1)],
                      frequency = frequency(x.nc))
  
  #Write down the holdout values from the test set
  nc.holdout[i,] <- nc.roll.test[i-1+(1:h)]
  
  #Produce forecasts and write them down
  nc.forecasts[i,] <- forecast(ets(our_train_set, "ANN"),h=h)$mean
}

#MAE for each horizon
colMeans(abs(nc.holdout - nc.forecasts))



###SES###

#Fit SES with fixed intial seed 
es_ANN_initial_1 <- es(x.nc, model = "ANN", initial = x.nc[1],
                       h=h, holdout = TRUE)

es_ANN_initial_1$accuracy

#Fit SES with optimized seed
es_ANN_opt <- es(x.nc, model = "ANN", h=h, holdout= TRUE)

es_ANN_opt$accuracy

#Benchmarking
#Fit SES with optimized seed 

nc.naive <- es(x.nc, model = "ANN", persistance = 1, 
               h=h, holdout = TRUE)

nc.naive$accuracy


##Other SES methods, Holt's method

#This is meant for data with trend so check if it is true

#Calculate Holt Method
ets_ANN <- ets(nc.train, model = "AAN")
ets_ANN

coef(ets_ANN)

forecast(ets_ANN, h=h)$mean

plot(forecast(ets_ANN, h=h))

#Calculate a Damped Holt Method 
ets_AAdn <- ets(nc.train, model = "AAN", damped = TRUE)

ets_AAdn

#Fit a holt's method , no damped trend
ets_AAdn <- ets(nc.train, model = "AAN", damped = FALSE)


es_AAdn <- es(x.nc, model = "AAdN", h=h, holdout = TRUE)

accuracy(forecast(ets_AAdn, h=h)$mean, nc.test)

accuracy(es_AAdn$forecast, nc.test)

##Holt-Winters

#This is meant for trend seasonal data so check if that is true


#Fit a model using ets()
ets_AAA <- ets(nc.train, model = "AAA", damped = FALSE)
#do the same thing using es():
es_AAA <- es(nc.train, model = "AAA", h=h)

ets_AAA
es_AAA

accuracy(es_AAA$forecast, nc.test)


#Selecting best model based on optimization

#calculate an optimized ETS method using ets()
ets_ZZZ <- ets(nc.train, model = "ZZZ")
#Do the same thing using es()
es_ZZZ <- es(nc.train, model = "ZZZ")

#Select the most appropriate non-seasonal model with ets()
ets_ZZN <- ets(nc.train, model = "ZZN")
#Do the same thing with es()
es_ZZN <- es(nc.train, model = "ZZN", silent = "a")



#Stationary tests

#KPSS
kpss.test(x.sc)
#since p-value smaller than printed p-value, we can reject null hypothesis that the data is 
#stationary and conclude that the data is not stationary

#ADF
adf.test(x.sc)

#This is a test to test with the null being that the data is stationary and that the alternative
#is that the data is not stationary
#since p-value is 0.19, we can't reject the null so it shows that the data is stationary




#KPSS
kpss.test(x.nc)
#since p-value smaller than printed p-value, we can reject null hypothesis that the data is 
#stationary and conclude that the data is not stationary

#ADF
adf.test(x.nc)

#This is a test to test with the null being that the data is stationary and that the alternative
#is that the data is not stationary
#since p-value is 0.19, we can't reject the null so it shows that the data is stationary




##Differencing

#Support Calls
diff_sc <- diff(sc.train)

plot(diff_sc)


#taking two differences

diff_sc1 <- diff(sc.train, differences = 2)

plot(diff_sc1)


diff_sc2 <- diff(sc.train, lag = 7)

plot(diff_sc2)


#Plot ACF and PACF of first differences
tsdisplay(diff(diff_sc2))

tsdisplay(x.sc)

#Plot ACF and PACF of first and seasonal differences
tsdisplay(diff(diff(sc.train),lag=7))


#New subscription calls
diff_nc <- diff(nc.train)

plot(diff_nc)


#taking two differences

diff_nc1 <- diff(nc.train, differences = 2)

plot(diff_nc1)


diff_nc2 <- diff(nc.train, lag = 7)

plot(diff_nc2)



#Plot ACF and PACF of first differences
tsdisplay(diff(diff_nc))

#Plot ACF and PACF of first and seasonal differences
tsdisplay(diff(diff(nc.train),lag=7))



#Box Jenkins Methodology

#Support Calls
#Fitting ARIMA

#KPSS
kpss.test(x.sc)
#since p-value smaller than printed p-value, we can reject null hypothesis that the data is 
#stationary and conclude that the data is not stationary

#ADF
adf.test(x.sc)

#This is a test to test with the null being that the data is stationary and that the alternative
#is that the data is not stationary
#since p-value is 0.19, we can't reject the null so it shows that the data is stationary




tsdisplay(sc.train)
tsdisplay(diff(sc.train, lag = 7))

#Calculate ARIMA(2,1,0)
fit1 <- Arima(sc.train, order=c(2,1,0))
#Plot series, ACF, and PACF of the residuals
tsdisplay(residuals(fit1))
summary(fit1)


#Fitting model manually, adding AR term
fit2 <- Arima(sc.train, order = c(0,0,0), seasonal = c(1,0,0))
tsdisplay(residuals(fit2))
summary(fit2)


Box.test(residuals(fit2), type = c("Box-Pierce", "Ljung-Box"))

Box.test(residuals(fit2), lag = 1, type = c("Ljung-Box"))


plot(x = 4, y = 20000, type = "n", main = "ARIMA(0,0,0)(1,0,0) for Technical Support Calls", xlim = c(1,8), ylim = range(x.sc))
lines(sc.train)
lines(fit2$x, col = "red")
lines(forecast(fit2, h=h)$mean, col = "blue")
lines(sc.test, col = "dark green")
forecast <- forecast(fit2, h=h)

accuracy(forecast(fit2, h=h)$mean, sc.test)


qqnorm(fit2$residuals)
qqline(fit2$residuals)

tsdisplay(fit2$residuals)


#Extracting coefficients
coef(fit1)

forecast(fit1, h=7)


qqnorm(fit1$residuals)
qqline(fit1$residuals)

qqnorm(fit2$residuals)
qqline(fit2$residuals)



#Estimate AUTO ARIMA
auto_fit <- auto.arima(sc.train)
summary(auto_fit)

plot(x = 4, y = 20000, type = "n", main = "Auto ARIMA for Technical Support Calls", xlim = c(1,8), ylim = range(x.sc))
lines(sc.train)
lines(auto_fit$x, col = "red")
lines(forecast(auto_fit, h=h)$mean, col = "blue")
lines(sc.test, col = "dark green")
forecast <- forecast(auto_fit, h=h)

accuracy(forecast(auto_fit, h=h)$mean, sc.test)



#Find best method via AIC 
auto.arima1 <- auto.arima(sc.train, ic = "aic")
summary(auto.arima1)
#Find the best method with ADF test 
auto.arima2 <- auto.arima(sc.train, test = "adf")
summary(auto.arima2)
#Plot ACF and PACF of first differences
tsdisplay(diff(diff_sc))



#Doing test using nsdiffs
nsdiffs(x.sc, test = "ocsb") #This is the recommended seasonal difference test
#shows that we should do 1 difference
nsdiffs(x.sc, test = "ch")
#shows that we should do 0 difference

ndiffs(x.sc, test = "adf")
#shows we should do 0 difference
ndiffs(x.sc, test = "pp")
#shows that we should do 0 differecne
ndiffs(x.sc, test = "kpss") #KPSS is the recommended test


arima2 <- Arima(sc.train, order= c(0,0,0), seasonal = c(0,0,1))
summary(arima2)

arima3 <- Arima(diff_sc, order= c(0,0,0), seasonal = c(1,0,0))
summary(arima3)

accuracy(forecast(arima2, h=h)$mean, sc.test)
accuracy(forecast(arima3, h=h)$mean, sc.test)


#SARIMA (1,1,0)(0,1,0)[12]

fit1 <- Arima(sc.train, order = c(1,1,0), seasonal = c(0,1,0))
tsdisplay(residuals(fit1))

Box.test(residuals(fit1), type = c("Box-Pierce", "Ljung-Box"))

Box.test(residuals(fit1), type = c("Ljung-Box"))



#Fitting auto arima
fit2 <- auto.arima(sc.train)
fit2$arma

#fitted values of autoarima
fitted(fit2)

summary(fit2)
summary(fit1)


qqnorm(fit1$residuals)
qqline(fit1$residuals)

qqnorm(fit2$residuals)
qqline(fit2$residuals)




#New Subscription Calls


#KPSS
kpss.test(x.nc)
#since p-value smaller than printed p-value, we can reject null hypothesis that the data is 
#stationary and conclude that the data is not stationary

#ADF
adf.test(x.nc)

#This is a test to test with the null being that the data is stationary and that the alternative
#is that the data is not stationary
#since p-value is 0.19, we can't reject the null so it shows that the data is stationary


tsdisplay(nc.train)
tsdisplay(diff_nc)
tsdisplay(diff_nc1)


kpss.test(diff_nc)
#since p-value smaller than printed p-value, we can reject null hypothesis that the data is 
#stationary and conclude that the data is not stationary

#ADF
adf.test(diff_nc)

#This is a test to test with the null being that the data is stationary and that the alternative
#is that the data is not stationary
#since p-value is 0.19, we can't reject the null so it shows that the data is stationary


diff_nc <- diff(nc.train)
diff_nc.test <- diff(nc.test)

#Fitting ARIMA

#Calculate ARIMA(2,1,0)
fit1 <- Arima(nc.train, order=c(1,1,1), seasonal = c(1,0,0))
#Plot series, ACF, and PACF of the residuals
tsdisplay(residuals(fit1))


Box.test(residuals(fit1), type = c("Box-Pierce", "Ljung-Box"))

Box.test(residuals(fit1), type = c("Ljung-Box"))



plot(x = 4, y = 20000, type = "n", main = "ARIMA(1,1,1)(1,0,0) for New Subscription Calls", xlim = c(1,8), ylim = range(x.nc))
lines(nc.train)
lines(fit1$x, col = "red")
lines(forecast(fit1, h=h)$mean, col = "blue")
lines(nc.test, col = "dark green")
forecast <- forecast(fit1, h=h)

accuracy(forecast(fit1, h=h)$mean, nc.test)


fit1 <- Arima(diff_nc, order=c(0,0,0), seasonal = c(1,0,1))
#Plot series, ACF, and PACF of the residuals
tsdisplay(residuals(fit1))



plot(x = 4, y = 20000, type = "n", main = "ARIMA(0,0,0)(1,0,0) for New Subscription Calls", xlim = c(1,8), ylim = range(diff_nc))
lines(diff_nc)
lines(fit1$x, col = "red")
lines(forecast(fit1, h=h)$mean, col = "blue")
lines(diff_nc.test, col = "dark green")
forecast <- forecast(fit1, h=h)

accuracy(forecast(fit1, h=h)$mean, diff_nc.test)






#Fitting model manually, adding AR term
fit2 <- Arima(nc.train, order = c(2,1,0))
tsdisplay(residuals(fit2))
summary(fit2)



#Extracting coefficients
coef(fit1)

forecast(fit1, h=7)


qqnorm(fit1$residuals)
qqline(fit1$residuals)

qqnorm(fit2$residuals)
qqline(fit2$residuals)



#Estimate AUTO ARIMA
auto_fit <- auto.arima(nc.train)
summary(auto_fit)
#Find best method via AIC 
arima1 <- auto.arima(nc.train, ic = "aic")
summary(arima1)
#Find the best method with ADF test 
arima2 <- auto.arima(nc.train, test = "adf")
summary(arima2)



Box.test(residuals(arima1), type = c("Box-Pierce", "Ljung-Box"))

Box.test(residuals(arima1), type = c("Ljung-Box"))



plot(x = 4, y = 20000, type = "n", main = "Auto ARIMA(1,1,1)(2,0,0) for New Subscription Calls", xlim = c(1,8), ylim = range(x.nc))
lines(nc.train)
lines(arima1$x, col = "red")
lines(forecast(arima1, h=h)$mean, col = "blue")
lines(nc.test, col = "dark green")

accuracy(forecast(auto_fit, h=h)$mean, nc.test)
accuracy(forecast(arima1, h=h)$mean, nc.test)
accuracy(forecast(arima2, h=h)$mean, nc.test)

tsdisplay(arima1$residuals)



#Doing test using nsdiffs
nsdiffs(x.nc, test = "ocsb") #This is the recommended seasonal difference test
#shows that we should do 1 difference
nsdiffs(x.nc, test = "ch")
#shows that we should do 0 difference

ndiffs(x.nc, test = "adf")
#shows we should do 0 difference
ndiffs(x.nc, test = "pp")
#shows that we should do 0 differecne
ndiffs(x.nc, test = "kpss") #KPSS is the recommended test





#SARIMA (1,1,0)(0,1,0)[12]

fit1 <- Arima(nc.train, order = c(1,1,0), seasonal = c(0,1,0))
tsdisplay(residuals(fit1))


#Fitting auto arima
fit2 <- auto.arima(nc.train)
fit2$arma

#fitted values of autoarima
fitted(fit2)

summary(fit2)
summary(fit1)


qqnorm(fit1$residuals)
qqline(fit1$residuals)

qqnorm(fit2$residuals)
qqline(fit2$residuals)


#### Linear Regression 

x.train <- x[1:41,]
x.test <- x[42:49,]
length(x.train$supportcalls)

plot(x)
cor(x)



#Fit a regression
lm1 <- lm(supportcalls ~ x1 + x2 + x3, data = x.train)

#Summary
summary(lm1)

#Extract standard error
sigma(lm1)


#Extract residuals 
lm1_resid <- residuals(lm1)

#Extract Residuals
lm1_fitted <- fitted(lm1)


#Plot Histogram
hist(lm1_resid)

#QQ Plot
qqnorm(lm1_resid)
qqline(lm1_resid)

#Jarque-bera test
jarque.bera.test(lm1_resid)

#Shapiro-Wilk test 
shapiro.test(lm1_resid)

#Kolmogorov-Smirnov test
ks.test(lm1_resid, y = "rnorm")

#Plot Residuals against Fitted Values
plot(lm1_fitted, lm1_resid)

#Plot Squared Residuals against Fitted Values
plot(lm1_fitted, lm1_resid^2)

#Plot Residuals against all x variables
pairs(lm1_resid ~ . , data = x.test[,3:5])

#Plot Residuals against Time
plot(lm1_resid, type = "l")


#Plot Residuals against Time
plot(lm1_resid, type = "l")
#Draw the mean line at 0 in Blue
abline(h=0, col = "blue")


#ACF and the PACF of the residuals 
tsdisplay(lm1_resid)


#Create studentized residuals 
lm1_resid_st <- rstandard(lm1)

#Plot the Residuals
plot(lm1_resid_st)
#Draw two horizontal lines at 2 and -2 
abline(h = c(-2,2), col = "red")

library(car)


#####Linear Regression

#Technical support calls

x.train <- x[1:42,]
x.test <- x[43:49,]
lm0 <- glm(supportcalls~1, data = x.train)
lm1 <- glm(supportcalls ~ x1 + x2 + x3, data = x.train)
summary(lm1)
Anova(lm1)



lm1_resid <- residuals(lm1)

lm1_fitted <- fitted(lm1)

tsdisplay(lm1_resid)



day1 <- rep(c(1,0,0,0,0,0,0),6)
day2 <- rep(c(0,1,0,0,0,0,0),6)
day3 <- rep(c(0,0,1,0,0,0,0),6)
day4 <- rep(c(0,0,0,1,0,0,0),6)
day5 <- rep(c(0,0,0,0,1,0,0),6)
day6 <- rep(c(0,0,0,0,0,1,0),6)

event <- c(0,0,0,0,0,0,0,
           0,0,0,0,0,0,0,
           0,0,0,0,0,0,0,
           1,1,1,1,1,1,1,
           1,1,1,1,1,1,1,
           1,1,1,1,1,1,1)

x.train <- cbind(x.train, day1,day2,day3,day4,day5,day6,event)


colnames(x.train) <- c("supportcalls", "newcalls","x1","x2","x3","day1", "day2", "day3", "day4", "day5", "day6", "event")

lm2 <- glm(supportcalls ~ x1 + x2 + x3 + day1 + day2 + day3 + day4 + day5+day6+event, data = x.train)
summary(lm2)
Anova(lm2)


lm2_resid <- resid(lm2)
tsdisplay(lm2_resid)

step1 <- step(object = lm0,scope = formula(lm2), direction = "both")
summary(step1)
tsdisplay(resid(step1))
mean(resid(step1))
hist(resid(step1))
plot(resid(step1), type = "l")
Box.test(residuals(step1), type = c("Box-Pierce", "Ljung-Box"))

Box.test(residuals(step1), lag = 1, type = c("Ljung-Box"))


vif(step1)


day1 <- rep(c(1,0,0,0,0,0,0),1)
day2 <- rep(c(0,1,0,0,0,0,0),1)
day3 <- rep(c(0,0,1,0,0,0,0),1)
day4 <- rep(c(0,0,0,1,0,0,0),1)
day5 <- rep(c(0,0,0,0,1,0,0),1)
day6 <- rep(c(0,0,0,0,0,1,0),1)

event <- c(1,1,1,1,1,1,1)


x.test <- cbind(x.test,day1,day2,day3,day4,day5,day6,event)
colnames(x.test) <- c("supportcalls", "newcalls","x1","x2","x3","day1", "day2", "day3", "day4", "day5", "day6", "event")

x.test.sc <- x.test[,c(1,3:ncol(x.test))]



predict1 <- predict(step1, x.test)

fitted1 <- ts(step1$fitted.values, frequency = 7)
pred1 <- ts(predict1,frequency =7, start = 7)

plot(x = 4, y = 20000, type = "n", main = "Linear Regression for Technical Support Calls", xlim = c(1,8), ylim = range(x.sc))
lines(sc.train)
lines(fitted1, col = "red")
lines(pred1, col = "blue")
lines(sc.test, col = "dark green")

accuracy(predict1, sc.test)





#New subsription Calls


glm0 <- glm(newcalls ~ 1, data = x.train)
glm1 <- glm(newcalls ~ x1 + x2 + x3, data = x.train)
summary(glm1)

tsdisplay(glm1$residuals)

glm2 <- glm(newcalls ~ x1 + x2 + x3 + day1 + day2 + day3+day4+day5+day6+event, data = x.train)
summary(glm2)


step2 <- step(glm0, formula(glm2), direction = "both")
summary(step2)


glm3 <- glm(newcalls ~ day1+day2+day3+day4+day5+day6+event, data = x.train)
Anova(glm3)
summary(glm3)
tsdisplay(glm3$residuals)

L1_newcalls <- lag(x.train.ts[,"newcalls"],k = -1)

x.train.colnames <- colnames(x.train)

x.train.ts <- cbind(x.train.ts,L1_newcalls)

x.train.ts <- ts(x.train, frequency = 7)

x.train.ts <- x.train.ts[,c(1:12,14)]

colnames(x.train.ts) <- c(x.train.colnames,"L1_newcalls")


glm4 <- glm(newcalls~ day1+day2+day3+day4+day5+day6+event +L1_newcalls, data = x.train)
summary(glm4)


glm5 <- glm(newcalls ~ event + L1_newcalls, data = x.train)
summary(glm5)



x.test.ts <- ts(x.test, frequency = 7, start = 7)

L1_newcalls.test <- lag(x.test.ts[,"newcalls"],k = -1)

x.test.ts <- cbind(x.test.ts,L1_newcalls.test)

x.test.ts <- ts(x.test.ts[1:7,], frequency = 7, start = 7)

colnames(x.test.ts) <- c(x.train.colnames, "L1_newcalls")
x.test.ts[1,"L1_newcalls.test"] <- 25837

predict1 <- predict(glm3, x.test.ts)

fitted1 <- ts(glm3$fitted.values, frequency = 7)
pred1 <- ts(predict1,frequency =7, start = 7)



plot(x = 4, y = 20000, type = "n", main = "Linear Regression for New Subscription Calls", xlim = c(1,8), ylim = range(x.nc))
lines(nc.train)
lines(fitted1, col = "red")
lines(pred1, col = "blue")
lines(nc.test, col = "dark green")


accuracy(predict1, nc.test)


tsdisplay(resid(glm5))





#Point Forecasts

#Technical Support Calls made using ARIMA(0,0,0)(1,0,0)

fit2 <- Arima(sc.train, order = c(0,0,0), seasonal = c(1,0,0))
tsdisplay(residuals(fit2))
summary(fit2)


plot(x = 4, y = 20000, type = "n", main = "ARIMA(0,0,0)(1,0,0) for Technical Support Calls", xlim = c(1,8), ylim = range(x.sc))
lines(sc.train)
lines(fit2$x, col = "red")
lines(forecast(fit2, h=h)$mean, col = "blue")
lines(sc.test, col = "dark green")
forecast <- forecast(fit2, h=21)

accuracy(forecast(fit2, h=h)$mean, sc.test)

forecast


#New Subscription Calls using ARIMA(1,1,1)(1,0,0)

#Calculate ARIMA(2,1,0)
fit1 <- Arima(nc.train, order=c(1,1,1), seasonal = c(1,0,0))
#Plot series, ACF, and PACF of the residuals
tsdisplay(residuals(fit1))



plot(x = 4, y = 20000, type = "n", main = "ARIMA(1,1,1)(1,0,0) for New Subscription Calls", xlim = c(1,8), ylim = range(x.nc))
lines(nc.train)
lines(fit1$x, col = "red")
lines(forecast(fit1, h=h)$mean, col = "blue")
lines(nc.test, col = "dark green")
forecast <- forecast(fit1, h=21)

accuracy(forecast(fit1, h=h)$mean, nc.test)


