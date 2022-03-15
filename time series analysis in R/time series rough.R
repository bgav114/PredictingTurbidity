library(forecast)
library(expsmooth)
library(dynlm)
library(x12)
library(zoo)
library(car)
library(dLagM)
library(AER)
library(Hmisc)
library(ggplot2)
library(readr)
library(tseries)
library(xts)
library(quantmod)
library(readr)
library(lubridate)
library(dplyr)
library(readxl)
library(dhReg)
library(fpp)
library(tsibble)
library(tsibbledata)
library(fable)
library(tidyverse)
library(feasts)
source("https://gist.githubusercontent.com/ellisp/4002241def4e2b360189e58c3f461b4a/raw/e959562be9e7a4d919a9c454d8b1b70cde904ab0/dualplot.R")

# Importing daily_2013_2014_rain_lev_turb daily_2013_train and daily_2013_test-------------------------------------------------------------------

daily_2013_2014_rain_lev_turb <- read_csv("~/Desktop/water quality RA/data files/initial/created datasets/daily_2013_2014_rain_lev_turb.csv")
class(daily_2013_2014_rain_lev_turb)

daily_2013_test <- read_csv("~/Desktop/water quality RA/data files/initial/created datasets/daily_2013_test.csv", 
                            col_types = cols(daily_rain = col_number(), 
                                             daily_WL = col_number(), daily_turbidity = col_number(), 
                                             date = col_date(format = "%Y-%m-%d")))

daily_2013_train <- read_csv("~/Desktop/water quality RA/data files/initial/created datasets/daily_2013_train.csv", 
                             col_types = cols(daily_rain = col_number(), 
                                              daily_WL = col_number(), daily_turbidity = col_number(), 
                                              date = col_date(format = "%Y-%m-%d")))

# Standardizing daily_2013_train and daily_2013 test-----------------------------------------------------------------------------------------------------------


daily_2013_test$daily_rain <- ((daily_2013_test$daily_rain - 
                                  mean(daily_2013_train$daily_rain,na.rm=T))/
                                 sd(daily_2013_train$daily_rain,na.rm=T))
daily_2013_test$daily_WL <- ((daily_2013_test$daily_WL - 
                                mean(daily_2013_train$daily_WL,na.rm=T))/
                               sd(daily_2013_train$daily_WL,na.rm=T))
daily_2013_test$daily_turbidity <- ((daily_2013_test$daily_turbidity - 
                                       mean(daily_2013_train$daily_turbidity,na.rm=T))/
                                      sd(daily_2013_train$daily_turbidity,na.rm=T))
daily_2013_test$max_temp <- ((daily_2013_test$max_temp - 
                                mean(daily_2013_train$max_temp,na.rm=T))/
                               sd(daily_2013_train$max_temp,na.rm=T))
daily_2013_test$total_solar_exposure <- ((daily_2013_test$total_solar_exposure - 
                                            mean(daily_2013_train$total_solar_exposure,na.rm=T))/
                                           sd(daily_2013_train$total_solar_exposure,na.rm=T))

daily_2013_2014_rain_lev_turb$daily_rain <- ((daily_2013_2014_rain_lev_turb$daily_rain - 
                                                mean(daily_2013_train$daily_rain,na.rm=T))/
                                               sd(ddaily_2013_train$daily_rain,na.rm=T))
daily_2013_2014_rain_lev_turb$daily_WL <- ((daily_2013_2014_rain_lev_turb$daily_WL - 
                                              mean(daily_2013_train$daily_WL,na.rm=T))/
                                             sd(daily_2013_train$daily_WL,na.rm=T))
daily_2013_2014_rain_lev_turb$daily_turbidity <- ((daily_2013_2014_rain_lev_turb$daily_turbidity - 
                                                     mean(daily_2013_train$daily_turbidity,na.rm=T))/
                                                    sd(daily_2013_train$daily_turbidity,na.rm=T))
daily_2013_2014_rain_lev_turb$max_temp <- ((daily_2013_2014_rain_lev_turb$max_temp -
                                              mean(daily_2013_train$max_temp,na.rm=T))/
                                             sd(daily_2013_train$max_temp,na.rm=T))
daily_2013_2014_rain_lev_turb$total_solar_exposure <- ((daily_2013_2014_rain_lev_turb$total_solar_exposure - 
                                                          mean(daily_2013_train$total_solar_exposure,na.rm=T))/
                                                         sd(daily_2013_train$total_solar_exposure,na.rm=T))

daily_2013_train$daily_rain <- ((daily_2013_train$daily_rain - 
                                   mean(daily_2013_train$daily_rain,na.rm=T))/
                                  sd(daily_2013_train$daily_rain,na.rm=T))
daily_2013_train$daily_WL <- ((daily_2013_train$daily_WL - 
                                 mean(daily_2013_train$daily_WL,na.rm=T))/
                                sd(daily_2013_train$daily_WL,na.rm=T))
daily_2013_train$daily_turbidity <- ((daily_2013_train$daily_turbidity - 
                                        mean(daily_2013_train$daily_turbidity,na.rm=T))/
                                       sd(daily_2013_train$daily_turbidity,na.rm=T))
daily_2013_train$max_temp <- ((daily_2013_train$max_temp - 
                                 mean(daily_2013_train$max_temp,na.rm=T))/
                                sd(daily_2013_train$max_temp,na.rm=T))
daily_2013_train$total_solar_exposure <- ((daily_2013_train$total_solar_exposure - 
                                             mean(daily_2013_train$total_solar_exposure,na.rm=T))/
                                            sd(daily_2013_train$total_solar_exposure,na.rm=T))


# Specifying the time series objects for daily_2013_train and daily_2013_test---------------------------------------------------------------------

datetimes <- daily_2013_2014_rain_lev_turb$date
datetimes_train <- daily_2013_train$date 
datetimes_test <- daily_2013_test$date 
ts_daily_train <- xts(daily_2013_train[,-c(1)], order.by = datetimes_train)
ts_daily_test <- xts(daily_2013_test[,-c(1)], order.by=datetimes_test)
ts_daily_full <- xts(daily_2013_2014_rain_lev_turb[,-c(1)], order.by=datetimes)

plot(ts_daily_train[,1],main = "Somerton - Epping Rainfall" , xlab = "1st Jan 2013 - 1st Dec 2013", ylab = "daily rainfall (mm)")
plot(ts_daily_train[,2],main = "GT Water Level" , xlab = "1st Jan 2013 - 1st Dec 2013", ylab = "daily water level (lt)")
plot(ts_daily_train[,3],main = "GT Turbidity" , xlab = "1st Jan 2013 - 1st Dec 2013", ylab = "daily turbidity (NTU)")
plot(ts_daily_train[,4],main = "Melb. Airport Maximum Temperature" , xlab = "1st Jan 2013 - 1st Dec 2013", ylab = "daily temperature (*C)")
plot(ts_daily_train[,5],main = "Melb. Airport Total Solar Exposure" , xlab = "1st Jan 2013 - 1st Dec 2013", ylab = "daily total solar exposure (*C)")

autoplot(ts_daily_train[,c(1:5)] , facets = TRUE) +
  xlab("date") + ylab("standardized values") +
  ggtitle("Daily Measurements 2013")

plot(as.data.frame(ts_daily_train), pch=20 , cex=0.45 , col="red")

ts_daily_train_turbidity <- xts(daily_2013_train$daily_turbidity, order.by = datetimes_train)
ts_daily_test_turbidity <- xts(daily_2013_test$daily_turbidity, order.by = datetimes_test)

# Visualizing acf and pacf for ts_daily_train-------------------------------------------------------------------------------------------------------------------------------------

acf(ts_daily_train)
pacf(ts_daily_train)
plot(ts_daily_train)

# Test for stationarity on ts_daily_train -------------------------------------------------------------------------------------------------------------------------------

length(ts_daily_train[,1])
k = trunc(12*((length(ts_daily_train[,1])/100)^(1/4)))
print(k)
adf.test(ts_daily_train$daily_rain, k=k) # stationary, p<alpha=5%, alt hypothesis (stat) accepted
adf.test(ts_daily_train$daily_WL, k=k) # stationary, p<alpha=5%, alt hypothesis (stat) accepted
adf.test(ts_daily_train$daily_turbidity, k=k) # non-stationary, p>alpha=5%, alt hypothesis (stat) rejected, confirmed by acf
adf.test(ts_daily_train$max_temp, k=k) # non-stationary, p>alpha=5%, alt hypothesis (stat) rejected, confirmed by acf
adf.test(ts_daily_train$total_solar_exposure, k=k) # non-stationary, p>alpha=5%, alt hypothesis (stat) rejected, confirmed by acf

# Dynamic Regression Model with auto.arima on ts_daily_train------------------------------------------------------------------------------------------------------------------------------------

fit.1 <- auto.arima(ts_daily_train[,"daily_turbidity"],xreg = ts_daily_train[,c("daily_rain")])
fit.1 # AICc=155.96
checkresiduals(fit.1) # LJ = white noise, spikes, non sig AC, skewed slightly

fit.2 <- auto.arima(ts_daily_train[,"daily_turbidity"],xreg = ts_daily_train[,c("daily_rain","max_temp")])
fit.2 # AICc=156.67
checkresiduals(fit.2) # LJ = white noise, spikes, non sig AC, skewed slightly

fit.3 <- auto.arima(ts_daily_train[,"daily_turbidity"],xreg = ts_daily_train[,c("daily_rain","total_solar_exposure")])
fit.3 # AICc=157.98
checkresiduals(fit.3) # LJ = white noise, spikes, non sig AC, skewed slightly

fit.4 <- auto.arima(ts_daily_train[,"daily_turbidity"],xreg = ts_daily_train[,c("daily_WL","daily_rain","total_solar_exposure")])
fit.4 # AICc=138.18
checkresiduals(fit.4) # LJ = white noise, spikes, no sig AC, skewed slightly

fit.5 <- auto.arima(ts_daily_train[,"daily_turbidity"],xreg = ts_daily_train[,c("daily_WL", "daily_rain")])
fit.5 # AICc=122.39 BEST 
checkresiduals(fit.5) # LJ = white noise, spikes, no sig AC, skewed (but better)

fit.6 <- auto.arima(ts_daily_train[,"daily_turbidity"],xreg = ts_daily_train[,c("daily_WL", "daily_rain", "max_temp")])
fit.6 # AICc=123.48
checkresiduals(fit.6)# LJ = white noise, spikes, no sig AC, skewed (but better)

fit.7 <- auto.arima(ts_daily_train[,"daily_turbidity"],xreg = ts_daily_train[,c("daily_WL", "daily_rain", "max_temp", "total_solar_exposure")])
fit.7 # AICc=125.58
checkresiduals(fit.7)# LJ = white noise, spikes, no sig AC, skewed slightly

ts_daily_test_ts <- ts(daily_2013_test[,"daily_turbidity"], start=311)

fcast <- forecast(fit.5, h=46, xreg = ts_daily_test[1:46,c("daily_WL", "daily_rain")])
autoplot(fcast) +
  autolayer(ts_daily_test_ts)+
  xlab("Observation no.") + ylab("standardized Turbidity")
fcast # BEST FORECASTS IS FOR BEST MODEL FIT.5

fcast.1 <- forecast(fit.4, h=46, xreg = ts_daily_test[1:46,c("daily_WL","daily_rain","total_solar_exposure")])
autoplot(fcast.1) +
  autolayer(ts_daily_test_ts)+
  xlab("Observation no.") + ylab("standardized Turbidity")
fcast.1 #effect of adding solar exposure brings the forecasts down a bit on avg

fcast.2 <- forecast(fit.6, h=46, xreg = ts_daily_test[1:46,c("daily_WL","daily_rain","max_temp")])
autoplot(fcast.2) +
  autolayer(ts_daily_test_ts)+
  xlab("Observation no.") + ylab("standardized Turbidity")
fcast.2 #effect of adding max_temp shoots the forecasts up a bit on avg

fcast.3 <- forecast(fit.7, h=46, xreg = ts_daily_test[1:46,c("daily_WL","daily_rain","max_temp","total_solar_exposure")])
autoplot(fcast.3) +
  autolayer(ts_daily_test_ts)+
  xlab("Observation no.") + ylab("standardized Turbidity")
fcast.3 #effect of adding max_temp and total_solar_exposure shoots the forecasts up a bit on avg

# Dynamic Harmonic Regression Model with fable and auto.arima on tsibble_train-------------------------------------------------------------------------------------------------

tsibble_train <- daily_2013_train %>% as_tsibble()
tsibble_test <- daily_2013_test %>% as_tsibble()
tsibble_full <- ts_daily_full[,1:3] %>% as_tsibble()
colSums(is.na(tsibble_train))
colSums(is.na(tsibble_test))
tsibble_train <- tsibble_train %>% fill_gaps(daily_turbidity = mean(daily_turbidity, na.rm=T))
tsibble_test <- tsibble_test %>% fill_gaps(daily_turbidity = mean(daily_turbidity, na.rm=T))

fit.h <- model(tsibble_train,
               `K = 1` = ARIMA(daily_turbidity ~ fourier(K=1) + PDQ(0,0,0)),
               `K = 2` = ARIMA(daily_turbidity ~ fourier(K=2) + PDQ(0,0,0)),
               `K = 3` = ARIMA(daily_turbidity ~ fourier(K=3) + PDQ(0,0,0)))
glance(fit.h)

best.fit.h <- model(tsibble_train, ARIMA(daily_turbidity ~ fourier(K=1) + PDQ(0,0,0)+ daily_WL + daily_rain))
report(best.fit.h) # AICc = 519
best.fit.h %>% gg_tsresiduals()# no sig AC, skewed slightly


new_data_test <- new_data(tsibble_train, 46)%>%
  mutate(daily_WL = tsibble_test$daily_WL,
         daily_rain = tsibble_test$daily_rain)

fcast.s <- best.fit.h %>% forecast(new_data = new_data_test)
fcast.s %>% autoplot(tsibble_train)

# TBATS Model for ts_daily_train-----------------------------------------------------------------------------------------------------------------------------

ts_daily_train_ts <- ts(daily_2013_train[,"daily_turbidity"], start=c(2013,1), frequency=7)
ts_daily_train_ts %>% tbats() %>% forecast() %>% autoplot() + xlab("Date") + ylab("daily_turbidity")

# best model - fit.5----------------------------------------------------------------------------------------------------------------------------

fit.5 <- auto.arima(ts_daily_train[,"daily_turbidity"],xreg = ts_daily_train[,c("daily_WL", "daily_rain")])
fit.5 # AICc=122.39 BEST 
checkresiduals(fit.5) 

fcast <- forecast(fit.5, h=46, xreg = ts_daily_test[1:46,c("daily_WL", "daily_rain")])
autoplot(fcast) +
  autolayer(ts_daily_test_ts)+
  xlab("Observation no.") + ylab("standardized Turbidity")
fcast

cbind("Regression Errors" = residuals(fit.5, type="regression"),
      "ARIMA Errors" = residuals(fit.5, type = "innovation")) %>%
  autoplot(facets=TRUE)

# comparing forecasts to actual testing data for 46 days

RMSE(fcast$mean) # 0.2842519
RMSE(fcast$residuals) # 0.02855103

# Time series cross-validation (46 period windows starting from 182)-----------------------------------------------------------------------------------------------------------

all_folds_forecast <- data.frame(lower_80=numeric(0),lower_95=numeric(0),upper_80=numeric(0),upper_95=numeric(0),
                                 forcasted_turbidity=numeric(0),actual_turbidity=numeric(0), daily_WL=numeric(0),
                                 daily_rain = numeric(0))
rmse_vector <- c()#gotta do 7*4=28 in testing 111+28=139 until you get to 278 which is 0.8 of full data then actual testing set after 278 has 29 observations 
a_start <- 1
a <- 196 #same starting point 
p <- 1 #(or do 46 period window until a<=310)
nrow(ts_daily_full)
while(a <=356){
  
  fit.5 <- auto.arima(ts_daily_full[a_start:a, "daily_turbidity"], xreg = ts_daily_full[a_start:a, c("daily_WL", "daily_rain")])
  fcasts <- forecast(fit.5, h=p, xreg = ts_daily_full[(a+1):(a+p), c("daily_WL", "daily_rain")])
  
  one_period_forecast <- data.frame(lower_80=fcasts$lower[,1][1:p],
                                    lower_95=fcasts$lower[,2][1:p],
                                    upper_80=fcasts$upper[,1][1:p],
                                    upper_95=fcasts$upper[,2][1:p],
                                    forecasted_turbidity = fcasts$mean[1:p],
                                    actual_turbidity = ts_daily_full[(a+1):(a+p), "daily_turbidity"],
                                    daily_WL = ts_daily_full[(a+1):(a+p), c("daily_WL")],
                                    daily_rain = ts_daily_full[(a+1):(a+p), c("daily_rain")])
  all_folds_forecast <- rbind(all_folds_forecast,one_period_forecast)
  rmse_vector <- c(rmse_vector, RMSE(fcasts$residuals))
  
  a_start <- a_start+p
  a <- a+p
  
}

ts_fcasts_turbidity <- c(rep(NA, 196), all_folds_forecast$forecasted_turbidity)
plot(daily_2013_2014_rain_lev_turb$daily_turbidity, type="l")
lines(ts_fcasts_turbidity, col = "red")

mean(rmse_vector)

rmse_folds <- data.frame(folds = c(1:28),
                         RMSE = rmse_vector)


# Time series Cross Validation (80% to 20% split from 182) ------------------------------------------------------------------

all_folds_forecast <- data.frame(lower_80=numeric(0),lower_95=numeric(0),upper_80=numeric(0),upper_95=numeric(0),
                                 forcasted_turbidity=numeric(0),actual_turbidity=numeric(0), daily_WL=numeric(0),
                                 daily_rain = numeric(0))
rmse_vector <- c()#gotta do 7*4=28 in testing 111+28=139 until you get to 278 which is 0.8 of full data then actual testing set after 278 has 29 observations 
mark <- 1
fold_set <- c(228, 285, 356)

while(mark <=3){
  
  a <- round(fold_set[mark]*0.8)
  p <- round(fold_set[mark]*0.2)
  
  fit.5 <- auto.arima(ts_daily_full[1:a, "daily_turbidity"], xreg = ts_daily_full[1:a, c("daily_WL", "daily_rain")])
  fcasts <- forecast(fit.5, h=p, xreg = ts_daily_full[(a+1):(a+p), c("daily_WL", "daily_rain")])
  
  one_period_forecast <- data.frame(lower_80=fcasts$lower[,1][1:p],
                                    lower_95=fcasts$lower[,2][1:p],
                                    upper_80=fcasts$upper[,1][1:p],
                                    upper_95=fcasts$upper[,2][1:p],
                                    forecasted_turbidity = fcasts$mean[1:p],
                                    actual_turbidity = ts_daily_full[(a+1):(a+p), "daily_turbidity"],
                                    daily_WL = ts_daily_full[(a+1):(a+p), c("daily_WL")],
                                    daily_rain = ts_daily_full[(a+1):(a+p), c("daily_rain")])
  all_folds_forecast <- rbind(all_folds_forecast,one_period_forecast)
  rmse_vector <- c(rmse_vector, RMSE(fcasts$residuals))
  
  mark <- mark+1
  
}

ts_fcasts_turbidity <- c(rep(NA, 181), all_folds_forecast$forecasted_turbidity)
plot(daily_2013_2014_rain_lev_turb$daily_turbidity, type="l")
lines(ts_fcasts_turbidity, col = "blue")


mean(rmse_vector)

rmse_folds <- data.frame(folds = c(1:28),
                         RMSE = rmse_vector)

















all_folds_forecast$residuals <- (all_folds_forecast$forecasted_turbidity - all_folds_forecast$daily_turbidity)

all_folds_forecast <- all_folds_forecast %>% mutate(predict_category = case_when(residuals>=-0.5 & residuals<=1 ~ 'A',
                                                                                 residuals>=-2 & residuals<=-0.5 ~ '-A',
                                                                                 residuals>=-3 & residuals<=-2 ~ '-B',
                                                                                 residuals<=-3 ~ '-C',
                                                                                 residuals>=1 & residuals<=2 ~ '+A',
                                                                                 residuals>=2 ~ '+B'))

ggplot(all_folds_forecast, aes(x=predict_category, y=daily_turbidity)) + 
  geom_boxplot()












ts_daily_test_ts <- ts(daily_2013_test[40:46,"daily_turbidity"], start=350)
fcast <- forecast(fit.5, h=7, xreg = ts_daily_test[40:46,c("daily_WL", "daily_rain")])
autoplot(fcast) +
  autolayer(ts_daily_test_ts)+
  xlab("Observation no.") + ylab("standardized Turbidity")

nrow(ts_daily_train)
fcast$

autoplot(ts_daily_test_ts)

fcasts

nrow(ts_daily_full)
mean(rmse_vector)
hist(fcasts$residuals)
rmse_vector
fcasts$residuals





# Transforming ts_daily_train_turbidity

ts_daily_train_log <- log(ts_daily_train) #all variables (log, train)
ts_daily_test_log <- log(ts_daily_test) #all variables (log, test)
ts_daily_train_turbidity_log <- log(ts_daily_train_turbidity) #only turbidity (log,train)
ts_daily_test_turbidity_log <- log(ts_daily_test_turbidity) #only turbidity (log,test)
table(is.infinite(ts_daily_train_turbidity_log)) #getting rid of 2 infinite values
#ts_daily_train_turbidity_log[!is.finite(ts_daily_train_turbidity_log)] <- NA
#ts_daily_train_turbidity_log <- na.omit(ts_daily_train_turbidity_log)

# ARIMA fitting ts_daily_train_turbidity (4,2,2) & (AUTO) 

fit.1 <- arima(ts_daily_train_turbidity, order=c(4,2,2))
fit.1
checkresiduals(fit.1)
fit.2 <- auto.arima(ts_daily_train_turbidity)
fit.2
checkresiduals(fit.2)

# ARIMA fitting ts_daily_train_turbidity_log (5,0,0) & (AUTO) 

fit.3 <- arima(ts_daily_train_turbidity_log, order=c(5,0,0))
fit.3
checkresiduals(fit.3)
fit.4 <- auto.arima(ts_daily_train_turbidity_log)
fit.4
checkresiduals(fit.4)

# Cross validation using tscv on ts_daily_train_turbidity

e <- tsCV(ts_daily_train_turbidity, rwf, h=1)
sqrt(mean(e^2, na.rm=TRUE))
sqrt(mean(residuals(rwf(ts_daily_train_turbidity))^2, na.rm=TRUE))
e

# Cross validation using tscv on ts_daily_train_turbidity_log

e <- tsCV(ts_daily_train_turbidity_log, rwf, h=1)
sqrt(mean(e^2, na.rm=TRUE))
sqrt(mean(residuals(rwf(ts_daily_train_turbidity_log))^2, na.rm=TRUE))
e


#xreg - include light temp rain lev in auto.arima






# Comparing actual and predicted turbidity values

# making time series for predicted values only
ts_2013_predicted <- xts(predicted, order.by = datetimes[-c(1,2,3)])
plot(ts_2013_predicted)

# making dataset for actual and predicted values
daily_2013_actual_predicted <- daily_2013_2014_rain_lev_turb[-c(1,2,3),3]
daily_2013_actual_predicted$daily_turbidity <- log(daily_2013_actual_predicted$daily_turbidity)
daily_2013_actual_predicted$predicted <- predicted

# making time series for actual and predicted values
ts_2013_actual_predicted <- xts(daily_2013_actual_predicted, order.by = datetimes[-c(1,2,3)])

# visual comparison times sereis for actual and predicted values
plot(ts_2013_actual_predicted, yax.flip=T)

# making a difference column -reevaluate which dataset to put into
daily_2013_actual_predicted$difference <- (daily_2013_actual_predicted$daily_turbidity - daily_2013_actual_predicted$predicted)

i <- 5
(ts_daily_2013_turbidity_log[(i-3),1] * coeff3)
predicted



i <- 5

(intercept + (ts_daily_2013_turbidity_log[(i-1),1] * coeff1) +(ts_daily_2013_turbidity_log[(i-2),1] * coeff2) + (ts_daily_2013_turbidity_log[(i-3),1] * coeff3))























# ROUGH ------------------------------------------------------------------------------------------------

# Visualising the time series' together

# Dates associated with rainfall>10 coincides with biggest spikes in turbidity 

desc_daily_2013_2014_rain_lev_turb <- daily_2013_2014_rain_lev_turb[order(daily_2013_2014_rain_lev_turb$`Rainfall amount (millimetres)`),]
vis_rain_dates_turbidity <- plot(y=daily_2013_2014_rain_lev_turb$daily_turbidity, 
                                 x=daily_2013_2014_rain_lev_turb$date, type="l", 
                                 ylab = "Daily Turbidity Merri Creek (GT)", main = "Turbidity spikes during high rainfall")+
  abline(v = as.numeric(desc_daily_2013_2014_rain_lev_turb$date[341:357]),col="red", lwd=1, lty=2 )
vis_rain_dates_turbidity

# ROUGH 

desc_daily_2013_2014_rain_lev_turb <- daily_2013_2014_rain_lev_turb[order(daily_2013_2014_rain_lev_turb$`Rainfall amount (millimetres)`),]
vis_rain_dates_turbidity <- plot(y=daily_2013_2014_rain_lev_turb$daily_turbidity, x=daily_2013_2014_rain_lev_turb$date, type="l", ylab = "Daily Turbidity Merri Creek (GT)")+
  abline(v = as.numeric(desc_daily_2013_2014_rain_lev_turb$date[341:357]),col="red", lwd=1, lty=2 )

ablin
abline(v=as.numeric(example$ymdPX[nrow(example)]), lwd=2, col="red")
desc_daily_2013_2014_rain_lev_turb$date[341:357]




daily_2013_2014_rain_lev_turb %>% filter(daily_2013_2014_rain_lev_turb$`Rainfall amount (millimetres)`>0.0)
daily_2013_2014_rain_lev_turb %>% tail()
desc_daily_2013_2014_rain_lev_turb <- daily_2013_2014_rain_lev_turb[order(daily_2013_2014_rain_lev_turb$`Rainfall amount (millimetres)`),]

ts.plot(ts_daily_2013_2014_rain_lev_turb, gpars= list(col=rainbow(3)))
ts_daily_2013_2014_rain_lev_turb[,c(1,2)]
max(daily_2013_2014_rain_lev_turb$`Rainfall amount (millimetres)`)

ggplot() + 
  geom_line(data = daily_2013_2014_rain_lev_turb, aes(x = `Rainfall amount (millimetres)`, y = date), color = "red") +
  geom_line(data = daily_2013_2014_rain_lev_turb, aes(x = daily_turbidity, y = date, color = "blue")) +
  xlab('data_date') +
  ylab('percent.change')

daily_2013_2014_rain_lev_turb$`Rainfall amount (millimetres)`

dualplot(x1 = time)

dualplot(x1 = daily_2013_2014_rain_lev_turb$date, y1 = daily_2013_2014_rain_lev_turb$`Rainfall amount (millimetres)`,
         x2 = daily_2013_2014_rain_lev_turb$date, y2 = daily_2013_2014_rain_lev_turb$daily_turbidity, ylim=c(10),
         ylab1 = "BHP", ylab2 = "Fonterra", 
         legx = "topright", main = "Price per share of two key firms" )

ts_daily_2013_2014_rain_lev_turb$`Rainfall amount (millimetres)`
dualplot()
