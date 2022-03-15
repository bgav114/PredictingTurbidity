library(readr)
library(keras)
library(caret)
library(tensorflow)
set.seed(123)
library(TSLSTM)

df <- read_csv("Desktop/water quality RA/data files/initial/created datasets/daily_2013_2014_rain_lev_turb.csv", 
               col_types = cols(date = col_skip(), daily_rain = col_number(), 
                                daily_WL = col_number(), daily_turbidity = col_number(), 
                                max_temp = col_number(), total_solar_exposure = col_number()))

# main model ----------------------------------

y <- df[1:200,"daily_turbidity"]
x <- df[1:200,c("daily_WL", "daily_rain", "max_temp", "total_solar_exposure")]
multi_lstm <- 
  ts.lstm(
    ts=y,
    xreg = x,
    tsLag=1,
    xregLag = 1,
    LSTMUnits=10,
    Epochs = 50,
    CompLoss = "mse",
    CompMetrics = "mae",
    ActivationFn = "tanh",
    SplitRatio = 0.99,
    ValidationSplit = 0.99
  )


forecasts_ypred <- c(rep(NA, nrow(multi_lstm$TrainFittedValue)), multi_lstm$TestPredictedValue)
plot(df$daily_turbidity, type='l', col='blue')
points(forecasts_ypred, col='red' )
lines(multi_lstm$TrainFittedValue, col="grey")


# cross validation 200 - 99%

all_LSTM_folds_forecast <- data.frame(forcasted_turbidity=numeric(0),actual_turbidity=numeric(0))
rmse_vector <- c()#gotta do 7*4=28 in testing 111+28=139 until you get to 278 which is 0.8 of full data then actual testing set after 278 has 29 observations 
a_start <- 1
a <- 200 #same starting point 
p <- 1 #(or do 46 period window until a<=310)

while(a <=356){

  multi_lstm <- 
    ts.lstm(
      ts= df[a_start:a,"daily_turbidity"],
      xreg = df[a_start:a,c("daily_WL", "daily_rain", "max_temp", "total_solar_exposure")],
      tsLag=1,
      xregLag = 1,
      LSTMUnits=10,
      Epochs = 50,
      CompLoss = "mse",
      CompMetrics = "mae",
      ActivationFn = "tanh",
      SplitRatio = 0.99,
      ValidationSplit = 0.99
    )
  
  one_period_forecast <- data.frame(forecasted_turbidity = multi_lstm$TestPredictedValue[1],
                                    actual_turbidity = ts_daily_full[(nrow(multi_lstm$TrainFittedValue)+1), "daily_turbidity"])
  all_LSTM_folds_forecast <- rbind(all_LSTM_folds_forecast,one_period_forecast)
  
  a_start <- a_start+1
  a <- a+1
  
}
length(all_LSTM_folds_forecast$forecasted_turbidity)
ts_fcasts_turbidity <- c(rep(NA, 196), all_LSTM_folds_forecast$forecasted_turbidity)
plot(df$daily_turbidity, type="l")
lines(ts_fcasts_turbidity, col = "red")

