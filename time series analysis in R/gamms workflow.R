library(readr)
library(mgcv)
library(tseries)
library(dplyr)
library(ggplot2)

daily_2013_2014_rain_lev_turb <- read_csv("Desktop/water quality RA/data files/initial/created datasets/daily_2013_2014_rain_lev_turb.csv", 
                                          col_types = cols(daily_rain = col_number(), 
                                                           daily_WL = col_number(), daily_turbidity = col_number(), 
                                                           max_temp = col_number(), total_solar_exposure = col_number(),
                                                           date = col_date(format = "%Y-%m-%d")))

daily_2013_test <- read_csv("Desktop/water quality RA/data files/initial/created datasets/daily_2013_test.csv", 
                            col_types = cols(daily_rain = col_number(), 
                                             daily_WL = col_number(), daily_turbidity = col_number(), 
                                             max_temp = col_number(), total_solar_exposure = col_number(),
                                             date = col_date(format = "%Y-%m-%d")))

daily_2013_train <- read_csv("Desktop/water quality RA/data files/initial/created datasets/daily_2013_train.csv",
                             col_types = cols(daily_rain = col_number(), 
                                              daily_WL = col_number(), daily_turbidity = col_number(), 
                                              max_temp = col_number(), total_solar_exposure = col_number(),
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



# Model with uncorrrelated errors ------------------------------------------------------------------------------

m <- gamm(daily_turbidity~ 
                  s(daily_WL, bs="gp") + 
                  s(daily_rain, bs="gp") + 
                  s(max_temp, bs="gp") + 
                  s(total_solar_exposure, bs="gp"),
          select=TRUE,
          family="gaussian",
          method="REML",
          data=daily_2013_train)
summary(m$gam)# significance of each smooth 0.46, 0.196, 0.333, 0.163
aAIC_m <- nrow(daily_2013_train)*log(m$gam$sig2) + (2*sum(m$gam$edf))
aAIC_m #2001.5 2122.035 2066.09 2130.732 (WL temp 0.588 1925.546) (WL temp sun 0.599 1925.049) (WL temp rain 0.632 1894.884) (all 0.645 1890.955)

plot(m$gam,
     all.terms=TRUE, pages=1, rug=T, residuals=T, pch=1, cex=0.25, 
     shade = TRUE, shade.col = "lightblue", shift = coef(m$gam)[1]) # partial effects
gam.check(m$gam) # you want a high p-value for each smooth which means errors rand dist
concurvity(m$gam, full = FALSE)

#layout(matrix(1:2, ncol = 1, nrow = 1))
plot(m$gam, scale = 0)
layout(1)

#layout(matrix(1:2, ncol = 2))
acf(resid(m$lme), lag.max = 36, main = "ACF")
pacf(resid(m$lme), lag.max = 36, main = "pACF")
layout(1)

# Model with correlated errors ------------------------------------------------------------------------------

ctrl <- list(niterEM = 0, msVerbose = TRUE, optimMethod="L-BFGS-B")

## AR(1)
m1 <- gamm(daily_turbidity~ 
                   s(daily_WL, bs="gp") + 
                   s(daily_rain, bs="gp") + 
                   s(max_temp, bs="gp") + 
                   s(total_solar_exposure, bs="gp"),
           select=TRUE,
           family="gaussian",
           method="REML",
           data=daily_2013_train, 
           correlation = corARMA(p = 1),
           control = ctrl)
summary(m1$gam)
aAIC_m <- nrow(daily_2013_train)*log(m1$gam$sig2) + (2*sum(m1$gam$edf))
aAIC_m #2096.554

plot(m1$gam,
     all.terms=TRUE, pages=1, rug=T, residuals=T, pch=1, cex=0.25, 
     shade = TRUE, shade.col = "lightblue", shift = coef(m$gam)[1]) # partial effects
gam.check(m1$gam) # you want a high p-value for each smooth which means errors rand dist
concurvity(m1$gam, full = FALSE)

##AR(2) 
m2 <- gamm(daily_turbidity~ 
                   s(daily_WL, bs="gp") + 
                   s(daily_rain, bs="gp") + 
                   s(max_temp, bs="gp") + 
                   s(total_solar_exposure, bs="gp"),
           select=TRUE,
           family="gaussian",
           method="REML",
           data=daily_2013_train, 
           correlation = corARMA(p = 2),
           control = ctrl)
summary(m2$gam)
aAIC_m <- nrow(daily_2013_train)*log(m2$gam$sig2) + (2*sum(m2$gam$edf))
aAIC_m #2093.808

plot(m2$gam,
     all.terms=TRUE, pages=1, rug=T, residuals=T, pch=1, cex=0.25, 
     shade = TRUE, shade.col = "lightblue", shift = coef(m$gam)[1]) # partial effects
gam.check(m2$gam) # you want a high p-value for each smooth which means errors rand dist
concurvity(m2$gam, full = FALSE)

## AR(3)
m3 <- gamm(daily_turbidity~ 
                   s(daily_WL, bs="gp") + 
                   s(daily_rain, bs="gp") + 
                   s(max_temp, bs="gp") + 
                   s(total_solar_exposure, bs="gp"),
           select=TRUE,
           family="gaussian",
           method="REML",
           data=daily_2013_train, 
           correlation = corARMA(p = 10),
           control = ctrl)
summary(m3$gam)
aAIC_m <- nrow(daily_2013_train)*log(m3$gam$sig2) + (2*sum(m3$gam$edf))
aAIC_m #2084.461

plot(m3$gam,
     all.terms=TRUE, pages=1, rug=T, residuals=T, pch=1, cex=0.25, 
     shade = TRUE, shade.col = "lightblue", shift = coef(m$gam)[1]) # partial effects
gam.check(m3$gam) # you want a high p-value for each smooth which means errors rand dist
concurvity(m3$gam, full = FALSE)

# Compare the predictions ------------------------------------------------------------------------------

anova(m$lme, m1$lme, m2$lme, m3$lme)

p  <- predict(m$gam,  newdata = daily_2013_test, se.fit = TRUE)
p1 <- predict(m1$gam, newdata = daily_2013_test, se.fit = TRUE)
p2 <- predict(m2$gam, newdata = daily_2013_test, se.fit = TRUE)
p3 <- predict(m3$gam, newdata = daily_2013_test, se.fit = TRUE)

all_values <- transform(daily_2013_test,
                        p  = p$fit,  se  = p$se.fit,
                        p1 = p1$fit, se1 = p1$se.fit,
                        p2 = p2$fit, se2 = p2$se.fit,
                        p3 = p3$fit, se3 = p3$se.fit)

# Plot the predictions ------------------------------------------------------------------------------

ylim <- with(daily_2013_test, range(p, p1, p2, p3))
ylab <- expression(daily_turbidity ~ (NTU ~ centred))
plot(daily_turbidity ~ date, data = daily_2013_2014_rain_lev_turb, type="l", col="black",
     ylab = ylab)
lines(p ~ date, data = all_values, col = "orange")
lines(p1 ~ date, data = all_values, col = "red")
lines(p2 ~ date, data = all_values, col = "blue")
lines(p3 ~ date, data = all_values, col = "forestgreen", lwd = 1)
legend("topleft",
       legend = c("Uncorrelated Errors", paste0("AR(", 1:3, ") Errors")),
       bty = "n", col = c("black","red","blue","forestgreen"),
       lty = 1, lwd = c(1,1,1))


# GAMS series cross-validation (46 period windows starting from 182)-----------------------------------------------------------------------------------------------------------

all_gams_folds_forecast <- c(forcasted_turbidity=numeric(0))
AIC_values <- c() #gotta do 7*4=28 in testing 111+28=139 until you get to 278 which is 0.8 of full data then actual testing set after 278 has 29 observations 
a_start <- 1
a <- 196 #same starting point 
p <- 1 #(or do 46 period window until a<=310)

while(a <=356){
        
        ctrl <- list(niterEM = 1, msVerbose = TRUE, optimMethod="L-BFGS-B")
        m3 <- gamm(daily_turbidity~ 
                           s(daily_WL, bs="gp") + 
                           s(daily_rain, bs="gp") + 
                           s(max_temp, bs="gp") + 
                           s(total_solar_exposure, bs="gp"),
                   select=TRUE,
                   family="gaussian",
                   method="REML",
                   data=daily_2013_2014_rain_lev_turb[a_start:a,], 
                   correlation = corARMA(p = 3)
                   #control = ctrl
                   )
        fcasts <- predict(m3$gam,  newdata = daily_2013_2014_rain_lev_turb[(a+1):(a+p),], se.fit = TRUE)
        all_gams_folds_forecast <- c(all_gams_folds_forecast, fcasts$fit)
        AIC_values <- c(AIC_values, nrow(daily_2013_2014_rain_lev_turb[a_start:a,])*log(m3$gam$sig2) + (2*sum(m3$gam$edf)))
        
        a_start <- a_start+p
        a <- a+p
}

ts_fcasts_turbidity <- c(rep(NA, 196), all_gams_folds_forecast)
plot(daily_2013_2014_rain_lev_turb$daily_turbidity, type="l")
lines(ts_fcasts_turbidity, col = "red")

mean(AIC_values)



# Time series Cross Validation (80% to 20% split from 182) ------------------------------------------------------------------

all_gams_folds_forecast <- c(forcasted_turbidity=numeric(0))
AIC_values <- c() #gotta do 7*4=28 in testing 111+28=139 until you get to 278 which is 0.8 of full data then actual testing set after 278 has 29 observations 
mark <- 1
fold_set <- c(228, 285, 356)

while(mark <=3){
        
        a <- round(fold_set[mark]*0.8)
        p <- round(fold_set[mark]*0.2)
        
        ctrl <- list(niterEM = 0, msVerbose = TRUE, optimMethod="L-BFGS-B")
        m3 <- gamm(daily_turbidity~ 
                           s(daily_WL, bs="gp") + 
                           s(daily_rain, bs="gp") + 
                           s(max_temp, bs="gp") + 
                           s(total_solar_exposure, bs="gp"),
                   select=TRUE,
                   family="gaussian",
                   method="REML",
                   data=daily_2013_2014_rain_lev_turb[1:a,], 
                   correlation = corARMA(p = 3),
                   control = ctrl)
        fcasts <- predict(m3$gam,  newdata = daily_2013_2014_rain_lev_turb[(a+1):(a+p),], se.fit = TRUE)
        all_gams_folds_forecast <- c(all_gams_folds_forecast, fcasts$fit)
        AIC_values <- c(AIC_values, nrow(daily_2013_2014_rain_lev_turb[1:a,])*log(m3$gam$sig2) + (2*sum(m3$gam$edf)))
        
        mark <- mark+1
        
}

ts_fcasts_turbidity <- c(rep(NA, 181), all_gams_folds_forecast)
plot(daily_2013_2014_rain_lev_turb$daily_turbidity, type="l")
lines(ts_fcasts_turbidity, col = "blue")


mean(AIC_values)






















# Hyperparameter fine tuning


x <- noquote(names(daily_2013_train)[-c(1,4)])
variables <- c()
aAIC <- c()


for (i in 1:ncol(combn(x, m=2, FUN=NULL))){
        
        var1 <- noquote(combn(x, m=2, FUN=NULL)[1,i])
        var2 <- noquote(combn(x, m=2, FUN=NULL)[2,i])
        
        g <- gamm(daily_turbidity~
                          s(noquote(combn(x, m=2, FUN=NULL)[1,i]), bs="gp") + 
                          s(noquote(combn(x, m=2, FUN=NULL)[1,i]), bs="gp"), 
                  select=TRUE,
                  family="gaussian",
                  method="REML",
                  data=daily_2013_train)
        
        variables <- c(variables, noquote(paste(combn(x, m=2, FUN=NULL)[,i],collapse = ",")))
        AIC <- nrow(daily_2013_train)*log(g$gam$sig2) + (2*sum(g$gam$edf))
        aAIC <- c(aAIC, AIC)
        
}



# ROUGH

var1 <- noquote(combn(x, m=2, FUN=NULL)[1,i])
var1
noquote(combn(x, m=2, FUN=NULL)[2,1])

daily_2013_train["daily_turbidity"][,1]
length(daily_2013_train$daily_rain)
colSums(is.na(daily_2013_train))

paste(1)


x <- names(daily_2013_train)[-1]
combn(x, m=2, FUN=NULL) #combos of 2 variables
ncol(combn(x, m=2, FUN=NULL)) #all combos FIRST FOR LOOP

combn(x, m=2, FUN=NULL)[,3] #all variable in a combo SECOND FOR LOOPb
combn(x, m=2, FUN=NULL)[1,3] #variable name 1
combn(x, m=2, FUN=NULL)[2,3] #variable name 2

length(combn(x, m=2, FUN=NULL)[,3])
rough_list <- c()
rough_list <- c(rough_list, paste(combn(x, m=2, FUN=NULL)[,3],collapse = ","))
rough_list[2]
