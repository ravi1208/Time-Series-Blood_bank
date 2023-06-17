
setwd("D:/Self Appraisal/Time Series - BLood Bank")
getwd()
library(timeSeries)
library(openxlsx)
library(TTR)
library(tseries)
library(forecast)
require(XLConnectJars)
getwd()
#install.packages("Arima")

wb <- loadWorkbook("Blood Bank Data Nashik Predicted.xlsx")
sheets(wb)
'Arpan Blood Bank Analysis'
collection <- readWorkbook(wb,sheet = "Collection Whole Blood Arpan")
Distribution <- readWorkbook(wb,sheet = "Distribution Arpan")
Distribution_total <- readWorkbook(wb,sheet = "Distribution Arpan Total Units")

#View(collection)
df_collect <- data.frame(collection[, c(-1,-4,-5,-6,-7,-8,-9,-10,-11,-12,-13,-14)])
#View(df_collect)
df1_collect <- df_collect[,c(-1,-2)]
#View(df1_collect)
test_data_collection <- ts(df1_collect,start=c(2010,8),end=c(2016,11),frequency = 12)
plot(test_data_collection)
#View(Distribution)
df_distribute <- data.frame(Distribution[, c(-1,-4,-5,-6,-7,-8,-9,-10,-11,-12,-13)])
#plot(df_distribute)
#View(df_distribute)

df1_distribute <- df_distribute[,c(-1,-2)]
#View(df1_distribute)

test_data <- ts(df1_distribute,start=c(2013,1),end=c(2016,11),frequency = 12)
summary(test_data)
plot(test_data,main = "Distribution of Blood units for Jan 2013 - Nov-2016",
     xlab="Time-Frame in Years", ylab="Units of Blood samples issued",
     type = "l")

#View(Distribution_total)
df_distribute_total <- data.frame(Distribution_total[, c(-1,-4,-5)])
#View(df_distribute_total)
typeof(df_distribute_total)
df1_distribute_total <- as.integer( df_distribute_total[,c(-1,-2)])
test_data_distribute_total <- ts(df1_distribute_total,start=c(2013,1),end=c(2016,11),frequency = 12)
plot(test_data_distribute_total,main = "Distribution of Blood units for Jan 2013 - Nov-2016",
     xlab="Time-Frame in Years", ylab="Units of Blood samples issued",
     type = "l" )

#blood GRoupwise
#View(Distribution)
df_distribute_BG <- data.frame(Distribution[, c(-1,-4,-5,-7,-8,-9,-10,-11,-12,-13,-14)])
#View(df_distribute_BG)
df1_distribute_BG <- (df_distribute_BG[,c(-1,-2)])
#write.csv(df1_distribute_BG, file = "Mydata.csv",row.names = T,)

test_data_distribute_BG <- ts(df1_distribute_BG,start=c(2013,1),end=c(2016,11),frequency = 12)
plot(test_data_distribute_BG,main = "Distribution of Blood units for Jan 2013 - Nov-2016",
     xlab="Time-Frame in Years", ylab="Units of Blood samples issued",
     type = "h" )



accuracy(meanf(test_data_distribute_BG,h=5,fan = F,lambda = NULL))
accuracy(naive(test_data_distribute_BG,h=5,fan = F,lambda = NULL))
accuracy(rwf(test_data_distribute_BG,h=5,fan = F,lambda = NULL))

'Forecast Errors
forecast errors are different from residuals in two ways. First, residuals are calculated on the training set while forecast errors are 
calculated on the test set. Second, residuals are based on one-step forecasts while forecast errors can involve multi-step forecasts.
'


'compare the RMSE obtained via time series cross-validation with the residual RMSE.'
#far2 <- function(x, h){forecast(Arima(x, order=c(2,0,0)), h=h)}
'Computing the forecast error'
e <- tsCV(test_data_distribute_BG, rwf, drift=TRUE, h=1,window = 30)
#print(e)
#plot(e)
'RMSE'
sqrt(mean(e^2, na.rm=TRUE))

'Residual RMSE'
sqrt(mean(residuals(rwf(test_data_distribute_BG, drift=TRUE))^2, na.rm=TRUE))
'A good way to choose the best forecasting model is to find the model with the smallest RMSE computed using time series cross-validation.'

'Random Walk'
forecast_error_rwf <- test_data_distribute_BG %>% 
  tsCV(forecastfunction=rwf, drift=TRUE, h=1)

forecast_error_rwf^2 %>% 
  mean(na.rm=TRUE) %>% sqrt()

forecast_error_rwf

'Naive'
fe_naive <- tsCV(test_data_distribute_BG, forecastfunction=naive, h=8)
mse <- colMeans(fe_naive^2, na.rm = T)
mse
library(ggplot2)
# Plot the MSE values against the forecast horizon
data.frame(h = 1:8, MSE = mse) %>%
  ggplot(aes(x = h, y = MSE)) + geom_point()

'Residual Error'
res_error <- test_data_distribute_BG %>% 
  rwf(drift=TRUE) %>% residuals()
res_error
res_error^2 %>% mean(na.rm=TRUE) %>% sqrt()

#ADF TEST
adf.test(test_data_distribute_BG,alternative = "stationary")
#since for adftest p value is 0.01 which is less than 0.05 thus differencing is not required.
#if >0.05 then differencing is required.

#Kwiatkowski-Phillips-Schmidt-Shin (KPSS) test. This reverses the hypotheses, 
#so the null-hypothesis is that the data are stationary. In this case, 
#small p-values (e.g., less than 0.05) suggest that differencing is required.
#reverse of adftest
kpss.test(test_data_distribute_BG)

#Differencing the data
diff_data <- diff(test_data_distribute_BG)
diff_data

#determining the appropriate number of differences required for non-seasonal time series.
#nsdiffs(test_data)

#check for seasonality in data
Stl = stl(x =test_data_distribute_BG,s.window = "periodic")
summary(Stl)
auto.arima(test_data_distribute_BG)
fit <- Arima(test_data_distribute_BG)
fit
#auto.arima(test_data_distribute_total)
fit <- arima(test_data_distribute_total, c(0, 1, 1),
             seasonal = list(order = c(0, 1, 1), period = 12))
fit
summary(fit)
#fit <- Arima(diff_data], order=c(0,1,2), seasonal=c(0,0,0), 
 #     xreg=NULL, include.mean=F, include.drift=F, 
  #    include.constant = F, method="CSS-ML",model=NULL
    #  )
summary(fit)

plot(forecast(fit,h=3),main = "Forecast for Next three months",
     xlab="Time-Frame in Years", ylab="Units of Blood samples issued",
     type = "l")

Box.test(test_data,lag=1,type="Ljung")

plot(pacf(test_data_distribute_BG,lag.max = NULL,na.action = na.pass,plot = F),
     main = "Partial ACF ")

#as the acf plots drops quickly ,the series is stationary.
plot(acf(test_data_distribute_BG,lag.max = NULL,na.action = na.pass,
         plot = F,type = "correlation"),main = "ACF")

acf(fit$residuals)
forecast(auto.arima(test_data_distribute_BG))

#n.ahead species the number of years the prediction is required.
predicted  <- predict(fit, n.ahead =2 ) 
plot(predicted$pred)
predicted

#prediction in the month of January till 2015
plot <- ts.plot(test_data_distribute_total,2.718^predicted$pred, log = "y", lty = c(1,3), 
                          xlab="Time-Frame in Years", ylab="Units of Blood samples issued",
                          main = "Prediction for January month till 2025",type ="h")


#smoothening the plot
test_smooth <- HoltWinters(test_data_distribute_total,beta = F,gamma = F)

plot(test_smooth)
(test_smooth$seasonal)
plot(test_smooth$fitted)
plot(HoltWinters(test_data,beta = F,gamma = F,
               seasonal = "additive",l.start = 618.4467))
forecast_data <- forecast.HoltWinters(test_smooth,h=5)
#plot.forecast(forecast_data,type = "l")
#acf(forecast_data$residuals,lag.max = 20,na.action = na.pass)


plot(forecast(fit,h=3),main = "Forecast for Next three months",
     xlab="Time-Frame in Years", ylab="Units of Blood samples issued",
     type = "l")


