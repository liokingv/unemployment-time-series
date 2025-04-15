library(forecasting)

# read unemployment data
unemp <- read.csv("unemployment_rate_data.csv") #remove extraneous columns from data
unemp <- unemp[1:2]
#head(unemp)

# transform data into time series object
unemp.ts <- ts(unemp[2], start=c(1948,1),end=c(2021,11), frequency=12) #print(unemp.ts)

# take logarithm of monthly unemployment rate to stabilize data 
unemp.log <- log(unemp.ts)

# plot the original series
#par(mfrow =c(2, 2))
plot(unemp.ts, ylab = "Unemployment Rate",
     main = "US Monthly Unemployment Rate from \nJan. 1948 to Nov. 2021",
     col = "darkblue") # histogram of original series
hist(unemp.ts, main = "Histogram of Unemployment Rate from \nJan. 1948 to Nov. 2021", col = "steelblue", xlab = "Unemployment Rate (%)")

# time series plot of logarithms of series
plot(unemp.log, ylab = "log Unemployment Rate",
     main = "log(US Monthly Unemployment Rate) from \nJan. 1948 to Nov.
2021")

hist(unemp.log)

# create a time variable
unemp.time <- time(unemp.ts)

# TREND ANALYSIS (Kernel Smoothing) FOR LOG
# fit a nonparametric trend
unemp.loess.log <- loess(unemp.log ~ unemp.time, span = 0.15) # get the trend
unemp.loess.pred.log <- predict(unemp.loess.log)
# change it into a time series object
unemp.loess.trend.log <- ts(unemp.loess.pred.log, start=c(1948,1), end=c(2021,11), frequency = 12)

# overlay the trend on the time plot
plot(unemp.log, ylab = "log(Unemployment Rate)") 
lines(unemp.loess.trend.log, col = "blue", lty = 1, lwd = 5)


# Trend removal
# cubic trend removed & formatted into time series
# unemp.loess.trend.log
# get the residuals
unemp.res.log <- unemp.loess.log$residuals
# change it into time series
unemp.res.log <- ts(unemp.res.log, start=c(1948,1), end=c(2021,11), deltat = 1/12)
# plot it
plot(unemp.res.log, xlab = "Year", ylab = "Residuals after removing trend")


# Seasonality removal
unemp.month <-factor(cycle(unemp.res.log))
fit.season <-lm(unemp.res.log ~ unemp.month)
# estimates seasonal components
unemp.season <-ts(fit.season$fitted, start=c(1948,1), end=c(2021,11), deltat = 1/12)
# With both linear trend and seasonality removed
unemp.rand <-ts(unemp.res.log - unemp.season, start=c(1948,1),
                end=c(2021,11), deltat = 1/12)

# plot the decomposition (trend, seasonality, and residual)
par(mfrow =c(2, 2))
plot(unemp.log, xlab = "Year", ylab = "log(Unemployment Rate)")
plot(unemp.loess.trend.log, xlab = "Year", ylab = "Trend")
plot(unemp.season, xlab = "Year", ylab = "Seasonality")
plot(unemp.rand, xlab = "Year", ylab = "Random")


# DIFFERENCING: remove both trend and seasonality at the same time
#plot(diff(unemp.log), ylab = "Difference")
#First order difference
diff1 <- c(NA, diff(unemp.log))
diff1 <- ts(diff1, start=c(1948,1), end=c(2021,11), deltat = 1/12)
plot(diff1, xlab = "Year", ylab = "First Order Differenced Series")

# Seasonal Difference
diff12 <- c(NA, diff(diff1, lag = 12))
diff12 <- ts(diff12, start=c(1948,1), end=c(2021,11), deltat = 1/12)
plot(diff12, xlab = "Year", ylab = "Seasonal Difference at Lag 12")

# Check the ACF and PACF of differenced series
par(mfrow = c(1, 2))
acf(diff12, lag.max = 60, na.action = na.pass,
    main = "ACF for differenced series")
pacf(diff12, lag.max = 60, na.action = na.pass,
     main = "PACF for differenced series")


# Fitting models based on AFC and PACF plots, then selecting based on AIC
# SARIMA(1,1,4) x (0,1,2)_12
model1 <- arima(unemp.log, order = c(1, 1, 4),
                seasonal = list(order = c(0, 1, 2), period = 12))
model1


# SARIMA(1,1,1) x (0,1,2)_12
model2 <- arima(unemp.log, order = c(1, 1, 1),
                seasonal = list(order = c(0, 1, 2), period = 12))
model2


# SARIMA(3,1,4) x (0,1,2)_12
model3 <- arima(unemp.log, order = c(3, 1, 4),
                seasonal = list(order = c(0, 1, 2), period = 12))
model3


# SARIMA(4,1,4) x (0,1,2)_12
model4 <- arima(unemp.log, order = c(4, 1, 4),
                seasonal = list(order = c(0, 1, 2), period = 12))

df <- rbind("SARIMA(1,1,4) x (0,1,2)_12" = c(model1$aic, model1$loglik),
            "SARIMA(1,1,1) x (0,1,2)_12" = c(model2$aic, model2$loglik),
            "SARIMA(3,1,4) x (0,1,2)_12" = c(model3$aic, model3$loglik),
            "SARIMA(4,1,4) x (0,1,2)_12" = c(model4$aic, model4$loglik))

df <- as.data.frame(df)
names(df) <- c("AIC", "Log Likelihood")
df

# Model Diagnostics on model3 = SARIMA(1,1,1) x (0,1,2)_12
# Fitting residuals and ACF/PACF plots
par(mfrow = c(1, 2))
res <- model2$residuals
acf(res, lag.max = 36)
pacf(res, lag.max = 36)
# Diagnostic plots
tsdiag(model2)
# QQ plot of residuals
# Outlier but probably normal enough
qqnorm(res)
qqline(res)


# Forecasting
# Get forecasted values, h=60
pred <- predict(model2, n.ahead = 60)
# Plot forecasted values, change xlim to get better view of forecast
plot(unemp.log, xlim = c(1996, 2026), ylim = c(-1, 3.25),
     main = "Forecast of Log(Unemployment Rate)",
     ylab = "log(Unrate)")

# Forecasted values
lines(pred$pred, col = "red")
# 95% forecasting limits
lines(pred$pred-2*pred$se,col='blue')
lines(pred$pred+2*pred$se,col='blue')
legend("bottomleft", legend = c("Forecasted Values", "95% Forecasting
Limits"),
       col = c("red", "blue"), lty = 1)


# Backtransformed
# Plot forecasted values, change xlim to get better view of forecast
plot(unemp.ts, xlim = c(1996, 2026), ylim = exp(c(-1, 3.25)),
     main = "Forecast of Unemployment Rate",
     ylab = "Unemployment Rate")
# Forecasted values
lines(exp(pred$pred), col = "red")

# 95% forecasting limits
lines(exp(pred$pred-2*pred$se),col='blue')
lines(exp(pred$pred+2*pred$se),col='blue')

legend("topleft", legend = c("Forecasted Values", "95% Forecasting Limits"),
       col = c("red", "blue"), lty = 1)


# Holt-Winters Forecasting
fore.log.unemp <- hw(log(unemp.ts), seasonal = "multiplicative", h=60)
plot(fore.log.unemp, xlim = c(1996, 2026))
fore.unemp <- hw(unemp.ts, seasonal = "multiplicative", h=60)
plot(fore.unemp, xlim = c(1996, 2026), ylab = "Unemployment Rate", xlab =
       "Year")

# DECOMPOSE the series
unemp.dec.log <- decompose(unemp.log) # plot everything plot(unemp.dec.log)

# Spectral Analysis 



