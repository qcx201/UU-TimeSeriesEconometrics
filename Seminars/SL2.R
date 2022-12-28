library(forecast)
# Industrial Production from Statistics Sweden, quaterly data 2000Q3-2015Q2
indprod <- read.delim("industrialproduction.txt", header=TRUE)
indtrend <- ts(indprod[, 3], start = c(2000, 3), frequency = 4)

indmod <- auto.arima(indtrend, seasonal = FALSE)

# One-step ahead
forecast(indmod, h = 1)

# Four steps ahead
forecast(indmod, h = 4)

# Plot the forecast
plot(forecast(indmod, h = 4))

# If you want to reduce the sample, window() is very convenient to use
indtrend
window(indtrend, end = 2007)
window(indtrend, end = c(2007, 2))
window(indtrend, end = 2007.25)

# Make predictions for 2007Q1 and onwards
# Total out of sample size
window(indtrend, start = 2007)
k <- length(window(indtrend, start = 2007))

# Save the model chosen
modelorder <- arimaorder(indmod)
preds <- ts(NA, start = c(2007, 1), end = c(2015, 2), frequency = 4)
for (t in 1:k) {
  loopmodel <- Arima(window(gdp, end = 2006.5 + t/4), order = modelorder)
  preds[t] <- forecast(loopmodel, h = 2)$mean
}

ts.plot(ts.union(indtrend, preds), col = 1:2)
