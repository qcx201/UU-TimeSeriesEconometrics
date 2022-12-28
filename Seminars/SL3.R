library(forecast)
# Simulate an AR(1), yt = 0.8 + 0.8*y_(t-1) + e_t, var(e_t) = 4
set.seed(20160922)
y <- arima.sim(n = 100, list(ar = 0.8, sd = 2)) + .8


arima(y, order = c(1, 0, 0), seasonal = c(0, 0, 0), method = "CSS")
arima(y, order = c(1, 0, 0), seasonal = c(0, 0, 0), method = "CSS-ML")
arima(y, order = c(1, 0, 0), seasonal = c(0, 0, 0), method = "ML")


exactML <- function(pars, y) {
  cc <- pars[1]
  phi <- pars[2]
  sigma2 <- pars[3]
  TT <- length(y)
  
  logLike <- 1/2*log(2*pi) - 1/2 * log(sigma2/(1-phi^2)) - 
    (y[1]-cc/(1-phi))^2/(2*sigma2/(1-phi^2))-(TT-1)/2*log(2*pi) - 
    (TT-1)/2*log(sigma2) - sum((y[2:TT]-cc-phi*y[1:TT-1])^2/(2*sigma2))
  return(-logLike)
}

optim(c(1, 0, 1), exactML, y = y)$par
