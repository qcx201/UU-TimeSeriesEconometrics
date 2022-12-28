######################
######################
# Dynamic multiplier
f11 <- function(j, lam1, lam2) {
  lam1^(j+1)/(lam1-lam2)+lam2^(j+1)/(lam2-lam1)
}

# Plot it for j=1, ..., 24
barplot(sapply(0:24, f11, lam1 = 0.5, lam2 = -0.1))



######################
######################
# House Price Index from Statistics Sweden, annual data 1982-2014
# Annual change
houseprice <- read.delim("houseprice.txt", header=FALSE)
houseprice <- ts(houseprice[, 2], start = 1982, frequency = 1, names = "Annual Change in House Price Index")

plot(houseprice)


# Merging series from different periods of time is easily accomplished
# Create a nonsense series, annual freq 1970-1999
temp <- ts(rnorm(30), start = 1970, frequency = 1)

merged <- cbind(houseprice, temp)






# Industrial Production from Statistics Sweden, quaterly data 2000Q3-2015Q2
indprod <- read.delim("industrialproduction.txt", header=TRUE)
indprod <- ts(indprod[, 2:3], start = c(2000, 3), frequency = 4)

# Multiple series can be plotted together or separately in the same figure
plot(indprod, plot.type = "multiple")
plot(indprod, plot.type = "single", lty = 1:2)

# Load the forecast package
library(forecast)

# tsdisplay plots the series, the ACF and the PACF
tsdisplay(houseprice)
tsdisplay(indprod[,1])
tsdisplay(indprod[,2])

# Estimate models: plot roots, compare AIC
house1 <- arima(houseprice, order = c(1, 0, 1))
plot(house1)
AIC(house1)

house2 <- arima(houseprice, order = c(3, 0, 0))
plot(house2)
AIC(house2)

house3 <- arima(houseprice, order = c(0, 0, 3))
plot(house3)
AIC(house3)

AIC(house1, house2, house3)


# !!!! WORD OF CAUTION !!!!

# Estimate an AR(1) by conditional sums of squares:
caution1 <- arima(houseprice, order = c(1, 0, 0), method = "CSS")
caution1 <- arima(houseprice, order = c(1, 0, 0), method = "ML")
caution1 <- arima(houseprice, order = c(1, 0, 0), method = "CSS-ML")

# House price index with its lag
houselag <- ts.union(houseprice, lag(houseprice, -1))
caution2 <- lm(houselag[, 1] ~ houselag[, 2])

# The 'intercept' in the arima output is actually not the intercept c, 
# it is the unconditional mean, which is c/(1-phi)
coef(caution1)
coef(caution2)

# Unconditional mean in caution2:
coef(caution2)[1]/(1-coef(caution2)[2])

# In the forecast package, there is a function for automatically selecting an appropriate arima model
autohouse    <- auto.arima(houseprice)
autoindprod  <- auto.arima(indprod[, 1])
autoindtrend <- auto.arima(indprod[, 2], max.D = 0, max.P = 0, max.Q = 0) # This is a trend: it should by construction be rid of any seasonal effects
