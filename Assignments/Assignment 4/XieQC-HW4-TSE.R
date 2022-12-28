library(rio)
library(dplyr)
library(vars)
library(urca)
library(fUnitRoots)
library(tseries)


### Load and clean data


# Search interest & exchange rates
url <- "https://www.riksbank.se/en-gb/statistics/search-interest--exchange-rates/?c=cAverage&f=Month&from=02%2f01%2f2012&g5-SEDP1WSTIBORDELAYC=on&g7-SEGVB10YC=on&s=Comma&to=21%2f10%2f2022&export=csv"

raw <- rio::import(url)

# 1-week STIBOR
i_1w <- raw[raw$Series == "STIBOR 1W", 4] %>%
  gsub(",", ".", .) %>%
  as.numeric() %>%
  ts(start=2012, frequency = 12)

# 10-year Swedish government bonds
i_10y <- raw[raw$Series == "SE GVB 10Y", 4] %>%
  gsub(",", ".", .) %>%
  as.numeric() %>%
  ts(start=2012, frequency = 12)

data <- ts.union(i_1w, i_10y) %>%
  window(start=c(2012, 1), end=c(2020, 7))

# 1. Plot data ####

# Plot data
jpeg("HW3-data.jpg", width = 650, height = 500)
plot.ts(data, plot.type = "multiple",
        main = 'Interest rates: 1 week and 10 year'
        )
dev.off()

# Differences is not stationary
jpeg("HW3-int-diff.jpg", width = 650, height = 400)
plot.ts(data[,1]-data[,2],
        main = 'Interest rate differences',
        ylab = 'i_1w - i_10Y')
dev.off()

# Test stationarity of interest rates
adfTest(i_1w, lags = 8, type = "ct")  # Case 4
adfTest(i_1w, lags = 8, type = "c")   # Case 2

adfTest(i_10y, lags = 8, type = "ct") # Case 4
adfTest(i_10y, lags = 8, type = "c")  # Case 2


# 2. Estimate cointegrating rank ####


# All are non-stationary
adfTest(data[,1], lags = 8, type = "ct")   # Case 4
adfTest(data[,1], lags = 8, type = "c")    # Case 2

adfTest(data[,2], lags = 8, type = "ct")   # Case 4
adfTest(data[,2], lags = 8, type = "c")    # Case 2


# Estimate var order
var <- VAR(data, type='const', lag.max=8)
var$p

# Johansen's procedure
jo <- ca.jo(data, type = "trace", K = var$p, spec = "transitory", ecdet = "none")
summary(jo)


# 3. Test i_1w - i_10Y is a cointegrating relation ####
B <- matrix(c(1, -1), nrow = 2)
test <- blrtest(z = jo, H=B, r=1)
summary(test)


# 4. Unit root test ####
y <- data[,1]-data[,2]
adfTest(y, type = "nc",lags=var$p)
adfTest(y, type = "c", lags=var$p)
adfTest(y, type = "ct", lags=var$p)