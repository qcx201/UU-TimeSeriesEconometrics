library(rio)
library(dplyr)
library(magrittr)
library(forecast)
library(xtable)

### Load and clean data

# https://scb.se/en/finding-statistics/statistics-by-subject-area/national-accounts/national-accounts/national-accounts-quarterly-and-annual-estimates/pong/tables-and-graphs/tables/gdp-quarterly/
# 11. GDP: expenditure approach (ENS 2010), seasonally adjusted (xlsx)
url <- "https://www.statistikdatabasen.scb.se/sq/17384"

raw <- rio::import(url)

# keep only row row number
n = 33
title = paste(raw[n, 1], raw[n, 2], sep='\n')
gdp <- raw[n, ] %>% t()

# set column name
colnames(gdp) <- title
rownames(gdp) <- raw[2, ]

# change in GDP volume
gdp <- gdp[-c(1:3)] %>%
  replace('..', 'na') %>%
  as.numeric() %>%
  na.omit() %>%
  ts(start=c(1980, 2), frequency = 4)


### Questions

### Question 1.1

# plot trend data, ACF and PACF
jpeg("HW2-gdp_tsdisplay.jpg", width = 650, height = 500)
tsdisplay(gdp, main=title, ylab='% change in GDP')
dev.off()

### Question 1.2

## generate prediction

model_predict <- function(model, sample, h=1, frequency=4){
  
  modelorder = arimaorder(model)
  
  # start end time of sample
  test_start <- time(sample)[1]
  test_end <- time(sample)[length(sample)]
  
  preds <- ts(NA, start=test_start+(h-1)/frequency,
              end=test_end,
              frequency=frequency)
  
  k <- length(preds)
  
  for (t in 1:k) {
    loopmodel <- Arima(window(gdp,end=test_start + (h+t-3)/frequency),
                       order=modelorder)
    
    preds[t] <- forecast(loopmodel, h=h)$mean[h]
  }
  
  return(preds)
}


## evaluation functions

RMSE <- function(model, sample, h=1){
  
  pred = model_predict(model, sample, h=h)
  
  k = length(pred)
  res <- 0
  for (t in 1:k) {
    res = res + (pred[t] - sample[t+h-1])^2
  }
  res = (res/(k-h+1))^(1/2)
  return(res)
}

MAE <- function(model, sample, h=1){
  pred = model_predict(model, sample, h=h)
  k = length(pred)
  res <- 0
  for (t in 1:k) {
    res = res + abs(pred[t] - sample[t+h-1])
  }
  res = (res/(k-h+1))
  return(res)
}

bias <- function(model, sample, h=1){
  pred = model_predict(model, sample, h=h)
  k = length(pred)
  res <- 0
  for (t in 1:k) {
    res = res + pred[t] - sample[t+h-1]
  }
  res = (res/(k-h+1))
  return(res)
}


# evaluation create models

orders <- matrix(NA, nrow=6, ncol=3)
ordnames <- matrix(NA, nrow=nrow(orders))
i <- 0
for (q in 1:2) {
  for (p in 1:2) {
    i = i + 1
    ordnames[i] = 'ARMA'
    orders[i, ] = c(p, 0, q)
  }
}

# self chosen model
ordnames[5] = 'choice.ARMA'
orders[5,] = c(2, 0, 2)

# auto model
model <- auto.arima(gdp, seasonal=FALSE, max.p=4, max.q=4)
modelorder = arimaorder(model)

ordnames[6] = 'auto.ARMA'
orders[6,] = modelorder

ordnames = paste(ordnames, '(p=', orders[,1], ', q=', orders[,3],')', sep='')
rownames(orders) = ordnames
orders

## evaluate the models on sample

sample = window(gdp, start=c(2012, 1), end = c(2015, 2))
res = matrix('', nrow=2*nrow(orders), ncol=6)

n <- 1

for (i in 1:nrow(orders)){
  
  cat('\n--------------------------------\n')
  
  name <- ordnames[i]
  cat(name, '\n')
  
  model <- arima(sample, order = orders[i, ])
  # plot(model)
  
  aic_val = AIC(model)
  cat('AIC:', aic_val, '\n\n')
  
  res[n, 1] = name
  res[n, 2] = round(aic_val, 3)
  
  for (h in 1:2) {
    rmse_val = RMSE(model, sample, h=h)
    mae_val = MAE(model, sample, h=h)
    bias_val = bias(model, sample, h=h)
    
    cat(gsub('%h', h, 'RMSE(h = %h) ='), rmse_val, ' ')
    cat(gsub('%h', h, 'MAE(h = %h) ='), mae_val, ' ')
    cat(gsub('%h', h, 'bias(h = %h) ='), bias_val, '\n')
    
    res[n, 3] = paste(h)
    res[n, 4] = round(rmse_val, 3)
    res[n, 5] = round(mae_val, 3)
    res[n, 6] = round(bias_val, 3)
    
    n = n + 1
    
  }
}

colnames(res) <- c('Model', 'AIC', 'h',
                   'RMSE(h)', 'MAE(h)', 'BIAS(h)'
                   )

tab = xtable(res, type = "latex")

align(tab) <- "c|l|c||c|ccc|"

hlines <- append(c(-1, -1), 2 * c(0:6))
hlines <- append(hlines, 12)

print(tab, file = "HW2-res_table.tex",
      hline.after = hlines,
      include.rownames = FALSE
      )
