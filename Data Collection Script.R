#### Time Series Analysis Group Project ####
#### Load Libraries ####
library(cdcfluview)
library(tidyverse)
library(ggplot2)
library(tseries)
library(forecast)

#Set Working directory
setwd("~/Bellarmine Advanced Analytics/Time Series Forecasting/Time_Series_Project_2018")

#Read in flu data
flu.data <- read.csv("FluCasesReported.csv")
flu.ts <- ts(flu.data[,c(15,19,20,22)], start = 2009, frequency = 52)

flu.ts


# Plot All cases reported for flu data
autoplot(flu.ts[,3]) +
      ggtitle("Flu Cases per Week") +
      xlab("Weeks, 2009-present") +
      ylab("Flu Cases")


# Plot onto a polar plot
ggseasonplot(flu.ts[ ,3], polar = TRUE) +
      ylab("Flu Cases")+
      ggtitle("Flu Cases by Week")


# Plot onto a subseries plot
ggsubseriesplot(flu.ts[ ,3], polar = TRUE) +
      ylab("Flu Cases")+
      ggtitle("Flu Cases by Week")

## Decompose the time series
flu_decomp <- decompose(flu.ts[,3], type = "multiplicative")
plot(flu_decomp)

##ACF Plot
flu_acf <- acf(flu.ts[,3])

flu.diff <- diff(flu.ts[,3])
flu.acfd <- acf(flu.diff)
