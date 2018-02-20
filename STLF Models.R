

#### Time Series Analysis Group Project ####
#### Load Libraries ####
library(cdcfluview)
library(tidyverse)
library(ggplot2)
library(tseries)
library(forecast)
#Read in flu data
flu.data <- read.csv("FluCasesReported.csv")
flu.ts <- ts(flu.data[,c(15,19,20,22)], start = 2010, frequency = 52)
flu.ts
#### Explortatory Analysis ####

# Plot All cases reported for flu data
autoplot(flu.ts[,3]) +
      ggtitle("Flu Cases per Week") +
      xlab("Weeks, 2009-present") +
      ylab("Flu Cases")

# Plot Flu A and B
autoplot(flu.ts[,1:2]) +
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

# Decompose Flu A
flua_decomp <- decompose(flu.ts[,1])
plot(flua_decomp)

# Decompose Flu B
flub_decomp <- decompose(flu.ts[,2], type = "multiplicative")
plot(flub_decomp)

##ACF Plot all Flu 
flu_acf <- acf(flu.ts[,3])
flu.diff <- diff(flu.ts[,3])
flu.acfd <- acf(flu.diff)

## ACF Flu A
flua_acf <- acf(flu.ts[,1])
flua.diff <- diff(flu.ts[,1])
flua.acfd <- acf(flua.diff)

## ACF Flu B
flub_acf <- acf(flu.ts[,2])
flub.diff <- diff(flu.ts[,2])
flub.acfd <- acf(flub.diff)

## Create a Forcast Model with Flu A
fitA <- stlf(flu.ts[,1])
plot(fitA)

# Plot with BoxCox Lambda transformation
lambda <- BoxCox.lambda(flu.ts[,1])
fitA2 <- stlf(flu.ts[,1], lambda = lambda)
plot(fitA2)
autoplot(fitA2, level = c(80, 95), h = 100) +
      ggtitle("Forecast for Flu A") +
      xlab("Weeks, 2009-2020") +
      ylab("Flu Cases") + 
      theme_classic() +
      theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"))

checkresiduals(fitA2)

## Forcast for Flu B
fitB <- stlf(flu.ts[,2]) 
plot(fitB)

# Plot with BoxCox Lambda transformation
lambda2 <- BoxCox.lambda(flu.ts[,2])
fitB2 <- stlf(flu.ts[,2], lambda = lambda2) 
plot(fitB2)
autoplot(fitB2, level = c(80, 95), h = 100) +
      ggtitle("Forecast for Flu B") +
      xlab("Weeks, 2009-2020") +
      ylab("Flu Cases") + 
      theme_classic() +
      theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"))

checkresiduals(fitB2)

# Forcast for all Flu
fitall <- stlf(flu.ts[,3])
autoplot(fitall)

# Plot with BoxCox Lambda transformation
lambda3 <- BoxCox.lambda(flu.ts[,3])
fitall2 <- stlf(flu.ts[,3], lambda = lambda3)
autoplot(fitall2)
autoplot(fitall2, level = c(80, 95), h = 100) +
      ggtitle("Forecast for A and B") +
      xlab("Weeks, 2009-2020") +
      ylab("Flu Cases") + 
      theme_classic() +
      theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"))

checkresiduals(fitall2)
