#### Time Series Analysis Group Project ####
#### Load Libraries ####
library(cdcfluview)
library(tidyverse)
#### Acquiring Data ####
## Geographic Level Data ##
data <- geographic_spread()
data <- data[c(1,4:7)]
## Hospitalizations ##
hosdata <- hospitalizations()
## Get State Data ##
statedata <- ili_weekly_activity_indicators()
statedata <- filter(statedata, activity_level_label != "Insufficient Data")
statedata <- statedata[c(1,4:8)]
