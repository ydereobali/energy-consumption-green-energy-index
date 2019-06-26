##########################################################################
# Title: Calculate green energy consumption rate (index of the household
# Author: Yigit Dereobali
# Name: ubiqum module 3 task 2 index
# Description: Import hourly power prices for the current day.
#   Consumers can adjust their hourly consumption accordingly.
#   The lower the price, the more is renewable power generation.
#   Import yesterday's power generation data of renewable and 
#   conventional power generation. 
#   Calculate the ratio of renewable energy within household's consumption 
# Date: 21.06.2019
# Version: 1.0
##########################################################################


# LIBRARIES ####
library(readr)
library(dplyr)
library(caret)
library(ggplot2)
library(data.table)
library(plotly)
library(stats)
library(RMySQL)
library(lubridate)
library(zoo)
library(forecast)


# IMPORT DATA ####
# import already crawled hourly price data
hourly_prices <- fread("~/Desktop/Kodlama/python bible/current_hourly_power_prices.csv")

# import and process demand and supply data
demandsupply <- fread("demandsupply.csv")
demandsupply$renewable.generation.ratio <- demandsupply$renewables_generation / demandsupply$country_consumption


# CALCULATIONS ####
# create consumption data table
consumption <- data.table(main.meter = mydata$Global_active_power)
consumption$date <- mydata$Date
consumption$hour <- mydata$hour


# set day parameters
current.day <- today(tzone = "GMT")
previous.day <- current.day - 1 
next.day <- current.day + 1


# create yesterday's hourly consumption
# day parameter should be used
previous.day.consumption <- consumption[consumption$date == "2009-10-22"] # set previous.day
previous.day.hourly.consumption <- previous.day.consumption %>% 
  group_by(hour) %>% 
  summarise(main.meter = sum(main.meter))


# claculate green energy consumption of the household
previous.day.hourly.consumption$renewable.generation.ratio <- demandsupply$renewable.generation.ratio
previous.day.hourly.consumption$green.consumption <- previous.day.hourly.consumption$main.meter * previous.day.hourly.consumption$renewable.generation.ratio


# calculate green energy consumption ratio of the household
sum.green.consumption <- sum(previous.day.hourly.consumption$green.consumption)
sum.main.meter <- sum(previous.day.hourly.consumption$main.meter)
your.green.energy.consumption.ratio <- sum.green.consumption / sum.main.meter * 100


# RESULT ####
# green energy consumption ratio
print(your.green.energy.consumption.ratio)
