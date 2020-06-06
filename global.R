library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(DT)
source("prepare_data.R")

# Load data
offersInTime = read.csv('./data/Offersintime.csv', stringsAsFactors = F, sep = ";",check.names = F)
allData = read.csv('./data/HI_ALL.csv', stringsAsFactors = F, sep = ";",check.names = F)


# Prepare data
allData = prepareAllData(allData)
offersInTime = prepareOffersInTime(offersInTime)

# ?? without effect -_-
offersInTime = offersInTime[offersInTime$Auction_ID %in% allData$Auction_ID,]

print(sapply(allData,class))

       