library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(DT)
library(readxl)
library(tidyverse)
library(leaflet)
library(data.table)
library(plotly)

source("prepare_data.R")
# 
# # Load data
# allData = read.csv('./data/HI_ALL.csv', stringsAsFactors = F, sep = ";",check.names = F)
# offersInTime = read.csv('./data/Offersintime.csv', stringsAsFactors = F, sep = ";",check.names = F)
# items=read.csv('./data/Items.csv', stringsAsFactors = F, sep = ";",check.names = F)
# 
# 
# # Prepare data
# allData = prepareAllData(allData)
# offersInTime = prepareOffersInTime(offersInTime)
# items=prepareItems(items)
# 
# # ?? without effect -_-
# offersInTime = offersInTime[offersInTime$Auction_ID %in% allData$Auction_ID,]
# 
# print(sapply(allData,class))
# 
#        

allData = read.csv2('./data/prepared_data/allData.csv', stringsAsFactors = F, sep = ";",check.names = F)
offersInTime = read.csv2('./data/prepared_data/offersInTime.csv', stringsAsFactors = F, sep = ";",check.names = F)
offersInTime$Participant_ID <- as.factor(offersInTime$Participant_ID)

items = read.csv2('./data/prepared_data/items.csv', stringsAsFactors = F, sep = ";",check.names = F)
participants = data.table::fread(input = './data/Orderparticipant.csv')
map_data = prepareMapData(allData)

