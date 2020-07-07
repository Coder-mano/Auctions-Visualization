source("prep_functions.R")

allData = read.csv('./data/HI_ALL.csv', stringsAsFactors = F, sep = ";",check.names = F)
offersInTime = read.csv('./data/Offersintime.csv', stringsAsFactors = F, sep = ";",check.names = F)
items=read.csv('./data/Items.csv', stringsAsFactors = F, sep = ";",check.names = F)
map_data = NULL 

prepareAllData <- function(allData) {
  
  names(allData)[names(allData) == 'Klient'] <- 'Client'
  names(allData)[names(allData) == '_auctionData_vzorova_string'] <- 'auctionData_pattern'
  names(allData)[names(allData) == 'ID aukcie'] <- 'Auction_ID'
  names(allData)[names(allData) == '_eAuctionEvaulatedBy_string'] <- 'Evaluated_By'
  names(allData)[names(allData) == '_auctionData_typ_string'] <- 'Type'
  names(allData)[names(allData) == '_auctionData_typ_upresneni_string'] <- 'Type_Clarification'
  names(allData)[names(allData) == '_auctionData_vyhlas_id_statu_string'] <- 'ID_State'
  
  # Missing values replacement
  allData[['Type_Clarification']][allData[['Type_Clarification']]==''] <- 'Undefined'
  
  # Client ID trimming
  allData$ClientSuffix <- sub(".*-","",allData$Client)
  allData$Client <- sub("\\-.*","",allData$Client)
  
  allData$Type_Combined <- apply(allData[,c("Type","Type_Clarification")],M = 1, FUN = findType)
  allData$Type_Combined  <- vapply(allData$Type_Combined , paste, collapse = "", character(1L))
  
  # # Example auctions filtering
  # allData = filter(allData,auctionData_pattern == 0)
  # allData$auctionData_pattern <- NULL
  # 
  #---------------------MAP
  
  #replace numbers with country names
  allData$ID_State[allData$ID_State == 60 ] <- "Czech Republic"
  allData$ID_State[allData$ID_State == 211] <- "Slovakia"
  allData$ID_State[allData$ID_State == 107] <- "France"
  allData$ID_State[allData$ID_State == 105] <- "Germany"
  allData$ID_State[allData$ID_State == 200] <- "Finland"
  allData$ID_State[allData$ID_State == 187] <- "Italy"
  allData$ID_State[allData$ID_State == 209] <- "Netherlands"
  allData$ID_State[allData$ID_State == 15 ] <- "Hungary"
  allData$ID_State[allData$ID_State == 61 ] <- "Spain"
  allData$ID_State[allData$ID_State == 84 ] <- "Sweden"
  allData$ID_State[is.na(allData$ID_State)] <- 0
  allData$ID_State[allData$ID_State == 0  ] <- "Other"
  
  #add longitude and latitude
  return(allData)
}

prepareMapData <- function(map_data) {
 
  map_data = unique(allData[,c("ID_State")])
  map_data$lng <- ifelse(map_data$ID_State == "Slovakia", 19.69902 ,
                         ifelse(map_data$ID_State == "France", 2.213749,
                                ifelse(map_data$ID_State == "Czech Republic", 15.47296,
                                       ifelse(map_data$ID_State == "Finland", 25.74815,
                                              ifelse(map_data$ID_State == "Germany", 10.45153,
                                                     ifelse(map_data$ID_State == "Italy", 12.56738,
                                                            ifelse(map_data$ID_State == "Netherlands", 5.291266,
                                                                   ifelse(map_data$ID_State == "Hungary", 19.5033,
                                                                          ifelse(map_data$ID_State == "Spain", -3.74922,
                                                                                 ifelse(map_data$ID_State =="Sweden",18.6435,0))))))))))
  
  
  map_data$lat <- ifelse(map_data$ID_State == "Slovakia", 48.69903 ,
                         ifelse(map_data$ID_State == "France", 46.22764,
                                ifelse(map_data$ID_State == "Czech Republic", 49.81749,
                                       ifelse(map_data$ID_State == "Finland", 61.92411,
                                              ifelse(map_data$ID_State == "Germany", 51.16569,
                                                     ifelse(map_data$ID_State == "Italy", 41.87194,
                                                            ifelse(map_data$ID_State == "Netherlands", 52.13263,
                                                                   ifelse(map_data$ID_State == "Hungary", 47.16249,
                                                                          ifelse(map_data$ID_State == "Spain", 40.46367, 
                                                                                 ifelse(map_data$ID_State == "Sweden", 60.12816,0.1))))))))))
  
  
  map_data$max_BID_Value <- ifelse(map_data$ID_State == "Slovakia", 562068000 ,
                                   ifelse(map_data$ID_State == "France", 66660000,
                                          ifelse(map_data$ID_State == "Czech Republic", 2500000000,
                                                 ifelse(map_data$ID_State == "Finland", 15525529,
                                                        ifelse(map_data$ID_State == "Germany", 11597801,
                                                               ifelse(map_data$ID_State == "Italy", 121094945268,
                                                                      ifelse(map_data$ID_State == "Netherlands", 0,
                                                                             ifelse(map_data$ID_State == "Hungary", 163370,
                                                                                    ifelse(map_data$ID_State == "Spain", 51972150,
                                                                                           ifelse(map_data$ID_State == "Sweden",1425829,3911780))))))))))
  
  
  map_data$min_BID_Value <- ifelse(map_data$ID_State == "Slovakia", -60010 ,
                                   ifelse(map_data$ID_State == "France", 0,
                                          ifelse(map_data$ID_State == "Czech Republic", -2775270,
                                                 ifelse(map_data$ID_State == "Finland", 39619.62,
                                                        ifelse(map_data$ID_State == "Germany", 0,
                                                               ifelse(map_data$ID_State == "Italy", -155000,
                                                                      ifelse(map_data$ID_State == "Netherlands", 0,
                                                                             ifelse(map_data$ID_State == "Hungary", 43050,
                                                                                    ifelse(map_data$ID_State == "Spain", 0, 
                                                                                           ifelse(map_data$ID_State == "Sweden",1425829,0.0))))))))))
  
  #--------------------- end -> MAP
}

prepareOffersInTime <- function(offersInTime) {
  # Prepare data
  names(offersInTime)[names(offersInTime) == 'New BID'] <- 'New_BID'
  names(offersInTime)[names(offersInTime) == 'ID itemu'] <- 'Item_ID'
  names(offersInTime)[names(offersInTime) == 'Poradie zmeny'] <- 'Change_order'
  names(offersInTime)[names(offersInTime) == 'ID aukcie'] <- 'Auction_ID'
  names(offersInTime)[names(offersInTime) == 'Klient'] <- 'Client'
  names(offersInTime)[names(offersInTime) == 'cas'] <- 'Date'
  names(offersInTime)[names(offersInTime) == 'ID ucastnika'] <- 'Participant_ID'
  
  
  
  
  # toFactor
  offersInTime$Participant_ID <- as.factor(offersInTime$Participant_ID)
  #offersInTime$New_BID <- strtoi(offersInTime$New_BID)
  offersInTime$Date = as.POSIXct(offersInTime$Date, format= " %d.%m.%y %H:%M")
  
  offersInTime = addTotalBids(offersInTime)
  
  return(offersInTime) 
}

prepareItems = function(items){
  names(items)[names(items) == 'ID aukcie'] <- 'Auction_ID1'
  names(items)[names(items) == 'ID itemu'] <- 'Item_ID1'
  names(items)[names(items) == '_mnozstvi_string'] <- 'Quantity'
  names(items)[names(items) == '_cena_minula_string'] <- 'Past_Price'
  #library(dplyr)
  items=items[c(1:13)]
  return(items)
}


#write.table(prepareOffersInTime(offersInTime),file = "./data/offersInTime.csv",row.names = F,col.names = T,sep = ",")
#write.table(prepareItems(items),file = "./data/items.csv",row.names = F,col.names = T,sep = ",")
#write.table(prepareAllData(allData),file='./data/allData.csv',row.names = F,col.names = T,sep = ",")

allData = prepareAllData(allData)
offersInTime = prepareOffersInTime(offersInTime)
items = prepareItems(items)

# create folder & save prepared data
dir.create("data/prepared_data", showWarnings = FALSE)

write.csv2(allData, "./data/prepared_data/allData.csv", row.names = FALSE)
write.csv2(offersInTime, "./data/prepared_data/offersInTime.csv", row.names = FALSE)
write.csv2(items, "./data/prepared_data/items.csv", row.names = FALSE)
