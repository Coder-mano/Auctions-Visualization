# Data preparation

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
  
  # Example auctions filtering
  allData = filter(allData,auctionData_pattern == 0)
  allData$auctionData_pattern <- NULL
  
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
  
  unique(allData$ID_State)
  allData$lng <- ifelse(allData$ID_State == "Slovakia", 19.69902 ,
                        ifelse(allData$ID_State == "France", 2.213749,
                               ifelse(allData$ID_State == "Czech Republic", 15.47296,
                                      ifelse(allData$ID_State == "Finland", 25.74815,
                                             ifelse(allData$ID_State == "Germany", 10.45153,
                                                    ifelse(allData$ID_State == "Italy", 12.56738,
                                                           ifelse(allData$ID_State == "Netherlands", 5.291266,
                                                                  ifelse(allData$ID_State == "Hungary", 19.5033,
                                                                         ifelse(allData$ID_State == "Spain", -3.74922,
                                                                                ifelse(allData$ID_State =="Sweden",18.6435,0))))))))))
  
  
  allData$lat <- ifelse(allData$ID_State == "Slovakia", 48.69903 ,
                        ifelse(allData$ID_State == "France", 46.22764,
                               ifelse(allData$ID_State == "Czech Republic", 49.81749,
                                      ifelse(allData$ID_State == "Finland", 61.92411,
                                             ifelse(allData$ID_State == "Germany", 51.16569,
                                                    ifelse(allData$ID_State == "Italy", 41.87194,
                                                           ifelse(allData$ID_State == "Netherlands", 52.13263,
                                                                  ifelse(allData$ID_State == "Hungary", 47.16249,
                                                                         ifelse(allData$ID_State == "Spain", 40.46367, 
                                                                                ifelse(allData$ID_State == "Sweden", 60.12816,0.1))))))))))
  
  
  allData$max_BID_Value <- ifelse(allData$ID_State == "Slovakia", 562068000 ,
                                  ifelse(allData$ID_State == "France", 66660000,
                                         ifelse(allData$ID_State == "Czech Republic", 2500000000,
                                                ifelse(allData$ID_State == "Finland", 15525529,
                                                       ifelse(allData$ID_State == "Germany", 11597801,
                                                              ifelse(allData$ID_State == "Italy", 121094945268,
                                                                     ifelse(allData$ID_State == "Netherlands", 0,
                                                                            ifelse(allData$ID_State == "Hungary", 163370,
                                                                                   ifelse(allData$ID_State == "Spain", 51972150,
                                                                                          ifelse(allData$ID_State == "Sweden",1425829,3911780))))))))))
  
  
  allData$min_BID_Value <- ifelse(allData$ID_State == "Slovakia", -60010 ,
                                  ifelse(allData$ID_State == "France", 0,
                                         ifelse(allData$ID_State == "Czech Republic", -2775270,
                                                ifelse(allData$ID_State == "Finland", 39619.62,
                                                       ifelse(allData$ID_State == "Germany", 0,
                                                              ifelse(allData$ID_State == "Italy", -155000,
                                                                     ifelse(allData$ID_State == "Netherlands", 0,
                                                                            ifelse(allData$ID_State == "Hungary", 43050,
                                                                                   ifelse(allData$ID_State == "Spain", 0, 
                                                                                          ifelse(allData$ID_State == "Sweden",1425829,0.0))))))))))
  
  
  #--------------------- end -> MAP
  return(allData)
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

