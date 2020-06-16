# Data preparation

prepareAllData <- function(allData) {
  
  names(allData)[names(allData) == 'Klient'] <- 'Client'
  names(allData)[names(allData) == '_auctionData_vzorova_string'] <- 'auctionData_pattern'
  names(allData)[names(allData) == 'ID aukcie'] <- 'Auction_ID'
  names(allData)[names(allData) == '_eAuctionEvaulatedBy_string'] <- 'Evaluated_By'
  names(allData)[names(allData) == '_auctionData_typ_string'] <- 'Type'
  names(allData)[names(allData) == '_auctionData_typ_upresneni_string'] <- 'Type_Clarification'
  
  # Missing values replacement
  allData[['Type_Clarification']][allData[['Type_Clarification']]==''] <- 'Undefined'
  
  # Client ID trimming
  allData$ClientSuffix <- sub(".*-","",allData$Client)
  allData$Client <- sub("\\-.*","",allData$Client)
  
  # Example auctions filtering
  allData = filter(allData,auctionData_pattern == 0)
  allData$auctionData_pattern <- NULL
  
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
  library(dplyr)
  items=select(items, Klient, Auction_ID1, Item_ID1, Quantity, Past_Price)
  return(items)
}

