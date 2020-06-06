
prepareAllData <- function(allData) {
  
  names(allData)[names(allData) == '_auctionData_vzorova_string'] <- 'auctionData_pattern'
  names(allData)[names(allData) == 'ID aukcie'] <- 'Auction_ID'
  
  allData = filter(allData,auctionData_pattern == 0)
  
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
  offersInTime$New_BID <- strtoi(offersInTime$New_BID)
  offersInTime$Date = as.POSIXct(offersInTime$Date, format= " %d.%m.%y %H:%M")
  
  return(offersInTime) 
}

