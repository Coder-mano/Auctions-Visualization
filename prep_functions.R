
findType = function(data){
  type = data[[1]]
  typeClarif = data[[2]]
  resulType = ""
  if(type == "Purchase"){
    resulType = switch (typeClarif,
                        "Undefined" = "ERMMA",
                        "Demand" = "RFx - Demand",
                        "Qualifications" = "Qualifications",
                        "137" = "Electronic auction according to Act No.137/2006",
                        "134" = "Electronic auction according to Act No.134/2016",
                        "25" = "Electronic auction according to Act No.25/2006",
                        "343" = "Electronic auction according to Act No.343/2015",
                        "Sealed" = "Sealed offers",
                        "Nipon" = "NIPPON",
                        "Holland" = "HOLLAND",
                        "3" = "Type = Purchase & Clarification = 3",
                        "0" = "Type = Purchase & Clarification = 0"
    )
    
  }else if(type == "Sale"){
    resulType = switch (typeClarif,
                        "Undefined" = "ERMMA (Sale)",
                        "278" = "Electronic auction according to Act No.278/1993",
                        "drazbaproe" = "Auction",
                        "Nipon" = "NIPPON (sale)",
                        "Holland" = "HOLLAND sales e-auction",
                        "3" = "Type = Sale & Clarification = 3",
                        "0" = "Type = Sale & Clarification = 0"
    )
  }else{
    resulType = switch(type, 
                       "0" = paste("Type = 0 & Clarification = ", typeClarif),
                       "Undefined" = paste("Type = Undefined & Clarification = ", typeClarif)
    )
  }
  
  return(resulType)
}


addTotalBids <- function(offersInTime){
  offersInTime$New_BID = as.numeric(offersInTime$New_BID)
  offersInTime <- na.omit(offersInTime) 
  
  #Bid differences for each item & each Participant in auction
  dataT = offersInTime %>%
    group_by(Client,Auction_ID,Participant_ID,Item_ID) %>%
    arrange(Change_order) %>%
    mutate(diff = as.numeric(New_BID - lag(New_BID, default = first(New_BID))))
  dataT = data.frame(dataT)
  
  #Total bid value
  dataT = dataT %>%
    group_by(Client,Auction_ID,Participant_ID) %>%
    arrange(Change_order) %>%
    mutate(New_BID_Total = ifelse(diff == 0,as.numeric(cumsum(New_BID)),as.numeric((cumsum(New_BID)-New_BID + diff))))
  # Add to value if first occurance of item, else add difference
  dataT = data.frame(dataT)
  
  #Auction bid progress
  dataT = dataT %>% 
    group_by(Client,Auction_ID) %>%
    mutate(Best_Bids = getBestBids(New_BID_Total))
  
  #Item bid progress
  dataT = data.frame(dataT)%>% 
    group_by(Client,Auction_ID,Item_ID) %>%
    mutate(Best_Bids_Items = getBestBids(New_BID))
  
  dataT = data.frame(dataT)
  return(dataT)
}

getBestBids <- function(col){ 
  bestBids = numeric(length = 0)
  #check bid progress tendency (increasing/decreasing)
  if(col[1] < col[length(col)]){
    # value of bid * 1 if is greater than all rows before, * 0 else
    bestBids = as.numeric(col) * as.numeric(c(FALSE, sapply(2:length(col), function(i)
      all(col[i] > col[1:(i-1)]))))
  }else{
    # value of bid * 1 if is less than all rows before, * 0 else
    bestBids = as.numeric(col) * as.numeric(c(FALSE, sapply(2:length(col), function(i)
      all(col[i] < col[1:(i-1)]))))
  }
  #starting bid
  bestBids[1] = col[1]
  
  if(length(col) <2)
    bestBids = col
  
  return(bestBids)
}


