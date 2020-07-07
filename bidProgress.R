#**************
# Bid progress UI.R

bidProgress <- tabItem(tabName = "bids",
                       fluidRow(
                         box(width = 12,
                             column(width = 4,
                                    selectizeInput('bidType', 'Type', 
                                                   choices = na.omit(unique(allData$Type))
                                    )),
                             column(width = 4,
                                    selectizeInput('bidClarification', 'Type Clarification', 
                                                   choices = c("") #observed na.omit(unique(allData$Type_Combined))
                                    )),
                             column(
                               width = 4,
                               selectizeInput('bidEvaluation', 'Evaluation', 
                                              choices = c("") #observed #na.omit(unique(allData$EvaluatedBy))
                               )),
                             column(
                               width = 4,
                               selectizeInput('bidAuction', 'Auction ID', 
                                              choices = c("") #observed #na.omit(unique(offersInTime$Auction_ID))
                               )),
                             column(
                               width = 4,
                               selectizeInput('bidItem', 'Item ID', 
                                              choices = c("") #observed
                               )),
                             column(
                               width = 4,
                               radioButtons("group", NULL,
                                            c("Agregated" = "agregated",
                                              "Participants" = "participants",
                                              "Progress" = "progres"))
                             ),
                             column(
                               width = 12,
                               plotlyOutput("bidAuctions")),
                             DT::dataTableOutput('bidDesc')
                         ))
)



# Auctions server.R

# Observe Type selection
bidTypeObserver <- function(input,session) {
  updateSelectInput(
    session, 'bidClarification',    # Update lower hierarchy dropdown
    choices = na.omit(unique(filter(allData, Type == input$bidType)$Type_Combined))
  )
  bidClarificationObserver(input,session)  
}

# Observe Type Clarification
bidClarificationObserver <- function(input,session) {
  updateSelectInput(
    session, 'bidEvaluation',
    choices = na.omit(unique(filter(allData, Type == input$bidType & Type_Combined == input$bidClarification)$Evaluated_By))
  )
  bidEvaluationObserver(input,session)
}

# Observe Evaluation selection
bidEvaluationObserver <- function(input,session) {
  updateSelectInput(
    session, 'bidAuction',
    choices = na.omit(unique(filter(allData, Evaluated_By == input$bidEvaluation & Type == input$bidType & Type_Combined == input$bidClarification)$Auction_ID))
  )
}

bidRenderAuctionPlot <- function(input,output,session){
  cl = filter(offersInTime, Auction_ID == input$bidAuction)
  cl = cl$Client[1]
  updateSelectInput(
    session, 'bidItem',
    choices = c("All",filter(offersInTime, Auction_ID == input$bidAuction & Client == cl)$Item_ID)
  )
  return(renderPlotly({
    
    # basic tmp filtering
    subData = filter(offersInTime, Auction_ID == input$bidAuction)
    subData = filter(subData, Client == subData$Client[1])
    if(input$bidItem != "All")
      subData = filter(subData, Item_ID == input$bidItem)
    #print(sapply(subData,class))
    
    desc = filter(allData, Auction_ID == input$bidAuction)
    desc = filter(desc, Client == subData$Client[1])
    
    subData$New_BID = as.integer(subData$New_BID)
    
    #tmp visuals
    output$bidDesc = DT::renderDataTable({
      datatable(desc, rownames = F,options = list(scrollX = TRUE))
    })
    
    subData$Participant_ID = as.character(subData$Participant_ID)
    if (nrow(subData) != 0) { 
      if (input$group == "agregated") {
        p <- plot_ly(
          subData, x = ~Change_order, y = ~New_BID, type = 'scatter',
          text = ~paste("Bid value: ", New_BID, '<br>Participant:', Participant_ID), mode = 'lines+markers')%>%
          config(displayModeBar = FALSE)
        
      }else if(input$group == "participants"){
        col = ifelse(input$bidItem == "All","New_BID_Total","New_BID")
        p <- plot_ly(
          subData, x = ~Change_order, y = ~subData[,c(col)], type = 'scatter', color = ~Participant_ID,
          text = ~paste("Bid value: ", subData[,c(col)], '<br>Participant:', Participant_ID), mode = 'lines+markers')%>%
          layout(yaxis = list(title = col)) %>% config(displayModeBar = FALSE)
        
      }else{
        
        if (input$bidItem == "All") {
          subData2 <- filter(subData, Best_Bids != 0)
          p <- plot_ly(
            subData2, x = ~Change_order, y = ~Best_Bids, type = 'scatter', marker = list(color ="hsl(0, 100%, 50%)"), line = list(color ="hsl(0, 100%, 50%)"),
            text = ~paste("Best bid: ", subData2[,c("Best_Bids")], '<br>Participant:', Participant_ID), mode = 'lines+markers')%>%
            config(displayModeBar = FALSE)
        }else{
          subData2 <- filter(subData, subData$Best_Bids_Items != 0)
          p <- plot_ly(
            subData2, x = ~Change_order, y = ~Best_Bids_Items, type = 'scatter', marker = list(color ="hsl(0, 100%, 50%)"), line = list(color ="hsl(0, 100%, 50%)"),
            text = ~paste("Best bid: ", subData2[,c("Best_Bids_Items")], '<br>Participant:', Participant_ID), mode = 'lines+markers')%>%
            config(displayModeBar = FALSE)
          
        }
      }
    }
    p
  }))
}