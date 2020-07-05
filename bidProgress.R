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
                                                   choices = c("") #observed na.omit(unique(allData$Type_Clarification))
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
                               radioButtons("group", "Docasny button:",
                                            c("Agregated" = "agregated",
                                              "Participants" = "participants"))
                             ),
                             column(
                               width = 12,
                               plotOutput("bidAuctions")),
                             DT::dataTableOutput('bidDesc')
                         ))
)



# Auctions server.R

# Observe Type selection
bidTypeObserver <- function(input,session) {
  updateSelectInput(
    session, 'bidClarification',    # Update lower hierarchy dropdown
    choices = na.omit(unique(filter(allData, Type == input$bidType)$Type_Clarification))
  )
  bidClarificationObserver(input,session)  
}

# Observe Type Clarification
bidClarificationObserver <- function(input,session) {
  updateSelectInput(
    session, 'bidEvaluation',
    choices = na.omit(unique(filter(allData, Type == input$bidType & Type_Clarification == input$bidClarification)$Evaluated_By))
  )
  bidEvaluationObserver(input,session)
}

# Observe Evaluation selection
bidEvaluationObserver <- function(input,session) {
  updateSelectInput(
    session, 'bidAuction',
    choices = na.omit(unique(filter(allData, Evaluated_By == input$bidEvaluation & Type == input$bidType & Type_Clarification == input$bidClarification)$Auction_ID))
  )
}

bidRenderAuctionPlot <- function(input,output,session){
  updateSelectInput(
    session, 'bidItem',
    choices = filter(offersInTime, Auction_ID == input$bidAuction)$Item_ID
  )
  return(renderPlot({
    
    # basic tmp filtering
    subData = filter(offersInTime, Item_ID == input$bidItem)
    subData = filter(subData, Auction_ID == input$bidAuction)
    #print(sapply(subData,class))
    
    desc = filter(allData, Auction_ID == input$bidAuction)
    desc = filter(desc, Client == subData$Client[1])
    
    subData$New_BID = as.integer(subData$New_BID)
    
    #tmp visuals
    output$bidDesc = DT::renderDataTable({
      datatable(desc, rownames = F,options = list(scrollX = TRUE))
    })
    
    if (nrow(subData) != 0) { 
    if (input$group == "agregated") {
    ggplot(data = subData, aes(x = subData$Change_order, y = subData$New_BID, colour = "red")) +
      geom_line() + guides(color = FALSE, size = FALSE) +
      xlab('Change_order') + theme_bw() + geom_point()
    }else{
      ggplot(data = subData, aes(x = subData$Change_order, y = subData$New_BID, colour = subData$Participant_ID,group = subData$Participant_ID)) +
        geom_line() +
        xlab('Change_order') + theme_bw() +   geom_point()
    }
    }
  }))
  
  
}