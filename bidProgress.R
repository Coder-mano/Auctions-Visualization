#**************
# Bid progress UI.R

bidProgress <- tabItem(tabName = "bids",
                    fluidRow(
                      box(
                        width = 3,
                        selectizeInput('bidType', 'Type', 
                                       choices = na.omit(unique(allData$Type))
                        )),
                      box(
                        width = 3,
                        pickerInput('bidClarification',"Clarification Type",
                                  choices=c(""), options = list(`actions-box` = TRUE),multiple = T)), #observed
                      box(
                        width = 3,
                        pickerInput('bidEvaluation',"Evaluation",
                                    choices=c(""), options = list(`actions-box` = TRUE),multiple = T)), #observed
                      box(
                        width = 3,
                        pickerInput('bidAuction',"Auction ID",
                                    choices=c(""), options = list(`actions-box` = TRUE),multiple = T)), #observed
                      box(
                        width = 3,
                        pickerInput('bidItem',"Item ID",
                                    choices=c(""), options = list(`actions-box` = TRUE),multiple = T)), #observed
                      box(
                        radioButtons("group", "Docasny button:",
                                     c("Agregated" = "agregated",
                                       "Participants" = "participants"))
                      )
                      
                    ),
                    
                    fluidRow(
                      box(
                        width = 12,
                        plotOutput("bidAuctions"),
                        DT::dataTableOutput('bidDesc')
                      )
                    )
)


# Auctions server.R

# Observe Type selection
bidTypeObserver <- function(input,session) {
  updatePickerInput(
    session, 'bidClarification',    # Update lower hierarchy dropdown
    choices = na.omit(unique(filter(allData, Type == input$bidType)$Type_Clarification))
  )
  bidClarificationObserver(input,session)  
}

# Observe Type Clarification
bidClarificationObserver <- function(input,session) {
  if(length(input$bidClarification) > 0){
    updatePickerInput(
      session, 'bidEvaluation',
      choices = na.omit(unique(filter(allData, Type == input$bidType & is.element(Type_Clarification, input$bidClarification))$Evaluated_By))
    )
  }
  # nothing selected
  else{
    print(input$bidClarification)
    updatePickerInput(
      session, 'bidEvaluation',
      choices = na.omit(unique(filter(allData, Type == input$bidType)$Evaluated_By))
    )
  }
  
  bidEvaluationObserver(input,session)
}

# Observe Evaluation selection
bidEvaluationObserver <- function(input,session) {
  if(length(input$bidEvaluation) > 0 & length(input$bidClarification) > 0){
    updatePickerInput(
      session, 'bidAuction',
      choices = na.omit(unique(filter(allData, is.element(Evaluated_By, input$bidEvaluation) & Type == input$bidType & is.element(Type_Clarification, input$bidClarification))$Auction_ID))
    )
  }
  # nothing selected
  else{
    updatePickerInput(
      session, 'bidAuction',
      choices = na.omit(unique(filter(allData, Type == input$bidType)$Auction_ID))
    )
  }
}

bidRenderAuctionPlot <- function(input,output,session){
  updatePickerInput(
    session, 'bidItem',
    choices = unique(filter(offersInTime, is.element(Auction_ID, input$bidAuction))$Item_ID)
  )
  return(renderPlot({
    
    # basic tmp filtering
    subData = filter(offersInTime, is.element(Item_ID, input$bidItem))
    subData = filter(subData, is.element(Auction_ID, input$bidAuction))
    #print(sapply(subData,class))
    
    desc = filter(allData, is.element(Auction_ID, input$bidAuction))
    desc = filter(desc, Client == subData$Client[1])
    
    subData$New_BID = as.integer(subData$New_BID)
    
    #tmp visuals
    output$bidDesc = DT::renderDataTable({
      datatable(desc, rownames = F,options = list(scrollX = TRUE))
    })
    
    if (nrow(subData) != 0) { 
    if (input$group == "agregated") {
    ggplot(data = subData, aes(x = subData$Change_order, y = subData$New_BID, colour = "red")) +
      geom_line() +
      xlab('Change_order') + theme_bw() +   geom_point()
    }else{
      ggplot(data = subData, aes(x = subData$Change_order, y = subData$New_BID, colour = subData$Participant_ID,group = subData$Participant_ID)) +
        geom_line() +
        xlab('Change_order') + theme_bw() +   geom_point()
    }
    }
  }))
}