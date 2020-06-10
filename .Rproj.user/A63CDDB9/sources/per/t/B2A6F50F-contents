#**************
# Auctions UI.R

auctions <- tabItem(tabName = "auction",
                    fluidRow(
                      box(
                        width = 3,
                        selectizeInput('type', 'Type', 
                                       choices = na.omit(unique(allData$Type))
                        )),
                      box(
                        width = 3,
                        selectizeInput('clarification', 'Type Clarification', 
                                       choices = c("") #observed na.omit(unique(allData$Type_Clarification))
                        )),
                      box(
                        width = 3,
                        selectizeInput('evaluation', 'Evaluation', 
                                       choices = c("") #observed #na.omit(unique(allData$EvaluatedBy))
                        )),
                      box(
                        width = 3,
                        selectizeInput('auction', 'Auction ID', 
                                       choices = c("") #observed #na.omit(unique(offersInTime$Auction_ID))
                        )),
                      box(
                        width = 3,
                        selectizeInput('item', 'Item ID', 
                                       choices = c("") #observed
                        ))
                    ),
                    
                    fluidRow(
                      box(
                        width = 12,
                        plotOutput("auctions"),
                        DT::dataTableOutput('desc')
                      )
                    )
)


# Auctions server.R

# Observe Type selection
typeObserver <- function(input,session) {
  updateSelectInput(
    session, 'clarification',    # Update lower hierarchy dropdown
    choices = na.omit(unique(filter(allData, Type == input$type)$Type_Clarification))
  )
  clarificationObserver(input,session)  
}

# Observe Type Clarification
clarificationObserver <- function(input,session) {
  updateSelectInput(
    session, 'evaluation',
    choices = na.omit(unique(filter(allData, Type == input$type & Type_Clarification == input$clarification)$Evaluated_By))
  )
  evaluationObserver(input,session)
}

# Observe Evaluation selection
evaluationObserver <- function(input,session) {
  updateSelectInput(
    session, 'auction',
    choices = na.omit(unique(filter(allData, Evaluated_By == input$evaluation & Type == input$type & Type_Clarification == input$clarification)$Auction_ID))
  )
}

renderAuctionPlot <- function(input,output,session){
  updateSelectInput(
    session, 'item',
    choices = filter(offersInTime, Auction_ID == input$auction)$Item_ID
  )
  return(renderPlot({
    
    # basic tmp filtering
    subData = filter(offersInTime, Item_ID == input$item)
    subData = filter(subData, Auction_ID == input$auction)
    #print(sapply(subData,class))
    
    desc = filter(allData, Auction_ID == input$auction)
    desc = filter(desc, Client == subData$Client[1])
    
    #tmp visuals
    output$desc = DT::renderDataTable({
      datatable(desc, rownames = F,options = list(scrollX = TRUE))
    })
    
    ggplot(data = subData, aes(x = subData$Change_order, y = subData$New_BID, colour = subData$Participant_ID,group = subData$Participant_ID)) +
      geom_line() +
      xlab('Change_order') + theme_bw() +   geom_point()
  }))
}