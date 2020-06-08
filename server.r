# Define server logic required
server <- function(input, output, session) {
  
  # Data
  output$allData = DT::renderDataTable({
    datatable(allData, rownames = F,options = list(scrollX = TRUE))
  })
  output$offersintime = DT::renderDataTable({
    datatable(offersInTime, rownames = F,options = list(scrollX = TRUE))
  })
  
  # Authors
  output$authors <- renderTable({
    read_excel(paste('tasks', ".xlsx", sep=""), 1)
  })
  
  # Observe the Type selection
  observeEvent(input$type,{ 
    updateSelectInput(
      session, 'clarification',
      choices = na.omit(unique(filter(allData, Type == input$type)$Type_Clarification))
    )
  })
  # Observe the Type Clarification
  observeEvent(input$clarification,{ 
    updateSelectInput(
      session, 'evaluation',
      choices = na.omit(unique(filter(allData, Type == input$type & Type_Clarification == input$clarification)$Evaluated_By))
    )
  })
  # Observe the Evaluation selection
  observeEvent(input$evaluation,{ 
    updateSelectInput(
      session, 'auction',
      choices = na.omit(unique(filter(allData, Evaluated_By == input$evaluation & Type == input$type & Type_Clarification == input$clarification)$Auction_ID))
    )
  })
  
  # Observe the Auction selection
  observeEvent(input$auction,{ 
    selectedAuction = input$auction
    updateSelectInput(
      session, 'item',
      choices = filter(offersInTime, Auction_ID == selectedAuction)$Item_ID
    )
    output$auctions <- renderPlot({
      
      # basic tmp filtering
      subData = filter(offersInTime, Item_ID == input$item)
      subData = filter(subData, Auction_ID == selectedAuction)
      #print(sapply(subData,class))
      #tmp visuals
      desc = filter(allData, Auction_ID == selectedAuction)
      desc = filter(desc, Client == subData$Client[1])
      
      output$desc = DT::renderDataTable({
        datatable(desc, rownames = F,options = list(scrollX = TRUE))
      })
      ggplot(data = subData, aes(x = subData$Change_order, y = subData$New_BID, colour = subData$Participant_ID,group = subData$Participant_ID)) +
        geom_line() +
        xlab('Change_order') + theme_bw() +   geom_point()
      
    })
  })
}
   



