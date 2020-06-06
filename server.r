# Define server logic required
server <- function(input, output, session) {
  
  output$allData = DT::renderDataTable({
    datatable(allData, rownames = F,options = list(scrollX = TRUE))
  })
  output$offersintime = DT::renderDataTable({
    datatable(offersInTime, rownames = F,options = list(scrollX = TRUE))
  })
  
  # observe the Auction selection
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
      #tmp sorting (change_order)
      ggplot(data = subData, aes(x = subData$Change_order, y = subData$New_BID, colour = subData$Participant_ID,group = subData$Participant_ID)) +
        geom_line() +
        xlab('Change_order') + theme_bw() +   geom_point()
    })
  })
}


