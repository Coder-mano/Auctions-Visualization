# Define server logic required

server <- function(input, output, session) {
  
  # Data
  output$allData = DT::renderDataTable({
    datatable(allData, rownames = F,options = list(scrollX = TRUE))
  })
  output$offersintime = DT::renderDataTable({
    datatable(offersInTime, rownames = F,options = list(scrollX = TRUE))
  })
  output$items = DT::renderDataTable({
    datatable(items, rownames = F,options = list(scrollX = TRUE))
  })
  
  # Auctions
  observeEvent(input$type,{
    typeObserver(input,session)
  })
  observeEvent(input$clarification,{ 
    clarificationObserver(input,session)
  })
  observeEvent(input$evaluation,{ 
    evaluationObserver(input,session)
  })
  
  observeEvent(input$auction,{ 
    output$auctions <- renderAuctionPlot(input,output,session)
  })
  
  #Bids progress 
  output$bids_plot <- renderPlot({
    subData2 = filter(offersInTime, Client == 100)
    subData2 = filter(subData2, Auction_ID == input$A_ID)
    subData2 = filter(subData2, Item_ID == input$I_ID)
    subData2$New_BID = as.integer(subData2$New_BID)
    ggplot(data = subData2, aes(x = subData2$Change_order, y = subData2$New_BID, colour = "red")) + geom_line() + theme_bw() + geom_point() + xlab("Number of bid") + ylab("Bid ammount") + theme(legend.position = "none")
  })
  
  output$bids_table <- renderDataTable({
    subData2 = filter(offersInTime, Client == 100)
    subData2 = filter(subData2, Auction_ID == input$A_ID)
    subData2 = filter(subData2, Item_ID == input$I_ID)
    subData2$New_BID = as.integer(subData2$New_BID)
    datatable(subData2, rownames = F,options = list(scrollX = TRUE))
  })
  # Items
  output$items_plot = renderPlot({
    data3 = filter(items, Klient == input$klient)
  })
  output$items_table <- renderDataTable({
    data3 = filter(items, Klient == input$klient)
    datatable(data3)
  })
  
  # Authors
  output$authors <- renderTable({
    read_excel(paste('tasks', ".xlsx", sep=""), 1)
  })
}




