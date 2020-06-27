# Define server logic required

server <- function(input, output, session) {
  
  #Home
  output$Auction_type = renderPlot({
    types = as.factor(allData$Type)
    types = droplevels(types, exclude = c("","0"))
    types = na.omit(types)
    pie(table(types), col = rainbow(2), main = "Auction types", labels = c("Buy", "Sell"))
  })
  output$Accessibility = renderPlot({
    pie(table(allData$`_auctionData_verejna_string`), col = rainbow(2), main = "Auction Accessibility", labels = c("Private","Public"))
  })
  
  output$Currency = renderPlot({
    currency = as.factor(allData$`_auctionData_mena_string`)
    currency = droplevels(currency, exclude = c("ks", "THB", "0", "GBP", "%", "AUD", "I", "RUB", "<NA>"))
    pie(table(currency), col = rainbow(7), main = "Most Used Currency")
  })
  output$Evaluation = renderPlot({
    evaluation_type = as.factor(allData$Evaluated_By)
    evaluation_type = droplevels(evaluation_type, exclude = c("", "2722195"))
    evaluation_type = na.omit(evaluation_type)
    pie(table(evaluation_type), col = rainbow(7), main = "Auction Evaluation by",labels = "")
    #legend("bottomleft", legend = c("PomocnĂ� vĂ˝poÄŤet - max", "PomocnĂ� vĂ˝poÄŤet - min", "CelkovĂˇ nabĂ�tka ĂşÄŤastnĂ�ka", "HodnocenĂ�", "JednotlivĂ© poloĹľky", "MultikriteriĂˇlnĂ� hodnocenĂ�", "Skupiny poloĹľek" ),bty="n", fill = rainbow(7))
    
  })
  
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
  observeEvent(input$bidType,{
    bidTypeObserver(input,session)
  })
  observeEvent(input$bidClarification,{ 
    bidClarificationObserver(input,session)
  })
  observeEvent(input$bidEvaluation,{ 
    bidEvaluationObserver(input,session)
  })
  
  observeEvent(input$bidAuction,{ 
    output$bidAuctions <- bidRenderAuctionPlot(input,output,session)
  })
  
  
  # Items ---------------
  output$items_plot = renderPlot({
    data3 = filter(items, Klient == input$klient)
    data3=filter(data3,Auction_ID1 == input$aukcia)
    #data3=filter(data3,Past_Price == input$aukcia )
    data3$Past_Price = as.integer(data3$Past_Price)
    ggplot(data3, aes(x=data3$Item_ID1, y=data3$Past_Price)) + xlab("Item ID")+ ylab("Past price") +
      geom_segment( aes(x=data3$Item_ID1, xend=data3$Item_ID1, y=0, yend=data3$Past_Price)) +
      geom_point( size=4, color="red", fill=alpha("orange", 0.3), alpha=0.7, shape=21, stroke=2)
    
    
  })
  output$items_plot1 = renderPlot({
    data3 = filter(items, Klient == input$klient)
    data3=filter(data3,Auction_ID1 == input$aukcia)
    #data3=filter(data3,Past_Price == input$aukcia )
    data3$Past_Price = as.integer(data3$Past_Price)
    ggplot(data3, aes(x=data3$Item_ID1, y=data3$Quantity)) + xlab("Item ID")+ ylab("Quantity") +  
      #geom_segment( aes(x=data3$Item_ID1, xend=data3$Item_ID1, y=0, yend=data3$Past_Price)) +
      geom_point( size=4, color="red", fill=alpha("orange", 0.3), alpha=0.7, shape=21, stroke=2)
    
  })
  output$items_table <- renderDataTable({
    data3 = filter(items, Klient == input$klient)
    data3=filter(data3, Auction_ID1 == input$aukcia)
    datatable(data3)
  })
  
  # Authors
  output$authors <- renderTable({
    read_excel(paste('tasks', ".xlsx", sep=""), 1)
  })
}







