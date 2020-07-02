# Define server logic required

server <- function(input, output, session) {
  
  #Home
   #map
  output$map <- renderLeaflet({
    leaflet(allData) %>% 
      addTiles() %>%
      setView(lng=15, lat= 57,zoom=3) %>%
      addMarkers(lng = allData$lng, lat = allData$lat,
                 popup = ~paste("", allData$ID_State, "<br>",
                                "Max BID: ", allData$max_BID_Value, "<br>",
                                "Min BID: ", allData$min_BID_Value
                 )
      )
  })
  
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
  # output$items_plot = renderPlot({
  # data3 = filter(items, Klient == input$klient)
  # data3=filter(data3,Auction_ID1 == input$aukcia)
    #data3=filter(data3,Past_Price == input$aukcia )
  #  data3$Past_Price = as.integer(data3$Past_Price)
  #  ggplot(data3, aes(x=data3$Auction_ID1, y=data3$Past_Price)) + xlab("Item ID")+ ylab("Past price") +
  #  geom_segment( aes(x=data3$Auction_ID1, xend=data3$Auction_ID1, y=0, yend=data3$Past_Price)) +
  #  geom_point( size=4, color="red", fill=alpha("orange", 0.3), alpha=0.7, shape=21, stroke=2)
    
    
  # })
  output$items_plot1 = renderPlot({
    data3 = filter(items, Item_ID1 == input$item1)
    #data3=filter(data3,Auction_ID1 == input$aukcia)
    #data3=filter(data3,Past_Price == input$aukcia )
    #data3$Past_Price = as.integer(data3$Past_Price)
    ggplot(data = data3, aes(x = data3$Auction_ID1, y = data3$Quantity, colour = "red")) +
      geom_line(size=1, color="red") +
      xlab('Auction ID') + ylab('Quantity') + theme_bw() +   
      geom_point( size=4, color="red", fill=alpha("orange", 0.3), alpha=0.7, shape=21, stroke=2)
    
  })
  output$items_table <- renderDataTable({
    data3 = filter(items, Item_ID1 == input$item1)
    #data3=filter(data3, Auction_ID1 == input$aukcia)
    datatable(data3)
  })
  
  # Overview ---------------
  output$num_auctions <- renderValueBox({
    my_data = subset(items, Klient == input$m_klient)
    sum_auctions = length(unique(my_data$Auction_ID1))
    infoBox(
      "Auctions", paste0(sum_auctions), icon = icon("store"),
      color = "purple"
    )
  })
  
  output$num_items <- renderValueBox({
    my_data = subset(items, Klient == input$m_klient)
    sum_item = sum(as.numeric(my_data$Quantity), na.rm = TRUE)
    infoBox(
      "Purchased items", paste0(sum_item), icon = icon("shopping-basket"),
      color = "yellow"
    )
  })
  
  output$money_talks <- renderValueBox({
    my_data = subset(items, Klient == input$m_klient)
    sum_money = sum(as.numeric(my_data$Past_Price), na.rm = TRUE)
    infoBox(
      "Money spent", paste0(sum_money), icon = icon("euro-sign"),
      color = "green"
    )
  })
  
  output$topten_quantity <- renderPlot({
    items2 = items[c(3,4)]
    items2 = items2 %>% mutate(Past_Price = coalesce(Past_Price, 0))
    items2 = items2[with(items2, order(-Past_Price)), ]
    
    user_input = as.integer(input$top_x)
    dt3 = items2[1:user_input,]
    dotchart(dt3$Past_Price, labels = dt3$Item_ID1,
             cex = 0.6, ylab = "item_id", xlab = "value",
             main=paste0("Top ", user_input, " items, with highest value"),
             width = 4)
    
  })
  
  output$topten_money <- renderPlot({
    items2 = items[c(1,4)]
    items2 = items2 %>% mutate(Past_Price = coalesce(Past_Price, 0))
    library(data.table)
    dt <- data.table(items2)
    dt2 <- dt[,list(sumamount = sum(Past_Price)), by = c("Klient")]
    dt2 = dt2[with(dt2, order(-sumamount)), ]
    
    user_input = as.integer(input$top_x)
    dt3 = dt2[1:user_input,]
    dotchart(dt3$sumamount, labels = dt3$Klient,
             cex = 0.6, ylab = "client_id", xlab = "money spent",
             main=paste0("Top ", user_input, " clients, who spent most at auctions"),
             width = 4)
    
  })
  
  # Authors
  output$authors <- renderTable({
    read_excel(paste('tasks', ".xlsx", sep=""), 1)
  })
}
