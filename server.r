# Define server logic required

server <- function(input, output, session) {
  
  #Home
  output$map <- renderLeaflet({
    leaflet(allData) %>% 
      addTiles() %>%
      setView(lng=15, lat= 57,zoom=3) %>%
      addMarkers(lng = as.numeric(map_data$lng), lat = as.numeric(map_data$lat),
                 popup = ~paste("", map_data$ID_State, "<br>",
                                "Max BID: ", map_data$max_BID_Value, "<br>",
                                "Min BID: ", map_data$min_BID_Value
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
    pie(table(evaluation_type), col = rainbow(7), main = "Auction Evaluation by")
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
  
  
  # Items
  output$items_plot = renderPlot({
    colors = c("Past_Price" = "blue", "Quantity" = "red")
    data3 = filter(items, Item_ID1 == input$itemIn)
    ggplot(data3, aes(x=data3$Auction_ID1))+
      geom_line(aes(y=data3$Past_Price, color="Past_Price"), size = 1.2)+
      geom_line(aes(y=data3$Quantity, color="Quantity"), size = 1.2)+
      labs(x="Auction ID",y="Value", color="Legend")+
      scale_color_manual(values=colors)
  })
  
  output$items_table <- renderDataTable({
    data3 = filter(items, Item_ID1 == input$itemIn)
    datatable(data3)
  })
  
  
  #Participants
  output$participants_table <- renderDataTable({
    
    dt1 = as.data.table(participants, key = c("Klient","ID aukcie"))
    dt2 = as.data.table(allData, key = c("Client","Auction_ID"))
    colnames(dt1) = c("Client",
                      "Auction_ID",
                      "ID_ucastnika",
                      "Poradie",
                      "x",
                      "y",
                      "z",
                      "mozu_chybat")
    dt1$Client = as.numeric(dt1$Client)
    dt2$Client = as.numeric(dt2$Client)
    
    merged_table = merge(dt1,dt2, by = c("Client","Auction_ID"))
    
    
    kategory = input$kategoria
    
    if(kategory == "All categories"){
      vyhrane_aukcie_0 = merged_table[merged_table$Poradie == 1,][,3]
      
      tab1 = table(vyhrane_aukcie_0)
      tab1 = as.data.frame(tab1)
      colnames(tab1) = c("ID_ucastnika","pocet_vyher")
      tab1 = tab1[order(-tab1$pocet_vyher),]
    }
    
    else {
      vyhrane_aukcie_0 = merged_table[merged_table$Poradie == 1 & merged_table$Type_Clarification == kategory,][,3]
      
      tab1 = table(vyhrane_aukcie_0)
      tab1 = as.data.frame(tab1)
      colnames(tab1) = c("ID_ucastnika","pocet_vyher")
      tab1 = tab1[order(-tab1$pocet_vyher),]
    }
    
    datatable(tab1)
  })
  output$participants_plot = renderPlotly({
    
    dt1 = as.data.table(participants, key = c("Klient","ID aukcie"))
    dt2 = as.data.table(allData, key = c("Client","Auction_ID"))
    colnames(dt1) = c("Client",
                      "Auction_ID",
                      "ID_ucastnika",
                      "Poradie",
                      "x",
                      "y",
                      "z",
                      "mozu_chybat")
    dt1$Client = as.numeric(dt1$Client)
    dt2$Client = as.numeric(dt2$Client)
    
    merged_table = merge(dt1,dt2, by = c("Client","Auction_ID"))
    
    
    nakup_table = merged_table[Type =="Purchase"]
    prodej_table = merged_table[Type =="Sale"]
    
    
    if (input$typ == "Purchase"){
      poradie = as.numeric(input$umiestnenie)
      
      kategoria_nakup =  nakup_table[nakup_table$Poradie == poradie]
      kategoria_nakup =  kategoria_nakup[,kategoria_nakup$Type_Clarification]
      kategoria_nakup = as.data.table(table(kategoria_nakup))
      
      fig <- plot_ly(kategoria_nakup, x = ~kategoria_nakup, y = ~N, type = 'bar', name = 'Participants')%>%
        layout(
          xaxis = list(
            title = "Auction Categories"),
          yaxis = list(
            title = "Number of participants"),
          autosize=FALSE)
    }
    else {
      poradie = as.numeric(input$umiestnenie)
      
      kategoria_prodej =  prodej_table[prodej_table$Poradie == poradie]
      kategoria_prodej =  kategoria_prodej[,kategoria_prodej$Type_Clarification]
      kategoria_prodej = as.data.table(table(kategoria_prodej))
      fig <- plot_ly(kategoria_prodej, x = ~kategoria_prodej, y = ~N, type = 'bar', name = 'Participants')%>%
        layout(
          xaxis = list(
            title = "Auction Categories"),
          yaxis = list(
            title = "Number of participants"),
          autosize=FALSE)
    }
    fig
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
  
  
  # Data Info
  
  #GRAFY
  output$graph = renderPlot({
    if (input$datatype == "1") {
      types = as.factor(allData$Type)
      types = droplevels(types, exclude = c("","0"))
      types = na.omit(types)
      pie(table(types), col = rainbow(2), main = "Type", labels = c("Buy", "Sell"))
      
    }
    else if (input$datatype == "2") {
      publicAuction = as.factor(allData$`_auctionData_verejna_string`)
      publicAuction = na.omit(publicAuction)
      levels(publicAuction) = c("Private", "Public")
      barplot(table(publicAuction), col = rainbow(2), main = "Auction Accessibility", ylim = c(0,15000))
    } 
    else if (input$datatype == "3") {
      currency = as.factor(allData$`_auctionData_mena_string`)
      currency = droplevels(currency, exclude = c("ks", "THB", "0", "GBP", "%", "AUD", "I", "RUB", "<NA>"))
      barplot(table(currency), col = rainbow(7), main = "Most Used Currency", ylim = c(0,15000))    
    }
    else if (input$datatype == "4") {
      evaluation_type = as.factor(allData$Evaluated_By)
      evaluation_type = droplevels(evaluation_type, exclude = c("", "2722195"))
      evaluation_type = na.omit(evaluation_type)
      barplot(table(evaluation_type), col = rainbow(7), main = "Auction Evaluation by", ylim = c(0,15000))
    }
    else if (input$datatype == "5") {
      clarification = as.factor(allData$Type_Clarification)
      clarification = droplevels(clarification, exclude = c("", "0", "25", "278", "3"))
      clarification = na.omit(clarification)
      barplot(table(clarification), col = rainbow(10), main = "Clarification Type", ylim = c(0,15000))
    } 
    
    
  })
  
  #TExT
  output$atribute_info = renderText({
    if (input$datatype == "1") {
      paste("This attribute tells you whether the auction was used to buy or sell.")
    }
    else if (input$datatype == "2") {
      paste("Accessibility attribute says who is able to reach the auction. There are only two options - private and public. Public Auctions are available for everyone. The value of the current high bid is displayed on the listing page. Buyers who want to win the auction make bids that are higher than the current high bid.
            In a private sale, the offers made to the seller are kept private - even after the sale is completed. No one but the buyer and seller will know what a property sold for.")
    }
    else if (input$datatype == "3") {
      paste("This chart shows which currency is most used in transactions. Different currencies can be used in the system from which the data originates. The most used are euros and Czech crowns.")
    }
    else if (input$datatype == "4") {
      paste("Atribute Auction Evaluation says more about auction and how it is evaluated. This evaluation of auction depends on auction type - Absolute Auction, Minimum-Bid Auction, Multi-Parcel Auction, Reserve Auction,...")
    }
    else if (input$datatype == "5") {
      paste("This attribute clarifies the type of auction in the data. Only some data have this closer specification. The graph shows the number of individual values, but also the number of undefined ones.")
    }
    
    
    })
  
  }
