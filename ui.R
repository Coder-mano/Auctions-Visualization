source("bidProgress.R")
source("auctions.R")

ui <- dashboardPage(
  dashboardHeader(title = "Assignment"),
  dashboardSidebar( sidebarMenu(
    menuItem("Home", tabName = "home", icon = icon("home")),
    menuItem("Data", tabName = "data", icon = icon("database")),
    menuItem("Overview", tabName = "m_overview", icon = icon("line-chart")),
    #menuItem("Auctions", tabName = "auction", icon = icon("line-chart")),
    menuItem("Bid progress", tabName = "bids", icon = icon("money")),
    menuItem("Items", tabName = "items", icon = icon("bong")),
    menuItem("Participants", tabName = "part", icon = icon("users")),
    menuItem("Attribute Information", tabName = "att_info", icon = icon("database")),
    menuItem("Task List", tabName = "authors", icon = icon("share-alt"))
  )),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = 'home', titlePanel("Dashboard"),
              leafletOutput("map",width = "100%", height = "400px"),
              fluidRow(
               box(width = 3,title = "Overview", solidHeader = T, status = "primary",
                   print("Number of auctions"),
                   titlePanel(length(unique(offersInTime$Auction_ID))),
                   print("Items in auctions"),
                   titlePanel(length(unique(offersInTime$Item_ID)))
               )
              ),
              fluidRow(
                box(width = 3, status = "primary",
                    plotOutput("Auction_type")
                ),
                box(width = 3, status = "primary",
                    plotOutput("Accessibility")
                ),
                box(width = 3, status = "primary",
                    plotOutput("Currency")
                ),
                box(width = 3, status = "primary",
                    plotOutput("Evaluation")
                )
              )
      ),
      tabItem(tabName = 'data',
              tabBox(title = tagList(shiny::icon("database"), "Data"), width = 15,
                     tabPanel('HI_ALL.csv',
                              fluidRow(
                                box(
                                  DT::dataTableOutput('allData'),
                                  width = 12, solidHeader = T,
                                  status = 'warning'))
                              
                     ),tabPanel('offersintime.csv',
                                fluidRow(
                                  box(
                                    DT::dataTableOutput('offersintime'),
                                    width = 12, solidHeader = T,
                                    status = 'warning'))
                                
                     ),tabPanel('items.csv',
                                fluidRow(
                                  box(
                                    DT::dataTableOutput('items'),
                                    width = 12, solidHeader = T,
                                    status = 'warning'))
                     )
              )),
      bidProgress,
      tabItem(tabName = "items",
              fluidRow(
                box(
                  width = 3,
                  selectizeInput('item1', 'ItemID',
                                 choices = items$Item_ID1)
                )
                #box(
                #width = 3,
                # selectizeInput('aukcia', 'ID aukcie',
                #                   choices = unique(items$Auction_ID1))
                # )
                 ),
              
              fluidRow(
                box(
                  width = 12,
                 # plotOutput("items_plot"),
                  plotOutput("items_plot1"),
                  dataTableOutput("items_table")
                )
              )
      ),
      
       tabItem(tabName = "part",
              fluidRow(
                box(
                  width = 10,
                  selectInput("kategoria", "Auction category:",
                              choices = c("All categories",unique(allData$Type_Clarification))),
                  dataTableOutput("participants_table")
                )
              ),
              fluidRow(
                box(
                  width = 7,
                  selectInput("typ", "Type of auction:",
                              choices = c("Purchase","Sale")),
                  selectInput("umiestnenie", "Participant position:",
                              choices = unique(participants$Poradie)),
                  plotlyOutput("participants_plot",height = "400px", width = "400px")
                )
              )
              
              
      ),
      
      tabItem(tabName = "m_overview",
              fluidRow(
                box(
                  width = 2,
                  selectizeInput('m_klient', 'Choose client:',
                                 choices = unique(items$Klient))
                )),
              valueBoxOutput("num_auctions", width = 4),#pocet zucastnenych akcii
              valueBoxOutput("num_items", width = 4),#pocet nakupenych itemov
              valueBoxOutput("money_talks", width = 4),#kolko dokopy minul
              tags$br(), 
              
             # fluidRow(tags$div(HTML("<br><br><br><br><br><br><b>-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------</b>")),
             fluidRow(box(width = 2, selectInput("top_x", "Choose a number for Top X charts:", seq(3, 15, by=1)))),
             fluidRow(box(width = 6,plotOutput("topten_money", width = "90%")),
                      box(width = 6,plotOutput("topten_quantity", width = "90%"))
                    )
      ),
      
      tabItem(tabName = "authors",
              tableOutput('authors')),
      
      
      tabItem(tabName = "att_info",
              selectInput("datatype",
                          label = h2("Choose atribute"), 
                          choices = c("Type" = "1", 
                                      "Accessibility" = "2",
                                      "Most Used Currency" = "3",
                                      "Auction Evaluation" = "4",
                                      "Clarification" = "5"
                          ),
                          
                          selected = "Type"),
              h3(textOutput("atribute_info")),
              plotOutput("graph")
              
      )
      
      
      
      
    )
  )
)
