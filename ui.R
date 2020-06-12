source("auctions.R")

ui <- dashboardPage(
  dashboardHeader(title = "Assignment"),
  dashboardSidebar( sidebarMenu(
    menuItem("Home", tabName = "home", icon = icon("home")),
    menuItem("Data", tabName = "data", icon = icon("database")),
    menuItem("Auctions", tabName = "auction", icon = icon("line-chart")),
    menuItem("Bid progress", tabName = "bids", icon = icon("money")),
    menuItem("Items", tabName = "items", icon = icon("bong")),
    menuItem("Participants", tabName = "part", icon = icon("users")),
    menuItem("Task List", tabName = "authors", icon = icon("share-alt"))
  )),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = 'home', titlePanel("Dashboard"),
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
                     ),tabPanel("futuretab")
              )),
      
      auctions,
      tabItem(tabName = "bids",
              fluidRow(
                box(
                  width = 3,
                  selectizeInput('A_ID', 'Auction ID',
                                 choices = unique(offersInTime$Auction_ID))
                ),
                box(
                  width = 3,
                  selectizeInput('I_ID', 'Item ID',
                                 choices = unique(offersInTime$Item_ID))
                )
              ),
                
              fluidRow(
                box(
                  width = 12,
                  plotOutput("bids_plot"),
                  dataTableOutput("bids_table")
                )
              )
              ),
      tabItem(tabName = "items",
              fluidRow(
                box(
                  width = 3,
                  selectizeInput('klient', 'Klient',
                                 choices = unique(items$Klient))
                )
              ),
              
              fluidRow(
                box(
                  width = 12,
                  plotOutput("items_plot"),
                  dataTableOutput("items_table")
                )
              )
      ),
      
      tabItem(tabName = "part"),
      
      tabItem(tabName = "authors",
              tableOutput('authors'))
    )
  )
)
