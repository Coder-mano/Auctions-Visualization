source("auctions.R")

ui <- dashboardPage(
  dashboardHeader(title = "Assignment"),
  dashboardSidebar( sidebarMenu(
    menuItem("Home", tabName = "home", icon = icon("home")),
    menuItem("Data", tabName = "data", icon = icon("database")),
    menuItem("Auctions", tabName = "auction", icon = icon("line-chart")),
    menuItem("Bid progress", tabName = "bids", icon = icon("money")),
    menuItem("Participants", tabName = "part", icon = icon("users")),
    menuItem("Task List", tabName = "authors", icon = icon("share-alt"))
  )),
  
  dashboardBody(
    tabItems(
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
                                
                     ),tabPanel('FuturePanel')
              )),
      
      auctions,
      tabItem(tabName = "bids",
              fluidRow(
                box(
                  width = 3,
                  selectizeInput('A_ID', 'Auction ID',
                                 choices = unique(offersInTime$Auction_ID))
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
      
      tabItem(tabName = "part"),
      
      tabItem(tabName = "authors",
              tableOutput('authors'))
    )
  )
)


