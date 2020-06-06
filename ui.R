
library(shiny)

ui <- dashboardPage(
  dashboardHeader(title = "Assignment"),
  dashboardSidebar( sidebarMenu(
    menuItem("Home", tabName = "home", icon = icon("home")),
    menuItem("Data", tabName = "data", icon = icon("database")),
    menuItem("Auction", tabName = "auction", icon = icon("line-chart")),
    menuItem("Participants", tabName = "part", icon = icon("users"))
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
                     
              )
      ),tabItem(tabName = "auction",
                fluidRow(
                  box(
                    selectizeInput('auction', 'Auction ID', 
                                   choices = na.omit(unique(offersInTime$Auction_ID))
                    )),
                  box(
                    selectizeInput('item', 'Item ID', 
                                   choices = c("") #observed
                    ))
                ),
                
                fluidRow(
                  box(
                    width = 12,
                    plotOutput("auctions")),
                )
                
      ),tabItem(tabName = "part"
               
      )
    )
  )
)

