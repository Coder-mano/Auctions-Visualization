
library(shiny)

ui <- dashboardPage(
  dashboardHeader(title = "Assignment"),
  dashboardSidebar( sidebarMenu(
    menuItem("Home", tabName = "home", icon = icon("home")),
    menuItem("Data", tabName = "data", icon = icon("database")),
    menuItem("Auctions", tabName = "auction", icon = icon("line-chart")),
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
                     
              )
      ),tabItem(tabName = "auction",
                fluidRow(
                  box(
                    width = 3,
                    selectizeInput('type', 'Type', 
                                   choices = na.omit(unique(allData$Type))
                    )),
                  box(
                    width = 3,
                    selectizeInput('clarification', 'Type Clarification', 
                                   choices = c("") #observed na.omit(unique(allData$Type_Clarification))
                    )),
                  box(
                    width = 3,
                    selectizeInput('evaluation', 'Evaluation', 
                                   choices = c("") #observed #na.omit(unique(allData$EvaluatedBy))
                    )),
                  box(
                    width = 3,
                    selectizeInput('auction', 'Auction ID', 
                                   choices = c("") #observed #na.omit(unique(offersInTime$Auction_ID))
                    )),
                  box(
                    width = 3,
                    selectizeInput('item', 'Item ID', 
                                   choices = c("") #observed
                    ))
                ),
                
                fluidRow(
                  box(
                    width = 12,
                    plotOutput("auctions"),
                    DT::dataTableOutput('desc')
                  )
                )
                
      ),tabItem(tabName = "part"
                
      ),tabItem(tabName = "authors",
                tableOutput('authors'))
    )
  )
)

