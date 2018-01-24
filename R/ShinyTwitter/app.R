library(shiny)
library(shinydashboard)

source("global.R")
source("Functions.R")

ui <- dashboardPage(
  header = dashboardHeader(title = "Shiny Twitter"),
  sidebar = dashboardSidebar(
    sidebarMenu(
      menuItem(
        text = "Dashboard",
        tabName = "dashboard"
      ),
      menuItem(
        text = "Data",
        tabName = "data"
      ),
      textInput("username",label = "",value = "@realDonaldTrump",placeholder = "Username"),
      sliderInput("n_obs",label = "# of obs",min = 50,max = 150,value = 25,step = 10),
      actionButton("refresh",label = "Refresh", icon = icon("refresh"),width = "85%")
    )
  ),
  body = dashboardBody(
    tabItems(
      tabItem(
        tabName = "dashboard",
        column(
          width = 4,
          valueBoxOutput("obs",width = 12),
          valueBoxOutput("positive",width = 12),
          valueBoxOutput("negative",width = 12),
          leafletOutput("mymap")
        ),
        column(
          width = 8,
          box(
            title = "Word cloud",solidHeader = TRUE,
          plotOutput("cloud")
          ),
          box(
            title = "Sentiments",solidHeader = TRUE,
            plotlyOutput("sentiment")
          )
          
        )
      ),
      tabItem(
        tabName = "data",
        dataTableOutput("clean_data")
      )
    )
  )
)
server <- function(input, output, session) {
  clean_data<-eventReactive(input$refresh,{
    get_data(username = input$username,n_obs =input$n_obs )
  })
  
  output$clean_data<-renderDataTable({
    clean_data()
  })
  
  tokens<-reactive({
    tokenise(clean_data())
  })
  
  output$obs <- renderValueBox({
    valueBox(
      clean_data()%>%nrow()%>%as.character(), "# Obs", icon = icon("list"),
      color = "purple"
    )
  })
  
  reactiveP_N<-reactive({
    P_N(tokens = tokens())
  })
  
  output$positive <- renderValueBox({
    valueBox(
      reactiveP_N()%>%filter(sentiment=="positive")%>%select(prop)%>%unlist()%>%as.vector(), "positive", icon = icon("list"),
      color = "green"
    )
  })
  
  output$negative <- renderValueBox({
    valueBox(
      reactiveP_N()%>%filter(sentiment=="negative")%>%select(prop)%>%unlist()%>%as.vector(), "negative", icon = icon("list"),
      color = "red"
    )
  })
  
  reactive_term_matrix<-reactive({
    term_matrix(tokens())
  })
  
  output$cloud<-renderPlot({
    comparison.cloud(
      reactive_term_matrix(),
      colors = c("red", "green"),
      max.words = 100
    )
  })
  
  reactive_sentiment<-reactive({
    sentiments(tokens())
  })
  
  output$sentiment<-renderPlotly({
    reactive_sentiment()%>%
      plot_ly(x=~sentiment,y=~n,type = "bar")
  })
  
  output$mymap <- renderLeaflet({
    leaflet(data = clean_data()) %>% 
      addTiles() %>%
      addMarkers(~longitude, ~latitude, popup = ~as.character(status))
  })
  
}

shinyApp(ui = ui, server = server)

