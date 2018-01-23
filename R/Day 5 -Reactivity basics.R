library(shiny)
library(shinydashboard)

ui<-dashboardPage(
  header = dashboardHeader(
    title = "Reactivity"
  ),
  sidebar = dashboardSidebar(
    sidebarMenu(
      menuItem(
        text = "Observe*",
        icon = icon("dashboard"),
        tabName = "observe"
      ),
      menuItem(
        text = "reactive",
        icon = icon("dashboard"),
        tabName = "reactive"
      ),
      menuItem(
        text = "eventReactive",
        icon = icon("dashboard"),
        tabName = "eventReactive"
      ),
      menuItem(
        text = "isolate",
        icon = icon("dashboard"),
        tabName = "isolate"
      )
    )
  ),
  body = dashboardBody(
    tabItems(
      tabItem(
        tabName = "observe",
        textInput("observe_name",label = "Name"),
        textOutput("observe_output")
      ),
      tabItem(
        tabName = "reactive",
        textInput("reactive_name",label = "Name"),
        textOutput("reactive_output")
      ),
      tabItem(
        tabName = "invalidateLater",
        textInput("invalidateLater_name",label = "Name"),
        textOutput("invalidateLater_output")
      ),
      tabItem(
        tabName = "eventReactive",
        textInput("eventReactive_name",label = "Name"),
        actionButton(inputId = "update",label = "update"),
        textOutput("eventReactive_output")
      ),
      tabItem(
        tabName = "isolate",
        textInput("isolate_name",label = "Name"),
        textOutput("isolate_output")
      )
    )
  )
)

server<-function(input,output,session){
  ############### Observe ###############
  output$observe_output<-renderText({
    paste("Hello",input$observe_name)
  })
  
  ############### Reactive ###############
  myReactiveFunction<-reactive({
    paste("Hello",input$reactive_name)
  })
  output$reactive_output<-renderText({
    myReactiveFunction()
  })
  
  ############### invalidateLater ###############
  myReactiveFunction_2<-reactive({
    invalidateLater(5000)
    paste("Hello",input$invalidateLater_name)
  })
  output$invalidateLater_output<-renderText({
    myReactiveFunction_2()
  })

  ############### eventReactive ###############
  myEventReactiveFunction<-eventReactive(input$update,{
    paste("Hello",input$eventReactive_name)
  })
  output$eventReactive_output<-renderText({
    myEventReactiveFunction()
  })

  ############### isolate ###############
  myReactiveFunction_3<-reactive({
    paste("Hello",isolate(input$isolate_name))
  })
  output$isolate_output<-renderText({
    myReactiveFunction_3()
  })
  
}

shinyApp(ui,server)