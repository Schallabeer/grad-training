library(shiny)
library(shinydashboard)

ui<-dashboardPage(
  header = dashboardHeader(
    title = "Demo"
  ),
  sidebar = dashboardSidebar(
    sidebarMenu(
      menuItem(
        text = "Tab 1",
        icon = icon("dashboard"),
        tabName = "tab_1"
        ),
      menuItem(
        text = "Collapsed tab",
        icon = icon ("folder"),
        menuSubItem(
          text = "Tab 2.1",
          icon = icon("dashboard"),
          tabName = "tab_21"
        ),
        menuSubItem(
          text = "Tab 2.2",
          icon = icon("dashboard"),
          tabName = "tab_22"
        )
      )
    )
  ),
  body = dashboardBody(
    tabItems(
      tabItem(
        tabName = "tab_1",
        h1("This is tab 1")
      ),
      tabItem(
        tabName = "tab_21",
        h1("This is tab 2.1")
      ),
      tabItem(
        tabName = "tab_22",
        h1("This is tab 2.2")
      )
    )
  )
)

server<-function(input,output,session){
  
}

shinyApp(ui,server)