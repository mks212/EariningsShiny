library(shinydashboard)

shinyUI(dashboardPage(
  dashboardHeader(title = "Earnings Analysis"),
  dashboardSidebar(
    sidebarUserPanel("Earnings"),
    sidebarMenu(
      menuItem("Stock", tabName = "stock", icon = icon("stock"))),
    selectizeInput("selected",
                   "Select Stock to Display",
                   choice),
    sliderInput("length", "Days Around Announcement", 1, 30, 5)
            ), #dashboardSidebar
  dashboardBody(
              fluidRow(
                infoBoxOutput("avgBox"),
                infoBoxOutput("posBox"),
                infoBoxOutput("negBox")
              ),
              fluidRow(
                box(plotOutput("barPlot")),
                box(plotOutput("season_ratePlot"))
              )
        )#dashboardBody
  )#page
)#shinyUI
