## app.R ##
library(shinydashboard)


header <- dashboardHeader(title = "Terrae")


sidebar <- dashboardSidebar(sidebarMenu(
  menuItem("Home", tabName = "Inicio", icon = icon("th")),
  menuItem("Project information", tabName = "Información", icon = icon("dashboard")),
  menuItem("Social network analysis", tabName = "social network analysis ", icon = icon("bar-chart-o")),
  menuItem("Data set", tabName = "Conjunto de datos", icon = icon("table")),
  menuItem("Concepts", tabName = "Conceptos", icon = icon("question"))
  
))

body <- dashboardBody(
  tabItems(
    # CONTENIDO DEL INICIO
    tabItem(tabName = "Home",
            h2("Sismos según su magnitud"),
            fluidRow(
              
              infoBox("Menor", 10 * 2, icon = icon("credit-card"), fill = TRUE),
              infoBox("Ligero", 10 * 2, icon = icon("credit-card"), fill = TRUE),
              infoBox("Moderado", 10 * 2, icon = icon("credit-card"), fill = TRUE),
              infoBox("Fuerte", 10 * 2, icon = icon("credit-card"), fill = TRUE),
              infoBox("Moderado", 10 * 2, icon = icon("credit-card"), fill = TRUE),
              infoBox("Grande", 10 * 2, icon = icon("credit-card"), fill = TRUE)
              
            ),
            ## 1.2 Time serise plot ----------------------------------------
            h2("New Zealand trade over the past 20 years"),
            fluidRow(
              box(plotOutput("plot2", height = 250)),
              
              box(
                title = "Controls",
                sliderInput("slider", "Number of observations:", 1, 100, 50)
              )
            )
    ),
    
    # CONTENIDO DEL SNA 
    tabItem(tabName = "widgets",
            h2("Widgets tab content")
    )
  )
)

ui <- dashboardPage(header,sidebar,body)

server <- function(input, output) {
  set.seed(122)
  histdata <- rnorm(500)
  
  output$plot2 <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data)
  })
}

shinyApp(ui, server)