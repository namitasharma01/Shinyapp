# Install dependent package apikolada from github to run the shiny app

devtools::install_github("namitasharma01/R-Programming-Lab/apikolada", upgrade = "always")
library("apikolada")

# class object of type api.kolada to interface with the API
obj.kolada <- apikolada::api.kolada()

# Shiny app UI
ui <- shiny::fluidPage(
  
  # App title
  shiny::titlePanel("Key Performance Indicators for Municipalities"),
  
  # Layout for input and output
  shiny::sidebarLayout(
    
    # Panel for user inputs
    shiny::sidebarPanel(
      shiny::selectInput("municipality", 
                         "Choose a municipality", 
                         choices = obj.kolada$get.muni()$title),
      
      shiny::selectInput("kpigroup", 
                         "Choose a KPI group", 
                         choices = obj.kolada$get.kpi.group()$title),
      
      shiny::selectInput("kpi", 
                         "Choose a KPI", 
                         choices = obj.kolada$get.kpi.member(kpigroup = "")$member_title),      
      
      shiny::radioButtons("gender",
                          "Filter KPI data based on gender",
                          choices = c("T", "M", "K")),
      
      shiny::numericInput("no.obs", "Number of observations to view", 10),
      
      shiny::actionButton("update", "Update View")
    ),
    
    shiny::mainPanel(
      
      # Tabs output - Plot and Summary of KPI value over the years
      shiny::tabsetPanel(type = "tabs",
                         shiny::tabPanel("Plot", 
                                         shiny::h4(shiny::textOutput("plottitle")),
                                         shiny::plotOutput("plotkpi"), 
                                         shiny::tableOutput("view")),
                         shiny::tabPanel("Summary", 
                                         shiny::verbatimTextOutput("summary")))
    )
  )
)

# App logic
server <- function(input, output, session) {
  
  # List only the KPIs grouped under the KPI group selected by user
  observeEvent(input$kpigroup, {
    updateSelectInput(session,
                      "kpi",
                      choices = obj.kolada$get.kpi.member(kpigroup = input$kpigroup)$member_title)
  })
  
  # Refresh output table and plot only when user clicks on update button
  muni.kpi.df <- eventReactive(input$update, {
    obj.kolada$get.muni.kpi(kpigroup = input$kpigroup, 
                            muni     = input$municipality, 
                            kpi      = input$kpi,
                            gender   = input$gender)
  }, ignoreNULL = FALSE)
  
  # Reactive text for the plot heading and y-axis based on KPI input
  plot.label <- eventReactive(input$update, {
    input$kpi
  })
  
  # Plot title
  output$plottitle <- renderText({
    if (!is.null(muni.kpi.df())) {
      plot.label()
    } else {
      muni.kpi.df() #"Sorry! Data not available for the requested input from API"
    }
  })
  
  # Output KPI plot data
  output$plotkpi <- renderPlot({
    if (!is.null(muni.kpi.df())) {    
      obj.kolada$plot.muni.kpi(period_val = muni.kpi.df()$period, 
                               kpi_val    = muni.kpi.df()$value, 
                               kpi_label  = plot.label())
    }
  })
  
  # Output KPI data as a table
  output$view <- renderTable({ 
    head(muni.kpi.df(), n = isolate(input$no.obs))
  })
  
  # KPI summary
  output$summary <- renderPrint({
    if (!is.null(muni.kpi.df())) {    
      obj.kolada$summary(muni.kpi.df())
    } else {
      muni.kpi.df() #"Sorry! Data not available for the requested input from API"
    }
  })    
}

# Shiny app
shiny::shinyApp(ui, server)
