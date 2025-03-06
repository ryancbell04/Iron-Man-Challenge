library(shiny)

ui <- fluidPage(
  titlePanel("Ironman Challenge Progress"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("swim_progress", "Swim Progress (Laps):", min = 0, max = 85, value = 44),
      sliderInput("bike_progress", "Bike Progress (Miles):", min = 0, max = 112, value = 18),
      sliderInput("run_progress", "Run Progress (Miles):", min = 0, max = 26, value = 6),
      numericInput("swim_laps", "Swim Laps:", value = 44, min = 0, max = 84),
      numericInput("bike_miles", "Bike Miles:", value = 18, min = 0, max = 112),
      numericInput("run_miles", "Run Miles:", value = 6, min = 0, max = 26),
      #actionButton("update_miles", "Update")
    ),
    mainPanel(
      htmlOutput("progress_summary"),
      strong("Total Miles: "), textOutput("total_miles")
    )
  )
)

server <- function(input, output, session) {
  total_miles_reactive <- reactiveVal(0)
  
  observeEvent(input$update_miles, {
    total_miles_reactive((input$swim_laps / 35) + input$bike_miles + input$run_miles)
  })
  
  observeEvent(input$swim_progress, {
    updateNumericInput(session, "swim_laps", value = input$swim_progress)
  })
  observeEvent(input$bike_progress, {
    updateNumericInput(session, "bike_miles", value = input$bike_progress)
  })
  observeEvent(input$run_progress, {
    updateNumericInput(session, "run_miles", value = input$run_progress)
  })
  
  observeEvent(input$swim_laps, {
    updateSliderInput(session, "swim_progress", value = input$swim_laps)
  })
  observeEvent(input$bike_miles, {
    updateSliderInput(session, "bike_progress", value = input$bike_miles)
  })
  observeEvent(input$run_miles, {
    updateSliderInput(session, "run_progress", value = input$run_miles)
  })
  
  output$total_miles <- renderText({
    total_miles <- (input$swim_progress / 35) + input$bike_progress + input$run_progress + total_miles_reactive()
    sprintf("%.2f miles", total_miles)
  })
  
  output$progress_summary <- renderUI({
    swim_done <- (input$swim_progress / 85) * 100
    bike_done <- input$bike_progress / 112 * 100
    run_done <- input$run_progress / 26 * 100
    total_mins <- (input$swim_progress * 1) + (input$bike_progress * 4) + (input$run_progress * 9)
    
    HTML(paste(
      "<h3>Progress Summary</h3>",
      sprintf("<p>Swim: %.2f%% complete</p>", swim_done),
      sprintf("<p>Bike: %.2f%% complete</p>", bike_done),
      sprintf("<p>Run: %.2f%% complete</p>", run_done),
      sprintf("<p><strong>Total Minutes Done: %d</strong></p>", total_mins)
    ))
  })
}

shinyApp(ui = ui, server = server)