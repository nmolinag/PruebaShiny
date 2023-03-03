library(shiny)

# Define UI
ui <- fluidPage(
  
  # File input to upload list of activities
  fileInput("activity_file", "Upload activity file"),
  
  tableOutput("loaded_activities"),
  # Select input to display activities
  selectInput("activity_select", "Select activity", choices = NULL),
  
  # Checkbox group input for toggles
  checkboxGroupInput("toggles", "Select toggles", 
                     choices = c("Toggle 1", "Toggle 2", "Toggle 3", "Toggle 4")),
  
  # Numeric input for risk levels
  numericInput("toggle_risk_1", "Toggle 1 risk level", value = 1),
  numericInput("toggle_risk_2", "Toggle 2 risk level", value = 2),
  numericInput("toggle_risk_3", "Toggle 3 risk level", value = 3),
  numericInput("toggle_risk_4", "Toggle 4 risk level", value = 4),
  
  # Table to display new list of activities
  tableOutput("new_activities")
  
)

# Define server
server <- function(input, output, session) {
  
  # Reactive function to read activity file and display choices
  activity_choices <- reactive({
    req(input$activity_file)
    activities <- readLines(input$activity_file$datapath, encoding = "ISO-8859-1")
    activities
  })
  
  # Update activity select input based on uploaded file
  
  observeEvent(activity_choices(), {
    updateSelectInput(session, "activity_select", choices = activity_choices())
  })
  
  # Reactive function to generate new list of activities based on toggles
  new_activities <- reactive({
    req(input$toggles)
    toggle_risks <- c(input$toggle_risk_1, input$toggle_risk_2, input$toggle_risk_3)
    activity_risks <- matrix(c(1,2,3,2,3,4,3,4,5), nrow = 3)
    toggle_indices <- match(input$toggles, colnames(activity_risks))
    activity_risks[,toggle_indices] <- toggle_risks
    activity_risks <- apply(activity_risks, 1, sum)
    activities <- data.frame(activity = input$activity_select, risk = activity_risks)
    activities <- activities[order(activities$risk, decreasing = TRUE),]
    activities$activity
  })
  
  # Update new activities table output based on toggles
  output$new_activities <- renderTable({
    req(input$toggles)
    data.frame(activity = new_activities())
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)
