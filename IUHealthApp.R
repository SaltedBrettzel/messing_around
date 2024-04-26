
# Load required libraries
library(shiny)
library(DescTools)

two_hundred <- matrix(0, nrow = 200, ncol = 3)

colnames(two_hundred) <- c("Percentage","Out of..","Total Days")

two_hundred[,1] <- 0.005

increment <- 0.005

for (i in 2:200) {
  two_hundred[i, "Percentage"] <- two_hundred[i - 1, "Percentage"] + increment
}

two_hundred[,2] <- 200

two_hundred[,3] <- (two_hundred[,1]*two_hundred[,2])

two_hundred <- as.data.frame(two_hundred)

Percents <- two_hundred$Percentage

closest_days_worked <- Closest(Percents, 0.460)

two_hundred[,1]==closest_days_worked

oops <- two_hundred[two_hundred[,1]==closest_days_worked,]

# Generate the data frame
two_hundred <- data.frame(
  Percentage = seq(0, 1, by = 0.005),
  `Out of..` = 200,
  `Total Days` = 200 * seq(0, 1, by = 0.005)
)

# Load required libraries
library(shiny)
library(DescTools)

# Generate the data frame
two_hundred <- data.frame(
  Percentage = seq(0, 1, by = 0.005),
  `Out of..` = 200,
  `Total Days` = 200 * seq(0, 1, by = 0.005)
)

# Define the UI
ui <- fluidPage(
  tags$head(
    tags$style(
      HTML(
        "
        body {
          background-color: rgba(153, 0, 0, 1); /* Dark red background color with 80% opacity */
        }
        .table-light {
          color: #555555; /* Lighter color for table text */
        }
        .result-panel {
          background-color: white; /* White background color for result panel */
          padding: 20px; /* Add padding to the panel */
          border-radius: 10px; /* Rounded corners */
          margin-top: 20px; /* Add space between input and result */
        }
        .title-panel {
          background-color: white; /* White background color for title panel */
          padding: 10px; /* Add padding to the panel */
          border-radius: 5px; /* Rounded corners */
          margin-bottom: 20px; /* Add space between title panel and inputs */
        }
        "
      )
    )
  ),
  div(class = "title-panel",  # Add class to div
      titlePanel("Percentage Lookup")),  # Add titlePanel inside div
  sidebarLayout(
    sidebarPanel(
      selectInput("num_inputs", "Select number of percentage values to input:", 
                  choices = 1:20, selected = 1),
      uiOutput("on_service_sliders"),  # Dynamic UI for sliders
      uiOutput("input_areas"),
      actionButton("submit", "Submit")
    ),
    mainPanel(
      img(src = "https://cdn.iuhealth.org/images/logo-full.svg", width = "50%", height = "auto"),  # Image URL
      div(class = "result-panel",
          tableOutput("result")
      )
    )
  )
)

# Define the server logic
server <- function(input, output, session) {
  # Generate multiple input areas dynamically
  output$input_areas <- renderUI({
    num_inputs <- input$num_inputs
    lapply(1:num_inputs, function(i) {
      numericInput(inputId = paste0("percentage", i), 
                   label = paste("FTE Percentage for Practitioner", i, ":"), 
                   value = 0, min = 0, max = 1, step = 0.005)
    })
  })
  
  # Generate multiple sliders for 'on_service' dynamically
  output$on_service_sliders <- renderUI({
    num_inputs <- input$num_inputs
    lapply(1:num_inputs, function(i) {
      sliderInput(inputId = paste0("on_service_", i), 
                  label = paste("On-Service Weeks for Practitioner", i, ":"), 
                  min = 1, max = 10, value = 5)
    })
  })
  
  # Calculate and display results based on user input
  observeEvent(input$submit, {
    num_inputs <- input$num_inputs
    percentages <- sapply(1:num_inputs, function(i) input[[paste0("percentage", i)]])
    
    # Calculate the closest percentage rows
    closest_rows <- lapply(percentages, function(percent) Closest(two_hundred$Percentage, percent))
    
    # Filter the data frame to get the closest rows
    results <- lapply(closest_rows, function(row) two_hundred[two_hundred$Percentage == row, ])
    
    # Combine the results into a single table
    result_table <- do.call(rbind, results)
    
    # Round the result_table to 4 decimal places
    result_table <- signif(result_table, 4)
    
    # Perform multiplication based on 'on-service' values
    on_service_values <- sapply(1:num_inputs, function(i) input[[paste0("on_service_", i)]])
    result_table$`On-Service` <- on_service_values * 2
    
    # Return the combined result table
    output$result <- renderTable({
      result_table
    }, digits = 3)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
