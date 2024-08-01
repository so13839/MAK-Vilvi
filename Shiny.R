library(shiny)
library(xgboost)
library(caret)
library(dplyr)

# Assuming the data frame 'Acinetobacter_regression' and the model 'xgb.model' are preloaded
# If not, you should load your data and model here
xgb.model <- readRDS("xgb.model.rds")
# Define UI
ui <- fluidPage(
  titlePanel("Amikacin Resistance Prediction"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("gender", "Gender:",
                  choices = c("Male", "Female")),
      
      selectInput("speciality", "Speciality:",
                  choices = c("Clinic / Office",
                              "Emergency Room",
                              "General Unspecified ICU",
                              "Medicine General",
                              "Medicine ICU",
                              "None Given",
                              "Other",
                              "Pediatric General",
                              "Pediatric ICU",
                              "Surgery General",
                              "Surgery ICU")),
      
      selectInput("source", "Source:",
                  choices = c("Abdominal Fluid",
                              "Abscess",
                              "Aspirate",
                              "Bladder",
                              "Blood",
                              "Bodily Fluids",
                              "Bronchoalveolar lavage",
                              "Bronchus",
                              "Burn",
                              "Catheters",
                              "Colon",
                              "Decubitus",
                              "Drains",
                              "Endotracheal aspirate",
                              "Eye",
                              "Gall Bladder",
                              "Gastric Abscess",
                              "Genitourinary: Other",
                              "Instruments: Other",
                              "Integumentary (Skin Nail Hair)",
                              "Intestinal: Other",
                              "Kidney",
                              "None Given",
                              "Peritoneal Fluid",
                              "Pleural Fluid",
                              "Respiratory: Other",
                              "Skin",
                              "Skin: Other",
                              "Sputum",
                              "Synovial Fluid",
                              "Tissue Fluid",
                              "Trachea",
                              "Ulcer",
                              "Urethra",
                              "Urine",
                              "Wound")),
      
      selectInput("imipenem", "Imipenem (I):",
                  choices = c("Resistant", "Intermediate", "Susceptible")),
      
      selectInput("meropenem", "Meropenem (I):",
                  choices = c("Resistant", "Intermediate", "Susceptible")),
      
      numericInput("temperature", "Temperature (°C):",
                   value = NA, min = -10, max = 50, step = 0.1),
      
      numericInput("co2_emissions", "CO2 Emissions (Metric Tons):",
                   value = NA, min = 0, max = 1000, step = 0.1),
      
      numericInput("sea_level_rise", "Sea Level Rise (mm):",
                   value = NA, min = -10, max = 10, step = 0.1),
      
      numericInput("precipitation", "Precipitation (mm):",
                   value = NA, min = 0, max = 500, step = 0.1),
      
      numericInput("humidity", "Humidity (%):",
                   value = NA, min = 0, max = 100, step = 0.1),
      
      numericInput("wind_speed", "Wind Speed (km/h):",
                   value = NA, min = 0, max = 150, step = 0.1),
      
      actionButton("predict", "Predict")
    ),
    
    mainPanel(
      h3("Prediction Result"),
      textOutput("prediction"),
      tableOutput("prediction_table")
    )
  )
)

# Define Server
server <- function(input, output) {
  
  observeEvent(input$predict, {
    
    # Convert categorical inputs to numeric format similar to the training data
    input_data <- data.frame(
      Gender = as.numeric(factor(input$gender, levels = c("Male", "Female"))),
      Speciality = as.numeric(factor(input$speciality, levels = c("Clinic / Office",
                                                                  "Emergency Room",
                                                                  "General Unspecified ICU",
                                                                  "Medicine General",
                                                                  "Medicine ICU",
                                                                  "None Given",
                                                                  "Other",
                                                                  "Pediatric General",
                                                                  "Pediatric ICU",
                                                                  "Surgery General",
                                                                  "Surgery ICU"))),
      Source = as.numeric(factor(input$source, levels = c("Abdominal Fluid",
                                                          "Abscess",
                                                          "Aspirate",
                                                          "Bladder",
                                                          "Blood",
                                                          "Bodily Fluids",
                                                          "Bronchoalveolar lavage",
                                                          "Bronchus",
                                                          "Burn",
                                                          "Catheters",
                                                          "Colon",
                                                          "Decubitus",
                                                          "Drains",
                                                          "Endotracheal aspirate",
                                                          "Eye",
                                                          "Gall Bladder",
                                                          "Gastric Abscess",
                                                          "Genitourinary: Other",
                                                          "Instruments: Other",
                                                          "Integumentary (Skin Nail Hair)",
                                                          "Intestinal: Other",
                                                          "Kidney",
                                                          "None Given",
                                                          "Peritoneal Fluid",
                                                          "Pleural Fluid",
                                                          "Respiratory: Other",
                                                          "Skin",
                                                          "Skin: Other",
                                                          "Sputum",
                                                          "Synovial Fluid",
                                                          "Tissue Fluid",
                                                          "Trachea",
                                                          "Ulcer",
                                                          "Urethra",
                                                          "Urine",
                                                          "Wound"))),
      Imipenem_I = as.numeric(factor(input$imipenem, levels = c("Resistant", "Intermediate", "Susceptible"))),
      Meropenem_I = as.numeric(factor(input$meropenem, levels = c("Resistant", "Intermediate", "Susceptible"))),
      Temperature = input$temperature,
      CO2.Emissions = input$co2_emissions,
      Sea.Level.Rise = input$sea_level_rise,
      Precipitation = input$precipitation,
      Humidity = input$humidity,
      Wind.Speed = input$wind_speed
    )
    
    # Check if any input is NA and handle accordingly
    if (any(is.na(input_data))) {
      output$prediction <- renderText({
        "Please input all values before predicting."
      })
      output$prediction_table <- renderTable({
        NULL
      })
      return()
    }
    
    # Convert input data to matrix
    input_matrix <- as.matrix(input_data)
    input_dmatrix <- xgb.DMatrix(data = input_matrix)
    
    # Predict using the pre-trained model
    prediction <- predict(xgb.model, newdata = input_dmatrix, reshape = TRUE)
    prediction <- data.frame(prediction)
    colnames(prediction) <- c("Intermediate", "Resistant", "Susceptible")
    
    # Get the class with the highest probability
    predicted_class <- apply(prediction, 1, which.max)
    predicted_class[predicted_class == 1] <- "Intermediate"
    predicted_class[predicted_class == 2] <- "Resistant"
    predicted_class[predicted_class == 3] <- "Susceptible"
    
    # Update the prediction result on UI
    output$prediction <- renderText({
      paste("The predicted Amikacin resistance category is:", predicted_class)
    })
    
    # Display the probability table
    output$prediction_table <- renderTable({
      prediction
    }, rownames = TRUE)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
