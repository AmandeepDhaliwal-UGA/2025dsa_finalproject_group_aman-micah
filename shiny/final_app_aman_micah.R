
# Created with the assistance of Google Gemini Advanced (2.0 Flash Version)

library(tidyverse)
library(shiny)
library(shinyjs)

weather_sheet <- read.csv("shiny/train-final-soils-weather.csv")
#predictions_data <- read.csv("shiny/DATASET_HERE.csv") # Load the new dataset

# Define the soil property options with cleaner names
soil_properties <- c(
  "Soil pH" = "soilpH",
  "Soil Organic Matter %" = "om_pct",
  "Soil Potassium (ppm)" = "soilk_ppm",
  "Soil Phosphorus (ppm)" = "soilp_ppm"
)

# Define the precipitation options with cleaner names
precip_properties <- c(
  "March Precipitation (mm)" = "monthly_precip_mar",
  "April Precipitation (mm)" = "monthly_precip_apr",
  "May Precipitation (mm)" = "monthly_precip_may",
  "June Precipitation (mm)" = "monthly_precip_jun",
  "July Precipitation (mm)" = "monthly_precip_jul",
  "August Precipitation (mm)" = "monthly_precip_aug",
  "September Precipitation (mm)" = "monthly_precip_sep",
  "October Precipitation (mm)" = "monthly_precip_oct",
  "November Precipitation (mm)" = "monthly_precip_nov"
)

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .title {
        font-size: 2.5em;
        font-weight: bold;
        color: #333;
        text-align: center;
        margin-bottom: 0px;
      }
      .subtitle {
        font-size: 1.2em;
        color: #666;
        text-align: center;
        margin-top: 5px;
        margin-bottom: 15px;
      }
    "))
  ),
  tags$div(class = "title", "CRSS 8030 Final Project - XGBoost"),
  tags$div(class = "subtitle", "Amandeep Dhaliwal and Micah Jones"),
  navbarPage(
    "", # Empty title for navbarPage to avoid redundancy
    tabPanel("EDA: Soil Properties and Yield",
             sidebarLayout(
               sidebarPanel(
                 selectInput("soil_property", "Select Soil Property:", choices = soil_properties)
               ),
               mainPanel(
                 plotOutput("yield_soil_plot")
               )
             )
    ),
    tabPanel("EDA: Monthly Precipitation and Yield",
             sidebarLayout(
               sidebarPanel(
                 selectInput("precip_property", "Select Precipitation Month:", choices = precip_properties)
               ),
               mainPanel(
                 plotOutput("yield_precip_plot")
               )
             )
    ),
    tabPanel("EDA: Yield Distribution By Hybrid",
             sidebarLayout(
               sidebarPanel(
                 selectInput("hybrid_selector", "Select Hybrid(s):", choices = unique(weather_sheet$hybrid), multiple = TRUE)
               ),
               mainPanel(
                 plotOutput("yield_distribution_plot")
               )
             )
    ),
    tabPanel("XGBoost - Variable Ranking",
             mainPanel(
               uiOutput("variable_ranking_image")
             )
    ),
    tabPanel("XGBoost Predicted vs. Observed", # New tab
             mainPanel(
               uiOutput("pvo_plot")
             )
    )
  )
)

server <- function(input, output) {
  output$yield_soil_plot <- renderPlot({
    selected_property_name <- names(soil_properties[soil_properties == input$soil_property])
    
    ggplot(weather_sheet, aes_string(x = input$soil_property, y = "yield_adj_mg_ha")) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE, color = "blue") +
      labs(
        x = selected_property_name,
        y = "Adjusted Yield (mg/ha)",
        title = paste("Corn Yield vs.", selected_property_name)
      ) +
      theme_minimal()
  })
  
  output$yield_precip_plot <- renderPlot({
    selected_property_name <- names(precip_properties[precip_properties == input$precip_property])
    
    ggplot(weather_sheet, aes_string(x = input$precip_property, y = "yield_adj_mg_ha")) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE, color = "blue") +
      labs(
        x = selected_property_name,
        y = "Adjusted Yield (mg/ha)",
        title = paste("Corn Yield vs.", selected_property_name)
      ) +
      theme_minimal()
  })
  
  output$yield_distribution_plot <- renderPlot({
    # Filter by selected hybrids
    filtered_data <- weather_sheet %>%
      filter(hybrid %in% input$hybrid_selector)
    
    ggplot(filtered_data, aes(x = yield_adj_mg_ha, color = hybrid)) +
      geom_density(alpha = 0.7) +
      labs(
        x = "Adjusted Yield (mg/ha)",
        y = "Density",
        color = "Hybrid",
        title = "Distribution of Adjusted Yield by Hybrid"
      ) +
      theme_minimal()
  })
  
  output$variable_ranking_image <- renderUI({
    tags$img(src = ".../shiny/var_importance_xgboost.png", alt = "Variable Ranking", width = "100%")
  })
  
  
  output$pvo_plot <- renderUI({
    tags$img(src = ".../shiny/xgb_pred_vs_obs.png", alt = "Predicted vs Observed", width = "100%")
  })
}

shinyApp(ui = ui, server = server)

