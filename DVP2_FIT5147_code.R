library(shiny)
library(ggplot2)
library(dplyr)
library(readr)
library(plotly)
library(leaflet)

# Load three datasets corresponding to different regions
nyc <- read_csv("cleaned_NYC_building_data.csv")  # dataset of Newyork
utah <- read_csv("Water_Related_Land_Use_(2023).csv") # dataset of Utah
ca <- read_csv("fiscalyear_annual_data_20240409.csv") #dataset of California

# Define how each metric column maps to a grouping label (x-axis title)
metric_titles <- list(
  "2023_Eto_inches" = "Supplier Name",
  "2023_Effective_Precipitation_inches" = "Supplier Name",
  "IRR_Method" = "Irrigation Method",
  "Basin" = "Basin",
  "City" = "Borough",
  "Primary Property Type - Portfolio Manager-Calculated" = "Property Type"
)

# Define how each metric column maps to a value label (y-axis title)
value_titles <- list(
  "2023_Eto_inches" = "Total ETo (inches)",
  "2023_Effective_Precipitation_inches" = "Precipitation Efficiency (inches)",
  "IRR_Method" = "Average Acres",
  "Basin" = "Average Acres",
  "City" = "Average ENERGY STAR Score",
  "Primary Property Type - Portfolio Manager-Calculated" = "Average ENERGY STAR Score"
)

# Simulate coordinates to support spatial plotting in Leaflet
set.seed(123)
ca_coords <- ca %>% mutate(lat = runif(n(), 34, 38), lon = runif(n(), -122.5, -117))
utah_coords <- utah %>% mutate(lat = runif(n(), 37, 41), lon = runif(n(), -113.5, -109))
nyc_coords <- nyc %>% mutate(lat = runif(n(), 40.6, 40.9), lon = runif(n(), -74.1, -73.7))

# Define Shiny UI layout with dropdown filters and side-by-side visual outputs
ui <- fluidPage(
  titlePanel("Integrated Spatial–Statistical Performance Viewer"),
  fluidRow(
    column(
      width = 2,
      tags$div(style = "margin-top: 10px;",
               selectInput("region", "Select Region:", choices = c("California", "Utah", "New York City")),
               uiOutput("metric_ui")
      )
    ),
    column(
      width = 10,
      fluidRow(
        column(6, leafletOutput("regionMap", height = "700px")),
        column(6, plotlyOutput("boxplot", height = "700px"))
      ),
      verbatimTextOutput("summaryText")
    )
  )
)

# Define server logic for interactivity and visualisation
server <- function(input, output, session) {
  # Dynamically render metric dropdown based on selected region
  output$metric_ui <- renderUI({
    if (input$region == "California") {
      selectInput("metric", "Select Metric:", choices = c(
        "Total Evapotranspiration (ETo)" = "2023_Eto_inches",
        "Precipitation Efficiency" = "2023_Effective_Precipitation_inches"
      ))
    } else if (input$region == "Utah") {
      selectInput("metric", "Select Metric:", choices = c(
        "Average Acres by Irrigation Method" = "IRR_Method",
        "Average Acres by Basin" = "Basin"
      ))
    } else {
      selectInput("metric", "Select Metric:", choices = c(
        "Average ENERGY STAR Score by Borough" = "City",
        "Average ENERGY STAR Score by Property Type" = "Primary Property Type - Portfolio Manager-Calculated"
      ))
    }
  })
  # Prepare grouped and aggregated data for mapping and plotting
  selected_data <- reactive({
    req(input$region, input$metric)
    
    if (input$region == "California") {
      metric_col <- input$metric
      ca_coords %>%
        group_by(Group = Supplier_Name) %>%
        summarise(Value = if (metric_col == "2023_Eto_inches") {
          sum(.data[[metric_col]], na.rm = TRUE)
        } else {
          mean(.data[[metric_col]], na.rm = TRUE)
        }, .groups = "drop") %>%
        mutate(lat = runif(n(), 34, 38), lon = runif(n(), -122.5, -117))
      
    } else if (input$region == "Utah") {
      group_col <- input$metric
      utah %>%
        group_by(Group = .data[[group_col]]) %>%
        summarise(Value = mean(Acres, na.rm = TRUE), .groups = "drop") %>%
        mutate(lat = runif(n(), 37, 41), lon = runif(n(), -113.5, -109))
      
    } else {
      group_col <- input$metric
      nyc$`ENERGY STAR Score` <- as.numeric(nyc$`ENERGY STAR Score`)
      nyc %>%
        filter(!is.na(`ENERGY STAR Score`)) %>%
        group_by(Group = .data[[group_col]]) %>%
        summarise(Value = mean(`ENERGY STAR Score`), .groups = "drop") %>%
        mutate(lat = runif(n(), 40.6, 40.9), lon = runif(n(), -74.1, -73.7))
    }
  })
  # Render interactive map using Leaflet with colored markers and tooltips
  output$regionMap <- renderLeaflet({
    df <- selected_data()
    req(df)
    
    pal <- colorNumeric("YlGnBu", df$Value)
    
    leaflet(df) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addCircleMarkers(
        lng = ~lon, lat = ~lat,
        radius = ~sqrt(Value) * 0.8, # scale size by value
        fillColor = ~pal(Value),
        fillOpacity = 0.9,
        color = "black", stroke = TRUE, weight = 1,
        label = ~paste0("<strong>", Group, "</strong><br>", value_titles[[input$metric]], ": ", round(Value, 2)),
        labelOptions = labelOptions(direction = "auto", style = list("font-size" = "12px"))
      ) %>%
      addLegend("bottomright", pal = pal, values = ~Value, title = value_titles[[input$metric]])
  })
  # Render interactive boxplot of top 10 groups using Plotly
  output$boxplot <- renderPlotly({
    df <- selected_data()
    req(df)
    
    top_df <- df %>% arrange(desc(Value)) %>% slice_head(n = 10)
    
    gg <- ggplot(top_df, aes(
      x = reorder(Group, -Value), 
      y = Value,
      text = paste0(metric_titles[[input$metric]], ": ", Group, "<br>",
                    value_titles[[input$metric]], ": ", round(Value, 2))
    )) +
      geom_boxplot(fill = "#2b8cbe", alpha = 0.4) +
      geom_point(size = 3, color = "#045a8d") +
      labs(
        title = paste0(
          "Top 10 Boxplot of ", value_titles[[input$metric]], 
          "\nby ", metric_titles[[input$metric]], 
          " in ", input$region
        ),
        x = metric_titles[[input$metric]],
        y = value_titles[[input$metric]]
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(size = 13, lineheight = 1.1, face = "bold")
      )
    
    ggplotly(gg, tooltip = "text")
  })
  
  # Output numeric summary of selected metric values
  output$summaryText <- renderPrint({
    df <- selected_data()
    req(df)
    summary(df$Value)
  })
}

# Run the Shiny application
shinyApp(ui, server)
