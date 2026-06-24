library(shiny)
library(readr)
library(dplyr)
library(ggplot2)

monthly_df <- read_csv("StatewiseTestingDetails_Monthly.csv", show_col_types = FALSE)
india_map_data <- read_csv("india_map_data.csv", show_col_types = FALSE)

monthly_df$Month <- as.Date(paste0(monthly_df$Month, "-01"))

ui <- fluidPage(
  titlePanel("India COVID-19 Monthly Testing Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "state",
        "Select State",
        choices = sort(unique(monthly_df$State)),
        selected = unique(monthly_df$State)[1]
      ),
      selectInput(
        "metric",
        "Select Metric",
        choices = c("TotalSamples", "Negative", "Positive"),
        selected = "TotalSamples"
      )
    ),
    
    mainPanel(
      h3("Monthly Trend"),
      plotOutput("trend_plot"),
      h3("India State Map"),
      plotOutput("india_map_plot", height = "600px")
    )
  )
)

server <- function(input, output) {
  
  output$trend_plot <- renderPlot({
    monthly_df %>%
      filter(State == input$state) %>%
      ggplot(aes(x = Month, y = .data[[input$metric]])) +
      geom_line(color = "blue", linewidth = 1.2) +
      geom_point(color = "red", size = 2) +
      labs(
        title = paste("Monthly", input$metric, "for", input$state),
        x = "Month",
        y = input$metric
      ) +
      theme_minimal()
  })
  
  output$india_map_plot <- renderPlot({
    ggplot(india_map_data, aes(x = long, y = lat, group = group)) +
      geom_polygon(fill = "lightblue", color = "white") +
      coord_fixed(1.3) +
      theme_void() +
      labs(title = "India State Map")
  })
}

shinyApp(ui, server)