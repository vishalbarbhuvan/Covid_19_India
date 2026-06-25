library(shiny)
library(shinydashboard)
library(dplyr)
library(stringr)
library(ggplot2)
library(plotly)
library(scales)
library(tibble)
library(readr)

testing_data <- read_csv("StatewiseTestingDetails.csv", show_col_types = FALSE)

names(testing_data) <- names(testing_data) |>
  str_trim()

if ("Updated On" %in% names(testing_data)) {
  names(testing_data)[names(testing_data) == "Updated On"] <- "Date"
}

testing_data <- testing_data %>%
  mutate(
    Date = as.Date(Date, format = "%d/%m/%Y")
  )

num_cols <- intersect(c("TotalSamples", "Positive", "Negative"), names(testing_data))
testing_data[num_cols] <- lapply(testing_data[num_cols], function(x) suppressWarnings(as.numeric(x)))

testing_data <- testing_data %>%
  filter(!is.na(Date), !is.na(State))

ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "COVID-19 India Dashboard"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      selectInput(
        "state",
        "Select State",
        choices = c("All", sort(unique(testing_data$State))),
        selected = "All"
      ),
      dateRangeInput(
        "date_range",
        "Select Date Range",
        start = min(testing_data$Date, na.rm = TRUE),
        end = max(testing_data$Date, na.rm = TRUE),
        min = min(testing_data$Date, na.rm = TRUE),
        max = max(testing_data$Date, na.rm = TRUE)
      )
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "dashboard",
        
        fluidRow(
          valueBoxOutput("total_samples_box", width = 4),
          valueBoxOutput("positive_box", width = 4),
          valueBoxOutput("negativity_box", width = 4)
        ),
        
        fluidRow(
          box(
            width = 12, title = "State Summary Table",
            status = "primary", solidHeader = TRUE,
            tableOutput("state_summary")
          )
        ),
        
        fluidRow(
          box(
            width = 6, title = "Testing Trend Over Time",
            status = "warning", solidHeader = TRUE,
            plotlyOutput("tests_plot", height = 320)
          ),
          box(
            width = 6, title = "Positive Cases Over Time",
            status = "danger", solidHeader = TRUE,
            plotlyOutput("positive_plot", height = 320)
          )
        ),
        
        fluidRow(
          box(
            width = 12, title = "Positivity Rate by State",
            status = "success", solidHeader = TRUE,
            plotlyOutput("positivity_plot", height = 420)
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  filtered_data <- reactive({
    req(input$date_range)
    
    data <- testing_data %>%
      filter(Date >= input$date_range[1], Date <= input$date_range[2])
    
    if (input$state != "All") {
      data <- data %>% filter(State == input$state)
    }
    
    data
  })
  
  state_data <- reactive({
    req(input$date_range)
    
    testing_data %>%
      filter(Date >= input$date_range[1], Date <= input$date_range[2]) %>%
      group_by(State) %>%
      summarise(
        TotalSamples = suppressWarnings(max(TotalSamples, na.rm = TRUE)),
        Positive = suppressWarnings(max(Positive, na.rm = TRUE)),
        Negative = suppressWarnings(max(Negative, na.rm = TRUE)),
        .groups = "drop"
      ) %>%
      mutate(
        TotalSamples = ifelse(is.infinite(TotalSamples), NA, TotalSamples),
        Positive = ifelse(is.infinite(Positive), NA, Positive),
        Negative = ifelse(is.infinite(Negative), NA, Negative),
        PositivityRate = ifelse(
          !is.na(TotalSamples) & !is.na(Positive) & TotalSamples > 0,
          (Positive / TotalSamples) * 100,
          NA_real_
        )
      ) %>%
      filter(!is.na(State)) %>%
      arrange(desc(Positive))
  })
  
  output$state_summary <- renderTable({
    state_data() %>%
      select(State, TotalSamples, Positive, Negative, PositivityRate)
  }, striped = TRUE, hover = TRUE, spacing = "s")
  
  output$total_samples_box <- renderValueBox({
    total_samples <- filtered_data() %>%
      summarise(v = suppressWarnings(max(TotalSamples, na.rm = TRUE))) %>%
      pull(v)
    
    if (length(total_samples) == 0 || is.infinite(total_samples) || is.na(total_samples)) {
      total_samples <- 0
    }
    
    valueBox(
      comma(total_samples),
      "Total Samples",
      icon = icon("vials"),
      color = "aqua"
    )
  })
  
  output$positive_box <- renderValueBox({
    total_positive <- filtered_data() %>%
      summarise(v = suppressWarnings(max(Positive, na.rm = TRUE))) %>%
      pull(v)
    
    if (length(total_positive) == 0 || is.infinite(total_positive) || is.na(total_positive)) {
      total_positive <- 0
    }
    
    valueBox(
      comma(total_positive),
      "Total Positive",
      icon = icon("virus"),
      color = "red"
    )
  })
  
  output$negativity_box <- renderValueBox({
    df <- filtered_data()
    
    total_samples <- df %>%
      summarise(v = suppressWarnings(max(TotalSamples, na.rm = TRUE))) %>%
      pull(v)
    
    total_positive <- df %>%
      summarise(v = suppressWarnings(max(Positive, na.rm = TRUE))) %>%
      pull(v)
    
    if (length(total_samples) == 0 || is.infinite(total_samples) || is.na(total_samples)) {
      total_samples <- 0
    }
    
    if (length(total_positive) == 0 || is.infinite(total_positive) || is.na(total_positive)) {
      total_positive <- 0
    }
    
    negativity_rate <- ifelse(total_samples > 0, ((total_samples - total_positive) / total_samples) * 100, 0)
    
    valueBox(
      paste0(round(negativity_rate, 2), "%"),
      "Negativity Rate",
      icon = icon("chart-pie"),
      color = "green"
    )
  })
  
  output$tests_plot <- renderPlotly({
    plot_data <- filtered_data() %>%
      group_by(Date) %>%
      summarise(
        TotalSamples = suppressWarnings(max(TotalSamples, na.rm = TRUE)),
        .groups = "drop"
      ) %>%
      mutate(TotalSamples = ifelse(is.infinite(TotalSamples), NA, TotalSamples)) %>%
      filter(!is.na(TotalSamples))
    
    p <- ggplot(plot_data, aes(x = Date, y = TotalSamples)) +
      geom_line(color = "#0073C2FF", linewidth = 1) +
      labs(x = "Date", y = "Total Samples") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  output$positive_plot <- renderPlotly({
    plot_data <- filtered_data() %>%
      group_by(Date) %>%
      summarise(
        Positive = suppressWarnings(max(Positive, na.rm = TRUE)),
        .groups = "drop"
      ) %>%
      mutate(Positive = ifelse(is.infinite(Positive), NA, Positive)) %>%
      filter(!is.na(Positive))
    
    p <- ggplot(plot_data, aes(x = Date, y = Positive)) +
      geom_line(color = "#D7263D", linewidth = 1) +
      labs(x = "Date", y = "Positive Cases") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  output$positivity_plot <- renderPlotly({
    plot_data <- state_data()
    
    if (input$state != "All") {
      plot_data <- plot_data %>% filter(State == input$state)
    }
    
    p <- ggplot(plot_data, aes(x = reorder(State, PositivityRate), y = PositivityRate)) +
      geom_col(fill = "#2E8B57") +
      coord_flip() +
      labs(x = "State", y = "Positivity Rate (%)") +
      theme_minimal()
    
    ggplotly(p)
  })
}

shinyApp(ui, server)