library(shiny)
library(shinydashboard)
library(dplyr)
library(plotly)
library(ggplot2)
library(readr)
library(scales)
library(stringr)
library(jsonlite)
library(shinycssloaders)

testing_data <- read_csv("StatewiseTestingDetails.csv", show_col_types = FALSE)

testing_data <- testing_data %>%
  mutate(
    Date = as.Date(Date),
    State = str_trim(State),
    TotalSamples = suppressWarnings(as.numeric(TotalSamples)),
    Negative = suppressWarnings(as.numeric(Negative)),
    Positive = suppressWarnings(as.numeric(Positive))
  ) %>%
  filter(!is.na(Date), !is.na(State))

fix_state_names <- function(x) {
  case_when(
    x == "Orissa" ~ "Odisha",
    x == "Pondicherry" ~ "Puducherry",
    x == "NCT of Delhi" ~ "Delhi",
    x == "Jammu & Kashmir" ~ "Jammu and Kashmir",
    TRUE ~ x
  )
}

safe_max <- function(x) {
  x <- x[!is.na(x) & is.finite(x)]
  if (length(x) == 0) return(NA_real_)
  max(x)
}

safe_sum <- function(x) {
  x <- x[!is.na(x) & is.finite(x)]
  if (length(x) == 0) return(0)
  sum(x)
}

testing_data <- testing_data %>%
  mutate(State = fix_state_names(State))

india_geojson_url <- "https://gist.githubusercontent.com/jbrobst/56c13bbbf9d97d187fea01ca62ea5112/raw/e388c4cae20aa53cb5090210a42ebb9b765c0a36/india_states.geojson"
india_geojson <- jsonlite::read_json(india_geojson_url, simplifyVector = FALSE)

ui <- dashboardPage(
  skin = "blue",
  
  dashboardHeader(title = span("COVID-19 India", style = "font-size: 18px;")),
  
  dashboardSidebar(
    width = 220,
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("tachometer-alt")),
      br(),
      selectInput(
        "state",
        "Select State",
        choices = c("All", sort(unique(testing_data$State))),
        selected = "All",
        width = "100%"
      ),
      dateRangeInput(
        "date_range",
        "Date Range",
        start = min(testing_data$Date, na.rm = TRUE),
        end = max(testing_data$Date, na.rm = TRUE),
        min = min(testing_data$Date, na.rm = TRUE),
        max = max(testing_data$Date, na.rm = TRUE),
        width = "100%"
      )
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        body, .content-wrapper, .right-side {
          font-size: 13px;
        }
        .main-header .logo {
          font-size: 18px !important;
          font-weight: 600;
        }
        .main-sidebar {
          font-size: 13px;
        }
        .sidebar-menu > li > a {
          font-size: 14px;
        }
        .form-control {
          font-size: 13px;
          height: 34px;
        }
        .control-label {
          font-size: 13px;
          font-weight: 600;
        }
        .small-box {
          min-height: 90px !important;
        }
        .small-box h3 {
          font-size: 20px !important;
          font-weight: 700;
          margin-bottom: 6px;
        }
        .small-box p {
          font-size: 13px !important;
        }
        .box-title {
          font-size: 16px !important;
          font-weight: 600;
        }
        .table {
          font-size: 12px;
        }
      "))
    ),
    
    tabItems(
      tabItem(
        tabName = "dashboard",
        
        fluidRow(
          valueBoxOutput("total_samples_box", width = 4),
          valueBoxOutput("positive_box", width = 4),
          valueBoxOutput("positivity_box", width = 4)
        ),
        
        fluidRow(
          box(
            width = 6,
            title = "India State Map",
            status = "primary",
            solidHeader = TRUE,
            withSpinner(plotlyOutput("india_map", height = 430), type = 4)
          ),
          box(
            width = 6,
            title = "Positivity Rate by State",
            status = "success",
            solidHeader = TRUE,
            withSpinner(plotlyOutput("positivity_plot", height = 430), type = 4)
          )
        ),
        
        fluidRow(
          box(
            width = 6,
            title = "Testing Trend Over Time",
            status = "warning",
            solidHeader = TRUE,
            withSpinner(plotlyOutput("tests_plot", height = 300), type = 4)
          ),
          box(
            width = 6,
            title = "Positive Cases Over Time",
            status = "danger",
            solidHeader = TRUE,
            withSpinner(plotlyOutput("positive_plot", height = 300), type = 4)
          )
        ),
        
        fluidRow(
          box(
            width = 12,
            title = "State Summary Table",
            status = "primary",
            solidHeader = TRUE,
            withSpinner(tableOutput("state_summary"), type = 4)
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  filtered_data <- reactive({
    df <- testing_data %>%
      filter(Date >= input$date_range[1], Date <= input$date_range[2])
    
    if (input$state != "All") {
      df <- df %>% filter(State == input$state)
    }
    
    df
  })
  
  state_data <- reactive({
    testing_data %>%
      filter(Date >= input$date_range[1], Date <= input$date_range[2]) %>%
      group_by(State) %>%
      summarise(
        TotalSamples = safe_max(TotalSamples),
        Positive = safe_max(Positive),
        Negative = safe_max(Negative),
        .groups = "drop"
      ) %>%
      mutate(
        PositivityRate = ifelse(
          !is.na(TotalSamples) & TotalSamples > 0 & !is.na(Positive),
          100 * Positive / TotalSamples,
          NA_real_
        )
      ) %>%
      filter(!is.na(State), !is.na(PositivityRate))
  })
  
  output$total_samples_box <- renderValueBox({
    df <- filtered_data()
    
    total_samples <- if (input$state == "All") {
      df %>%
        group_by(State) %>%
        summarise(v = safe_max(TotalSamples), .groups = "drop") %>%
        summarise(total = safe_sum(v)) %>%
        pull(total)
    } else {
      safe_max(df$TotalSamples)
    }
    
    if (length(total_samples) == 0 || is.na(total_samples) || !is.finite(total_samples)) {
      total_samples <- 0
    }
    
    valueBox(comma(total_samples), "Total Samples", icon = icon("vials"), color = "aqua")
  })
  
  output$positive_box <- renderValueBox({
    df <- filtered_data()
    
    total_positive <- if (input$state == "All") {
      df %>%
        group_by(State) %>%
        summarise(v = safe_max(Positive), .groups = "drop") %>%
        summarise(total = safe_sum(v)) %>%
        pull(total)
    } else {
      safe_max(df$Positive)
    }
    
    if (length(total_positive) == 0 || is.na(total_positive) || !is.finite(total_positive)) {
      total_positive <- 0
    }
    
    valueBox(comma(total_positive), "Total Positive", icon = icon("virus"), color = "red")
  })
  
  output$positivity_box <- renderValueBox({
    df <- filtered_data()
    
    if (input$state == "All") {
      total_samples <- df %>%
        group_by(State) %>%
        summarise(v = safe_max(TotalSamples), .groups = "drop") %>%
        summarise(total = safe_sum(v)) %>%
        pull(total)
      
      total_positive <- df %>%
        group_by(State) %>%
        summarise(v = safe_max(Positive), .groups = "drop") %>%
        summarise(total = safe_sum(v)) %>%
        pull(total)
    } else {
      total_samples <- safe_max(df$TotalSamples)
      total_positive <- safe_max(df$Positive)
    }
    
    if (length(total_samples) == 0 || is.na(total_samples) || !is.finite(total_samples)) total_samples <- 0
    if (length(total_positive) == 0 || is.na(total_positive) || !is.finite(total_positive)) total_positive <- 0
    
    positivity_rate <- ifelse(total_samples > 0, 100 * total_positive / total_samples, 0)
    
    valueBox(paste0(round(positivity_rate, 2), "%"), "Positivity Rate", icon = icon("chart-line"), color = "green")
  })
  
  output$india_map <- renderPlotly({
    map_df <- state_data() %>%
      mutate(
        hover_text = paste0(
          "<b>", State, "</b><br>",
          "Total Samples: ", comma(TotalSamples), "<br>",
          "Positive: ", comma(Positive), "<br>",
          "Negative: ", comma(Negative), "<br>",
          "Positivity Rate: ", round(PositivityRate, 2), "%"
        )
      )
    
    req(nrow(map_df) > 0)
    
    plot_ly(
      data = map_df,
      type = "choropleth",
      geojson = india_geojson,
      locations = ~State,
      z = ~PositivityRate,
      featureidkey = "properties.ST_NM",
      text = ~hover_text,
      hoverinfo = "text",
      source = "india_map_source",
      colors = colorRamp(c("#fee5d9", "#fb6a4a", "#cb181d")),
      marker = list(line = list(color = "white", width = 0.8))
    ) %>%
      layout(
        geo = list(
          fitbounds = "locations",
          visible = FALSE
        ),
        margin = list(l = 0, r = 0, t = 0, b = 0),
        font = list(size = 11)
      )
  })
  
  observeEvent(event_data("plotly_click", source = "india_map_source"), {
    click_data <- event_data("plotly_click", source = "india_map_source")
    if (!is.null(click_data) && !is.null(click_data$location)) {
      selected_state <- click_data$location
      if (selected_state %in% unique(testing_data$State)) {
        updateSelectInput(session, "state", selected = selected_state)
      }
    }
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
      theme_minimal(base_size = 11) +
      theme(
        axis.text.y = element_text(size = 9),
        axis.text.x = element_text(size = 9),
        axis.title = element_text(size = 11)
      )
    
    ggplotly(p) %>% layout(font = list(size = 11))
  })
  
  output$tests_plot <- renderPlotly({
    plot_data <- filtered_data() %>%
      group_by(Date) %>%
      summarise(TotalSamples = safe_max(TotalSamples), .groups = "drop") %>%
      filter(!is.na(TotalSamples))
    
    p <- ggplot(plot_data, aes(x = Date, y = TotalSamples)) +
      geom_line(color = "#0073C2FF", linewidth = 0.9) +
      labs(x = "Date", y = "Total Samples") +
      theme_minimal(base_size = 11)
    
    ggplotly(p) %>% layout(font = list(size = 11))
  })
  
  output$positive_plot <- renderPlotly({
    plot_data <- filtered_data() %>%
      group_by(Date) %>%
      summarise(Positive = safe_max(Positive), .groups = "drop") %>%
      filter(!is.na(Positive))
    
    p <- ggplot(plot_data, aes(x = Date, y = Positive)) +
      geom_line(color = "#D7263D", linewidth = 0.9) +
      labs(x = "Date", y = "Positive Cases") +
      theme_minimal(base_size = 11)
    
    ggplotly(p) %>% layout(font = list(size = 11))
  })
  
  output$state_summary <- renderTable({
    state_data() %>%
      arrange(desc(Positive)) %>%
      mutate(PositivityRate = round(PositivityRate, 2)) %>%
      select(State, TotalSamples, Positive, Negative, PositivityRate)
  }, striped = TRUE, hover = TRUE, spacing = "s")
}

shinyApp(ui, server)