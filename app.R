library(cranlogs)
library(shiny)
library(ggplot2)
library(lubridate)
library(dplyr)
library(patchwork)

ui <- fluidPage(
  titlePanel("CRAN Package Download Statistics"),
  sidebarLayout(
    sidebarPanel(
      textInput("package_name", "Enter CRAN Package Name:", ""),
      dateInput("from_date", "From Date:", value = Sys.Date() - 30, max = Sys.Date()),
      dateInput("to_date", "To Date:", value = Sys.Date(), max = Sys.Date()),
      actionButton("submit", "Get Download Stats"),
      h3("Download Statistics Summary"),
      verbatimTextOutput("download_summary")
    ),
    mainPanel(
      plotOutput("download_plot"),
      fluidRow(
        column(6, plotOutput("weekly_downloads")),
        column(6, plotOutput("monthly_downloads"))
      )
    )
  )
)

server <- function(input, output, session) {
  
  # Reactive function to get download stats
  get_download_stats <- reactive({
    package_name <- input$package_name
    from_date <- input$from_date
    to_date <- input$to_date
    cran_downloads(package_name, from = from_date, to = to_date)
  })
  
  # Plot the daily download stats
  output$download_plot <- renderPlot({
    data <- get_download_stats()
    ggplot(data, aes(x = date, y = count)) +
      geom_line() +
      labs(x = "Date", y = "Downloads", title = paste("Daily Downloads for", input$package_name))
  })
  
  # Plot the monthly downloads
  output$weekly_downloads <- renderPlot({
    data <- get_download_stats()
    monthly_data <- data %>% mutate(month = floor_date(date, "month"))
    
    ggplot(monthly_data, aes(x = month, y = count)) +
      geom_line() +
      labs(x = "Month", y = "Cumulative Downloads", title = paste("Montly Downloads for", input$package_name))
  })
  
  # Plot the monthly cumulative downloads
  output$monthly_downloads <- renderPlot({
    data <- get_download_stats()
    monthly_data <- data %>%
      mutate(month = floor_date(date, "month")) %>%
      group_by(month) %>%
      summarise(count = sum(count))
    
    ggplot(monthly_data, aes(x = month, y = cumsum(count))) +
      geom_line() +
      labs(x = "Month", y = "Cumulative Downloads", title = paste("Monthly Cumulative Downloads for", input$package_name))
  })
  
  # Display the download statistics summary
  output$download_summary <- renderText({
    data <- get_download_stats()
    total_downloads <- sum(data$count)
    avg_downloads_per_day <- mean(data$count)
    total_downloads_per_week <- sum(data %>% mutate(week = floor_date(date, "week")) %>% group_by(week) %>% summarise(count = sum(count)))
    total_downloads_per_month <- sum(data %>% mutate(month = floor_date(date, "month")) %>% group_by(month) %>% summarise(count = sum(count)))
    
    sprintf("Total Downloads: %d\nAverage Downloads per Day: %.2f\nTotal Downloads per Week: %d\nTotal Downloads per Month: %d",
            total_downloads, avg_downloads_per_day, total_downloads_per_week, total_downloads_per_month)
  })
  
}

shinyApp(ui, server)