library(cranlogs)
library(shiny)
library(ggplot2)
library(lubridate)
library(dplyr)
library(patchwork)
library(tidyr)
library(purrr)
library(tools)

ui <- fluidPage(
  theme = bslib::bs_theme(version = 4, bootswatch = "darkly"),
  titlePanel("CRAN Package Downloads"),
  sidebarLayout(
    sidebarPanel(
      selectizeInput("package_name", "Select CRAN Packages:",
                     choices = NULL,
                     multiple = TRUE,
                     options = list(maxItems = 10)),
      radioButtons("time_unit", "Time Unit:",
                   choices = c("Daily" = "daily",
                               "Weekly" = "weekly",
                               "Monthly" = "monthly"),
                   selected = "daily"),
      dateInput("from_date", "From Date:",
                value = Sys.Date() - 30,
                max = Sys.Date()),
      dateInput("to_date", "To Date:",
                value = Sys.Date(),
                max = Sys.Date()),
      actionButton("submit", "Get Download Stats",
                   class = "btn-primary"),
      h3("Summary"),
      tags$div(
        style = "height: 350px; overflow-y: scroll;",
        verbatimTextOutput("download_summary")
      )
    ),
    mainPanel(
      plotOutput("download_plot", height = "400px"),
      br(),
      plotOutput("total_downloads", height = "400px")
    )
  )
)

server <- function(input, output, session) {
  # Get list of all CRAN packages
  all_packages <- as.data.frame(available.packages(repos = "https://cloud.r-project.org"))
  
  # Initialize selectize input with all CRAN packages, default to ggplot2 and dplyr
  updateSelectizeInput(session, "package_name",
                       choices = sort(all_packages$Package),
                       selected = c("ggplot2", "dplyr"),
                       server = TRUE,
                       options = list(
                         maxItems = 10,
                         placeholder = 'Select packages'
                       ))
  
  # Reactive function to get download stats
  get_download_stats <- eventReactive(input$submit, {
    req(input$package_name)
    from_date <- input$from_date
    to_date <- input$to_date
    
    withProgress(message = 'Fetching download stats...', {
      # Get downloads for all selected packages
      all_downloads <- lapply(input$package_name, function(pkg) {
        incProgress(1/length(input$package_name))
        downloads <- try(cran_downloads(pkg, from = from_date, to = to_date))
        if(inherits(downloads, "try-error")) {
          return(NULL)
        }
        downloads$package <- pkg
        return(downloads)
      })
    })
    
    # Remove NULL entries and combine all downloads into one dataframe
    downloads <- bind_rows(all_downloads[!sapply(all_downloads, is.null)])
    
    if(nrow(downloads) == 0) {
      return(NULL)
    }
    
    downloads %>%
      mutate(
        weekly = floor_date(date, "week"),
        monthly = floor_date(date, "month")
      )
  })
  
  # Plot the downloads based on selected time unit
  output$download_plot <- renderPlot({
    req(get_download_stats())
    data <- get_download_stats()
    
    time_group <- switch(input$time_unit,
                         "daily" = "date",
                         "weekly" = "weekly",
                         "monthly" = "monthly")
    
    grouped_data <- data %>%
      group_by(package, !!sym(time_group)) %>%
      summarise(count = sum(count), .groups = "drop")
    
    ggplot(grouped_data, aes(x = !!sym(time_group), y = count, color = package)) +
      geom_line(linewidth = 1) +
      scale_color_brewer(palette = "Set1") +
      labs(
        x = NULL,
        y = "Downloads",
        title = paste(stringr::str_to_title(input$time_unit), "Downloads")
      ) +
      theme_dark(base_size = 15) +
      theme(
        text = element_text(color = "white"),
        plot.title = element_text(hjust = 0.5),
        plot.background = element_rect(fill = "black"),
        panel.background = element_rect(fill = "black"),
        panel.grid = element_line(color = "grey30"),
        legend.background = element_rect(fill = "black"),
        legend.text = element_text(color = "white"),
        legend.title = element_text(color = "white"),
        legend.key = element_rect(fill = "black"),
        legend.position = "bottom"
      )
  })
  
  # Plot total downloads since CRAN release
  output$total_downloads <- renderPlot({
    req(get_download_stats())
    data <- get_download_stats()
    
    cumulative_data <- data %>%
      group_by(package) %>%
      arrange(date) %>%
      mutate(cumulative = cumsum(count))
    
    ggplot(cumulative_data, aes(x = date, y = cumulative, color = package)) +
      geom_line(linewidth = 1) +
      scale_color_brewer(palette = "Set1") +
      labs(
        x = NULL,
        y = "Downloads",
        title = "Total Downloads Since CRAN Release"
      ) +
      theme_dark(base_size = 15) +
      theme(
        text = element_text(color = "white"),
        plot.title = element_text(hjust = 0.5),
        plot.background = element_rect(fill = "black"),
        panel.background = element_rect(fill = "black"),
        panel.grid = element_line(color = "grey30"),
        legend.background = element_rect(fill = "black"),
        legend.text = element_text(color = "white"),
        legend.title = element_text(color = "white"),
        legend.key = element_rect(fill = "black"),
        legend.position = "none"
      )
  })
  
  # Display the download statistics summary
  output$download_summary <- renderText({
    req(get_download_stats())
    data <- get_download_stats()
    
    summary_stats <- data %>%
      group_by(package) %>%
      summarise(
        total_downloads = format(sum(count), big.mark = ","),
        avg_downloads_per_day = format(round(mean(count), 0), big.mark = ","),
        avg_downloads_per_week = format(round(sum(count) / n_distinct(weekly), 0), big.mark = ","),
        avg_downloads_per_month = format(round(sum(count) / n_distinct(monthly), 0), big.mark = ","),
        .groups = "drop"
      )
    
    paste(
      sapply(1:nrow(summary_stats), function(i) {
        stats <- summary_stats[i, ]
        sprintf(
          "Package: %s\nTotal Downloads: %s\nAverage Downloads per Day: %s\nAverage Downloads per Week: %s\nAverage Downloads per Month: %s\n\n",
          stats$package,
          stats$total_downloads,
          stats$avg_downloads_per_day,
          stats$avg_downloads_per_week,
          stats$avg_downloads_per_month
        )
      }),
      collapse = ""
    )
  })
}

shinyApp(ui, server)