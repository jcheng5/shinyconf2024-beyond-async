library(shiny)
library(crew)
library(promises)
library(bslib)
library(ggplot2)
library(httr)
library(jsonlite)
library(dplyr)
library(future)
plan(multisession)

fetch_stock_data <- function(symbol, delay_secs) {
  start_date <- Sys.Date() - 365
  end_date <- Sys.Date()

  Sys.sleep(delay_secs)

  # get stock data
  url <- paste0("https://query1.finance.yahoo.com/v8/finance/chart/", symbol, "?period1=",
    as.numeric(as.POSIXct(start_date)), "&period2=", as.numeric(as.POSIXct(end_date)),
    "&interval=1d")

  response <- GET(url)
  json_data <- fromJSON(content(response, as = "text"))
  if (!is.null(json_data$chart$error$description)) {
    stop(simpleError(json_data$chart$error$description))
  }
  prices <- json_data$chart$result$indicators$quote[[1]]$close[[1]]
  dates <- as.Date(as.POSIXct(json_data$chart$result$timestamp[[1]], origin = "1970-01-01"))

  data.frame(Date = dates, Close = prices, stringsAsFactors = FALSE)
}

cardUI <- function(id, ticker) {
  ns <- NS(id)
  card(
    card_header(ticker),
    plotOutput(ns("plot"))
  )
}

cardServer <- function(id, ticker, relative, delay_secs) {
  moduleServer(
    id,
    function(input, output, session) {
      df_task <- ExtendedTask$new(function(ticker, delay_secs) {
        future({
          fetch_stock_data(ticker, delay_secs)
        })
      })
      df_task$invoke(ticker, isolate(delay_secs()))

      df_scaled <- reactive({
        if (relative()) {
          df_task$result() |>
            mutate(Close = Close / Close[[1]])
        } else {
          df_task$result()
        }
      })

      output$plot <- renderPlot({
        ggplot(df_scaled(), aes(x = Date, y = Close)) +
          geom_line(color = "steelblue") +
          labs(
            x = "Date",
            y = paste("Closing Price", if (relative()) "(Relative)")
          ) +
          theme_minimal()
      }, res = 130)
    }
  )
}

controller <- crew::crew_controller_local(workers = 4, seconds_idle = 10)

ui <- page_sidebar(fillable = FALSE,
  sidebar = sidebar(
    textInput("ticker", "Stock ticker", value = "VTI"),
    actionButton("add", "Add", class="btn-primary"),
    hr(),
    radioButtons("scale", "Display scale", c(
      "Absolute dollar" = "absolute",
      "Relative to first price" = "relative"
    )),
    hr(),
    numericInput("delay_secs", "Artificial delay, in seconds", 2),
  ),
  layout_columns(id = "results", col_widths = 2, fillable = FALSE)
)

server <- function(input, output, session) {
  observeEvent(input$add, {
    req(input$ticker)
    ticker <- toupper(input$ticker)

    id <- paste0("panel_", sample.int(99999999, 1))

    insertUI("#results", where = "beforeEnd", immediate = TRUE, cardUI(
      id, ticker
    ))
    updateTextInput(session, "ticker", value = "")

    cardServer(id, ticker, reactive(input$scale == "relative"), reactive(input$delay_secs))
  })
}

shinyApp(ui, server)
