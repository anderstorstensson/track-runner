library(shiny)

ui <- fluidPage(
  titlePanel("Track Pace Converter"),
  sidebarLayout(
    sidebarPanel(
      radioButtons("mode", "Convert:",
                   choices = c("Lap time → Pace" = "lap2pace",
                               "Pace → Lap time" = "pace2lap")),
      radioButtons("unit", "Units:",
                   choices = c("Metric (minutes/km)" = "metric",
                               "Imperial (minutes/mile)" = "imperial"),
                   selected = "metric"),
      numericInput("lap_distance", "Lap distance (m):", value = 400, min = 50, step = 50),
      conditionalPanel(
        condition = "input.mode == 'lap2pace'",
        numericInput("lap_minutes", "Lap minutes:", value = 1, min = 0),
        numericInput("lap_seconds", "Lap seconds:", value = 30, min = 0, max = 59)
      ),
      conditionalPanel(
        condition = "input.mode == 'pace2lap'",
        numericInput("pace_minutes", "Pace minutes per unit:", value = 3, min = 0),
        numericInput("pace_seconds", "Pace seconds per unit:", value = 30, min = 0, max = 59)
      )
    ),
    mainPanel(
      h3("Result"),
      verbatimTextOutput("result")
    )
  )
)

server <- function(input, output, session) {

  output$result <- renderText({

    if (input$mode == "lap2pace") {
      lap_time <- input$lap_minutes * 60 + input$lap_seconds

      if (input$unit == "metric") {
        pace_sec_per_m <- lap_time / input$lap_distance
        pace_per_km <- pace_sec_per_m * 1000
        pace_min <- floor(pace_per_km / 60)
        pace_sec <- round(pace_per_km %% 60)
        paste0("Pace: ", pace_min, ":", sprintf("%02d", pace_sec), " per km")

      } else {
        # Imperial: minutes per mile, 1 mile = 1609.34 m
        pace_sec_per_m <- lap_time / input$lap_distance
        pace_per_mile <- pace_sec_per_m * 1609.34
        pace_min <- floor(pace_per_mile / 60)
        pace_sec <- round(pace_per_mile %% 60)
        paste0("Pace: ", pace_min, ":", sprintf("%02d", pace_sec), " per mile")
      }

    } else if (input$mode == "pace2lap") {
      pace_time <- input$pace_minutes * 60 + input$pace_seconds

      if (input$unit == "metric") {
        lap_time <- pace_time * input$lap_distance / 1000
        lap_min <- floor(lap_time / 60)
        lap_sec <- round(lap_time %% 60)
        paste0("Lap split: ", lap_min, ":", sprintf("%02d", lap_sec),
               " for ", input$lap_distance, " m")

      } else {
        lap_time <- pace_time * input$lap_distance / 1609.34
        lap_min <- floor(lap_time / 60)
        lap_sec <- round(lap_time %% 60)
        paste0("Lap split: ", lap_min, ":", sprintf("%02d", lap_sec),
               " for ", input$lap_distance, " m")
      }
    }
  })
}

shinyApp(ui, server)
