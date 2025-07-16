library(shiny)
library(scales)
library(ggplot2)
library(dplyr)

# Helper function to format pace
format.Pace <- function(pace.sec) {
  paste(floor(pace.sec / 60), formatC(round(pace.sec %% 60, 1), width = 2, flag = "0"), sep = ":")
}

ui <- fluidPage(
  titlePanel("Concept2 Intervals Analyser"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload Concept2 CSV", accept = ".csv"),
      numericInput("warmup", "Warm-up Distance (m)", value = 0),
      numericInput("interval", "Interval Distance (m)", value = 500),
      numericInput("n_intervals", "Number of Intervals", value = 10),
      numericInput("rest", "Rest Distance (m)", value = 750),
      actionButton("analyse", "Analyse")
    ),
    mainPanel(
      plotOutput("pacePlot"),
      tableOutput("summaryTable")
    )
  )
)

server <- function(input, output) {
  observeEvent(input$analyse, {
    req(input$file)
    
    # test data:
    # data <- read.csv("data/concept2-result-104136592.csv")[-1, ]
    # input <- list(warmup = 0, interval = 1300, n_intervals = 3, rest = 1300)
    
    data <- read.csv(input$file$datapath)[-1, ]
    data$timer.min <- as.numeric(data$Time..seconds.) / 60
    data$Distance <- as.numeric(data$Distance..meters.)
    data$Pace <- as.numeric(data$Pace..seconds.)
    data$Stroke.Rate <- as.numeric(data$Stroke.Rate)
    data$Heart.Rate <- as.numeric(data$Heart.Rate)
    
    # Remove unnecessary columns
    data <- data %>% select(timer.min, Distance, Pace, Stroke.Rate, Heart.Rate)
    
    # Trim warm-up
    data <- data[data$Distance >= input$warmup, ]
    data$timer.min <- data$timer.min - min(data$timer.min)
    data$Distance <- data$Distance - min(data$Distance)
    
    # Total workout distance
    on <- rep(input$interval, input$n_intervals)
    total <- sum(on) + input$rest * (length(on) - 1)
    
    # Trim cool-down
    data <- data[data$Distance <= total, ]
    data$Stroke.Rate[1] <- data$Stroke.Rate[2]
    
    # Interval summary
    on.off <- data.frame(t(matrix(c(0, cumsum(c(rbind(on, input$rest))[-length(on)*2])), nrow = 2)))
    names(on.off) <- c("start", "stop")
    on.off$Mean.Pace <- apply(on.off[, c("start", "stop")], 1, function(x) {
      mean(data$Pace[data$Distance > x[1] & data$Distance < x[2]])
    })
    on.off$Mean.Pace.formatted <- format.Pace(on.off$Mean.Pace)
    on.off$halfway <- (on.off$start + on.off$stop) / 2
    on.off$mean.rate <- round(apply(on.off[, c("start", "stop")], 1, function(x) {
      mean(data$Stroke.Rate[data$Distance > x[1] & data$Distance < x[2]])
    }))
    on.off$mean.hr <- round(apply(on.off[, c("start", "stop")], 1, function(x) {
      mean(data$Heart.Rate[data$Distance > x[1] & data$Distance < x[2]])
    }))
    on.off$number <- 1:nrow(on.off)
    on.off$Distance <- input$interval
    
    output$summaryTable <- renderTable({
      on.off %>% 
        select(number, Distance, Mean.Pace.formatted, mean.rate, mean.hr) %>%
        rename(
          `Interval` = number,
          `Distance (m)` = Distance,
          `Pace` = Mean.Pace.formatted,
          `Stroke Rate` = mean.rate,
          `Heart rate` = mean.hr
        )
    }, digits = 0)
    
    output$pacePlot <- renderPlot({
      Pace.col <- alpha("darkblue", 0.7)
      hr.col <- alpha("red", 0.7)
      
      par(mar = c(5.1, 4.1, 4.1, 4.1))
      plot(Pace ~ Distance, data = data, type = "l", col = Pace.col, lwd = 1.2,
           xlim = range(data$Distance),
           ylim = c(160, min(data$Pace) - 5),
           ylab = "", xlab = "Distance (m)", axes = FALSE)
      
      Pace.ticks <- seq(10, 200, 5)
      x.at <- pretty(c(0, total * 0.9))
      axis(1, pos = 162, at = x.at)
      axis(2, at = Pace.ticks, col.axis = Pace.col, pos = 0,
           labels = paste(floor(Pace.ticks / 60), formatC(Pace.ticks %% 60, width = 2, flag = "0"), sep = ":"), cex.axis = 0.8)
      mtext("Split/500m", side = 2, line = 2, col = Pace.col)
      
      abline(h = seq(60, 180, 1), col = alpha("black", 0.1))
      abline(h = seq(60, 180, 5), col = alpha("black", 0.05), lwd = 1.8)
      
      lines(I((173 - data$Heart.Rate / 2.5)) ~ Distance, data = data, col = hr.col, lwd = 1.2)
      
      hr.labels <- seq(0, 220, 5)
      axis(4, at = 173 - hr.labels / 2.5, labels = hr.labels, cex.axis = 0.8,
           pos = min(max(data$Distance), total), col.axis = hr.col)
      mtext("Heart rate (BPM)", side = 4, line = 2, col = hr.col)
      
      text(x = on.off$halfway, y = on.off$Mean.Pace * 0.92, labels = format.Pace(on.off$Mean.Pace),
           col = Pace.col, adj = c(0.5, 1))
      text(x = on.off$halfway, y = on.off$Mean.Pace * 0.96, labels = paste(on.off$mean.rate, "s/m"),
           col = Pace.col, adj = c(0.5, 0), cex = 0.6)
      
      arrows(x0 = on.off$start, x1 = on.off$stop, y0 = 162, y1 = 162,
             length = 0, col = "red", lwd = 2)
      arrows(x0 = on.off$start[-1], x1 = on.off$stop[-nrow(on.off)], y0 = 162, y1 = 162,
             length = 0, col = "green", lwd = 2)
    })
  })
}

shinyApp(ui = ui, server = server)

# to publish the app to shinyapps.io:
# rsconnect::deployApp()
