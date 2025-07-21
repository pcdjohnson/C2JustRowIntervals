library(shiny)
library(scales)


# function to format pace
format.Pace <- function(pace.sec) {
  paste(floor(round(pace.sec, 1) / 60), 
        formatC(round(round(pace.sec, 1) %% 60, 1), width = 4, flag = "0", 
                digits = 1, format = "f"), 
        sep = ":")
}


# user interface
ui <- fluidPage(
  titlePanel("Concept2 Intervals Analyser"),
  
  p("This app analyses interval workouts from Concept2 rowing machines that were rowed on Just Row."),
  p("Upload a CSV file exported from the Concept2 Logbook, then specify your warm-up, interval, and rest times/distances."),
  p("Click 'Analyse' to view a pace plot and interval summary."),
  p("The code is freely available on ",
    tags$a(href = "https://github.com/pcdjohnson/C2JustRowIntervals", "GitHub", target = "_blank"), "."),
  
  sidebarLayout(
   
    sidebarPanel(
      fileInput("file", "Upload Concept2 CSV", accept = ".csv"),
      
      # Switch between time and distance
      radioButtons("duration.unit", "Session Duration Type:",
                   choices = c("Time (minutes)" = "minutes", "Distance (metres)" = "metres"),
                   selected = "metres"),
      
      numericInput("warmup", "Warm-up duration (minutes/metres)", value = 0),
      numericInput("interval", "Interval duration (minutes/metres)", value = 500),
      numericInput("n_intervals", "Number of Intervals", value = 10),
      numericInput("rest", "Rest duration (minutes/metres)", value = 750),
      
      
      # # Conditional input for time
      # conditionalPanel(
      #   condition = "input.duration.unit == 'minutes'",
      #   numericInput("duration.time", "Duration (minutes):", value = 30)
      # ),
      # 
      # # Conditional input for distance
      # conditionalPanel(
      #   condition = "input.duration.unit == 'metres'",
      #   numericInput("duration.distance", "Duration (metres):", value = 5000)
      # ),
      
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
    # input <- list(warmup = 0, interval = 1300, n_intervals = 3, rest = 1300, duration.unit = "metres") # distance (m) example
    # data <- read.csv("data/concept2-result-104317619.csv")[-1, ]
    # input <- list(warmup = 3, interval = 1, n_intervals = 12, rest = 3, duration.unit = "minutes") # time (min) example

    data <- read.csv(input$file$datapath)[-1, ]
    data$Time.min <- as.numeric(data$Time..seconds.) / 60
    data$Distance <- as.numeric(data$Distance..meters.)
    data$Pace <- as.numeric(data$Pace..seconds.)
    data$Stroke.Rate <- as.numeric(data$Stroke.Rate)
    data$Heart.Rate <- as.numeric(data$Heart.Rate)
    
    # Remove unnecessary columns
    data <- data[, c("Time.min", "Distance", "Pace", "Stroke.Rate", "Heart.Rate")]
    
    # choose duration units 
    if(input$duration.unit == "metres") {
      data$duration <- data$Distance
      duration.label <- "Distance (m)"
    }
    if(input$duration.unit == "minutes") {
      data$duration <- data$Time.min
      duration.label <- "Time (min)"
    }
    
    # Trim warm-up
    data <- data[data$duration >= input$warmup, ]
    data$Time.min <- data$Time.min - min(data$Time.min)
    data$Distance <- data$Distance - min(data$Distance)
    data$duration <- data$duration - min(data$duration)
    
    # Total workout duration
    on <- rep(input$interval, input$n_intervals)
    total <- sum(on) + input$rest * (length(on) - 1)
    
    # Trim cool-down
    data <- data[data$duration <= total, ]
    
    # Impute missing stroke rate values
    data$Stroke.Rate[1] <- data$Stroke.Rate[2]
    data$Stroke.Rate[is.na(data$Stroke.Rate)] <- data$Stroke.Rate[which(is.na(data$Stroke.Rate))+1]
    
    # Interval summary
    sumtab <- data.frame(t(matrix(c(0, cumsum(c(rbind(on, input$rest))[-length(on)*2])), nrow = 2)))
    names(sumtab) <- c("start", "stop")
    sumtab$Mean.Pace <- apply(sumtab[, c("start", "stop")], 1, function(x) {
      mean(data$Pace[data$duration > x[1] & data$duration < x[2]])
    })
    sumtab$halfway <- (sumtab$start + sumtab$stop) / 2
    sumtab$mean.rate <- round(apply(sumtab[, c("start", "stop")], 1, function(x) {
      mean(data$Stroke.Rate[data$duration > x[1] & data$duration < x[2]])
    }))
    sumtab$mean.hr <- round(apply(sumtab[, c("start", "stop")], 1, function(x) {
      mean(data$Heart.Rate[data$duration > x[1] & data$duration < x[2]])
    }))
    sumtab$number <- 1:nrow(sumtab) 
    sumtab$duration <- input$interval
    
    sumtab.mean <- data.frame(rbind(apply(sumtab, 2, mean)))

    sumtab$number <- factor(sumtab$number, c("Mean", sumtab$number))
    sumtab.mean$number <- factor("Mean", c("Mean", sumtab$number))
    
    sumtab$Mean.Pace.formatted <- format.Pace(sumtab$Mean.Pace)
    sumtab.mean$Mean.Pace.formatted <- format.Pace(sumtab.mean$Mean.Pace)
    
    sumtab.formatted <- sumtab[, c("number", "duration", "Mean.Pace.formatted", "mean.rate", "mean.hr")]
    sumtab.mean.formatted <- sumtab.mean[, names(sumtab.formatted)]
    sumtab.final <- rbind(sumtab.mean.formatted, sumtab.formatted)
    sumtab.final$Interval
    names(sumtab.final) <-
      c("Interval", duration.label, "Pace", "Stroke Rate", "Heart rate")
    sumtab.final$Interval <- as.character(sumtab.final$Interval)

    output$summaryTable <- renderTable({
      sumtab.final
    }, digits = 0)
    
    output$pacePlot <- renderPlot({
      Pace.col <- alpha("darkblue", 0.7)
      hr.col <- alpha("red", 0.7)
      
      par(mar = c(5.1, 4.1, 4.1, 4.1))
      plot(Pace ~ duration, data = data, type = "l", col = Pace.col, lwd = 1.2,
           xlim = range(data$duration),
           ylim = c(160, min(data$Pace) - 5),
           ylab = "", xlab = duration.label, axes = FALSE)
      
      Pace.ticks <- seq(10, 200, 5)
      x.at <- pretty(c(0, total * 0.9))
      axis(1, pos = 162, at = x.at)
      axis(2, at = Pace.ticks, col.axis = Pace.col, pos = 0,
           labels = paste(floor(Pace.ticks / 60), formatC(Pace.ticks %% 60, width = 2, flag = "0"), sep = ":"), cex.axis = 0.8)
      mtext("Split/500m", side = 2, line = 2, col = Pace.col)
      
      abline(h = seq(60, 180, 1), col = alpha("black", 0.1))
      abline(h = seq(60, 180, 5), col = alpha("black", 0.05), lwd = 1.8)
      
      lines(I((173 - data$Heart.Rate / 2.5)) ~ duration, data = data, col = hr.col, lwd = 1.2)
      
      hr.labels <- seq(0, 220, 5)
      axis(4, at = 173 - hr.labels / 2.5, labels = hr.labels, cex.axis = 0.8,
           pos = min(max(data$duration), total), col.axis = hr.col)
      mtext("Heart rate (BPM)", side = 4, line = 2, col = hr.col)
      
      text(x = sumtab$halfway, y = sumtab$Mean.Pace * 0.92, labels = format.Pace(sumtab$Mean.Pace),
           col = Pace.col, adj = c(0.5, 1))
      text(x = sumtab$halfway, y = sumtab$Mean.Pace * 0.96, labels = paste(sumtab$mean.rate, "s/m"),
           col = Pace.col, adj = c(0.5, 0), cex = 0.6)
      
      arrows(x0 = sumtab$start, x1 = sumtab$stop, y0 = 162, y1 = 162,
             length = 0, col = "red", lwd = 2)
      arrows(x0 = sumtab$start[-1], x1 = sumtab$stop[-nrow(sumtab)], y0 = 162, y1 = 162,
             length = 0, col = "green", lwd = 2)
    })
  })
}

shinyApp(ui = ui, server = server)

# to publish the app to shinyapps.io:
# rsconnect::deployApp()
