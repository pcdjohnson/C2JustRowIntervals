rm(list = ls())
library(scales)

### Inputs

# load data
dat <- read.csv("concept2-result-104136595.csv")[-1, ]

# define intervals (distance in m)
warmup.distance <- 0
interval.distance <- 1300
interval.number <- 3
rest.distance <- 1300

### End of inputs

# Function for formatting pace in seconds per 500m as min:seconds
format.Pace <- 
  function(pace.sec) {
    paste(floor(pace.sec/60), formatC(round(pace.sec %% 60, 1), width = 2, flag = "0"), sep = ":")
  }

# Clean up data frame
dat$timer.min <- dat$Time..seconds./60
dat$Distance <- dat$Distance..meters.
dat$Pace <- dat$Pace..seconds.
dat$Time..seconds. <- dat$Pace..seconds. <- dat$Distance..meters. <- NULL

# interval distance vector
on <- rep(interval.distance, interval.number)
(total <- sum(on) + rest.distance * (length(on) - 1))

# trim warm-up off the start
dat <- dat[dat$Distance >= warmup.distance, ]
dat$timer.min <- dat$timer.min - min(dat$timer.min)
dat$Distance <- dat$Distance - min(dat$Distance)

# trim cool-down off the end
dat <- dat[dat$Distance <= total, ]
dat$Stroke.Rate[1] <- dat$Stroke.Rate[2]

# make a data frame storing the summary data on each interval
on.off <- data.frame(t(matrix(c(0, cumsum(c(rbind(on, rest.distance))[-length(on)*2])), nrow = 2)))
names(on.off) <- c("start", "stop")
on.off$Mean.Pace <-
  apply(on.off[, c("start", "stop")], 1, function(x) mean(dat$Pace[dat$Distance > x[1] & dat$Distance < x[2]]))
on.off$Mean.Pace.formatted <- format.Pace(on.off$Mean.Pace)
on.off$halfway <- (on.off$start + on.off$stop)/2
on.off$mean.rate <-
  round(apply(on.off[, c("start", "stop")], 1, function(x) mean(dat$Stroke.Rate[dat$Distance > x[1] & dat$Distance < x[2]])))


# plot the split and heart rate with summary data
Pace.col <- alpha("darkblue", 0.7)
hr.col <- alpha("red", 0.7)
par(mar = c(5.1, 4.1, 4.1, 4.1))
plot(Pace ~ Distance, data = dat, type = "l", col = Pace.col, lwd = 1.2,
     xlim = range(dat$Distance),
     ylim = c(160, min(dat$Pace) - 5),
     #ylim = c(min(dat$Heart.Rate), max(dat$power.W)),
     ylab = "",
     xlab = "min", axes = FALSE)

Pace.ticks <- seq(10, 200, 5)
names(Pace.ticks)
x.at <- pretty(c(0, total * 0.9))
axis(1, 
     pos = 162, 
     at = x.at)
axis(2, at = Pace.ticks, #col = Pace.col, 
     col.axis = Pace.col, 
     pos = 0,
     labels = paste(floor(Pace.ticks/60), formatC(Pace.ticks %% 60, width = 2, flag = "0"), 
                    sep = ":"), cex.axis = 0.8)
mtext("Split/500m", side = 2, line = 2, col = Pace.col)

abline(h = seq(60, 180, 1), col = alpha("black", 0.1))
abline(h = seq(60, 180, 5), col = alpha("black", 0.05), lwd = 1.8)

lines(I((173 - Heart.Rate/2.5)) ~ Distance, data = dat, col = hr.col, lwd = 1.2)

hr.labels <- seq(0, 220, 5)
axis(4, at = 173 - hr.labels/2.5, labels = hr.labels, cex.axis = 0.8, 
     pos = min(max(dat$Distance), total),
     #col = hr.col, 
     col.axis = hr.col)
mtext("Heart rate (BPM)", side = 4, line = 2, col = hr.col)

text(x = on.off$halfway, y = on.off$Mean.Pace * 0.92, labels = format.Pace(on.off$Mean.Pace),
     col = Pace.col, adj = c(0.5, 1))
text(x = on.off$halfway, y = on.off$Mean.Pace * 0.96, labels = paste(on.off$mean.rate, "s/m"),
     col = Pace.col, adj = c(0.5, 0), cex = 0.6)

arrows(x0 = on.off$start, x1 = on.off$stop, y0 = 162, y1 = 162,
       length = 0, col = "red", lwd = 2)
arrows(x0 = on.off$start[-1], x1 = on.off$stop[-nrow(on.off)], y0 = 162, y1 = 162,
       length = 0, col = "green", lwd = 2)

