###--------------------------------------------------
### Two Y Axes
###--------------------------------------------------

### https://twitter.com/LizAnnSonders/status/687980610544119808

library(ggplot2)
library(tidyr)
library(lubridate)
library(dplyr)

theme_set(theme_minimal())

data <- read.csv("data/fred-data.csv")
data$date <- ymd(data$date)

## Time series
data$SP500 <- ts(data$SP500, frequency = 52, start=c(2009, 03, 11))
data$BOGMBASEW <- ts(data$BOGMBASEW, frequency = 52, start=c(2009, 03, 11))

## First differences
SPd <- diff(data$SP500)
BOGd <- diff(data$BOGMBASEW)
data.diff <- data.frame(SPd, BOGd)

p <- ggplot(data.diff, aes(y=BOGd, x=SPd))
p + geom_point() + geom_smooth(method="lm", se=FALSE)
p + geom_path()

## Don't do this
mod <- lm(SPd ~ BOGd, data=data.diff)

## Long
data.m <- gather(data, series, value, SP500:BOGMBASEW)


p <- ggplot(data.m, aes(x=date, y=value, color=series))
p + geom_line() + theme(legend.position = "top")

p <- ggplot(data, aes(x=BOGMBASEW, y=SP500))
p + geom_point() + geom_smooth()

p + geom_path()
