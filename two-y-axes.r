###--------------------------------------------------
### Two Y Axes
###--------------------------------------------------

### https://twitter.com/LizAnnSonders/status/687980610544119808

library(ggplot2)
library(tidyr)
library(lubridate)
library(dplyr)
library(forecast)

theme_set(theme_minimal())

## Make a "figures" subdirectory if one doesn't exist
ifelse(!dir.exists(file.path("figures")),
       dir.create(file.path("figures")),
       FALSE)


data <- read.csv("data/fred-data.csv")
data$date <- ymd(data$date)

## Time series
data$SP500 <- ts(data$SP500, frequency = 52, start=c(2009, 03, 11))
data$BOGMBASEW <- ts(data$BOGMBASEW, frequency = 52, start=c(2009, 03, 11))


### Quick convenience function, as we're goingn to make this plot four
### times.
two.y <- function(x, y1, y2,
                  y1.lim = range(y1),
                  y2.lim = range(y2),
                  y2.lab = "Billions of Dollars",
                  ttxt = NULL,
                  ...) {

    ## y1.lim <- range(y1)
    ## y2.lim <- range(y2)
    y1.lo <- y1.lim[1]
    y1.hi <- y1.lim[2]
    y2.lo <- y2.lim[1]
    y2.hi <- y2.lim[2]

    par(mar=c(5,4,4,5)+.1)
    plot(x, y1,
         type="l",
         col="deepskyblue4",
         xlab="Date",
         ylab="Index",
         ylim=c(y1.lo-100, y1.hi+100))

    par(new=TRUE)

    plot(x, y2, type="l",
         col="firebrick",
         xaxt="n",
         yaxt="n",
         xlab="",
         ylab="",
         ylim=c(y2.lo, y2.hi))
    title(main = ttxt)

    axis(4)

    mtext(y2.lab, side=4, line=3)
    legend("topleft",
           col=c("deepskyblue4","firebrick"),
           bty="n", lty=1,
           legend=c("S&P 500", "Monetary Base"))


}

pdf(file="figures/two-y-by-four.pdf", height = 8, width = 10)

par(mar=c(0,0,0,0)+.1)
par(mfrow = c(2,2))
## 1. Base plot
two.y(x=data$date,
      y1=data$SP500,
      y2=data$BOGMBASEW/1000,
      ttxt = "Original")

## 2. Change an axis
two.y(x=data$date,
      y1=data$SP500,
      y2=data$BOGMBASEW/1000,
      y1.lim = c(696, 2126),
      y2.lim = c(0, 5000),
      ttxt = "Start y2 at zero")


## 3. Change y1 axis limits
two.y(x=data$date,
      y1=data$SP500,
      y2=data$BOGMBASEW/1000,
      y1.lim = c(0, 4000),
      ttxt = "Start y1 at zero, max both at max y2")


## 4. Put them both on the same axis
## (kind of a degenerate case)
two.y(x=data$date,
      y1=data$SP500,
      y2=data$BOGMBASEW,
      y1.lim = c(0, max(data$BOGMBASEW + 1000)),
      y2.lim = c(0, max(data$BOGMBASEW + 1000)),
      y2.lab = "Millions of Dollars",
      ttxt = "Both on the same scale")

dev.off()


### Split axis plots can be better, but in this case the series are
### different units, not just different magnitudes, so it's not a good
### idea---the issues with compressing the axes of each series arise
### again. But here's one way to do it anyway.


pdf(file="figures/split-plot.pdf", height=5, width=6)

## The layout matrix makes my head hurt.
## Read the layout() and matrix() docs carefully,
## or Paul Murrell's excellent book.
layout(matrix(c(1,1,2,2), 2, 2,
              byrow = TRUE),
       widths=c(3,1),
       heights=c(2,1))

par(mar=c(0.5,4,1,4)+.1)

plot(data$date, data$BOGMBASEW/1000,
     type="l",
     col="firebrick",
     xlab="",
     xaxt="n",
     ylab="$bn")

par(mar=c(2,4,0.2,4)+.1)

plot(data$date, data$SP500,
     type="l",
     col="deepskyblue4",
     xlab="Date",
     ylab="Index")

dev.off()

## Naive regression
out.lm <- lm(SP500 ~ BOGMBASEW, data=data)

## Autocorrelation
pdf(file="figures/SP500-autocorr.pdf", height = 5, width = 8)
par(mfrow=c(1,2))
out.res <- ts(resid(out.lm), start=c(2009, 03, 11), frequency = 52)
plot.ts(out.res, ylab="SP500 Residuals", main = "Residuals vs Time")
abline(0,0)
Acf(out.res, main = "Autocorrelation")
dev.off()

pdf(file="figures/sp500-money-scatter.pdf", height=6, width=6)
plot(data$BOGMBASEW/1000, data$SP500, xlab="Money Base ($bn)", ylab="S&P 500", main="Why Aintcha Rich?")
dev.off()


## First differences
SPd <- diff(data$SP500)
BOGd <- diff(data$BOGMBASEW)
data.diff <- data.frame(SPd, BOGd)

p <- ggplot(data.diff, aes(y=BOGd, x=SPd))
p + geom_point(pch=21)


##

library(car)
rho.hat <- durbinWatsonTest(out.lm)$r
out.aa <- auto.arima(x=data$SP500, xreg=data$BOGMBASEW)
out.arm <- Arima(x=data$SP500, xreg=data$BOGMBASEW, order=c(0, 0, 5))

## Long
data.m <- gather(data, series, value, SP500:BOGMBASEW)

p <- ggplot(data.m, aes(x=date, y=value, color=series))
p + geom_line() + theme(legend.position = "top")

p <- ggplot(data, aes(x=BOGMBASEW, y=SP500))
p + geom_point() + geom_smooth()

p + geom_path()
