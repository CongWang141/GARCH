library(quantmod)

# import data for Google stock price
googl <- getSymbols("GOOGL", from="2010-01-01", to="2022-08-03", auto.assign = F)
head(googl[, 1:5], 5)

# calculate daily returns
daily_ret <- (googl$GOOGL.Close-stats::lag(googl$GOOGL.Close))/stats::lag(googl$GOOGL.Close)

daily_ret <- data.frame(index(daily_ret), daily_ret)
colnames(daily_ret) <- c("date", "return")
rownames(daily_ret) <- 1:nrow(daily_ret)

# plot the return
library(ggplot2)
p1 <- ggplot(daily_ret, aes(x=date, y=return))
p1 + geom_line(colour="steelblue")

# plot acf
acf1 <- acf(daily_ret[, 2],
    type = "correlation",
    plot = TRUE,
    na.action = na.pass)

acf2 <- acf(daily_ret[, 2],
    type = "partial",
    plot = TRUE,
    na.action = na.pass)

# calculate squared series 
daily_ret$sqr_return <- (daily_ret$return)^2

# plot squared series
p2 <- ggplot(daily_ret, aes(x=date, y=sqr_return))
p2 + geom_line(colour="steelblue")

# plot acf for squared series
acf(daily_ret[, 3],
    type = "correlation",
    plot = TRUE,
    na.action = na.pass)
acf(daily_ret[, 3],
    type = "partial",
    plot = TRUE,
    na.action = na.pass)

# histogram
p2 <- ggplot(daily_ret) 

p2 + geom_histogram(aes(x=return, y=..density..), binwidth = 0.005, color="steelblue", fill="grey", size=1) +
  stat_function(fun = dnorm, args = list(mean = mean(daily_ret$return, na.rm = T), sd = sd(daily_ret$return, na.rm = T)), size=1)

# get time serise data
library(PerformanceAnalytics)
library(xts)
daily_ret_xts <- xts(daily_ret[,-1], order.by=daily_ret[,1])

# calculate volatility (month)
realizedvol <- rollapply(daily_ret_xts, width = 20, FUN=sd.annualized)

# convert data to data.frame
vol <- data.frame(index(realizedvol), realizedvol)
colnames(vol) <- c("date", "volatility")

# plot volatility
p3 <- ggplot(vol, aes(x=date, y=volatility))
p3 +
  geom_line( color="steelblue")

# plot the autocorrelation 
acf(vol[, 2],
    type = "correlation",
    plot = TRUE,
    na.action = na.pass)

# fit a garch model
library(rugarch)
garch_spec <- ugarchspec(variance.model=list(model="sGARCH", garchOrder=c(1,1)), mean.model=list(armaOrder=c(0,0)))
fit_garch <- ugarchfit(spec = garch_spec, data = vol[-c(1:19),2])
fit_garch







