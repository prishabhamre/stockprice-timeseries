rm = Rocket_Motors.1

x = seq(0,999)
price = rm$Price_in_USD
plot(x, price[1:1000], type = "o", pch = 0.5, cex = 0.2, xlab = "Days(since 01JAN2020)",
     ylab = "Stock Price (USD)", main = "time-series Plot of Rocket Motors closing stock price")
abline(v = c(999*(1/3), 999*(2/3), 999), col = c(3, 3, 3), lwd = c(2, 2, 2))
acf(price,type="correlation")
acf(price,type="partial")
adf.test(price)

diff = (price[2:length(price)])-(price[1:(length(price)-1)])
plot(diff, type = "o", pch = 0.5, cex = 0.2, xlab = "Days(since 01JAN2020)",
     ylab = "Change in Stock Price (USD)", main = "Time-Series Plot of Change in Rocket Motors Closing Stock Price")
abline(v = c(999*(1/3), 999*(2/3), 999), col = c(3, 3, 3), lwd = c(2, 2, 2))
acf(diff,type="correlation")
acf(diff,type="partial")
adf.test(diff)

diff2 = (diff[2:length(diff)])-(diff[1:(length(diff)-1)])
plot(diff2, type = "o", pch = 0.5, cex = 0.2, xlab = "Days(since 01JAN2020)",
     ylab = "Change in Stock Price (USD)", main = "Time-Series Plot of Change in Rocket Motors Closing Stock Price")
abline(v = c(999*(1/3), 999*(2/3), 999), col = c(3, 3, 3), lwd = c(2, 2, 2))
acf(diff2,type="correlation")
acf(diff2,type="partial")
adf.test(diff2)

AR1 = arima(x = diff2, order = c(1, 0, 0), include.mean = FALSE, method ="ML")
AR1
AR2 = arima(x = diff2, order = c(2, 0, 0), include.mean = FALSE, method ="ML")
AR2
AR3 = arima(x = diff2, order = c(3, 0, 0), include.mean = FALSE, method ="ML")
AR3

MA1 = arima(x = diff2, order = c(0, 0, 1), include.mean = FALSE, method ="ML")
MA1
MA2 = arima(x = diff2, order = c(0, 0, 2), include.mean = FALSE, method ="ML")
MA2
MA3 = arima(x = diff2, order = c(0, 0, 3), include.mean = FALSE, method ="ML")
MA3

ARIMA = arima(x = diff2, order = c(2, 2, 3), include.mean = FALSE, method ="ML")
ARIMA


library(forecast)
auto = auto.arima(price)
auto

checkresiduals(auto)
autoplot(auto)
autoplot(forecast(auto))
autoplot(forecast(ARIMA))
