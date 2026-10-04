library(quantmod)
library(TTR)

load_stock_data <- function(file = "portfolio.txt") {
  symbols <- readLines(file, warn = FALSE)
  stock_data <- list()
  
  for (symbol in symbols) {
    data <- getSymbols(symbol, src = "yahoo", auto.assign = FALSE)
    stock_data[[symbol]] <- as.data.frame(data)
  }
  
  return(stock_data)
}

stocks <- load_stock_data()

calculate_statistics <- function(stock_data) {
  results <- list()
  
  for (symbol in names(stock_data)) {
    data <- stock_data[[symbol]]
    close_prices <- data[, paste0(symbol, ".Close")]
    
    statistical_mode <- function(x) {
      values <- unique(x)
      values[which.max(tabulate(match(x, values)))]
    }
    
    results[[symbol]] <- list(
      Moving_Average = SMA(close_prices, n = 20),
      Mean = mean(close_prices, na.rm = TRUE),
      Mode = statistical_mode(close_prices),
      Median = median(close_prices, na.rm = TRUE),
      Standard_Deviation = sd(close_prices, na.rm = TRUE)
    )
  }
  
  return(results)
}
statistics <- calculate_statistics(stocks)
display_stock_data <- function(stock_data) {
  for (symbol in names(stock_data)) {
    cat("\nStock:", symbol, "\n")
    print(head(stock_data[[symbol]]))
  }
}
display_stock_data(stocks)
plot_stock <- function(stock_data, symbol) {
  close_prices <- stock_data[[symbol]][, paste0(symbol, ".Close")]
  moving_average <- SMA(close_prices, n = 20)
  
  plot(close_prices,
       type = "l",
       main = paste(symbol, "Closing Price and 20-Day Moving Average"),
       xlab = "Trading Days",
       ylab = "Closing Price")
  
  lines(moving_average, lwd = 2)
  
  legend("topleft",
         legend = c("Closing Price", "20-Day Moving Average"),
         lty = 1,
         lwd = c(1, 2))
}

plot_stock(stocks, "AAPL")
