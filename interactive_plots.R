# ============================================================
# SELF-CONTAINED INTERACTIVE STOCK ANALYSIS & FORECASTING
# Downloads data and renders interactive charts directly in RStudio Viewer
# ============================================================

# 1. Install and Load Required Packages
required_packages <- c("quantmod", "TTR", "dplyr", "xts", "zoo", 
                       "lubridate", "dygraphs", "plotly", "forecast", "tseries")
cat("⏳ Checking package requirements...\n")
for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE)) {
    cat(paste("  Installing missing package:", pkg, "\n"))
    install.packages(pkg, repos = "https://cloud.r-project.org")
    library(pkg, character.only = TRUE)
  }
}
cat("✅ All required packages loaded.\n\n")

# 2. Download Data Dynamically
ticker <- "TRENT.NS" # You can change this to any Yahoo Finance ticker
start_date <- "2016-01-01" # Fetching last 10 years of data for analysis

cat(paste("⏳ Downloading data for", ticker, "from", start_date, "...\n"))
trent_xts <- getSymbols(ticker, src = "yahoo", from = start_date, auto.assign = FALSE)
trent_df <- data.frame(Date = index(trent_xts), coredata(trent_xts))
colnames(trent_df) <- c("Date", "Open", "High", "Low", "Close", "Volume", "Adjusted")
cat("✅ Data downloaded. Row count:", nrow(trent_df), "\n\n")

# 3. Calculate Technical Indicators
cat("⏳ Calculating indicators (MAs, Bollinger Bands, RSI, MACD)...\n")
trent_df <- trent_df %>%
  arrange(Date) %>%
  mutate(
    SMA_20  = as.numeric(SMA(Close, n = 20)),
    SMA_50  = as.numeric(SMA(Close, n = 50)),
    SMA_200 = as.numeric(SMA(Close, n = 200)),
    Daily_Return = (Close / lag(Close) - 1) * 100
  )

# Bollinger Bands
bb <- BBands(trent_df$Close, n = 20, sd = 2)
trent_df$BB_Lower <- as.numeric(bb[, "dn"])
trent_df$BB_Middle <- as.numeric(bb[, "mavg"])
trent_df$BB_Upper <- as.numeric(bb[, "up"])

# RSI
trent_df$RSI_14 <- as.numeric(RSI(trent_df$Close, n = 14))

# MACD
macd_calc <- MACD(trent_df$Close, nFast = 12, nSlow = 26, nSig = 9, maType = "EMA")
trent_df$MACD <- as.numeric(macd_calc[, "macd"])
trent_df$MACD_Signal <- as.numeric(macd_calc[, "signal"])
trent_df$MACD_Histogram <- trent_df$MACD - trent_df$MACD_Signal

# Save RDS copy for safety
saveRDS(trent_df, "trent_data.rds")
cat("✅ Technical calculations complete.\n\n")


# 4. RENDER INTERACTIVE PLOT 1: Dygraphs Candlestick + MAs + Bollinger Bands
cat("📊 Rendering Plot 1: Interactive Candlestick, Moving Averages & Volume...\n")

# Create xts object for pricing panel
dy_data <- trent_df %>% select(Date, Open, High, Low, Close, SMA_20, SMA_50, BB_Lower, BB_Upper)
dy_xts <- xts(dy_data %>% select(-Date), order.by = dy_data$Date)

# Build dygraphs layout
dy_price <- dygraph(dy_xts, group = "trent_dashboard", main = paste(ticker, "Interactive Stock Price Chart")) %>%
  dyCandlestick() %>%
  dyOptions(drawGrid = TRUE, gridLineColor = "#E0E0E0") %>%
  dyLegend(show = "always", hideOnMouseOut = FALSE) %>%
  # Configure indicator series
  dySeries("SMA_20", color = "#FF9800", strokeWidth = 1.5) %>%
  dySeries("SMA_50", color = "#E91E63", strokeWidth = 1.5) %>%
  dySeries("BB_Lower", color = "#2196F3", strokeWidth = 0.8, strokePattern = "dashed") %>%
  dySeries("BB_Upper", color = "#2196F3", strokeWidth = 0.8, strokePattern = "dashed")

# Render price chart (visible in Viewer)
print(dy_price)


# 5. RENDER INTERACTIVE PLOT 2: Plotly RSI (Relative Strength Index)
cat("📊 Rendering Plot 2: Interactive RSI (Relative Strength Index)...\n")

# Limit to last 500 trading days for detail visibility in Plotly
recent_df <- tail(trent_df, 500)

p_rsi <- plot_ly(recent_df, x = ~Date) %>%
  add_lines(y = ~RSI_14, name = "RSI (14)", line = list(color = "#7C4DFF", width = 1.5)) %>%
  # Overbought line
  add_segments(x = min(recent_df$Date), xend = max(recent_df$Date), y = 70, yend = 70,
               name = "Overbought (70)", line = list(color = "#FF1744", dash = "dash")) %>%
  # Oversold line
  add_segments(x = min(recent_df$Date), xend = max(recent_df$Date), y = 30, yend = 30,
               name = "Oversold (30)", line = list(color = "#00C853", dash = "dash")) %>%
  layout(
    title = paste(ticker, "- Relative Strength Index (RSI 14)"),
    xaxis = list(title = "Date"),
    yaxis = list(title = "RSI Value", range = c(0, 100)),
    plot_bgcolor = "#FAFAFA",
    paper_bgcolor = "#FFFFFF"
  )

# Render RSI plot (visible in Viewer)
print(p_rsi)


# 6. RENDER INTERACTIVE PLOT 3: Plotly MACD Chart
cat("📊 Rendering Plot 3: Interactive MACD (Moving Average Convergence Divergence)...\n")

p_macd <- plot_ly(recent_df, x = ~Date) %>%
  add_bars(y = ~MACD_Histogram, name = "Histogram", 
           marker = list(color = ifelse(recent_df$MACD_Histogram >= 0, "#00C853", "#FF1744"))) %>%
  add_lines(y = ~MACD, name = "MACD Line", line = list(color = "#1E88E5", width = 1.2)) %>%
  add_lines(y = ~MACD_Signal, name = "Signal Line", line = list(color = "#FF6F00", width = 1.2)) %>%
  layout(
    title = paste(ticker, "- MACD Analysis"),
    xaxis = list(title = "Date"),
    yaxis = list(title = "MACD Values"),
    plot_bgcolor = "#FAFAFA",
    paper_bgcolor = "#FFFFFF"
  )

# Render MACD plot (visible in Viewer)
print(p_macd)


# 7. RENDER INTERACTIVE PLOT 4: Plotly ARIMA Model 60-Day Forecast
cat("⏳ Fitting ARIMA model and calculating 60-day forecast...\n")
arima_model <- auto.arima(trent_df$Close, seasonal = FALSE)
fc_horizon <- 60
forecast_obj <- forecast(arima_model, h = fc_horizon)

# Generate future business dates
get_biz_dates <- function(start, n) {
  dates <- seq(start + 1, start + n * 2.5, by = "day")
  biz_dates <- dates[!weekdays(dates) %in% c("Saturday", "Sunday")]
  return(head(biz_dates, n))
}
future_dates <- get_biz_dates(max(trent_df$Date), fc_horizon)

# Prep Forecast dataframe
fc_df <- data.frame(
  Date    = future_dates,
  Close   = as.numeric(forecast_obj$mean),
  Lower80 = as.numeric(forecast_obj$lower[, "80%"]),
  Upper80 = as.numeric(forecast_obj$upper[, "80%"]),
  Lower95 = as.numeric(forecast_obj$lower[, "95%"]),
  Upper95 = as.numeric(forecast_obj$upper[, "95%"])
)

# Grab last 200 days of history for comparison
hist_df <- trent_df %>% tail(200) %>% select(Date, Close)

cat("📊 Rendering Plot 4: Interactive ARIMA Forecast...\n")
p_forecast <- plot_ly() %>%
  # Historical Data
  add_lines(data = hist_df, x = ~Date, y = ~Close, name = "Historical Close", line = list(color = "#1E88E5")) %>%
  # 95% Confidence Shading
  add_ribbons(data = fc_df, x = ~Date, ymin = ~Lower95, ymax = ~Upper95, name = "95% Confidence",
              fillcolor = "rgba(197, 202, 233, 0.4)", line = list(color = "transparent")) %>%
  # 80% Confidence Shading
  add_ribbons(data = fc_df, x = ~Date, ymin = ~Lower80, ymax = ~Upper80, name = "80% Confidence",
              fillcolor = "rgba(121, 134, 203, 0.5)", line = list(color = "transparent")) %>%
  # Forecast Mean
  add_lines(data = fc_df, x = ~Date, y = ~Close, name = "ARIMA Forecast", line = list(color = "#FF6F00", width = 2)) %>%
  layout(
    title = paste(ticker, "- 60-Day ARIMA Future Price Forecast"),
    xaxis = list(title = "Date"),
    yaxis = list(title = "Stock Price (₹)"),
    plot_bgcolor = "#FAFAFA",
    paper_bgcolor = "#FFFFFF"
  )

# Render Forecast plot (visible in Viewer)
print(p_forecast)

cat("\n🎉 Execution completed! All 4 interactive charts have been rendered.\n")
cat("💡 TIP: In RStudio, use the back/forward arrow buttons (◀ / ▶) at the top of the 'Viewer' pane to cycle through the interactive plots!\n")
