# ============================================================
# Trent STOCK ANALYSIS - PART 1
# Setup and Data Download with Technical Indicators
# ============================================================

# Install required packages if not already installed
packages <- c("quantmod", "TTR", "dplyr", "xts", "zoo", "lubridate")
for (pkg in packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, repos = "https://cloud.r-project.org")
    library(pkg, character.only = TRUE)
  }
}

cat("⏳ Fetching Trent Ltd. (TRENT.NS) stock data from Yahoo Finance...\n")

# Download historical stock data for Trent Ltd (NSE: TRENT) from 2006 to today
ticker <- "TRENT.NS"
start_date <- "2006-01-01"

tryCatch({
  # Fetch data
  trent_xts <- getSymbols(ticker, src = "yahoo", from = start_date, auto.assign = FALSE)
  
  # Convert to dataframe and rename columns
  trent_df <- data.frame(Date = index(trent_xts), coredata(trent_xts))
  colnames(trent_df) <- c("Date", "Open", "High", "Low", "Close", "Volume", "Adjusted")
  
  cat("✅ Data downloaded successfully. Total rows:", nrow(trent_df), "\n")
  
  # Calculate Technical Indicators
  cat("⏳ Calculating technical indicators...\n")
  
  # Moving Averages
  trent_df <- trent_df %>%
    mutate(
      SMA_20 = as.numeric(SMA(Close, n = 20)),
      SMA_50 = as.numeric(SMA(Close, n = 50)),
      SMA_200 = as.numeric(SMA(Close, n = 200))
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
  
  # Returns and Volatility
  trent_df <- trent_df %>%
    mutate(
      Daily_Return = (Close / lag(Close) - 1) * 100,
      Volatility_20 = as.numeric(runSD(Daily_Return, n = 20)),
      Cum_Return = Close / first(Close) - 1
    )
  
  # Date parts
  trent_df <- trent_df %>%
    mutate(
      Year = year(Date),
      MonthNum = month(Date),
      Month = factor(month.abb[MonthNum], levels = month.abb)
    )
  
  # Save to RDS for parts 2, 3 and the shiny app
  saveRDS(trent_df, "trent_data.rds")
  cat("✅ All indicators calculated and saved to 'trent_data.rds'\n")
  
}, error = function(e) {
  cat("❌ Error in fetching or processing data:", e$message, "\n")
})
