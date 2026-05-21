# ============================================================
# Trent STOCK ANALYSIS - PART 2
# Visualizations: Line Charts, Bar Plots, Financial Charts
# Using ggplot2
# ============================================================
# NOTE: Loads computed data from 'trent_data.rds' (runs setup first if needed)
# ============================================================

# Install required packages if not already installed
packages <- c("ggplot2", "dplyr", "tidyr", "scales")
for (pkg in packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, repos = "https://cloud.r-project.org")
    library(pkg, character.only = TRUE)
  }
}

# Load trent_df
if (file.exists("trent_data.rds")) {
  trent_df <- readRDS("trent_data.rds")
  cat("✅ Loaded stock data from 'trent_data.rds'\n")
} else {
  cat("⏳ 'trent_data.rds' not found. Running setup script first...\n")
  source("part1_setup_download.R")
  trent_df <- readRDS("trent_data.rds")
}

# Define a premium color palette
colors <- list(
  primary    = "#1E88E5",
  secondary  = "#FF6F00",
  positive   = "#00C853",
  negative   = "#FF1744",
  accent1    = "#7C4DFF",
  accent2    = "#00BCD4",
  bg_dark    = "#1A1A2E",
  text_light = "#E0E0E0",
  sma20      = "#FFEB3B",
  sma50      = "#FF9800",
  sma200     = "#F44336"
)

# Custom theme for professional plots
theme_trent <- function() {
  theme_minimal(base_size = 13) +
    theme(
      plot.title       = element_text(face = "bold", size = 16, hjust = 0.5,
                                      color = "#1A237E"),
      plot.subtitle    = element_text(size = 11, hjust = 0.5, color = "#455A64"),
      plot.caption     = element_text(size = 9, color = "#78909C"),
      axis.title       = element_text(face = "bold", size = 11),
      axis.text        = element_text(size = 10),
      legend.position  = "bottom",
      legend.title     = element_text(face = "bold"),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "#ECEFF1"),
      plot.background  = element_rect(fill = "#FAFAFA", color = NA),
      panel.background = element_rect(fill = "#FAFAFA", color = NA),
      plot.margin      = margin(15, 15, 15, 15)
    )
}

# Create a subdirectory for output plots to keep workspace clean
if (!dir.exists("plots")) {
  dir.create("plots")
}

# ============================================================
# 2.1  LINE CHART: Trent Closing Price Over Time
# ============================================================

p1 <- ggplot(trent_df, aes(x = Date, y = Close)) +
  geom_line(color = colors$primary, linewidth = 0.5, alpha = 0.9) +
  geom_smooth(method = "loess", se = TRUE, color = colors$secondary,
              fill = "#FFE0B2", alpha = 0.3, span = 0.1) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y",
               expand = c(0.01, 0)) +
  scale_y_continuous(labels = label_comma(prefix = "₹")) +
  labs(
    title    = "Trent Stock - Closing Price (2006–2026)",
    subtitle = "Trent Ltd. | NSE: Trent",
    x = "Date", y = "Closing Price (₹)",
    caption  = "Source: Yahoo Finance | LOESS smoothing overlay"
  ) +
  theme_trent()

ggsave("plots/chart_2_1_closing_price.png", p1, width = 10, height = 6, dpi = 150)
print(p1)
cat("✅ Chart 2.1: Closing Price Line Chart - Saved to plots/\n")

# ============================================================
# 2.2  LINE CHART: Closing Price with Moving Averages
# ============================================================

trent_ma <- trent_df %>%
  filter(!is.na(SMA_200)) %>%
  select(Date, Close, SMA_20, SMA_50, SMA_200) %>%
  pivot_longer(cols = -Date, names_to = "Series", values_to = "Price")

p2 <- ggplot(trent_ma, aes(x = Date, y = Price, color = Series)) +
  geom_line(aes(linewidth = Series), alpha = 0.85) +
  scale_linewidth_manual(values = c("Close" = 0.4, "SMA_20" = 0.6,
                                    "SMA_50" = 0.7, "SMA_200" = 0.9)) +
  scale_color_manual(
    values = c("Close" = colors$primary, "SMA_20" = colors$sma20,
               "SMA_50" = colors$sma50, "SMA_200" = colors$sma200),
    labels = c("Close", "SMA-20", "SMA-50", "SMA-200")
  ) +
  scale_y_continuous(labels = label_comma(prefix = "₹")) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  labs(
    title    = "Trent Stock with Moving Averages",
    subtitle = "20-day, 50-day, and 200-day Simple Moving Averages",
    x = "Date", y = "Price (₹)", color = "Series"
  ) +
  theme_trent() +
  guides(linewidth = "none")

ggsave("plots/chart_2_2_moving_averages.png", p2, width = 10, height = 6, dpi = 150)
print(p2)
cat("✅ Chart 2.2: Moving Averages Line Chart - Saved to plots/\n")

# ============================================================
# 2.3  BAR PLOT: Average Yearly Closing Price
# ============================================================

yearly_avg <- trent_df %>%
  group_by(Year) %>%
  summarise(
    Avg_Close  = mean(Close, na.rm = TRUE),
    Avg_Volume = mean(Volume, na.rm = TRUE),
    .groups = "drop"
  )

p3 <- ggplot(yearly_avg, aes(x = factor(Year), y = Avg_Close)) +
  geom_col(aes(fill = Avg_Close), width = 0.7, show.legend = FALSE) +
  geom_text(aes(label = paste0("₹", round(Avg_Close, 0))),
            vjust = -0.5, size = 3, fontface = "bold") +
  scale_fill_gradient(low = "#B3E5FC", high = "#0D47A1") +
  scale_y_continuous(labels = label_comma(prefix = "₹"),
                     expand = expansion(mult = c(0, 0.15))) +
  labs(
    title    = "Trent – Average Yearly Closing Price",
    subtitle = "Year-wise mean of daily closing prices",
    x = "Year", y = "Avg. Closing Price (₹)",
    caption  = "Source: Yahoo Finance"
  ) +
  theme_trent() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("plots/chart_2_3_yearly_avg_close.png", p3, width = 10, height = 6, dpi = 150)
print(p3)
cat("✅ Chart 2.3: Yearly Average Bar Plot - Saved to plots/\n")

# ============================================================
# 2.4  BAR PLOT: Monthly Average Volume
# ============================================================

monthly_vol <- trent_df %>%
  group_by(Month) %>%
  summarise(Avg_Volume = mean(Volume, na.rm = TRUE), .groups = "drop")

p4 <- ggplot(monthly_vol, aes(x = Month, y = Avg_Volume)) +
  geom_col(fill = colors$accent2, width = 0.65, alpha = 0.9) +
  geom_text(aes(label = paste0(round(Avg_Volume / 1e6, 1), "M")),
            vjust = -0.5, size = 3.2, fontface = "bold") +
  scale_y_continuous(labels = label_number(scale = 1e-6, suffix = "M"),
                     expand = expansion(mult = c(0, 0.12))) +
  labs(
    title    = "Trent – Average Monthly Trading Volume",
    subtitle = "Aggregated across all years (2006–2026)",
    x = "Month", y = "Avg. Volume",
    caption  = "Source: Yahoo Finance"
  ) +
  theme_trent()

ggsave("plots/chart_2_4_monthly_volume.png", p4, width = 10, height = 6, dpi = 150)
print(p4)
cat("✅ Chart 2.4: Monthly Volume Bar Plot - Saved to plots/\n")

# ============================================================
# 2.5  BAR PLOT: Year-wise Total Trading Volume
# ============================================================

yearly_volume <- trent_df %>%
  group_by(Year) %>%
  summarise(Total_Volume = sum(Volume, na.rm = TRUE), .groups = "drop")

p5 <- ggplot(yearly_volume, aes(x = factor(Year), y = Total_Volume)) +
  geom_col(aes(fill = Total_Volume), width = 0.7, show.legend = FALSE) +
  scale_fill_gradient(low = "#E1BEE7", high = "#6A1B9A") +
  scale_y_continuous(labels = label_number(scale = 1e-9, suffix = "B"),
                     expand = expansion(mult = c(0, 0.1))) +
  labs(
    title    = "Trent – Yearly Total Trading Volume",
    subtitle = "Total shares traded per year",
    x = "Year", y = "Total Volume (Billions)",
    caption  = "Source: Yahoo Finance"
  ) +
  theme_trent() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("plots/chart_2_5_yearly_volume.png", p5, width = 10, height = 6, dpi = 150)
print(p5)
cat("✅ Chart 2.5: Yearly Volume Bar Plot - Saved to plots/\n")

# ============================================================
# 2.6  FINANCIAL CHART: Candlestick-Style (OHLC Bars)
# ============================================================
# Show last 120 trading days for clarity

recent_df <- tail(trent_df, 120) %>%
  mutate(Direction = ifelse(Close >= Open, "Bullish", "Bearish"))

p6 <- ggplot(recent_df) +
  # High-Low range (wicks)
  geom_segment(aes(x = Date, xend = Date, y = Low, yend = High),
               color = "grey40", linewidth = 0.3) +
  # Open-Close body
  geom_rect(aes(xmin = Date - 0.3, xmax = Date + 0.3,
                ymin = pmin(Open, Close), ymax = pmax(Open, Close),
                fill = Direction), alpha = 0.9) +
  scale_fill_manual(values = c("Bullish" = colors$positive,
                               "Bearish" = colors$negative)) +
  scale_y_continuous(labels = label_comma(prefix = "₹")) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b\n%Y") +
  labs(
    title    = "Trent – Candlestick Chart (Last 120 Trading Days)",
    subtitle = "Green = Bullish (Close ≥ Open) | Red = Bearish (Close < Open)",
    x = "Date", y = "Price (₹)", fill = "Direction"
  ) +
  theme_trent()

ggsave("plots/chart_2_6_candlestick.png", p6, width = 10, height = 6, dpi = 150)
print(p6)
cat("✅ Chart 2.6: Candlestick Chart - Saved to plots/\n")

# ============================================================
# 2.7  FINANCIAL CHART: Bollinger Bands
# ============================================================

trent_bb <- trent_df %>%
  filter(!is.na(BB_Upper)) %>%
  tail(500)

p7 <- ggplot(trent_bb, aes(x = Date)) +
  geom_ribbon(aes(ymin = BB_Lower, ymax = BB_Upper),
              fill = "#BBDEFB", alpha = 0.4) +
  geom_line(aes(y = BB_Middle), color = "#1565C0",
            linewidth = 0.6, linetype = "dashed") +
  geom_line(aes(y = Close), color = colors$primary, linewidth = 0.5) +
  geom_line(aes(y = BB_Upper), color = "#E57373", linewidth = 0.4,
            linetype = "dotted") +
  geom_line(aes(y = BB_Lower), color = "#81C784", linewidth = 0.4,
            linetype = "dotted") +
  scale_y_continuous(labels = label_comma(prefix = "₹")) +
  labs(
    title    = "Trent – Bollinger Bands (Last 500 Days)",
    subtitle = "20-day SMA ± 2 Standard Deviations",
    x = "Date", y = "Price (₹)",
    caption  = "Band narrows = low volatility | Band widens = high volatility"
  ) +
  theme_trent()

ggsave("plots/chart_2_7_bollinger_bands.png", p7, width = 10, height = 6, dpi = 150)
print(p7)
cat("✅ Chart 2.7: Bollinger Bands Chart - Saved to plots/\n")

# ============================================================
# 2.8  FINANCIAL CHART: RSI (Relative Strength Index)
# ============================================================

trent_rsi <- trent_df %>%
  filter(!is.na(RSI_14)) %>%
  tail(500)

p8 <- ggplot(trent_rsi, aes(x = Date, y = RSI_14)) +
  geom_hline(yintercept = 70, color = colors$negative,
             linetype = "dashed", linewidth = 0.6) +
  geom_hline(yintercept = 30, color = colors$positive,
             linetype = "dashed", linewidth = 0.6) +
  geom_hline(yintercept = 50, color = "grey60",
             linetype = "dotted", linewidth = 0.4) +
  geom_line(color = colors$accent1, linewidth = 0.5) +
  geom_ribbon(aes(ymin = 30, ymax = pmin(RSI_14, 30)),
              fill = colors$positive, alpha = 0.2) +
  geom_ribbon(aes(ymin = pmax(RSI_14, 70), ymax = 70),
              fill = colors$negative, alpha = 0.2) +
  annotate("text", x = max(trent_rsi$Date), y = 72,
           label = "Overbought (70)", hjust = 1, color = colors$negative,
           fontface = "italic", size = 3.5) +
  annotate("text", x = max(trent_rsi$Date), y = 28,
           label = "Oversold (30)", hjust = 1, color = colors$positive,
           fontface = "italic", size = 3.5) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 10)) +
  labs(
    title    = "Trent – RSI (14-Period)",
    subtitle = "Relative Strength Index | Last 500 Trading Days",
    x = "Date", y = "RSI",
    caption  = "Above 70 = Overbought | Below 30 = Oversold"
  ) +
  theme_trent()

ggsave("plots/chart_2_8_rsi.png", p8, width = 10, height = 6, dpi = 150)
print(p8)
cat("✅ Chart 2.8: RSI Chart - Saved to plots/\n")

# ============================================================
# 2.9  FINANCIAL CHART: MACD
# ============================================================

trent_macd <- trent_df %>%
  filter(!is.na(MACD_Signal)) %>%
  tail(500)

p9 <- ggplot(trent_macd, aes(x = Date)) +
  geom_col(aes(y = MACD_Histogram,
               fill = ifelse(MACD_Histogram >= 0, "Positive", "Negative")),
           width = 1, show.legend = FALSE) +
  geom_line(aes(y = MACD), color = colors$primary, linewidth = 0.6) +
  geom_line(aes(y = MACD_Signal), color = colors$secondary, linewidth = 0.6) +
  scale_fill_manual(values = c("Positive" = colors$positive,
                               "Negative" = colors$negative)) +
  labs(
    title    = "Trent – MACD Indicator (Last 500 Days)",
    subtitle = "MACD Line (Blue) vs Signal Line (Orange) with Histogram",
    x = "Date", y = "MACD Value",
    caption  = "MACD(12,26,9) | Histogram = MACD - Signal"
  ) +
  theme_trent()

ggsave("plots/chart_2_9_macd.png", p9, width = 10, height = 6, dpi = 150)
print(p9)
cat("✅ Chart 2.9: MACD Chart - Saved to plots/\n")

# ============================================================
# 2.10  DAILY RETURNS DISTRIBUTION (Histogram + Density)
# ============================================================

mean_return <- mean(trent_df$Daily_Return, na.rm = TRUE)

p10 <- ggplot(trent_df %>% filter(!is.na(Daily_Return)),
              aes(x = Daily_Return)) +
  geom_histogram(aes(y = after_stat(density)),
                 bins = 100, fill = colors$primary, alpha = 0.6,
                 color = "white", linewidth = 0.1) +
  geom_density(color = colors$secondary, linewidth = 1) +
  geom_vline(xintercept = 0, color = "grey30", linetype = "dashed") +
  geom_vline(xintercept = mean_return,
             color = colors$negative, linetype = "dashed", linewidth = 0.7) +
  annotate("text",
           x = mean_return + 0.5,
           y = Inf, vjust = 2,
           label = paste0("Mean: ", round(mean_return, 3), "%"),
           color = colors$negative, fontface = "bold", size = 3.5) +
  labs(
    title    = "Trent – Distribution of Daily Returns",
    subtitle = "Histogram with kernel density overlay",
    x = "Daily Return (%)", y = "Density",
    caption  = "Red dashed line = Mean return"
  ) +
  theme_trent()

ggsave("plots/chart_2_10_returns_distribution.png", p10, width = 10, height = 6, dpi = 150)
print(p10)
cat("✅ Chart 2.10: Daily Returns Distribution - Saved to plots/\n")

# ============================================================
# 2.11  VOLATILITY OVER TIME
# ============================================================

p11 <- ggplot(trent_df %>% filter(!is.na(Volatility_20)),
              aes(x = Date, y = Volatility_20)) +
  geom_area(fill = "#FFCDD2", alpha = 0.5) +
  geom_line(color = colors$negative, linewidth = 0.4) +
  geom_smooth(method = "loess", se = FALSE, color = "#B71C1C",
              linewidth = 0.8, span = 0.05) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  labs(
    title    = "Trent – 20-Day Rolling Volatility",
    subtitle = "Standard deviation of daily returns (rolling window = 20)",
    x = "Date", y = "Volatility (Std Dev %)",
    caption  = "Higher spikes indicate periods of greater uncertainty"
  ) +
  theme_trent()

ggsave("plots/chart_2_11_volatility.png", p11, width = 10, height = 6, dpi = 150)
print(p11)
cat("✅ Chart 2.11: Volatility Chart - Saved to plots/\n")

# ============================================================
# 2.12  CUMULATIVE RETURN
# ============================================================

p12 <- ggplot(trent_df, aes(x = Date, y = Cum_Return * 100)) +
  geom_area(fill = "#C8E6C9", alpha = 0.5) +
  geom_line(color = colors$positive, linewidth = 0.5) +
  scale_y_continuous(labels = label_comma(suffix = "%")) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  labs(
    title    = "Trent – Cumulative Return (2006–2026)",
    subtitle = "Growth of ₹1 invested on day 1",
    x = "Date", y = "Cumulative Return (%)",
    caption  = "Source: Yahoo Finance"
  ) +
  theme_trent()

ggsave("plots/chart_2_12_cumulative_return.png", p12, width = 10, height = 6, dpi = 150)
print(p12)
cat("✅ Chart 2.12: Cumulative Return - Saved to plots/\n")

# ============================================================
# 2.13  HEATMAP: Monthly Returns by Year
# ============================================================

monthly_returns <- trent_df %>%
  group_by(Year, MonthNum) %>%
  summarise(Monthly_Return = (last(Close) - first(Close)) / first(Close) * 100,
            .groups = "drop") %>%
  mutate(Month = month.abb[MonthNum])

p13 <- ggplot(monthly_returns,
              aes(x = factor(Month, levels = month.abb),
                  y = factor(Year), fill = Monthly_Return)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = paste0(round(Monthly_Return, 1), "%")),
            size = 2.5, fontface = "bold") +
  scale_fill_gradient2(
    low = "#F44336", mid = "white", high = "#4CAF50",
    midpoint = 0, name = "Return %"
  ) +
  labs(
    title    = "Trent – Monthly Returns Heatmap",
    subtitle = "Green = positive return | Red = negative return",
    x = "Month", y = "Year"
  ) +
  theme_trent() +
  theme(panel.grid = element_blank())

ggsave("plots/chart_2_13_monthly_heatmap.png", p13, width = 10, height = 8, dpi = 150)
print(p13)
cat("✅ Chart 2.13: Monthly Returns Heatmap - Saved to plots/\n")

cat("\n🎉 All 13 visualizations generated successfully! Saved to 'plots/' directory.\n")
