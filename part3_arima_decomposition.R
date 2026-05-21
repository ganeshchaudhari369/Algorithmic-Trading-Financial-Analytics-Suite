# ============================================================
# Trent STOCK ANALYSIS - PART 3
# Time Series Decomposition and ARIMA Forecasting
# ============================================================
# NOTE: Loads computed data from 'trent_data.rds' (runs setup first if needed)
# ============================================================

# Install required packages if not already installed
packages <- c("ggplot2", "dplyr", "tidyr", "scales", "forecast", "tseries", "lubridate")
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

# Define a premium color palette (matching Part 2)
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

# Create plots folder if it doesn't exist
if (!dir.exists("plots")) {
  dir.create("plots")
}

# ============================================================
# 3.1  STATIONARITY TESTING (ADF TEST)
# ============================================================
cat("\n=== 3.1 Stationarity Testing (Augmented Dickey-Fuller Test) ===\n")

# Run ADF test on raw Closing prices
adf_raw <- adf.test(trent_df$Close, alternative = "stationary")
cat("ADF Test on Raw Close Price:\n")
cat("  - Test Statistic:", adf_raw$statistic, "\n")
cat("  - p-value:", adf_raw$p.value, "\n")
cat("  - Result:", ifelse(adf_raw$p.value < 0.05, "Stationary (Reject H0)", "Non-Stationary (Fail to Reject H0)"), "\n\n")

# Run ADF test on 1st Difference of Closing prices
price_diff <- diff(trent_df$Close)
adf_diff <- adf.test(price_diff, alternative = "stationary")
cat("ADF Test on Differenced Close Price:\n")
cat("  - Test Statistic:", adf_diff$statistic, "\n")
cat("  - p-value:", adf_diff$p.value, "\n")
cat("  - Result:", ifelse(adf_diff$p.value < 0.05, "Stationary (Reject H0)", "Non-Stationary (Fail to Reject H0)"), "\n\n")


# ============================================================
# 3.2  SEASONAL DECOMPOSITION (STL)
# ============================================================
cat("=== 3.2 Seasonal Decomposition ===\n")

# Daily stock data has missing weekends/holidays, which disrupts regular daily ts.
# We will aggregate to weekly average prices to create a clean, seasonal time series.
weekly_avg <- trent_df %>%
  mutate(WeekDate = floor_date(Date, unit = "week")) %>%
  group_by(WeekDate) %>%
  summarise(Close = mean(Close, na.rm = TRUE), .groups = "drop")

# Create ts object (approx 52 weeks per year)
start_year <- year(min(weekly_avg$WeekDate))
start_week <- week(min(weekly_avg$WeekDate))
weekly_ts <- ts(weekly_avg$Close, frequency = 52, start = c(start_year, start_week))

# Decompose using STL
decomp <- stl(weekly_ts, s.window = "periodic")

# Convert to dataframe for custom ggplotting
decomp_df <- data.frame(
  Date = weekly_avg$WeekDate,
  Observed = weekly_avg$Close,
  Trend = as.numeric(decomp$time.series[, "trend"]),
  Seasonal = as.numeric(decomp$time.series[, "seasonal"]),
  Remainder = as.numeric(decomp$time.series[, "remainder"])
)

# Convert to long format for faceted ggplot
decomp_long <- decomp_df %>%
  pivot_longer(cols = -Date, names_to = "Component", values_to = "Price") %>%
  mutate(Component = factor(Component, levels = c("Observed", "Trend", "Seasonal", "Remainder")))

p_decomp <- ggplot(decomp_long, aes(x = Date, y = Price)) +
  geom_line(color = colors$primary, linewidth = 0.5) +
  facet_wrap(~ Component, ncol = 1, scales = "free_y") +
  labs(
    title    = "Trent Stock - Seasonal Decomposition (STL)",
    subtitle = "Weekly closing prices decomposed into Trend, Seasonal, and Remainder components",
    x = "Date", y = "Price (₹) / Component Value",
    caption  = "Decomposition frequency = 52 weeks"
  ) +
  theme_trent() +
  theme(strip.text = element_text(face = "bold", color = "#1A237E"))

ggsave("plots/chart_3_1_decomposition.png", p_decomp, width = 10, height = 8, dpi = 150)
cat("✅ Chart 3.1: Seasonal Decomposition - Saved to plots/\n")


# ============================================================
# 3.3  ACF AND PACF PLOTS
# ============================================================
cat("=== 3.3 ACF and PACF Plots ===\n")

# Calculate ACF and PACF for differenced closing prices
acf_vals <- acf(price_diff, plot = FALSE, lag.max = 40)
pacf_vals <- pacf(price_diff, plot = FALSE, lag.max = 40)

acf_df <- data.frame(Lag = acf_vals$lag[-1], ACF = acf_vals$acf[-1])
pacf_df <- data.frame(Lag = pacf_vals$lag, PACF = pacf_vals$acf)

n_obs <- length(price_diff)
ci_limit <- 1.96 / sqrt(n_obs)

p_acf <- ggplot(acf_df, aes(x = Lag, y = ACF)) +
  geom_col(fill = colors$primary, width = 0.4) +
  geom_hline(yintercept = c(-ci_limit, ci_limit), color = colors$negative, linetype = "dashed", linewidth = 0.5) +
  geom_hline(yintercept = 0, color = "grey30", linewidth = 0.3) +
  labs(title = "Autocorrelation Function (ACF)", subtitle = "Differenced daily closing prices", x = "Lag", y = "ACF") +
  theme_trent()

p_pacf <- ggplot(pacf_df, aes(x = Lag, y = PACF)) +
  geom_col(fill = colors$secondary, width = 0.4) +
  geom_hline(yintercept = c(-ci_limit, ci_limit), color = colors$negative, linetype = "dashed", linewidth = 0.5) +
  geom_hline(yintercept = 0, color = "grey30", linewidth = 0.3) +
  labs(title = "Partial Autocorrelation Function (PACF)", subtitle = "Differenced daily closing prices", x = "Lag", y = "PACF") +
  theme_trent()

library(gridExtra)
p_acf_pacf <- gridExtra::arrangeGrob(p_acf, p_pacf, ncol = 2)
ggsave("plots/chart_3_2_acf_pacf.png", p_acf_pacf, width = 12, height = 5, dpi = 150)
cat("✅ Chart 3.2: ACF & PACF - Saved to plots/\n")


# ============================================================
# 3.4  ARIMA MODEL FITTING
# ============================================================
cat("=== 3.4 Fitting ARIMA Model ===\n")

# Use auto.arima to select the optimal model on daily Close prices
# We use approximation = FALSE for maximum accuracy (might take a few seconds)
arima_model <- auto.arima(trent_df$Close, seasonal = FALSE, approximation = FALSE, trace = TRUE)
summary(arima_model)

cat("\nOptimal ARIMA Model Selected:", arima_model_string <- forecast::arima.string(arima_model), "\n")


# ============================================================
# 3.5  RESIDUAL DIAGNOSTICS
# ============================================================
cat("\n=== 3.5 Residual Diagnostics ===\n")

residuals_val <- residuals(arima_model)

# Residuals ACF
res_acf <- acf(residuals_val, plot = FALSE, lag.max = 30)
res_acf_df <- data.frame(Lag = res_acf$lag[-1], ACF = res_acf$acf[-1])
p_res_acf <- ggplot(res_acf_df, aes(x = Lag, y = ACF)) +
  geom_col(fill = colors$accent1, width = 0.4) +
  geom_hline(yintercept = c(-ci_limit, ci_limit), color = colors$negative, linetype = "dashed", linewidth = 0.5) +
  geom_hline(yintercept = 0, color = "grey30", linewidth = 0.3) +
  labs(title = "Residuals ACF", subtitle = "Checks for remaining autocorrelation", x = "Lag", y = "ACF") +
  theme_trent()

# Residuals Distribution (Histogram + Density)
res_df <- data.frame(Residuals = as.numeric(residuals_val))
p_res_dist <- ggplot(res_df, aes(x = Residuals)) +
  geom_histogram(aes(y = after_stat(density)), bins = 80, fill = colors$primary, alpha = 0.6, color = "white", linewidth = 0.1) +
  geom_density(color = colors$secondary, linewidth = 1) +
  geom_vline(xintercept = mean(res_df$Residuals), color = colors$negative, linetype = "dashed") +
  labs(title = "Residuals Distribution", subtitle = "Checks for normality of error terms", x = "Residual Value", y = "Density") +
  theme_trent()

p_res_diagnostics <- gridExtra::arrangeGrob(p_res_acf, p_res_dist, ncol = 2)
ggsave("plots/chart_3_3_residuals.png", p_res_diagnostics, width = 12, height = 5, dpi = 150)
cat("✅ Chart 3.3: Residual Diagnostics - Saved to plots/\n")

# Run Ljung-Box test for independence of residuals
ljung_box <- Box.test(residuals_val, lag = 10, type = "Ljung-Box")
cat("Ljung-Box Test for Residuals:\n")
cat("  - Chi-squared:", ljung_box$statistic, "\n")
cat("  - p-value:", ljung_box$p.value, "\n")
cat("  - Result:", ifelse(ljung_box$p.value > 0.05, "No autocorrelation in residuals (White Noise, Pass)", "Autocorrelation exists in residuals (Fail)"), "\n\n")


# ============================================================
# 3.6  60-DAY ARIMA FORECAST
# ============================================================
cat("=== 3.6 Generating 60-Day Forecast ===\n")

# Forecast next 60 days
fc_horizon <- 60
forecast_obj <- forecast(arima_model, h = fc_horizon)

# Generate future dates (excluding weekends)
get_future_business_days <- function(start_date, n) {
  dates <- seq(start_date + 1, start_date + n * 2.5, by = "day")
  biz_dates <- dates[!weekdays(dates) %in% c("Saturday", "Sunday")]
  return(head(biz_dates, n))
}

last_date <- max(trent_df$Date)
future_dates <- get_future_business_days(last_date, fc_horizon)

# Extract forecast data
fc_df <- data.frame(
  Date   = future_dates,
  Close  = as.numeric(forecast_obj$mean),
  Lower80 = as.numeric(forecast_obj$lower[, "80%"]),
  Upper80 = as.numeric(forecast_obj$upper[, "80%"]),
  Lower95 = as.numeric(forecast_obj$lower[, "95%"]),
  Upper95 = as.numeric(forecast_obj$upper[, "95%"]),
  Type    = "Forecast"
)

# Prepare historical data (last 250 days for visualization clarity)
hist_df <- trent_df %>%
  tail(250) %>%
  select(Date, Close) %>%
  mutate(
    Lower80 = Close,
    Upper80 = Close,
    Lower95 = Close,
    Upper95 = Close,
    Type    = "Historical"
  )

# Combine historical and forecasted data
plot_fc_df <- rbind(hist_df, fc_df)

p_fc <- ggplot(plot_fc_df, aes(x = Date, y = Close)) +
  # 95% Confidence Band
  geom_ribbon(data = filter(plot_fc_df, Type == "Forecast"),
              aes(ymin = Lower95, ymax = Upper95), fill = "#C5CAE9", alpha = 0.5) +
  # 80% Confidence Band
  geom_ribbon(data = filter(plot_fc_df, Type == "Forecast"),
              aes(ymin = Lower80, ymax = Upper80), fill = "#7986CB", alpha = 0.5) +
  # Historical line
  geom_line(data = filter(plot_fc_df, Type == "Historical"),
            aes(color = "Historical"), linewidth = 0.7) +
  # Forecast line
  geom_line(data = filter(plot_fc_df, Type == "Forecast"),
            aes(color = "Forecast"), linewidth = 0.8) +
  # Customize scales
  scale_y_continuous(labels = label_comma(prefix = "₹")) +
  scale_x_date(date_breaks = "2 months", date_labels = "%b\n%Y") +
  scale_color_manual(
    name = "Series",
    values = c("Historical" = colors$primary, "Forecast" = colors$secondary)
  ) +
  labs(
    title    = paste("Trent Stock – 60-Day price Forecast"),
    subtitle = paste("Fitted Model:", arima_model_string, "| Purple Bands = 80% & 95% Confidence Intervals"),
    x = "Date", y = "Stock Price (₹)",
    caption  = paste("Forecast period:", format(min(future_dates), "%b %d, %Y"), "to", format(max(future_dates), "%b %d, %Y"))
  ) +
  theme_trent()

ggsave("plots/chart_3_4_arima_forecast.png", p_fc, width = 10, height = 6, dpi = 150)
print(p_fc)
cat("✅ Chart 3.4: 60-Day ARIMA Forecast - Saved to plots/\n")

# Save the fitted model and forecast object for use in Shiny
saveRDS(list(model = arima_model, forecast = forecast_obj, future_dates = future_dates), "trent_arima.rds")
cat("✅ Saved model and forecast to 'trent_arima.rds'\n")
cat("\n🎉 Part 3: Time Series & ARIMA Forecast completed successfully!\n")
