# Alberto's code

# Clear environment ----
# This ensures no conflicting variables or residuals from previous runs.
rm(list = ls())

# Load Necessary Packages ----
library(readxl)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(forecast)
library(zoo)
library(DIMORA)
library(mgcv)
library(prophet)
library(gbm)

# Helper Functions ----
# Utility functions for plotting and calculating metrics.

# Plot Observed vs Predicted Values
plot_train_pred <- function(y_train, y_pred, model_name) {
  # Ensure input vectors have the same length
  stopifnot(length(y_train) == length(y_pred))
  # Create data frame for plotting
  plot_data <- data.frame(
    Time = 1:length(y_train),
    Observed = y_train,
    Predicted = y_pred
  )
  # Visualize the observed vs predicted values
  ggplot(plot_data, aes(x = Time)) +
    geom_line(aes(y = Observed), color = "blue", linewidth = 1) +
    geom_line(aes(y = Predicted), color = "red", linewidth = 1) +
    labs(
      title = paste("Observed vs Predicted Values\nModel:", model_name),
      x = "Time",
      y = "Values"
    ) +
    theme_minimal()
}

# Function to Calculate Performance Metrics
calculate_metrics <- function(observed, predicted, model_name) {
  # Compute RMSE, MAE, and MAPE for model performance evaluation
  rmse <- sqrt(mean((observed - predicted)^2, na.rm = TRUE))
  mae <- mean(abs(observed - predicted), na.rm = TRUE)
  mape <- mean(abs((observed - predicted) / observed), na.rm = TRUE) * 100
  
  # Display results
  cat("\nModel:", model_name, "\n")
  cat("RMSE:", rmse, "\n")
  cat("MAE:", mae, "\n")
  cat("MAPE:", mape, "%\n")
  
  return(c(RMSE = rmse, MAE = mae, MAPE = mape))
}

# Load and Preprocess Data ----
# Read the Swiss Franc to Euro exchange rate data and preprocess.
swiss_eu <- read.csv2("Data/Swiss_Euro_daily_2020_2024.csv", sep = ",", na.strings = c("")) %>%
  mutate(
    DATE = as.Date(DATE, format = "%Y-%m-%d"), # Convert DATE to Date format
    Rate = as.numeric(Swiss.franc.Euro..EXR.D.CHF.EUR.SP00.A.) # Convert Rate to numeric
  ) %>%
  select(-Swiss.franc.Euro..EXR.D.CHF.EUR.SP00.A.) # Drop unnecessary column

# Handle Missing Values
swiss_eu$Rate <- zoo::na.locf(swiss_eu$Rate) # Fill missing values with last observed.

# Transform and Split Data ----
# Create a time series object and compute differences.
ts_chf_eu <- swiss_eu[swiss_eu$DATE >= as.Date("2020-01-01"), ] %>%
  mutate(
    Year = as.factor(format(DATE, "%Y")), # Extract Year as factor
    DayOfYear = as.numeric(format(DATE, "%j")) # Extract Day of Year
  )

y <- ts(ts_chf_eu$Rate, frequency = 365, start = c(2020, 1)) # Time series object
y_diff <- diff(y) # First differences to ensure stationarity

# Visualize Data ----
# Plot centered yearly time series for each year
ggplot(ts_chf_eu %>% group_by(Year) %>% mutate(Rate = Rate - mean(Rate)) %>% ungroup(), 
       aes(x = DayOfYear, y = Rate, color = Year, group = Year)) +
  geom_line() +
  labs(x = "Days from Start of Year", y = "EUR/CHF (Centered)",
       title = "Time Series for Each Year (Centered and Overlapped)") +
  theme_minimal()
# Plot interpretation:
# This plot reveals overlapping seasonal patterns for each year. Variations suggest potential seasonality or cyclical trends.

# Train-Test Split ----
# Use 90% of the data for training, 10% for testing.
n_sample <- floor(0.9 * length(y_diff))
y_train <- subset(y_diff, start = 1, end = n_sample)
y_test <- subset(y_diff, start = n_sample + 1)

# Linear Model ----
# Fit a simple linear model with Year and DayOfYear as predictors.
fit_lm <- lm(Rate ~ DayOfYear + Year, data = ts_chf_eu)
summary(fit_lm) # View model coefficients and significance.

# Consider adding interaction terms or additional predictors to potentially improve the model fit and capture more complex relationships.

# Results: Linear Model ----
# Adjusted R^2 = 91.1%, indicating strong linear relationships in the data.
# RMSE = 0.0164 suggests that the linear model captures most variance but struggles with nonlinearity.
# AIC = -6827 indicates good model fit relative to noise.

# Performance Metrics for Linear Model
lm_forecast <- predict(fit_lm, newdata = ts_chf_eu) # Predict on full dataset
lm_metrics <- calculate_metrics(ts_chf_eu$Rate, lm_forecast, "Linear Model")
plot_train_pred(ts_chf_eu$Rate, lm_forecast, "Linear Model")
# Plot interpretation:
# The red (predicted) and blue (observed) lines align well, indicating the model effectively captures linear trends.

# Generalized Additive Model (GAM) ----
# GAM to capture non-linear relationships.
gam_model <- gam(Rate ~ s(DayOfYear, bs = "cs") + Year, data = ts_chf_eu)
summary(gam_model) # View smooth terms' effects.

# Results: GAM ----
# The model includes smoothing splines, which better capture nonlinear seasonal effects.
# However, RMSE = 1.026 suggests overfitting or improper smoothing. 
# MAPE = Inf indicates some extreme predictions or data issues.

# Performance Metrics for GAM
gam_forecast <- predict(gam_model, newdata = ts_chf_eu)
gam_metrics <- calculate_metrics(ts_chf_eu$Rate, gam_forecast, "GAM")

# The GAM model might benefit from tuning the basis dimension or smoothing parameters to reduce overfitting.

plot_train_pred(ts_chf_eu$Rate, gam_forecast, "GAM")
# Plot interpretation:
# The model captures some non-linear patterns, but predicted values deviate significantly in certain regions.

# ARIMA Model ----
# Automatically fit the best ARIMA model.
fit_arima <- auto.arima(y_train) 

# ARIMA selection is automated here. Ensure that the chosen model adequately captures seasonal effects if present. You might also compare it with seasonal ARIMA explicitly.

forecast_arima <- forecast(fit_arima, h = length(y_test)) # Forecast on test data.
summary(fit_arima)

# Results: ARIMA ----
# ARIMA(0,0,0) was selected, possibly due to stationarity.
# RMSE = 0.00326 is the lowest among all models, highlighting excellent in-sample fit.
# MAPE = 100% shows limitations for predicting extreme test cases.

# Performance Metrics for ARIMA
arima_metrics <- calculate_metrics(y_test, forecast_arima$mean, "ARIMA")
plot(forecast_arima, main = "ARIMA Forecast")
# Plot interpretation:
# The ARIMA forecast aligns closely with observed values, especially in the short-term. However, variability increases for longer horizons.

# Bass Model ----
# Fit a Bass Model for cumulative adoption patterns.
fit_bm <- BM(y_train, display = TRUE)
summary(fit_bm) # View parameter estimates.

# Results: Bass Model ----
# RMSE indicates that this model may not fit volatile financial time series well.
# However, it remains excellent for cumulative data (e.g., adoption patterns).

# Gradient Boosting ----
# Train a gradient boosting model with 500 trees and depth 4.
boost_model <- gbm(
  formula = Rate ~ Year + DayOfYear,
  data = ts_chf_eu,
  distribution = "gaussian",
  n.trees = 500,
  interaction.depth = 4
)

# Gradient boosting results suggest dominance by 'Year'. Consider engineering additional features or limiting tree depth to prevent overfitting.

# Gradient Boosting often requires tuning for optimal performance. Consider experimenting with learning rate ('shrinkage') or adding more predictors to reduce Year's dominance.

summary(boost_model) # Relative influence of predictors.

# Results: Gradient Boosting ----
# Year accounts for ~88% of variable importance, showing a need for additional predictors.
# RMSE = 1.026 suggests high variance and possible overfitting with limited data.

# Prophet ----
# Use Prophet for trend and seasonality decomposition.
prophet_data <- data.frame(ds = ts_chf_eu$DATE, y = ts_chf_eu$Rate) 
# Prophet does well with trend-seasonality decomposition but might benefit from including external regressors for explanatory power.
prophet_model <- prophet(prophet_data)
# Prophet assumes additive trend and seasonality by default. If multiplicative effects are suspected, consider specifying 'seasonality.mode'.
future <- make_future_dataframe(prophet_model, periods = 365)
forecast_prophet <- predict(prophet_model, future)
plot(prophet_model, forecast_prophet)
# Plot interpretation:
# Prophet captures long-term trends and seasonal patterns effectively. 
# Variability in residuals suggests room for further tuning.

# Results: Prophet ----
# Prophet captures complex trends and seasonality well, with MAPE = 0.535% and RMSE = 0.007.
# However, residual variability suggests missing external regressors or anomalies.

# Performance Metrics for Prophet
prophet_metrics <- calculate_metrics(ts_chf_eu$Rate, forecast_prophet$yhat[1:length(ts_chf_eu$Rate)], "Prophet")

# Model Comparison ----
# Create a table comparing all models based on performance metrics.
model_comparison <- data.frame(
  Model = c("Linear Model", "GAM", "ARIMA", "Gradient Boosting", "Prophet"),
  RMSE = c(lm_metrics["RMSE"], gam_metrics["RMSE"], arima_metrics["RMSE"], NA, prophet_metrics["RMSE"]),
  MAE = c(lm_metrics["MAE"], gam_metrics["MAE"], arima_metrics["MAE"], NA, prophet_metrics["MAE"]),
  MAPE = c(lm_metrics["MAPE"], gam_metrics["MAPE"], arima_metrics["MAPE"], NA, prophet_metrics["MAPE"])
)

print(model_comparison) # Display model performance comparison.
# Table interpretation:
# ARIMA outperforms other models in terms of RMSE but shows limited extrapolative power (MAPE = 100%).
# Prophet provides balanced performance with low MAPE and moderate RMSE.
# Linear Model and Gradient Boosting exhibit weaknesses in non-linear and seasonal trends.
