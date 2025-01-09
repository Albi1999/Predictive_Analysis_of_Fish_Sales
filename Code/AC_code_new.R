# Fish Market Sales Analysis

# LINEAR REGRESSION -- RIGA 231 circa
# TODO: GUARDARE CON TEST DI SIGNIFICATIVITÁ SE POSSIAMO RIMUOVERE UNA VARIABILE 
#       E.G.: TOLGO trainm$Year
#       Imo usiamo drop1 e update peró meglio vedere cosa fa prof

# BASS Model  -- RIGA 243 circa
# TODO: NON SO UN TUBO DEL BASS MODEL E MI É DIFFICILE ITERPRETARE I RISULTATI, 
#       IN LINEA DI MASSIMA MI PARE FACCIA CAGARE

# NEXT STEPS
# TODO: INSERIRE VARIABILE DEL FISH CONSUMPTION NEI MODELLI IN CUI É POSSIBILE FARLO
#       PROVARE NUOVI MODELLI-->GUARDARE CODICE PROF E SE POSSIBILE DOCUMENTAZION PERCHE CE ROBA FIGA
#       TIPO QUELLO CHE HO TROVATO SU GAM



# TODO: UNDERSTAND ALL THE MODEL ADDED AND COMMENT IT 
#       ARIMA
#       EXPONENTIAL SMOOTHING
#       MULTIPLE LINEAR REGRESSION
#       ARIMAX
#       GRADIENT BOOSTING
#       GENERALIZED BASS MODEL
#       
#       DO SOME TUNING ON THE PARAMETERS OF THE MODELS AND UNDERSTAND THE RESULTS


# Clear workspace
rm(list = ls())

# Load packages ----
library(readxl)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(forecast)
library(lubridate)
library(lmtest) # Durbin-Watson test
library(DIMORA) # Bass Model
library(mgcv) # GAM Model
library(gbm) # Gradient Boosting Machine

# 1. Load Data ----

data <- read_csv("Data/data.csv")
data$Date <- as.Date(data$Date, format = "%Y-%m-%d")

# Check for NA values
cat("NA counts in dataset:\n")
print(colSums(is.na(data)))
# No NA in the dataset

# Add features for time-based analysis
data <- data %>% mutate(Month = format(Date, "%m"),
                        Year = as.factor(format(Date, "%Y")))

head(data)

## Explanatory Variable ----

fish_consumpition_monthly <- read_excel("Data/Fish_consumption_ita_raw.xlsx")

fish_consumpition_monthly <- fish_consumpition_monthly %>% filter(CG == "Salmon")

fish_consumpition_monthly <- fish_consumpition_monthly %>%
  mutate(kg = as.numeric(`volume(Kg)`)) 

fish_monthly_time_series <- fish_consumpition_monthly %>%
  group_by(year, month) %>%
  summarise(kg = sum(kg)) %>%
  ungroup()

fish_monthly_time_series <- fish_monthly_time_series %>%
  filter(year > 2020 | (year == 2020 & month %in% c(11, 12)))

### Std ----
fish_monthly_time_series <- fish_monthly_time_series %>%
  mutate(kg_std = as.vector(scale(kg)))

fish_monthly_time_series$Date <- as.Date(paste(fish_monthly_time_series$year, fish_monthly_time_series$month, "01", sep = "-"))

head(fish_monthly_time_series)

ggplot(fish_monthly_time_series, aes(x = Date, y = kg_std)) +
  geom_line(color = 'blue') +
  labs(title = 'Time Series of Consumed Kg (Salmon)', 
       x = 'Data', 
       y = 'Kg Consumed') +
  theme_minimal() +
  scale_x_date(labels = scales::date_format("%b %Y"), breaks = "3 months") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# USEFUL FUNCTIONS -----

split_train_test <- function(data, name_y, prop) {
  n_sample <- floor(nrow(data) * prop)
  train <- data[1:n_sample, ]
  y_train <- train[[name_y]]
  test <- data[(n_sample + 1):nrow(data), ]
  y_test <- test[[name_y]]
  return(list(train = train, y_train = y_train, test = test, y_test = y_test))
}

plot_train_test <- function(train_test, name_y) {
  train_length <- length(train_test$y_train)
  test_length <- length(train_test$y_test)
  time_index <- c(1:(train_length + test_length))
  data_plot <- data.frame(
    Time = time_index,
    Value = c(as.numeric(train_test$y_train), as.numeric(train_test$y_test)),
    Type = c(rep("Train", train_length), rep("Test", test_length))
  )
  ggplot(data_plot, aes(x = Time, y = Value, color = Type)) +
    geom_line() +
    labs(title = paste(name_y, ": Train vs Test"),
         x = "Time",
         y = "Value") +
    scale_color_manual(values = c("Train" = "blue", "Test" = "red")) +
    theme_minimal()
}

compute_AIC <- function(n, RSS, k) {
  # GUARDA DOCUMENTAZIONE AIC: ?AIC
  logLik <- -n / 2 * (log(2 * pi) + log(RSS / n) + 1)
  AIC <- -2 * logLik + 2 * k
  return(AIC)
}

plot_train_pred <- function(y_train, y_pred, model_name) {
  plot_data <- data.frame(
    Time = 1:length(y_train),
    Observed = y_train,
    Predicted = y_pred
  )
  ggplot(plot_data, aes(x = Time)) +
    geom_line(aes(y = Observed), color = "blue", linewidth = 1) +
    geom_line(aes(y = Predicted), color = "red", linewidth = 1) +
    labs(
      title = paste("Observed and Predicted Values\nModel:", model_name),
      x = "Time",
      y = "Values"
    ) +
    theme_minimal()
}

plot_actual_vs_forecast <- function(actual, forecast, model_name) {
  forecast_data <- data.frame(
    Time = 1:length(actual),
    Actual = actual,
    Forecast = forecast
  )
  ggplot(forecast_data, aes(x = Time)) +
    geom_line(aes(y = Actual, color = "Actual"), linewidth = 1) +
    geom_line(aes(y = Forecast, color = "Forecast"), linewidth = 1, linetype = "dashed") +
    labs(
      title = paste("Actual vs Forecasted Values\nModel:", model_name),
      x = "Time",
      y = "Values"
    ) +
    scale_color_manual(values = c("Actual" = "blue", "Forecast" = "red")) +
    theme_minimal()
}

compute_metrics <- function(actual, predicted) {
  mae <- mean(abs(actual - predicted))
  rmse <- sqrt(mean((actual - predicted)^2))
  return(list(MAE = mae, RMSE = rmse))
}

analyze_residuals <- function(residuals, model_name) {
  par(mfrow = c(1, 3))
  plot(residuals, main = paste("Residuals of", model_name), ylab = "Residuals")
  acf(residuals, main = "ACF of Residuals")
  hist(residuals, main = "Histogram of Residuals", breaks = 10, col = "gray")
  par(mfrow = c(1, 1))
}

time_series_cv <- function(series, model_func, h = 12) {
  n <- length(series)
  errors <- numeric()
  for (i in seq(n - h, by = 1)) {
    train <- series[1:i]
    test <- series[(i + 1):(i + h)]
    model <- model_func(train)
    pred <- forecast(model, h = h)$mean
    errors <- c(errors, mean((test - pred)^2))
  }
  mean(errors)
}

grid_search <- function(model_func, param_grid, data) {
  results <- list()
  for (params in param_grid) {
    model <- do.call(model_func, c(list(data), params))
    results <- append(results, list(list(params = params, model = model)))
  }
  results
}

ensemble_forecast <- function(models, h = 12) {
  forecasts <- lapply(models, function(model) forecast(model, h = h)$mean)
  ensemble <- rowMeans(do.call(cbind, forecasts))
  return(ensemble)
}

decompose_series <- function(series, type = "additive") {
  decomposition <- decompose(ts(series, frequency = 12), type = type)
  plot(decomposition)
  return(decomposition)
}


# Plots ----

ggplot(data, aes(x = Date)) +
  geom_line(aes(y = Baccala_Mantecato, color = "Baccala Mantecato")) +
  geom_line(aes(y = Baccala_Vicentina, color = "Baccala Vicentina")) +
  labs(title = "Time Series of Baccala Mantecato and Baccala Vicentina",
       x = "Date",
       y = "Quantity") +
  scale_color_manual(values = c("Baccala Mantecato" = "blue", "Baccala Vicentina" = "red"),
                     name = "Kind") +
  theme_minimal()

ggplot(data, aes(x = Month, y = Baccala_Mantecato, color = Year, group = Year)) +
  geom_line() + 
  labs(x = "Days from Start of Year", y = "Baccala_Mantecato", 
       title = "Time Series for Each Year") +
  theme_minimal() +
  theme(legend.title = element_blank())

ggplot(data, aes(x = Month, y = Baccala_Vicentina, color = Year, group = Year)) +
  geom_line() +
  labs(x = "Month of the Year", y = "Baccala_Vicentina", 
       title = "Time Series for Each Year") +
  theme_minimal() +
  theme(legend.title = element_blank())

# 2. TS Properties ----

ym = ts(data$Baccala_Mantecato, frequency = 12, start = c(2020,01))
yv = ts(data$Baccala_Vicentina, frequency = 12, start = c(2020,01))

plot.ts(yv, ylab="Baccala Vicentina", main="Time Series of Baccala Vicentina", col = "red")
plot.ts(ym, ylab="Baccala Mantecato", main="Time Series of Baccala Mantecato", col = "blue")

# Stationary check
acf(data$Baccala_Mantecato, main="ACF of Baccala Mantecato", col = "blue", lwd = 2)
acf(data$Baccala_Vicentina, main="ACF of Baccala Vicentina", col = "red", lwd = 2)

# 3. TRAIN/TEST SPLIT ----

train_testm <- split_train_test(data, "Baccala_Mantecato", 0.9)
plot_train_test(train_testm, "Baccala_Mantecato")

train_testv <- split_train_test(data, "Baccala_Vicentina", 0.9)
plot_train_test(train_testv, "Baccala_Vicentina")

trainm = train_testm$train
y_train_m = train_testm$y_train
testm = train_testm$test
y_test_m = train_testm$y_test

trainv = train_testv$train
y_train_v = train_testv$y_train
testv = train_testv$test
y_test_v = train_testv$y_test

# 4. Univariate Models ----

## 4.1 Linear Regression ----
fit_LR <- lm(train_testm$y_train ~ seq_along(train_testm$y_train))
summary(fit_LR)
dwtest(fit_LR)

# Residual Diagnostics 
ols_test_normality(fit_LR)
ols_test_correlation(fit_LR)
ols_test_breusch_pagan(fit_LR)
# ols_plot_diagnostics(fit_LR)

tt = 1:nrow(trainm)

fit_LR_trend <- lm(y_train_m ~ tt)
summary(fit_LR_trend)

plot_train_pred(y_train = y_train_m,
                y_pred = predict(fit_LR_trend),
                model_name = "Linear Regression w/ Monthly Seasonality")
##

fit_LR_month <- lm(y_train_m ~ tt + trainm$Month)
summary(fit_LR_month)

plot_train_pred(y_train = y_train_m,
                y_pred = predict(fit_LR_month),
                model_name = "Linear Regression w/ Monthly Seasonality")

##

fit_LR_year <- lm(y_train_m ~ tt + trainm$Year)
summary(fit_LR_year)

plot_train_pred(y_train = y_train_m,
                y_pred = predict(fit_LR_year),
                model_name = "Linear Regression w/ Monthly Seasonality")

##
fit_LR_year_month <- lm(y_train_m ~ tt + trainm$Year + trainm$Month)
summary(fit_LR_year_month)

plot_train_pred(y_train = y_train_m,
                y_pred = predict(fit_LR_year_month),
                model_name = "Linear Regression w/ Monthly Seasonality")

res_LR_year_month <- residuals(fit_LR_year_month)
plot(res_LR_year_month, ylab="residuals")
dwtest(fit_LR_year_month)

# Residual Diagnostics
ols_test_normality(fit_LR_year_month)
ols_test_correlation(fit_LR_year_month)
ols_test_breusch_pagan(fit_LR_year_month)
# ols_plot_diagnostics(fit_LR_year_month) # this integrate all the different plots below and more 
ols_plot_cooksd_chart(fit_LR_year_month)
ols_plot_resid_fit(fit_LR_year_month)
ols_plot_resid_stud_fit(fit_LR_year_month)
ols_plot_resid_lev(fit_LR_year_month)
ols_plot_resid_hist(fit_LR_year_month)
# we possibly need to deal with two outliers
# observation 25 and 37 are outliers

AIC(fit_LR_month)
AIC(fit_LR_year)
AIC(fit_LR_year_month) # lower AIC-->best model
# TODO: GUARDARE CON TEST DI SIGNIFICATIVITÁ SE POSSIAMO RIMUOVERE UNA VARIABILE 
#       E.G.: TOLGO trainm$Year

## 4.2 ARIMA ----
train_series <- ts(train_testm$y_train, frequency = 12)
fit_ARIMA <- auto.arima(train_series)
summary(fit_ARIMA)
forecast_ARIMA <- forecast(fit_ARIMA, h = length(train_testm$y_test))
plot(forecast_ARIMA)

plot_actual_vs_forecast(
  actual = train_testm$y_test,
  forecast = as.numeric(forecast_ARIMA$mean),
  model_name = "ARIMA"
)

## 4.3 Exponential Smoothing ----
fit_ES <- ets(train_series)
summary(fit_ES)
forecast_ES <- forecast(fit_ES, h = length(train_testm$y_test))
plot(forecast_ES)

plot_actual_vs_forecast(
  actual = train_testm$y_test,
  forecast = as.numeric(forecast_ES$mean),
  model_name = "Exponential Smoothing"
)

## 4.4 GAM (Generalized Additive Model) ----
gam_model <- gam(Baccala_Mantecato ~ s(as.numeric(Month)), data = trainm)
summary(gam_model)

plot(gam_model, se = TRUE, col = "blue", main = "GAM Model", ylab = "Baccala Mantecato", xlab = "Month")

gam.check(gam_model)
# leggendo documentazione, sembra figo -> https://www.rdocumentation.org/packages/mgcv/versions/1.9-1/topics/gam

gam_forecast <- predict(gam_model, newdata = trainm)
AIC(gam_model)

plot_train_pred(y_train = trainm$Baccala_Mantecato,
                y_pred = gam_forecast,
                model_name = "GAM Model")

res_GAM <- residuals(gam_model)
plot(res_GAM, ylab="residuals", main = "GAM Model Residuals", col = "blue")
abline(h = 0, col = "red")
dwtest(gam_model)

plot_actual_vs_forecast(
  actual = train_testm$y_test,
  forecast = predict(gam_model, newdata = train_testm$test),
  model_name = "GAM"
)

# 5. Multivariate Models ----

## 5.1 Multiple Linear Regression ----
fit_MLR <- lm(Baccala_Mantecato ~ Month + Year, data = train_testm$train)
summary(fit_MLR)
dwtest(fit_MLR)

## 5.2 ARIMAX ----
fit_ARIMAX <- auto.arima(train_testm$y_train, xreg = as.numeric(train_testm$train$Month))
summary(fit_ARIMAX)
forecast_ARIMAX <- forecast(fit_ARIMAX, xreg = as.numeric(train_testm$test$Month))
plot(forecast_ARIMAX)

plot_actual_vs_forecast(
  actual = train_testm$y_test,
  forecast = as.numeric(forecast_ARIMAX$mean),
  model_name = "ARIMAX"
)

## 5.3 Gradient Boosting ----
train_numeric <- train_testm$train %>%
  mutate(Month = as.factor(Month)) %>%
  select(-Date)  # Exclude the Date column

boost_model <- gbm(Baccala_Mantecato ~ ., data = train_numeric,
                   distribution = "gaussian",
                   n.trees = 500,
                   interaction.depth = 3)
summary(boost_model)

yhat_boost <- predict(boost_model, newdata = train_testm$test, n.trees = 500)
plot(yhat_boost, type = "l", col = "blue")

plot_actual_vs_forecast(
  actual = train_testm$y_test,
  forecast = yhat_boost,
  model_name = "Gradient Boosting"
)

# 6. Nonlinear Models ----

## 6.1 Bass Model ----
y_train_tsm = ts(train_testm$y_train, start = c(2021, 01), frequency = 12)
fit_BM <- BM(y_train_tsm, display = TRUE)
summary(fit_BM)

forecast_Bass <- predict(fit_BM, newx = 1:(length(train_testm$y_train) + length(train_testm$y_test)))

# Plot actual vs forecasted for Bass Model
plot_actual_vs_forecast(
  actual = train_testm$y_test,
  forecast = forecast_Bass[1:length(train_testm$y_test)],  # Match test length
  model_name = "Bass Model"
)

## 6.2 Generalized Bass Model ----
GGM_model <- GGM(train_testm$y_train, prelimestimates = c(10, 0.01, 0.1, 0.01, 0.1))
summary(GGM_model)
forecast_GGM <- predict(GGM_model, newx = 1:(length(train_testm$y_train) + length(train_testm$y_test)))

# Plot actual vs forecasted for Generalized Bass Model
plot_actual_vs_forecast(
  actual = train_testm$y_test,
  forecast = forecast_GGM[1:length(train_testm$y_test)],  # Match test length
  model_name = "Generalized Bass Model"
)
