# Fish Market Sales Analysis

# TOTO 08/01/2025
#   LINEAR REGRESSION
#   TODO: GUARDARE CON TEST DI SIGNIFICATIVITÁ SE POSSIAMO RIMUOVERE UNA VARIABILE 
#         E.G.: TOLGO trainm$Year
#         Imo usiamo drop1 e update peró meglio vedere cosa fa prof

#   BASS Model
#   TODO: NON SO UN TUBO DEL BASS MODEL E MI É DIFFICILE ITERPRETARE I RISULTATI, 
#         IN LINEA DI MASSIMA MI PARE FACCIA CAGARE

#   NEXT STEPS
#   TODO: INSERIRE VARIABILE DEL FISH CONSUMPTION NEI MODELLI IN CUI É POSSIBILE FARLO
#         PROVARE NUOVI MODELLI-->GUARDARE CODICE PROF E SE POSSIBILE DOCUMENTAZION PERCHE CE ROBA FIGA
#         TIPO QUELLO CHE HO TROVATO SU GAM


#   ALBI 09/01/2025
#   TODO: UNDERSTAND ALL THE MODEL ADDED AND COMMENT IT 
#         MULTIPLE LINEAR REGRESSION-->TOTO
#         SARIMA-->TOTO
#         ARIMAX-->FLAVIO
#         EXPONENTIAL SMOOTHING-->ALBERTO
#         KNN-->ALBERTO
#         GAM-->FLAVIO

#         LOCAL REGRESSION
#         SPLINES
#         GRADIENT BOOSTING
#         BASS MODEL
#         PROPHET MODEL
#         GENERALIZED BASS MODEL
#       
#         DO SOME TUNING ON THE PARAMETERS OF THE MODELS AND UNDERSTAND THE RESULTS

#     TOTO 09/01/2025
#       COMMENTO:
#         - TEST NORMALITÁ CON library(olsrr) (funzione='ols_test_normality') la usa la prof? ha senso? cosa fa?
#           stesso discorso vale per ols_test_correlation  e ols_test_breusch_pagan
#         
#       
#    ALBI 09/01/2025
#     START COMMENT
#       La prof non lo usa ma ne avevo già parlato con lei e ha detto che possiamo usare 
#       tutti i pacchetti che vogliamo basta che sappiamo cosa fanno e che non ci sono problemi
#       ovviamente dobbiamo sapere cosa fanno e commentare di conseguenza i risultati

help("ols_test_normality")
help("olsrr")

#       Comunque è un pacchetto per fare dei check sulla normalitá dei residui, sulla correlazione e sulla eteroschedasticitá delle variabili
#       ovviamente non dobbiamo usare tutte le sue funzioni, io intanto le avevo messe
#       La cosa figa però è i plot che fa come puoi vedere nella sezione della Linear Regression
#       I plot li ho anche già commentati brevemente
#     END COMMENT


#     ALBI 10/01/2025
#     Ho lavorato sui modelli Exponential smoothing
#     Ho provato a fare un tuning del modello ma non ho ottenuto risultati migliori
#     Ho provato a fare un modello con la regressione multipla e ho ottenuto un buon risultato



# Clear workspace
rm(list = ls())

# Load packages ----
library(readxl)
library(readr)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(forecast)
library(lubridate)
library(olsrr) # test normality
library(lmtest) # Durbin-Watson test
library(DIMORA) # Bass Model
library(mgcv) # GAM Model
library(gbm) # Gradient Boosting Machine
library(olsrr)
library(kknn) # KNN
library(lubridate) # Date manipulation

# setwd("D:/Projects_GitHub/BEFD_Project") COMMENTED SINCE IT IS ONLY YOUR PATH
# IF YOU EACH TIME OPEN RSTUDIO TRUGH THE PROJECT FILE YOU DON'T NEED TO SET THE WORKING DIRECTORY
source("Code/Functions_Utilies.R") # AM: CI RENDE QUESTO SCRIPT PIU PULITO

# 1. Load Data ----

data <- read_csv("Data/data.csv")
data$Date <- as.Date(data$Date, format = "%Y-%m-%d")

# Check for NA values
cat("NA counts in dataset:\n")
cat(paste(names(data), colSums(is.na(data)), sep=": "), sep="\n")
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

data$fish_cons <- fish_monthly_time_series$kg_std


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

# Models with monthly seasonality ("Linear Regression w/ Monthly Seasonality") performed significantly better than the baseline trend-only model in terms of R-squared and adjusted R-squared.
# The model with both yearly and monthly effects further improves the adjusted R-squared and has the lowest AIC (265.68), indicating the best fit among tested models.


res_LR_year_month <- residuals(fit_LR_year_month)
plot(res_LR_year_month, ylab="residuals")
dwtest(fit_LR_year_month)
# The DW statistic of 1.09 for the final model suggests positive autocorrelation in residuals, which violates regression assumptions.
# TODO: I think this is BAD

## -- BEST MODEL RIGTH NOW
fit_LR_year_month_fish <- lm(y_train_m ~ tt + trainm$Year + trainm$Month + trainm$fish_cons)
summary(fit_LR_year_month_fish)

plot_train_pred(y_train = y_train_m,
                y_pred = predict(fit_LR_year_month_fish),
                model_name = "Linear Regression w/ Monthly, Year Seasonality and Fish Consumption")

res_LR_year_month <- residuals(fit_LR_year_month_fish)
plot(res_LR_year_month, ylab="residuals")
abline(h = 0, col = "red")
dwtest(fit_LR_year_month)
acf(res_LR_year_month) # --> CE STAGIONALITÁ!!!!!!!!!!!!!!!


# Residual Diagnostics
ols_test_normality(fit_LR_year_month)
# Normality tests (Shapiro-Wilk, Anderson-Darling) confirm that residuals do not deviate significantly from normality, 
# except for the Cramer-von Mises test (a stricter test).
ols_test_correlation(fit_LR_year_month)
ols_test_breusch_pagan(fit_LR_year_month)
# ols_plot_diagnostics(fit_LR_year_month) # this integrate all the different plots below and more 
ols_plot_resid_fit(fit_LR_year_month)
# The residuals are randomly distributed around zero, which is desirable. 
# However, there may be slight clustering, hinting at unexplained variability or trends.
ols_plot_resid_stud_fit(fit_LR_year_month)
# Observation 25 and 37 are flagged as outliers with high studentized residuals. 
# These points may need further investigation.
ols_plot_cooksd_chart(fit_LR_year_month)
ols_plot_resid_lev(fit_LR_year_month)
# Observations with high leverage (near the threshold) indicate data points that might heavily influence the model. 
# Observation 37 is a standout and should be carefully considered for its impact.
ols_plot_resid_hist(fit_LR_year_month)
# The residuals appear to follow a roughly normal distribution, with some deviations at the tails.

AIC(fit_LR_month)
AIC(fit_LR_year)
AIC(fit_LR_year_month) # lower AIC-->best model
# TODO: GUARDARE CON TEST DI SIGNIFICATIVITÁ SE POSSIAMO RIMUOVERE UNA VARIABILE 
#       E.G.: TOLGO trainm$Year

## 4.2 ARIMA ----
train_series <- ts(train_testm$y_train, frequency = 12)
fit_ARIMA <- auto.arima(train_series)
summary(fit_ARIMA)
# The chosen model by auto.arima is ARIMA(0,1,0)(1,1,0)[12], which means:
#     Non-seasonal ARIMA terms: None for AR and MA (p=0, q=0) with first-order differencing (d=1).
#     Seasonal ARIMA terms: One seasonal autoregressive term (P=1), seasonal differencing (D=1), and no seasonal moving average (Q=0) with a periodicity of 12 (likely monthly data).

# The coefficient for the seasonal AR term is significant at -0.5504, indicating some level of negative autocorrelation between observations across seasonal cycles.

# Model diagnostics:
#   AIC = 196.27 and BIC = 199.07, both relatively low for a seasonal model.
#   Residual ACF (last column of error measures) shows minimal correlation (-0.168), suggesting reasonably white noise residuals.

# Training Set Error Metrics:
#   Mean Absolute Error (MAE) = 3.12, Mean Percentage Error (MPE) = 1.22%, and Mean Absolute Percentage Error (MAPE) = 11.42%.
#   RMSE (Root Mean Square Error) = 4.63, showing decent predictive power for this model on training data.


forecast_ARIMA <- forecast(fit_ARIMA, h = length(train_testm$y_test))
plot(forecast_ARIMA)
# The actual data seems to deviate significantly from the forecast's confidence bands in some regions, particularly suggesting either: 
#     Misspecified seasonal patterns.
#     Non-stationarity or non-linear trends not captured by the model.
plot_actual_vs_forecast(
  actual = train_testm$y_test,
  forecast = as.numeric(forecast_ARIMA$mean),
  model_name = "ARIMA"
)
# There is a clear discrepancy in the scale or trend alignment of the forecasts, particularly with over-forecasting for later periods.

## 4.3 Exponential Smoothing ----

### ETS Baccala Mantecato ----
# Initialize a data frame to store the results
results <- data.frame(
  Model = character(),
  MSE = numeric(),
  AIC = numeric(),
  stringsAsFactors = FALSE
)

# 1. ETS Model
train_series <- ts(train_testm$y_train, start = c(2021, 1), frequency = 12)
fit_ES <- ets(train_series)
forecast_ES <- forecast(fit_ES, h = length(train_testm$y_test))
mse_ets <- mse(forecast_ES$mean, train_testm$y_test)
aic_ets <- AIC(fit_ES)
results <- rbind(results, data.frame(Model = "ETS", MSE = mse_ets, AIC = aic_ets))

# 2. Additive Seasonality (Holt-Winters via ETS)
fit_hw_add <- ets(train_series, model = "AAA")
forecast_hw_add <- forecast(fit_hw_add, h = length(train_testm$y_test))
mse_hw_add <- mse(forecast_hw_add$mean, train_testm$y_test)
aic_hw_add <- AIC(fit_hw_add)
results <- rbind(results, data.frame(Model = "Holt-Winters Additive", MSE = mse_hw_add, AIC = aic_hw_add))

# 3. Holt-Winters Multiplicative Seasonality (via ETS)
fit_hw_mult <- ets(train_series, model = "MAM")
forecast_hw_mult <- forecast(fit_hw_mult, h = length(train_testm$y_test))
mse_hw_mult <- mse(forecast_hw_mult$mean, train_testm$y_test)
aic_hw_mult <- AIC(fit_hw_mult)
results <- rbind(results, data.frame(Model = "Holt-Winters Multiplicative", MSE = mse_hw_mult, AIC = aic_hw_mult))

# 4. Regression + ETS Model
fit_reg <- lm(y_train_m ~ trainm$Month + trainm$Year + trainm$fish_cons)
residuals_reg <- residuals(fit_reg)
fit_res_ets <- ets(residuals_reg)
forecast_res_ets <- forecast(fit_res_ets, h = length(train_testm$y_test))
mse_reg_ets <- mse(forecast_res_ets$mean, train_testm$y_test)
aic_reg <- AIC(fit_reg)  # AIC for the regression model
aic_res_ets <- AIC(fit_res_ets)  # AIC for the ETS model on residuals
results <- rbind(results, data.frame(Model = "Regression + ETS", MSE = mse_reg_ets, AIC = aic_res_ets))

# Print the results
print(results)

### ETS Baccala Vicentina ----
results <- data.frame(
  Model = character(),
  MSE = numeric(),
  AIC = numeric(),
  stringsAsFactors = FALSE
)

# 1. ETS Model
train_series_v <- ts(train_testv$y_train, start = c(2021, 1), frequency = 12)
fit_ES_v <- ets(train_series_v)
forecast_ES_v <- forecast(fit_ES_v, h = length(train_testv$y_test))
mse_ets <- mse(forecast_ES_v$mean, train_testv$y_test)
aic_ets <- AIC(fit_ES_v)
results <- rbind(results, data.frame(Model = "ETS", MSE = mse_ets, AIC = aic_ets))

# 2. Additive Seasonality (Holt-Winters via ETS)
fit_hw_add_v <- ets(train_series_v, model = "AAA")
forecast_hw_add_v <- forecast(fit_hw_add_v, h = length(train_testv$y_test))
mse_hw_add <- mse(forecast_hw_add_v$mean, train_testv$y_test)
aic_hw_add <- AIC(fit_hw_add_v)
results <- rbind(results, data.frame(Model = "Holt-Winters Additive", MSE = mse_hw_add, AIC = aic_hw_add))

# 3. Multiplicative Seasonality (Holt-Winters via ETS)
fit_hw_mult_v <- ets(train_series_v, model = "MAM")
forecast_hw_mult_v <- forecast(fit_hw_mult_v, h = length(train_testv$y_test))
mse_hw_mult <- mse(forecast_hw_mult_v$mean, train_testv$y_test)
aic_hw_mult <- AIC(fit_hw_mult_v)
results <- rbind(results, data.frame(Model = "Holt-Winters Multiplicative", MSE = mse_hw_mult, AIC = aic_hw_mult))

# 4. Regression + ETS Model
fit_reg_v <- lm(Baccala_Vicentina ~ Month + Year + fish_cons, data = trainv)
residuals_reg_v <- residuals(fit_reg_v)
fit_res_ets_v <- ets(residuals_reg_v)
forecast_res_ets_v <- forecast(fit_res_ets_v, h = length(train_testv$y_test))
mse_reg_ets <- mse(forecast_res_ets_v$mean, train_testv$y_test)
aic_reg_ets <- AIC(fit_res_ets_v)
results <- rbind(results, data.frame(Model = "Regression + ETS", MSE = mse_reg_ets, AIC = aic_reg_ets))

# Print the results
print(results)




## 4.4 GAM (Generalized Additive Model) ----
gam_model <- gam(Baccala_Mantecato ~ s(as.numeric(Month)), data = trainm)
summary(gam_model)
# Model Components:
#   Effective degrees of freedom (EDF) = 6.6, suggesting a moderately complex non-linear relationship.
# Model Fit:
#   Adjusted R-squared = 0.455, indicating that about 45.5% of the variance in Baccala_Mantecato is explained by the model.
#   Deviance explained = 54%, aligning with the R-squared value.

plot(gam_model, se = TRUE, col = "blue", main = "GAM Model", ylab = "Baccala Mantecato", xlab = "Month")
# The smooth term for Month shows a clear non-linear pattern, capturing seasonal variations.
# Confidence intervals widen in some areas, particularly near the end of the year, suggesting less data support for predictions in these months.

gam.check(gam_model)
# leggendo documentazione, sembra figo -> https://www.rdocumentation.org/packages/mgcv/versions/1.9-1/topics/gam
# The gam.check results indicate no issues with the smoothness parameter (k-index = 1.08, p-value = 0.65), confirming that the basis dimension is sufficient.

# Q-Q Plot: Deviance residuals align closely with the diagonal line, indicating that residuals are approximately normal.
# Residual vs Linear Predictor: The residuals show some structure and variance changes, but no severe departures from homoscedasticity.
# Histogram of Residuals: Residuals appear approximately symmetric, supporting normality assumptions.

gam_forecast <- predict(gam_model, newdata = trainm)
AIC(gam_model)
# AIC = 293.6, which can be compared with other models to assess relative fit.
plot_train_pred(y_train = trainm$Baccala_Mantecato,
                y_pred = gam_forecast,
                model_name = "GAM Model")
# Predictions closely track the observed values in the training data, showing the model effectively captures non-linear relationships.

res_GAM <- residuals(gam_model)
plot(res_GAM, ylab="residuals", main = "GAM Model Residuals", col = "blue")
abline(h = 0, col = "red")
# Residuals vs Index: Residuals are centered around zero, with no strong patterns indicating unmodeled trends.

dwtest(gam_model)
# Durbin-Watson Test:
# DW = 1.58 (p-value = 0.067), suggesting mild positive autocorrelation in residuals, though not statistically significant.

plot_actual_vs_forecast(
  actual = train_testm$y_test,
  forecast = predict(gam_model, newdata = train_testm$test),
  model_name = "GAM"
)
# The GAM forecast deviates from the actual test data, especially for the peaks and valleys, indicating limited extrapolation capability beyond the training range.


## 4.5 KNN ----

# Normalize a function for the data
normalize <- function(x) (x - min(x)) / (max(x) - min(x))

# Prepare data for KNN: Normalize numeric features
trainm_normalized <- trainm %>%
  mutate(across(where(is.numeric), normalize))

testm_normalized <- testm %>%
  mutate(across(where(is.numeric), normalize))

# Preprocessing: Remove the Date column and encode categorical variables
trainm_preprocessed <- trainm_normalized %>%
  select(-Date) %>%
  mutate(Month = as.numeric(Month), Year = as.numeric(as.character(Year)))

testm_preprocessed <- testm_normalized %>%
  select(-Date) %>%
  mutate(Month = as.numeric(Month), Year = as.numeric(as.character(Year)))

# Step 2: Split training data into train and validation sets
set.seed(123) # For reproducibility
train_indices <- sample(1:nrow(trainm_preprocessed), size = 0.8 * nrow(trainm_preprocessed))
validation_indices <- setdiff(1:nrow(trainm_preprocessed), train_indices)

train_split <- trainm_preprocessed[train_indices, ]
validation_split <- trainm_preprocessed[validation_indices, , drop = FALSE]
# Step 3: Tune k using validation data (including even k values)
k_values <- 1:20 # Include k from 1 to 20


results <- data.frame(k = k_values, Validation_MSE = numeric(length(k_values)))
results_test <- data.frame(k = k_values, Test_MSE = numeric(length(k_values)))

# Progress tracker
cat("Tuning KNN model with k-values:", k_values, "\n")

for (k in k_values) {
  # Train KNN model on validation set
  knn_model <- kknn(
    formula = Baccala_Mantecato ~ fish_cons,
    train = train_split,
    test = validation_split,
    k = k
  )
  
  # Predict on validation set
  preds_validation <- predict(knn_model)
  actual_validation <- validation_split$Baccala_Mantecato
  
  # Compute validation MSE
  mse_validation <- mean((actual_validation - preds_validation)^2)
  results[results$k == k, "Validation_MSE"] <- mse_validation
  
  # Predict on test set
  preds_test <- predict(knn_model, newdata = testm_preprocessed)
  actual_test <- testm_preprocessed$Baccala_Mantecato
  
  # Compute test MSE
  mse_test <- mean((actual_test - preds_test)^2)
  mae_test <- mean(abs(actual_test - preds_test))
  results_test[results_test$k == k, "Test_MSE"] <- mse_test
  
  # Print summary for current k
  cat(sprintf("k = %d | Validation MSE: %.5f | Test MSE: %.5f | Test MAE: %.5f\n", 
              k, mse_validation, mse_test, mae_test))
}

# Summary of Best Results
best_k_validation <- results$k[which.min(results$Validation_MSE)]
best_k_test <- results_test$k[which.min(results_test$Test_MSE)]
cat(sprintf("\nBest k (Validation): %d with MSE: %.5f\n", best_k_validation, min(results$Validation_MSE)))
cat(sprintf("Best k (Test): %d with MSE: %.5f\n", best_k_test, min(results_test$Test_MSE)))

# Find the best k
best_k <- results$k[which.min(results$Validation_MSE)]
cat("Best k:", best_k, "\n")
# Step 4: Train final KNN model with the best k on full training set
knn_final <- kknn(
  formula = Baccala_Mantecato ~ .,
  train = trainm_preprocessed,
  test = testm_preprocessed,
  k = best_k
)

# Step 5: Predict and evaluate on the test set
predictions <- fitted(knn_final)
actual <- testm_preprocessed$Baccala_Mantecato

mse_test <- mean((actual - predictions)^2)
mae_test <- mean(abs(actual - predictions))

cat("Test MSE:", mse_test, "\n")
cat("Test MAE:", mae_test, "\n")

# Step 6: Plot actual vs predicted
plot(actual, predictions, main = "Actual vs Predicted (KNN)",
     xlab = "Actual", ylab = "Predicted", pch = 19, col = "blue")
abline(0, 1, col = "red", lwd = 2)

# BEST K =14 with Test MSE: 0.08237638, Test MAE: 0.2608387 and Validation MSE: 0.01267

# Con K = 2
knn_final <- kknn(
  formula = Baccala_Mantecato ~ .,
  train = trainm_preprocessed,
  test = testm_preprocessed,
  k = 2
)

# Step 5: Predict and evaluate on the test set
predictions <- fitted(knn_final)
actual <- testm_preprocessed$Baccala_Mantecato

mse_test <- mean((actual - predictions)^2)
mae_test <- mean(abs(actual - predictions))

cat("Test MSE:", mse_test, "\n")
cat("Test MAE:", mae_test, "\n")

# Step 6: Plot actual vs predicted
plot(actual, predictions, main = "Actual vs Predicted (KNN)",
     xlab = "Actual", ylab = "Predicted", pch = 19, col = "blue")
abline(0, 1, col = "red", lwd = 2)




# 5. Multivariate Models ----

## 5.1 Multiple Linear Regression ----
fit_MLR <- lm(Baccala_Mantecato ~ Month + Year, data = train_testm$train)
summary(fit_MLR)
# Key Metrics:
#   Residual Standard Error: 4.539, indicating the average deviation of the predictions from the observed values
#   Multiple R-squared = 0.8298 and Adjusted R-squared = 0.7448:
#     About 83% of the variance in Baccala_Mantecato is explained by the model.
#     Adjusted R-squared reflects a slight penalty for the number of predictors, but it still suggests a strong fit.
# Significant Predictors:
#   Statistically significant coefficients:
#     Monthly effects: Months like August (p=0.002), October (p<0.001), and December (p<0.001) strongly influence the response.
#    Year effects: All year dummy variables (p<0.01) are significant, indicating a downward trend in Baccala_Mantecato across years.
#   Insignificant predictors: Some months (e.g., February, May, November) have high p-values (>0.1), suggesting their effects are not statistically meaningful.

dwtest(fit_MLR)
# DW = 1.0942, p-value = 0.00076: 
# Indicates significant positive autocorrelation in the residuals, suggesting temporal dependencies not captured by the model.

plot_train_pred(y_train = train_testm$y_train,
                y_pred = predict(fit_MLR),
                model_name = "Multiple Linear Regression")
# Predictions align well with the observed values, indicating the model fits the training data reasonably well.
# Peaks and troughs are captured, although minor discrepancies occur in highly fluctuating regions.

plot_actual_vs_forecast(
  actual = train_testm$y_test,
  forecast = predict(fit_MLR, newdata = train_testm$test),
  model_name = "Multiple Linear Regression"
)
# Forecasted values diverge significantly from actual test data for high peaks and deep troughs.
# The model seems to underestimate sharp upward trends and overestimate downward trends, indicating limitations in capturing dynamics outside the training range.

## 5.2 ARIMAX ----
fit_ARIMAX <- auto.arima(train_testm$y_train, xreg = as.numeric(train_testm$train$Month))
summary(fit_ARIMAX)
forecast_ARIMAX <- forecast(fit_ARIMAX, xreg = as.numeric(train_testm$test$Month))
plot(forecast_ARIMAX)
# The ARIMAX model predicts a linear increase in the values with wide confidence intervals. 
# This is indicative of its inability to capture the seasonality or cyclic patterns present in the data.

plot_actual_vs_forecast(
  actual = train_testm$y_test,
  forecast = as.numeric(forecast_ARIMAX$mean),
  model_name = "ARIMAX"
)
# The forecasted values are consistently below the actual test values, leading to a clear underestimation.

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

plot_actual_vs_forecast(
  actual = train_testm$y_test,
  forecast = forecast_Bass[1:length(train_testm$y_test)],  # Match test length
  model_name = "Bass Model"
)

## 6.2 Generalized Bass Model ----
GGM_model <- GGM(train_testm$y_train, prelimestimates = c(10, 0.01, 0.1, 0.01, 0.1))
summary(GGM_model)
forecast_GGM <- predict(GGM_model, newx = 1:(length(train_testm$y_train) + length(train_testm$y_test)))

plot_actual_vs_forecast(
  actual = train_testm$y_test,
  forecast = forecast_GGM[1:length(train_testm$y_test)],  # Match test length
  model_name = "Generalized Bass Model"
)

# 7. Model Comparison ----
actual <- train_testm$y_test
results <- data.frame(
  ARIMA = compute_metrics(actual, as.numeric(forecast_ARIMA$mean)),
  ES = compute_metrics(actual, as.numeric(forecast_ES$mean)),
  GAM = compute_metrics(actual, predict(gam_model, newdata = train_testm$test)),
  Boosting = compute_metrics(actual, yhat_boost),
  Bass = compute_metrics(actual, forecast_Bass)
)
print(t(results))

