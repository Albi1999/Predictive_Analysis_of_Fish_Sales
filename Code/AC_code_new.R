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

## 4.5 KNN ----

### KNN Baccala Mantecato ----

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

# Step 3: Tune k using validation data
k_values <- 1:20 # Include k from 1 to 20

results <- data.frame(k = k_values, Validation_MSE = numeric(length(k_values)))
results_train <- data.frame(k = k_values, Train_MSE = numeric(length(k_values)))

# Loop through k-values
for (k in k_values) {
  # Train KNN model on training data
  knn_model <- kknn(
    formula = Baccala_Mantecato ~ .,
    train = train_split,
    test = train_split, # Evaluate on train set for train MSE
    k = k
  )
  
  # Predict on training set
  preds_train <- predict(knn_model)
  actual_train <- train_split$Baccala_Mantecato
  
  # Compute Train MSE
  mse_train <- mean((actual_train - preds_train)^2)
  results_train[results_train$k == k, "Train_MSE"] <- mse_train
  
  # Train KNN model on validation set
  knn_model_val <- kknn(
    formula = Baccala_Mantecato ~ .,
    train = train_split,
    test = validation_split, # Evaluate on validation set for validation MSE
    k = k
  )
  
  # Predict on validation set
  preds_validation <- predict(knn_model_val)
  actual_validation <- validation_split$Baccala_Mantecato
  
  # Compute Validation MSE
  mse_validation <- mean((actual_validation - preds_validation)^2)
  results[results$k == k, "Validation_MSE"] <- mse_validation
}

# Combine Train and Validation Results for Plotting
results_combined <- merge(results, results_train, by = "k")

# Plot Train vs Validation MSE
ggplot(results_combined, aes(x = k)) +
  geom_line(aes(y = Train_MSE, color = "Train MSE"), size = 1) +
  geom_line(aes(y = Validation_MSE, color = "Validation MSE"), size = 1) +
  labs(title = "Train vs Validation MSE for KNN (Baccala Mantecato)",
       x = "k (Number of Neighbors)",
       y = "Mean Squared Error",
       color = "Legend") +
  theme_minimal()

# Select the best k based on validation MSE
best_k <- 5
cat(sprintf("Selected K: %d\n", best_k))

# Final Evaluation on Test Set with Best k
knn_final <- kknn(
  formula = Baccala_Mantecato ~ .,
  train = trainm_preprocessed,
  test = testm_preprocessed,
  k = best_k
)

# Predictions on Test Set
predictions <- predict(knn_final)
actual <- testm_preprocessed$Baccala_Mantecato

# Test MSE and MAE
mse_test <- mse(actual, predictions)
mae_test <- mean(abs(actual - predictions))
mse_validation <- results[results$k == best_k, "Validation_MSE"]

cat(sprintf("Test MSE: %.5f\n", mse_test))
cat(sprintf("Test MAE: %.5f\n", mae_test))
cat(sprintf("Validation MSE: %.5f\n", mse_validation))

# Plot Actual vs Predicted
plot(actual, predictions, main = "Actual vs Predicted (KNN for Baccala Mantecato)",
     xlab = "Actual", ylab = "Predicted", pch = 19, col = "blue")
abline(0, 1, col = "red", lwd = 2)

### KNN Baccala Vicentina ----
# Step 1: Preprocessing the Data
normalize <- function(x) (x - min(x)) / (max(x) - min(x))

# Normalize the features
trainv_normalized <- trainv %>%
  mutate(across(where(is.numeric), normalize))

testv_normalized <- testv %>%
  mutate(across(where(is.numeric), normalize))

# Preprocessing: Remove the Date column and encode categorical variables
trainv_preprocessed <- trainv_normalized %>%
  select(-Date) %>%
  mutate(Month = as.numeric(Month), Year = as.numeric(as.character(Year)))

testv_preprocessed <- testv_normalized %>%
  select(-Date) %>%
  mutate(Month = as.numeric(Month), Year = as.numeric(as.character(Year)))

# Step 2: Split Training Data into Train and Validation Sets
set.seed(123)
train_indices <- sample(1:nrow(trainv_preprocessed), size = 0.8 * nrow(trainv_preprocessed))
validation_indices <- setdiff(1:nrow(trainv_preprocessed), train_indices)

train_split <- trainv_preprocessed[train_indices, ]
validation_split <- trainv_preprocessed[validation_indices, ]

# Step 3: Tune k using Validation Data
k_values <- 1:20
# Initialize a data frame to store results for both training and validation MSE
results <- data.frame(k = k_values, Validation_MSE = numeric(length(k_values)), Train_MSE = numeric(length(k_values)), stringsAsFactors = FALSE)

for (k in k_values) {
  # Train KNN model on training data
  knn_model <- kknn(
    formula = Baccala_Vicentina ~ .,
    train = train_split,
    test = validation_split,
    k = k
  )
  
  # Predict on validation set
  preds_validation <- predict(knn_model)
  actual_validation <- validation_split$Baccala_Vicentina
  
  # Compute Validation MSE
  mse_validation <- mean((actual_validation - preds_validation)^2)
  results[results$k == k, "Validation_MSE"] <- mse_validation
  
  # Predict on training set for the same k
  preds_train <- predict(knn_model, newdata = train_split)
  actual_train <- train_split$Baccala_Vicentina
  
  # Compute Training MSE
  mse_train <- mean((actual_train - preds_train)^2)
  results[results$k == k, "Train_MSE"] <- mse_train
}

# Plot Train and Validation MSE
ggplot(results, aes(x = k)) +
  geom_line(aes(y = Train_MSE, color = "Train MSE"), size = 1) +
  geom_line(aes(y = Validation_MSE, color = "Validation MSE"), size = 1) +
  labs(
    title = "Train vs Validation MSE for KNN (Baccalà Vicentina)",
    x = "k (Number of Neighbors)",
    y = "Mean Squared Error",
    color = "Legend"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

# Find the Best k
best_k <- 5

# Step 4: Train Final Model with Best k
knn_final <- kknn(
  formula = Baccala_Vicentina ~ .,
  train = trainv_preprocessed,
  test = testv_preprocessed,
  k = best_k
)

# Step 5: Evaluate Final Model
predictions <- predict(knn_final)
actual <- testv_preprocessed$Baccala_Vicentina

mse_test <- mse(actual, predictions)
mae_test <- mean(abs(actual - predictions))
mse_validation <- results[results$k == best_k, "Validation_MSE"]

cat(sprintf("Test MSE: %.5f\n", mse_test))
cat(sprintf("Test MAE: %.5f\n", mae_test))
cat(sprintf("Validation MSE: %.5f\n", mse_validation))

# Optional: Plot Actual vs Predicted
plot(actual, predictions, main = "Actual vs Predicted (KNN for Baccalà Vicentina)",
     xlab = "Actual", ylab = "Predicted", pch = 19, col = "blue")
abline(0, 1, col = "red", lwd = 2)

## Local Regression ----

### Local Regression Baccala Mantecato ----

# Ensure data_copy exists as a copy of your dataset
data_copy <- data

# Add a time index to the copied dataset
data_copy$tt <- seq_len(nrow(data_copy))

# Perform a chronological train-test split (90% train, 10% test)
split_index <- floor(0.9 * nrow(data_copy))  # 90% split
trainm <- data_copy[1:split_index, ]
testm <- data_copy[(split_index + 1):nrow(data_copy), ]

# Fit Loess Model on the training data only
best_span <- 0.3  # Adjust span based on prior tuning
fit_loess <- loess(Baccala_Mantecato ~ tt, data = trainm, span = best_span)

# Generate Loess predictions for the training set
trainm$loess_fitted <- predict(fit_loess)

# Generate Loess predictions for the test set
testm$loess_forecast <- predict(fit_loess, newdata = data.frame(tt = testm$tt))

# Handle NA predictions for points outside the training range using a linear model
na_indices <- is.na(testm$loess_forecast)
if (any(na_indices)) {
  linear_model <- lm(Baccala_Mantecato ~ tt, data = trainm)
  testm$loess_forecast[na_indices] <- predict(linear_model, newdata = data.frame(tt = testm$tt[na_indices]))
}

# Combine train and test data into a single dataset for plotting
plot_data <- bind_rows(
  trainm %>% mutate(data_type = "Train Observed"),
  testm %>% mutate(data_type = "Test Observed")
)

# Plot Results
ggplot() +
  geom_line(data = plot_data %>% filter(data_type == "Train Observed"),
            aes(x = Date, y = Baccala_Mantecato, color = "Train Observed")) +
  geom_line(data = plot_data %>% filter(data_type == "Test Observed"),
            aes(x = Date, y = Baccala_Mantecato, color = "Test Observed")) +
  geom_line(data = trainm, aes(x = Date, y = loess_fitted, color = "Loess Fitted")) +
  scale_color_manual(values = c("Train Observed" = "#6BC3FF", 
                                "Test Observed" = "#FF7F7F", 
                                "Loess Fitted" = "#8FBC8F")) +
  labs(title = "Loess Fit for Baccala Mantecato",
       x = "Date", y = "Baccala Mantecato") +
  theme_minimal()

# Calculate MSE for Loess Fit on Test Data
valid_forecast <- !is.na(testm$Baccala_Mantecato)
loess_mse <- mean((testm$Baccala_Mantecato[valid_forecast] - testm$loess_forecast[valid_forecast])^2)
cat(sprintf("Loess MSE on Test Data (Baccala Mantecato): %.5f\n", loess_mse))

### Local Regression Baccala Vicentina ----

# Ensure data_copy exists as a copy of your dataset
data_copy <- data

# Add a time index to the copied dataset
data_copy$tt <- seq_len(nrow(data_copy))

# Perform a chronological train-test split (90% train, 10% test)
split_index <- floor(0.9 * nrow(data_copy))  # 90% split
trainv <- data_copy[1:split_index, ]
testv <- data_copy[(split_index + 1):nrow(data_copy), ]

# Fit Loess Model on the training data only
best_span <- 0.3  # Adjust span based on prior tuning
fit_loess <- loess(Baccala_Vicentina ~ tt, data = trainv, span = best_span)

# Generate Loess predictions for the training set
trainv$loess_fitted <- predict(fit_loess)

# Generate Loess predictions for the test set
testv$loess_forecast <- predict(fit_loess, newdata = data.frame(tt = testv$tt))

# Handle NA predictions for points outside the training range using a linear model
na_indices <- is.na(testv$loess_forecast)
if (any(na_indices)) {
  linear_model <- lm(Baccala_Vicentina ~ tt, data = trainv)
  testv$loess_forecast[na_indices] <- predict(linear_model, newdata = data.frame(tt = testv$tt[na_indices]))
}

# Combine train and test data into a single dataset for plotting
plot_data <- bind_rows(
  trainv %>% mutate(data_type = "Train Observed"),
  testv %>% mutate(data_type = "Test Observed")
)

# Plot Results
ggplot() +
  geom_line(data = plot_data %>% filter(data_type == "Train Observed"),
            aes(x = Date, y = Baccala_Vicentina, color = "Train Observed")) +
  geom_line(data = plot_data %>% filter(data_type == "Test Observed"),
            aes(x = Date, y = Baccala_Vicentina, color = "Test Observed")) +
  geom_line(data = trainv, aes(x = Date, y = loess_fitted, color = "Loess Fitted")) +
  scale_color_manual(values = c("Train Observed" = "#6BC3FF", 
                                "Test Observed" = "#FF7F7F", 
                                "Loess Fitted" = "#8FBC8F")) +
  labs(title = "Loess Fit for Baccala Vicentina",
       x = "Date", y = "Baccala Vicentina") +
  theme_minimal()

# Calculate MSE for Loess Fit on Test Data
valid_forecast <- !is.na(testv$Baccala_Vicentina)
loess_mse <- mean((testv$Baccala_Vicentina[valid_forecast] - testv$loess_forecast[valid_forecast])^2)
cat(sprintf("Loess MSE on Test Data (Baccala Vicentina): %.5f\n", loess_mse))
