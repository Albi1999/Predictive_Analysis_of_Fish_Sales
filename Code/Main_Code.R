# Business Economic and Financial Data
# Main Code

# Package Loading and General Configuration ----
rm(list=ls())

library(readxl)
library(readr)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(lmtest)
library(mgcv)
library(forecast)
library(lmtest)
library(gam)

source("Code/Functions_Utilies.R")

# Data Loading and Preprocessing ----
data <- read_csv(file.path("Data/data.csv"))
data$Date <- as.Date(data$Date, format = "%Y-%m-%d") # ensure date format
data <- data %>% mutate(Month = format(Date, "%m"),
                        Year = as.factor(format(Date, "%Y"))) # create useful features for the models
data$trend <- 1:nrow(data)

head(data,3)

# Check for missing values
colSums(is.na(data))

# Then we load a dataset found on [EUMOFA](https://eumofa.eu/web/guest/bulk-download).
# We also tried incorporating different variables, such as the NIC for fish products (Istat), 
# the production price of fish products (Istat), and others. 
# However, these variables did not prove to be significantly relevant for our analysis.
# Additionally, some other variables we tried do not have monthly data 
# that match the frequency of the sales data we are working with, 
# limiting their usefulness in the context of our time series analysis.

# Here we focus on the salmon consumption only because we have seen that 
# other fishes or their aggregate value lead worst results.

fish_cons <- read_excel(file.path("Data/Fish_consumption_ita_raw.xlsx")) %>%
  filter(CG == "Salmon") %>%
  mutate(kg = as.numeric(`volume(Kg)`)) %>%
  group_by(year, month) %>%
  summarise(kg = sum(kg), .groups = "drop") %>%
  filter(year > 2020 | (year == 2020 & month %in% c(11, 12))) %>%
  mutate(
    kg_std = scale(kg),
    Date = as.Date(paste(year, month, "01", sep = "-"))
  )  %>%
  select(Date, kg, kg_std)

head(fish_cons, 3)

# The dataset contains three columns:
#   Date: Unlike the data related to baccalà, the time series for salmon starts from November 2020. 
#         This is because we lack information for the last two months of 2024, so we applied a two-month lag. 
#         This choice also makes sense for future applications, as it would not be possible to forecast baccalà sales 
#         (whether mantecato or alla vicentina) for December when monthly consumption data for that month is already available.
#   kg_std: Represents the standardized version of the kg column. 
#           Since the values in kg are quite large, we opted to standardize them to simplify further analysis.
#   kg: Represents the quantity of salmon sold in Italy, measured in kilograms.

# Finally we aggregate the salmon monthly consumption time series to our data.
data$fish_cons <- fish_cons$kg_std[,1]
head(data,3)

# Explanatory Analysis ----

# First, we visualize the time series of Baccala Mantecato and Baccala Vicentina over time.
# This plot helps us compare the trend of sales for both products on a monthly basis.

ggplot(data, aes(x = Date)) +
  geom_line(aes(y = Baccala_Mantecato, color = "Baccala Mantecato"), size = 1) +
  geom_line(aes(y = Baccala_Vicentina, color = "Baccala Vicentina"), size = 1) +
  labs(title = "Monthly Time Series of Baccala Mantecato and Baccala Vicentina",
       x = "Date",
       y = "Quantity") +
  scale_color_manual(values = c("Baccala Mantecato" = "#FF7F7F", "Baccala Vicentina" = "#6BC3FF")) +
  theme_minimal() +
  theme(legend.position = "bottom")

# The sales quantities of Baccala Mantecato and Baccala Vicentina show significant differences, 
# with Baccala Mantecato consistently having a much higher volume of sales throughout all periods observed. 
# Additionally, Baccala Mantecato exhibits a much wider range of values, indicating greater variability in sales. 
# In contrast, Baccala Vicentina appears more stable, with sales peaks typically occurring towards the end of the year. 
# A similar trend is also seen for Baccala Mantecato, which experiences an uptick in sales during the final months of each year.

# Next, we plot the time series for both products grouped by year.
# The goal is to observe yearly trends and patterns for Baccala Mantecato and Baccala Vicentina separately.

plot1 <- ggplot(data, aes(x = Month, y = Baccala_Mantecato, color = Year, group = Year)) +
  geom_line() + 
  labs(x = "Month", y = "Baccala Mantecato", 
       title = "Monthly Time Series of Baccala Mantecato by Year") +
  theme_minimal() +
  theme(legend.title = element_blank())

plot2 <- ggplot(data, aes(x = Month, y = Baccala_Vicentina, color = Year, group = Year)) +
  geom_line() + 
  labs(x = "Month", y = "Baccala Vicentina", 
       title = "Monthly Time Series of Baccala Vicentina by Year") +
  theme_minimal() +
  theme(legend.title = element_blank())

grid.arrange(plot1, plot2, nrow = 2)

# Both graphs highlight the pattern mentioned earlier: for each year, 
# there is an increase in sales during the final months of the year, 
# particularly in September and December. 
# Additionally, regarding Baccala Mantecato, it appears that during the earlier 
# years of sales for which we have data, the quantity sold was generally higher in the off-peak months 
# (for example, the orange line for 2021 is higher compared to the other years). 
# However, in the later years, the quantity sold during the peak months has increased.

# Finally, we also examined the properties of the time series by plotting the autocorrelation functions (ACF).

ym = ts(data$Baccala_Mantecato, frequency = 12, start = c(2021, 1))
yv = ts(data$Baccala_Vicentina, frequency = 12, start = c(2021, 1))

if (end(ym)[1] != 2024 || end(ym)[2] != 12) {
  print("Error in ts ym")
}

if (end(yv)[1] != 2024 || end(yv)[2] != 12) {
  print("Error in ts yv")
}

Acf(data$Baccala_Mantecato, main = "ACF of Baccala Mantecato", col = "#FF7F7F", lwd = 2)
Acf(data$Baccala_Vicentina, main = "ACF of Baccala Vicentina", col = "#6BC3FF", lwd = 2)

# We now that autocorrelation occurs when the effect of a avriable is spread over time, 
# in these cases, most of the autocorrelations fall within the confidence bands, 
# indicating that the data does not show significant correlation for most lags. 
# However, within the bands, the autocorrelations exhibit a sinusoidal pattern, 
# suggesting the presence of seasonality in the data, where periodic fluctuations occur over time. 
# The peak at lag 12 further supports the idea of a cyclical effect.
# We will analyze the residuals of future models to confirm or disprove the presence of this seasonality.

# Train & Test Split ----

# In this section, we perform a train-test split to prepare the data for model training and evaluation. 
# We divide the time series data for both Baccala Mantecato and Baccala Vicentina into training and testing sets, 
# with 80% of the data allocated for training and the remaining 20% for testing.

prop <- 0.8

n_sample <- floor(nrow(data) * prop)

train <- data[1:n_sample, ]
y_trainm <- train[["Baccala_Mantecato"]]
y_trainv <- train[["Baccala_Vicentina"]]

test <- data[(n_sample + 1):nrow(data), ]

y_testm <- test[["Baccala_Mantecato"]]
y_testv <- test[["Baccala_Vicentina"]]

Type = c(rep("Train", dim(train)[1]), rep("Test", dim(test)[1]))
ggplot(data, aes(x = trend, y = Baccala_Mantecato, color = Type)) +
  geom_line(size = 1) +
  scale_color_manual(values = c("#FF7F7F", "#6BC3FF")) +
  labs(title = "Train and Test Data for Baccala Mantecato",
       x = "Time",
       y = "Quantity sold of Baccala Mantecato") +
  theme_minimal() +
  theme(legend.position = "bottom")

ggplot(data, aes(x = trend, y = Baccala_Vicentina, color = Type)) +
  geom_line(size = 1) +
  scale_color_manual(values = c("#FF7F7F", "#6BC3FF")) +
  labs(title = "Train and Test Data for Baccala Vicentina",
       x = "Time",
       y = "Quantity sold of Baccala Vicentina") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Modelling Phase ----

## Linear Regression Model ----

### Baccala Mantecato ----
# We start by estimating a simple linear regression model with all the possible variables

lr_m <- lm(Baccala_Mantecato ~ trend + Month + fish_cons, data = train)
summary(lr_m)

# The Multiple R-squared value of 0.9178 indicates that approximately 
# 91.78% of the variability in Baccala_Mantecato is explained by the model.

# Then we take a closer look to the coefficents:
#   - The trend variable has a positive coefficient of 0.18, meaning that, on average, 
#     Baccala_Mantecato increases by 0.18 units for each time period, assuming all other factors remain constant.
#   - The coefficient for fish_cons is 7.79, indicating a strong positive relationship. 
#     Specifically, for each unit increase in fish consumption, Baccala_Mantecato rises by approximately 7.79 units. 
#     In particular we will provide an exemple: if the fish consumption in Italy increases by a unit in October 2021, 
#     then keeping all the other variables fixed, the quantity sold in December 2021 will increase of 7.79 kg.
#   - Regarding the Month variables, they capture seasonal effects by representing deviations from the baseline month (January). 
#     Some months show statistically significant effects (e.g., February, August, September, October, and December. 
#     For example, December shows a substantial positive effect, indicating a peak in that month, 
#     while months like February and October have negative coefficients, reflecting significant declines compared to January.
                                                                                                                           
# Now we try removing variables to evaluate their impact on model performance using AIC and adjusted R² values.

lr_m_month <- lm(Baccala_Mantecato ~ trend + fish_cons, data = train)
lr_m_trend <- lm(Baccala_Mantecato ~ Month + fish_cons, data = train)
lr_m_fish_cons <- lm(Baccala_Mantecato ~ trend + Month, data = train)

cat("Model with trend and fish consumption:\n")
cat("  AIC:", AIC(lr_m_month), "\n")
cat("  Adjusted R²:", summary(lr_m_month)$adj.r.squared, "\n\n")

cat("Model with trend and month:\n")
cat("  AIC:", AIC(lr_m_fish_cons), "\n")
cat("  Adjusted R²:", summary(lr_m_fish_cons)$adj.r.squared, "\n\n")

cat("Model with month and fish consumption:\n")
cat("  AIC:", AIC(lr_m_trend), "\n")
cat("  Adjusted R²:", summary(lr_m_trend)$adj.r.squared, "\n\n")

cat("Full model:\n")
cat("  AIC:", AIC(lr_m), "\n")
cat("  Adjusted R²:", summary(lr_m)$adj.r.squared, "\n")

# As we can expect by the summary of the full model, from the AIC and Adjusted R² results 
# we clearly see that the best model fit belong to the full model with all predictors: 
# trend, monthly seasonality and fish consumption.

# Finally we will analyze the residuals.
resid_lr <- residuals(lr_m)
plot(resid_lr)

dwtest(lr_m)

# When we analyze the residuals of the linear regression model, as shown in the plot below, 
# we observe no particular patterns. 
# The residuals appear to be randomly scattered, indicating that the model has appropriately 
# captured the underlying trends in the data. This suggests that the assumptions of linearity, 
# constant variance, and independence are reasonably satisfied, and the model's fit is adequate for forecasting purposes. 
# We also perform the Durbin-Watson test, that is used to check for autocorrelation in the residuals of a regression model.
# Since the p-value is smaller than the significance level (0.05), we don't reject the null hypothesis 
# that the autocorrelation of the disturbances is 0.

# Finally we plot the predicted values and the actual ones. We also compute the MSE on the test set.
ggplot() +
 geom_line(aes(x = data$Date, y = data$Baccala_Mantecato, color = "Actual Values"), size = 1) +
 geom_line(aes(x = train$Date, y = fitted(lr_m), color = "Fitted Values (Train)"), size = 1) +
 geom_line(aes(x = test$Date, y = predict(lr_m, newdata = test), color = "Predicted Values (Test)"), size = 1) + 
 scale_color_manual(values = c("Actual Values" = "#FF7F7F", 
                               "Fitted Values (Train)" = "#6BC3FF", 
                               "Predicted Values (Test)" = "#8FBC8F"),
                    labels = c("Actual Values", "Fitted Values (Train)", "Predicted Values (Test)")) +
 
 labs(
   title = "Actual vs Fitted and Predicted Values for Baccala Mantecato (Linear Regression Model)",
   x = "Date",
   y = "Value",
   color = "Legend"
 ) +
 theme_minimal() +
 theme(legend.position = "bottom")

mse_lrm <- mse(predict(lr_m, newdata = test), test$Baccala_Mantecato)
print(mse_lrm)

### Baccala Vicentina ----

# The same analysis is conducted below for the Baccala Vicentina variable.
lr_v_full <- lm(Baccala_Vicentina ~ trend + Month + fish_cons, data = train)
summary(lr_v_full)

# This time the fish consumption seems to be not useful to imporve the model pefromance so we remove it
lr_v <- update(lr_v_full, .~. - fish_cons)
summary(lr_v)

# The adjusted R² increses from 0.8261 to 0.8305 suggesting that the reduced model is better than the full one. 
# Furthermore, the trend variable shows a large p-value, suggesting that it can be removed from the model 
# without significantly affecting the results.
lr_v <- update(lr_v, .~. - trend)
summary(lr_v)

# The best model for the Baccala Vicentina series is the one that includes only the monthly seasonality. 
# This model provides the best fit, indicating that the variations in the data are primarily driven by seasonal effects, 
# without the need for additional trend or other predictors.
# Moreover, the fitted model is statistically significant, as shown by the F-statistic of 18.21, with a p-value of 1.684e-09, 
# confirming the model's overall significance.

# Below, we compare the difference in AIC and adjusted R² between the full model and the one with only the monthly seasonality.
cat("Model with month:\n")
cat("  AIC:", AIC(lr_v), "\n")
cat("  Adjusted R²:", summary(lr_v)$adj.r.squared, "\n\n")

cat("Full model:\n")
cat("  AIC:", AIC(lr_v_full), "\n")
cat("  Adjusted R²:", summary(lr_v_full)$adj.r.squared, "\n")

# The model with only the monthly seasonality performs better than the full model in terms of both AIC and adjusted R².

# Finally we will analyze the residuals.
resid_lr <- residuals(lr_v)
plot(resid_lr)
dwtest(lr_v)

# When we analyze the residuals of the linear regression model, as shown in the plot below, we observe no particular patterns. 
# The residuals appear to be randomly scattered, indicating that the model has appropriately captured the underlying 
# trends in the data. This suggests that the assumptions of linearity, constant variance, and independence are 
# reasonably satisfied, and the model's fit is adequate for forecasting purposes.
# On the other hand we perform the Durbin-Watson test, that is used to check for autocorrelation in the residuals of a 
# regression model.mSince the p-value is greater than the significance level (0.05), we reject the null hypothesis.

# Finally we plot the predicted values and the actual ones. We also compute the MSE on the test set.

ggplot() +
 geom_line(aes(x = data$Date, y = data$Baccala_Vicentina, color = "Actual Values"), size = 1) +
 geom_line(aes(x = train$Date, y = fitted(lr_v), color = "Fitted Values (Train)"), size = 1) +
 geom_line(aes(x = test$Date, y = predict(lr_v, newdata = test), color = "Predicted Values (Test)"), size = 1) + 
 scale_color_manual(values = c("Actual Values" = "#FF7F7F", 
                               "Fitted Values (Train)" = "#6BC3FF", 
                               "Predicted Values (Test)" = "#8FBC8F"),
                    labels = c("Actual Values", "Fitted Values (Train)", "Predicted Values (Test)")) +
 labs(
   title = "Actual vs Fitted and Predicted Values for Baccala Vicentina (Linear Regression Model)",
   x = "Date",
   y = "Value",
   color = "Legend"
 ) +
 theme_minimal() +
 theme(legend.position = "bottom")

mse_lrv <- mse(predict(lr_v, newdata = test), test$Baccala_Vicentina)
print(mse_lrv)

## ARIMA Model ----

#### Baccala Mantecato ----

# To begin, we first transform the sales data of Baccala Mantecato into a time series object:
ts_m <- ts(train$Baccala_Mantecato, start = c(2021, 01), frequency = 12)
plot.ts(ts_m)

# From the plot above and the significance of the trend coefficient in the regression model discussed in the previous section, 
# we might consider taking the first difference to observe the behavior of the ACF and PACF.
ts_m1 <- diff(ts_m,1)
Acf(ts_m1)
Pacf(ts_m1)

# Both the ACF and PACF plots show a sinusoidal pattern, with all values falling within the confidence bands, 
# except for a significant spike at lag 12. 
# This suggests that a SARIMA model with a seasonal period s=12, accounting for monthly data with yearly seasonality, 
# might be appropriate for this time series.
ts_m_12 <- diff(ts_m, lag = 12)
Acf(ts_m_12)
Pacf(ts_m_12)

# Now, we will build three SARIMA models. 
# The first model will incorporate a non-seasonal differencing (with d=1), one autoregressive term (AR(1)), 
# and a seasonal differencing with a period of 12 (to account for the yearly seasonality). 
# The second model will instead include a moving average (MA(1)) term along with the differencing. 
# The last one only incorporate the differencing.

# SARIMA (1,1,0)(0,1,0)[12]
sarima_model1m <- Arima(ts_m, order=c(1,1,0), seasonal=c(0,1,0))
summary(sarima_model1m)

# SARIMA (0,1,1)(0,1,0)[12]
sarima_model2m <- Arima(ts_m, order=c(0,1,1), seasonal=c(0,1,0))
summary(sarima_model2m)

# SARIMA (0,1,0)(0,1,0)[12]
sarima_model3m <- Arima(ts_m, order=c(0,1,0), seasonal=c(0,1,0))
summary(sarima_model3m)

mse(test$Baccala_Mantecato, forecast(sarima_model1m, h = length(y_testm))$mean)
mse(test$Baccala_Mantecato, forecast(sarima_model2m, h = length(y_testm))$mean)
mse(test$Baccala_Mantecato, forecast(sarima_model3m, h = length(y_testm))$mean)

# Based on the AIC values, the SARIMA(0,1,1)(0,1,0)[12] model is the better model. 
# It has a lower AIC compared to the other SARIMA models. 
# By the way, evaluating the performance on the test set, we notice that the MSE is larger for the AIC best model, 
# while the SARIMA(0,1,0)(0,1,0)[12] perform better on the test set. We decide to continue whit the last one.

ggplot() +
  geom_line(aes(x = data$Date, y = data$Baccala_Mantecato, color = "Actual Values"), size = 1) +
  geom_line(aes(x = train$Date, y = fitted(sarima_model3m), color = "Train Fitted Values (SARIMA)"), size = 1) +
  geom_line(aes(x = test$Date, y = forecast(sarima_model3m, h = nrow(test))$mean, 
                color = "Test Predicted Values (SARIMA)"), size = 1) +
  labs(
    title = "Time Series: Actual vs Predicted Values (SARIMA Model)",
    x = "Date",
    y = "Value",
    color = "Legend"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom", text = element_text(size = 12))

mse_test_sarimam <- mse(test$Baccala_Mantecato, forecast(sarima_model3m, h = length(y_testm))$mean)

resid3 <- residuals(sarima_model3m)
mse_train_sarimam <- mean(resid3^2)
checkresiduals(sarima_model3m)
Pacf(resid3)

# Residuals does not suggest a really good fit but, as said before, the best test performance are reached by the selected model.
# Improving the model with AR o MA terms lead us to overfitting.

#### Baccala Vicentina ----

# To begin, we first transform the sales data of Baccalá Vicentina into a time series object:
ts_v <- ts(train$Baccala_Vicentina, start = c(2021, 01), frequency = 12)
plot.ts(ts_v)

Acf(ts_v)
Pacf(ts_v)
# From the plot above we clearly notice the presence of seasonality that is confirmed by the pacf and acf functions 
# with a significant spike at lag 12.

ts_v_12 <- diff(ts_v, lag = 12)
Acf(ts_v_12)
Pacf(ts_v_12)
# This suggests that a SARIMA model with a seasonal period s=12, accounting for monthly data with yearly seasonality, 
# might be appropriate for this time series.

# Now, we will build a SARIMA model model that incorporate only a seasonal differencing with a period of 12.
# The parameter D=2 lead us to lower AIC but a bigger mse, this is a case of overfitting.

auto_sarima_modelv <- auto.arima(ts_v)
summary(auto_sarima_modelv)

# SARIMA(0,0,0)(0,1,0)[12]
sarima_model2v <- Arima(ts_v, order=c(0,0,0), seasonal=c(0,1,0))
summary(sarima_model2v)

resid2 <- residuals(sarima_model2v)
Acf(resid2)
Pacf(resid2)

# The best model based on AIC is supposed to be the SARIMA(0,0,0)(0,1,0)[12].

mse_train_sarimav <- mean(resid2^2)
mse_test_sarimav <- mse(test$Baccala_Vicentina, forecast(sarima_model2v, h = length(y_testv))$mean)
# Note: We also tried different configurations, expecially after we saw the residuals plot but at the end, 
# this remain the best model possible.

ggplot() +
  geom_line(aes(x = data$Date, y = data$Baccala_Vicentina, color = "Actual Values"), size = 1) +
  geom_line(aes(x = train$Date, y = fitted(sarima_model2v), color = "Train Fitted Values (SARIMA)"), size = 1) +
  geom_line(aes(x = test$Date, y = forecast(sarima_model2v, h = nrow(test))$mean, 
                color = "Test Predicted Values (SARIMA)"), size = 1) +
  labs(
    title = "Time Series: Actual vs Predicted Values (SARIMA Model)",
    x = "Date",
    y = "Value",
    color = "Legend"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom", text = element_text(size = 12))

## ARMAX Model ----

# Now following the same reasoning of SARIMA models, 
# we compare to them the same models but also using the fish consumption information as xreg, 
# to see if it improves our results

### Baccalà mantecato ----

sarimax_model1m <- Arima(ts_m, order = c(1, 1, 0), seasonal = c(0, 1, 0), xreg = train$fish_cons)
sarimax_model2m <- Arima(ts_m, order = c(0, 1, 1), seasonal = c(0, 1, 0), xreg = train$fish_cons)
sarimax_model3m <- Arima(ts_m, order = c(0, 1, 0), seasonal = c(0, 1, 0), xreg = train$fish_cons)

# Once we have fitted the models, we compare them to the previous SARIMA considering the AIC as metric

AIC(sarima_model1m, sarimax_model1m)
AIC(sarima_model2m, sarimax_model2m)
AIC(sarima_model3m, sarimax_model3m)

# It is clear that using the fish consumption data we improve the models, but as we've seen for SARIMAs, 
# considering only AIC may not be enough, so now we also look at the MSEs

mse(test$Baccala_Mantecato, forecast(sarima_model1m, h = length(y_testm))$mean)
mse(test$Baccala_Mantecato, forecast(sarimax_model1m, h = length(y_testm),xreg = test$fish_cons)$mean)

mse(test$Baccala_Mantecato, forecast(sarima_model2m, h = length(y_testm))$mean)
mse(test$Baccala_Mantecato, forecast(sarimax_model2m, h = length(y_testm),xreg = test$fish_cons)$mean)

mse(test$Baccala_Mantecato, forecast(sarima_model3m, h = length(y_testm))$mean)
mse(test$Baccala_Mantecato, forecast(sarimax_model3m, h = length(y_testm),xreg = test$fish_cons)$mean)

# We see that in terms of MSE, SARIMAX brings to huge improvements for all 3 the considered models 
# reducing the MSEs by approximately 50%. 
# Following the same reasoning as before, and considering both the AIC and the MSE, 
# among the models we select the ARMAX(0,1,0)(0,1,0)[12].

# Now we look at that model prediction on the test set compared to the respective original SARIMA

ggplot() +
  geom_line(aes(x = data$Date, y = data$Baccala_Mantecato, color = "Actual Values"), size = 1) +
  geom_line(aes(x = test$Date, y = forecast(sarima_model3m, h = nrow(test))$mean, 
                color = "Predicted Values (Test) SARIMA"), size = 1) +
  geom_line(aes(x = test$Date, y = forecast(sarimax_model3m, h = nrow(test), xreg = test$fish_cons)$mean, 
                color = "Predicted Values (Test) SARIMAX"), size = 1) +
  scale_color_manual(values = c("Actual Values" = "black", 
                                "Predicted Values (Test) SARIMA" = "#6BC3FF",
                                "Predicted Values (Test) SARIMAX" = "#FF7F7F"),
                     labels = c("Actual Values", "Predicted Values (Test) SARIMA", "Predicted Values (Test) SARIMAX")) +
  labs(
    title = "Actual vs Predicted Values for Baccala Mantecato (SARIMA vs SARIMAX)",
    x = "Date",
    y = "Value",
    color = "Legend"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

# From the plot we clearly see the improvement obtained with the SARIMAX version, 
# confirming that the fish_cons variable helps on predicting future orders quantity.

# Now we see the residuals 
resid3_sarimax <- residuals(sarimax_model3m)
Acf(resid3_sarimax, main = "ACF for SARIMAX Residuals of Baccala Mantecato", col = "#FF7F7F", lwd = 2)
Pacf(resid3_sarimax, main = "PACF for SARIMAX Residuals of Baccala Mantecato", col = "#6BC3FF", lwd = 2)

# From the acf we have the doubt that we are missing an autoregressive component, 
# but including it leads to lower performances on the test set, so we decide to accept that loss 
# of information showed in the residuals to avoid overfitting.

### Baccala Vicentina ----
# Following the same approach, we try to study the effects of considering fish_cons on Baccala_vicentina 
# and compare it to the previously selected SARIMA(0,1,1)(0,1,0)[12] model

sarimax_model2v <- Arima(ts_v, order = c(0, 1, 1), seasonal = c(0, 1, 0), xreg = train$fish_cons)

AIC(sarima_model2v, sarimax_model2v)

mse(test$Baccala_Vicentina, forecast(sarima_model2v, h = length(y_testv))$mean)
mse(test$Baccala_Vicentina, forecast(sarimax_model2v, h = length(y_testv), xreg = test$fish_cons)$mean)

# Surprisingly for the Baccala_Vicentina, the originally selected SARIMA model performs better than 
# the SARIMAX one considering both AIC and MSE, so we discard it and select the original SARIMA.

ggplot() +
  geom_line(aes(x = data$Date, y = data$Baccala_Vicentina, color = "Actual Values"), size = 1) +
  geom_line(aes(x = test$Date, y = forecast(sarima_model2v, h = nrow(test))$mean, 
                color = "Predicted Values (Test) SARIMA"), size = 1) +
  geom_line(aes(x = test$Date, y = forecast(sarimax_model2v, h = nrow(test), xreg = test$fish_cons)$mean, 
                color = "Predicted Values (Test) SARIMAX"), size = 1) +
  scale_color_manual(values = c("Actual Values" = "black", 
                                "Predicted Values (Test) SARIMA" = "#6BC3FF",
                                "Predicted Values (Test) SARIMAX" = "#FF7F7F"),
                     labels = c("Actual Values", "Predicted Values (Test) SARIMA", "Predicted Values (Test) SARIMAX")) +
  labs(
    title = "Actual vs Predicted Values for Baccala Vicentina (SARIMA vs SARIMAX)",
    x = "Date",
    y = "Value",
    color = "Legend"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

# As we can see from the plot, the 2 predictions are very similar but probably fish_cons and the 
# peak that occurs every December except for the last one, which coincidentally is the test set, 
# lead the SARIMAX model to overestimate the quantities sold, thus explaining the worse results in terms 
# of AIC and MSE obtained earlier. So we confirm that for Baccala_Vicentina we discard the SARIMAX model.

## GAM Models ----

### Baccala Mantecato ----
# Now we explore if we can benefit from non-linear models, in particular we try a GAM model and 
# compare it with the Multiple Linear Regression selected model.
# We start from the LR model

g0 = gam(Baccala_Mantecato ~ trend + Month + fish_cons, data = train) #this is equivalent to lr_m

# Now we plot the linear effects to see if they may require smoothing functions

par(mfrow=c(1,2))
plot(g0)

# We clearly see a perfectly linear relation between the predictors and the response variable, 
# this plots suggests to stop our non-linear analysis. 
# To be sure we try anyway to include smoothing variables

g1 <- gam(Baccala_Mantecato ~ s(trend, df=2) + Month + s(fish_cons, df=2), data = train)
summary(g1)

# Based on the df and on the 'Anova for Nonparametric Effects' results, where the nonparametric 
# F-values for s(trend) and s(fish_cons) are not statistically significant 
# (with p-values of 0.19369 and 0.07973 respectively), it is clear that these variables do not 
# exhibit nonlinear relationships with the response variable as we initially expected.
# Therefore, for reasons of parsimony, interpretability, and simplicity, we exclude the GAM model 
# in favor of a straightforward multiple linear regression model, which aligns better with the data 
# and offers more meaningful results.

### Baccala Vicentina ----
# Let's see if the same holds for Baccala_Vicentina.

g0_v = gam(Baccala_Vicentina ~ trend + Month + fish_cons, data = train) #this is equivalent to lr_v_full 
# (remember that the best one was lr_v (only Month as X))

par(mfrow=c(1,2))
plot(g0)

# We clearly see that the behaviour of the explenatory variables is still linear. 
# This suggests to keep the linear regression model, but we have also seen, in the section dedicated 
# to linear regression, that for Baccala_Vicentina the best model was the reduced one obtained by 
# considering only the Month variable as predictor. 
# So we keep it.

# This analysis clearly indicates that both results suggest against the use of models that fit 
# nonlinear relationships, such as GAMs.

## Exponential Smoothing Model ----

# We decide to try also an exponential smoothing model, in particular we try the ETS model
# It is a simple model that can be used to forecast time series data, using the Exponential Smoothing method.

### Baccala Mantecato ----

# First we initialize a data frame to store the results of the models that we are going to test
results_m <- data.frame(
  Model = character(),
  MSE = numeric(),
  AIC = numeric(),
  stringsAsFactors = FALSE
)

# Create the time series object for training
train_series_m <- ts(train$Baccala_Mantecato, start = c(2021, 1), frequency = 12)

# ETS Model
# First we try the classic ETS model
fit_ES_m <- ets(train_series_m)
# The ets() function fits an exponential smoothing model, automatically selecting the best configuration 
# based on the data. 
forecast_ES_m <- forecast(fit_ES_m, h = length(y_testm))
mse_ets_m <- mse(forecast_ES_m$mean,  y_testm)
aic_ets_m <- AIC(fit_ES_m)
results_m <- rbind(results_m, data.frame(Model = "ETS", MSE = mse_ets_m, AIC = aic_ets_m))
# We obtained a MSE of 111.7995 and an AIC of 266.4093 wich are good results.
# So we decided to try to use the ETS model also with additive and multiplicative seasonality to 
# see if we can improve the results obtained with the classic ETS model.

# Holt-Winters Additive Seasonality (via ETS)
fit_hw_add_m <- ets(train_series_m, model = "AAA")
forecast_hw_add_m <- forecast(fit_hw_add_m, h = length(y_testm))
mse_hw_add_m <- mse(forecast_hw_add_m$mean, y_testm)
aic_hw_add_m <- AIC(fit_hw_add_m)
results_m <- rbind(results_m, data.frame(Model = "Holt-Winters Additive", MSE = mse_hw_add_m, AIC = aic_hw_add_m))
# The results are worse than the ETS model, with a MSE of 114.8030 and an AIC of 274.4056.

# Holt-Winters Multiplicative Seasonality (via ETS)
fit_hw_mult_m <- ets(train_series_m, model = "MAM")
forecast_hw_mult_m <- forecast(fit_hw_mult_m, h = length(y_testm))
mse_hw_mult_m <- mse(forecast_hw_mult_m$mean, y_testm)
aic_hw_mult_m <- AIC(fit_hw_mult_m)
results_m <- rbind(results_m, data.frame(Model = "Holt-Winters Multiplicative", MSE = mse_hw_mult_m, AIC = aic_hw_mult_m))

mse_ets_m_train <- mse(fitted(fit_ES_m), train$Baccala_Mantecato)

# It's appening the same as before, the results are worse than the ETS model, with a MSE of 177.2098 and an AIC of 274.7737.

# Print the results
print(results_m)
# So as we can see from the table the best model for the Baccala Mantecato series is the ETS model, inlcuding additive and multiplicative seasonality
# doesn’t improve the results obtained with the classic ETS model so we decide to keep the one with the lowest MSE.

tsdisplay(residuals(fit_ES_m), main = "Residuals of ETS Model")
# The residuals of the ETS model seems randomly scattered around 0, without any visible pattern.
# This indicates that the model has appropriately captured the underlying trends in the data.
# However, there are some peaks and dips in the residuals that might suggest minor variations not captured by the model.
# As also the not so low MSE suggests, the model is not perfect but it is the best we can get.

# The ACF and PACF of the residuals reveal whether the residuals are uncorrelated and resemble white noise. 
# In this case, most lags fall within the confidence intervals, indicating that the 
# residuals exhibit minimal autocorrelation. 
# This is a good sign as it suggests the ETS model is adequately modeling the data 
# without leaving behind systematic errors.

# Finally we plot the predicted values and the actual ones.
ggplot() +
  geom_line(aes(x = data$Date, y = data$Baccala_Mantecato, color = "Actual Values"), size = 1) +
  geom_line(aes(x = train$Date, y = fitted(fit_ES_m), color = "Train Fitted Values (ETS)"), size = 1) +
  geom_line(aes(x = test$Date, y = forecast(fit_ES_m, h = nrow(test))$mean, 
                color = "Test Predicted Values (ETS)"), size = 1) +
  labs(
    title = "Time Series: Actual vs Predicted Values - Baccala Mantecato\nETS Model",
    x = "Date",
    y = "Value",
    color = "Legend"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom", text = element_text(size = 12))

# As we can see from the plot the blue line representing the fitted values during the 
# training phase closely follows the red line of actual values, which indicates that the 
# ETS model is performing well in capturing the data's behavior during training.
# The Exponential Smoothing model is able to capture the underlying trends in the data,
# the predicted values (Green Line) are close to the actual ones, and the model seems to be a 
# good fit for the data, though it shows some deviation during more extreme values.

### Baccala Vicentina ----

# Now we do the same also for the Baccala Vicentina series

# Initialize a data frame to store the results
results_v <- data.frame(
  Model = character(),
  MSE = numeric(),
  AIC = numeric(),
  stringsAsFactors = FALSE
)

# Create the time series object for training
train_series_v <- ts(train$Baccala_Vicentina, start = c(2021, 1), frequency = 12)

# ETS Model
fit_ES_v <- ets(train_series_v)
forecast_ES_v <- forecast(fit_ES_v, h = length(y_testv))
mse_ets_v <- mse(forecast_ES_v$mean, y_testv)
aic_ets_v <- AIC(fit_ES_v)
results_v <- rbind(results_v, data.frame(Model = "ETS", MSE = mse_ets_v, AIC = aic_ets_v))
# With the initial exponential smoothing model we obtained a really good MSE of 1.475781 and an AIC of 105.6035,
# and these are already good results.
# However, we also try the ETS model with additive and multiplicative seasonality to see if we 
# can improve the already results.

# Holt-Winters Additive Seasonality (via ETS)
fit_hw_add_v <- ets(train_series_v, model = "AAA")
forecast_hw_add_v <- forecast(fit_hw_add_v, h = length(y_testv))
mse_hw_add_v <- mse(forecast_hw_add_v$mean, y_testv)
aic_hw_add_v <- AIC(fit_hw_add_v)
results_v <- rbind(results_v, data.frame(Model = "Holt-Winters Additive", MSE = mse_hw_add_v, AIC = aic_hw_add_v))
# Including additive seasonality doesn’t improve the results obtained with the classic ETS model, since 
# the MSE is 1.512441 and the AIC is 111.4078.

# Holt-Winters Multiplicative Seasonality (via ETS)
fit_hw_mult_v <- ets(train_series_v, model = "MAM")
forecast_hw_mult_v <- forecast(fit_hw_mult_v, h = length(y_testv))
mse_hw_mult_v <- mse(forecast_hw_mult_v$mean, y_testv)
mse_hw_mult_v_train <- mse(fitted(fit_hw_mult_v), train$Baccala_Vicentina)
aic_hw_mult_v <- AIC(fit_hw_mult_v)
results_v <- rbind(results_v, data.frame(Model = "Holt-Winters Multiplicative", MSE = mse_hw_mult_v, AIC = aic_hw_mult_v))
# Including multiplicative seasonality instead, slightly improve the obtained MSE reducing it to 1.434768, 
# on the other hand it increases the AIC to 109.9921.

# Print the results
print(results_v)
# Since our goal is to minimize the MSE, we decide to keep the Holt-Winters with multiplicative seasonality
# which has the lowest MSE among the three models.

tsdisplay(residuals(fit_hw_mult_v), main = "Residuals of Holt-Winters Multiplicative Seasonality Model")
# The residuals plot for the Holt-Winters Multiplicative Seasonality Model shows that the 
# residuals are randomly distributed around zero, which is an indication of a good fit. 
# The residuals do not exhibit any apparent pattern or seasonality, which means the model 
# has effectively accounted for the multiplicative seasonal variations in the data.

# The ACF and PACF for the residuals further confirm the randomness of the residuals. 
# There are no significant spikes outside the confidence bands, suggesting that the 
# residuals are uncorrelated and approximate white noise. 
# This validates the model's performance and confirms that it has captured the key 
# elements of the time series.

ggplot() +
  geom_line(aes(x = data$Date, y = data$Baccala_Vicentina, color = "Actual Values"), size = 1) +
  geom_line(aes(x = train$Date, y = fitted(fit_hw_mult_v ), color = "Train Fitted Values (HWMS)"), size = 1) +
  geom_line(aes(x = test$Date, y = forecast(fit_hw_mult_v , h = nrow(test))$mean, 
                color = "Test Predicted Values (HWMS)"), size = 1) +
  labs(
    title = "Time Series: Actual vs Predicted Values - Baccala Vicentina\nHolt-Winters Multiplicative Seasonality Model",
    x = "Date",
    y = "Value",
    color = "Legend"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom", text = element_text(size = 12))

# Looking to the plot we can see that the fitted values during the training period are 
# well aligned with the actual values, demonstrating that the Holt-Winters model 
# effectively captures the multiplicative seasonal behavior of the time series. 
# However, the model's performance slightly deteriorates during the peaks and troughs, 
# which is expected for such models.

# In the test period, the green line representing the predicted values shows a 
# good alignment with the actual values, maintaining the seasonal structure. 
# Some underestimations occur during sharp spikes in the data, but overall, 
# the model provides reliable forecasts.


## Local Regression ----

# Now we focus on the Local Regression model, which is a non-parametric regression 
# method that fits a separate regression model to each point in the dataset.

# The idea is to fit a Loess model to the data, firts to the Baccala Mantecato series and then to the Baccala Vicentina series.

# Add a time index to the dataset to perform local regression
data$tt <- seq_len(nrow(data))
train$tt <- seq_len(nrow(train))
test$tt <- (nrow(train) + 1):nrow(data)

### Baccala Mantecato ----

# We initially focus on the Baccala Mantecato series, by different test we 
# found that the best trade-off for the span parameter is 0.3.

# Fit a Loess Model on the training data
best_span <- 0.3  # Tuned parameter for smoothing
fit_loess_m <- loess(Baccala_Mantecato ~ tt, data = train, span = best_span)

# Predict for both train and test datasets
train$loess_fitted <- predict(fit_loess_m)
test$loess_forecast <- predict(fit_loess_m, newdata = test)
# We obtained all NA values in the forecast because the loess model 
# is not able to predict values outside the training data range since the
# loess algorithm uses locally weighted regression, which relies on neighboring data points to make predictions. 

# To handle this issue, we can use linear regression to extrapolate the NA values.

# Handle NA predictions using linear regression (extrapolation)
na_indices_m <- is.na(test$loess_forecast)
if (any(na_indices_m)) {
  linear_model_m <- lm(Baccala_Mantecato ~ trend + Month + fish_cons, data = train)
  test$loess_forecast[na_indices_m] <- predict(linear_model_m, newdata = test[na_indices_m, ])
}

# Plot Results
ggplot() +
  geom_line(data = train, aes(x = Date, y = Baccala_Mantecato, color = "Train Observed")) +
  geom_line(data = test, aes(x = Date, y = Baccala_Mantecato, color = "Test Observed")) +
  geom_line(data = train, aes(x = Date, y = loess_fitted, color = "Loess Fitted")) +
  geom_line(data = test, aes(x = Date, y = loess_forecast, color = "Linear Regression Forecast")) +
  scale_color_manual(values = c(
    "Train Observed" = "#6BC3FF",
    "Test Observed" = "#FF7F7F",
    "Loess Fitted" = "#8FBC8F",
    "Linear Regression Forecast" = "#FFD700"
  )) +
  labs(title = "Loess Fit for Baccala Mantecato",
       x = "Date",
       y = "Baccala Mantecato",
       color = "Legend") +
  theme_minimal() +
  theme(legend.position = "bottom", text = element_text(size = 12))

# Calculate MSE for Loess on the test set
mse_loess_m <- mse(test$loess_forecast, test$Baccala_Mantecato)
cat(sprintf("Loess MSE on Test Data (Baccala Mantecato): %.5f\n", mse_loess_m))
# The MSE obtained is the one of the linear regression model, 
# because the Loess model is not able to predict values outside the 
# training data range.

# Calculate MSE for Loess on the train set
mse_loess_m_train <- mse(train$loess_fitted, train$Baccala_Mantecato)
cat(sprintf("Loess MSE on Train Data (Baccala Mantecato): %.5f\n", mse_loess_m_train))


### Baccala Vicentina ----

# We now focus on the Baccala Vicentina series.

# Fit a Loess Model on the training data
best_span_v <- 0.3 # Tuned parameter for smoothing
fit_loess_v <- loess(Baccala_Vicentina ~ tt, data = train, span = best_span_v)

# Predict for both train and test datasets
train$loess_fitted_v <- predict(fit_loess_v)
test$loess_forecast_v <- predict(fit_loess_v, newdata = test)

# Similar to the Mantecato case, we obtained all NA values in the forecast
# because the loess model cannot predict values outside the training data range.

# To handle this issue, we use linear regression to extrapolate the NA values.

# Handle NA predictions using linear regression (extrapolation)
na_indices_v <- is.na(test$loess_forecast_v)
if (any(na_indices_v)) {
  linear_model_v <- lm(Baccala_Vicentina ~ Month, data = train)
  test$loess_forecast_v[na_indices_v] <- predict(linear_model_v, newdata = test[na_indices_v, ])
}

# Plot Results
ggplot() +
  geom_line(data = train, aes(x = Date, y = Baccala_Vicentina, color = "Train Observed")) +
  geom_line(data = test, aes(x = Date, y = Baccala_Vicentina, color = "Test Observed")) +
  geom_line(data = train, aes(x = Date, y = loess_fitted_v, color = "Loess Fitted")) +
  geom_line(data = test, aes(x = Date, y = loess_forecast_v, color = "Linear Regression Forecast")) +
  scale_color_manual(values = c(
    "Train Observed" = "#6BC3FF",
    "Test Observed" = "#FF7F7F",
    "Loess Fitted" = "#8FBC8F",
    "Linear Regression Forecast" = "#FFD700"
  )) +
  labs(title = "Loess Fit for Baccala Vicentina",
       x = "Date",
       y = "Baccala Vicentina",
       color = "Legend") +
  theme_minimal() +
  theme(legend.position = "bottom", text = element_text(size = 12))

# Calculate MSE for Loess on the test set
mse_loess_v <- mse(test$loess_forecast_v, test$Baccala_Vicentina)
cat(sprintf("Loess MSE on Test Data (Baccala Vicentina): %.5f\n", mse_loess_v))
# The MSE obtained is the one from the linear regression model because the Loess model
# cannot predict values outside the training data range.

# Calculate MSE for Loess on the train set
mse_loess_v_train <- mse(train$loess_fitted_v, train$Baccala_Vicentina)
cat(sprintf("Loess MSE on Train Data (Baccala Vicentina): %.5f\n", mse_loess_v_train))
