TODO: UNDERSTAND ALL THE MODEL ADDED AND COMMENT IT 

  FATTI
  
    MULTIPLE LINEAR REGRESSION-->TOTO
    
    GAM-->FLAVIO
    EXPONENTIAL SMOOTHING-->ALBI
    KNN-->ALBI
    
  DA FARE
  
    ARIMAX-->FLAVIO | SARIMA-->TOTO
    
    GENERALIZED BASS MODEL-->FLAVIO
    
    BASS MODEL-->FLAVIO
    LOCAL REGRESSION-->ALBI
    PROPHET MODEL-->TOTO
    SPLINES-->TOTO
    

TOTO 09/01/2025
    COMMENTO:
         - TEST NORMALITÁ CON library(olsrr) (funzione='ols_test_normality') la usa la prof? ha senso? cosa fa?
           stesso discorso vale per ols_test_correlation  e ols_test_breusch_pagan
         
       
    ALBI 09/01/2025
     START COMMENT
       La prof non lo usa ma ne avevo già parlato con lei e ha detto che possiamo usare 
       tutti i pacchetti che vogliamo basta che sappiamo cosa fanno e che non ci sono problemi
       ovviamente dobbiamo sapere cosa fanno e commentare di conseguenza i risultati



Upload only the sections of code that you are confident work correctly and are ready for submission in the exam. Include comments where possible for better clarity and understanding.


# Analysis of Baccala Mantecato and Baccala alla Vicentina

## Package Loading and General Configuration

```{r LIBRARIES, message=FALSE, warning=FALSE}
rm(list=ls())

library(readxl)
library(readr)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(lmtest)
library(mgcv)

```

```{r SETWD, warning=FALSE}
base_path <- "D:/Projects_GitHub/BEFD_Project/" # Set base path
source(file.path(base_path, "Code/Functions_Utilies.R"))
```

## Data Loading and Preprocessing ----

```{r DATA LOAD, message=FALSE, warning=FALSE}
data <- read_csv(file.path(base_path, "Data/data.csv"))
data$Date <- as.Date(data$Date, format = "%Y-%m-%d") # ensure date format
data <- data %>% mutate(Month = format(Date, "%m"),
                        Year = as.factor(format(Date, "%Y"))) # create useful features for the models
data$trend <- 1:nrow(data)

head(data,3)
```

Check for missing values
```{r NAN CHECK}
colSums(is.na(data))
```

Then we load a dataset found on [EUMOFA](https://eumofa.eu/web/guest/bulk-download).
We also tried incorporating different variables, such as the NIC for fish products (Istat), the production price of fish products (Istat), and others. However, these variables did not prove to be significantly relevant for our analysis.
Additionally, some other variables we tried do not have monthly data that match the frequency of the sales data we are working with, limiting their usefulness in the context of our time series analysis.

Here we focus on the salmon consumption only because we have seen that other fishes or their aggregate value lead worst results.
```{r FISH LOAD}
fish_cons <- read_excel(file.path(base_path, "Data/Fish_consumption_ita_raw.xlsx")) %>%
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
```

The dataset contains three columns:

Date: Unlike the data related to baccalà, the time series for salmon starts from November 2020. This is because we lack information for the last two months of 2024, so we applied a two-month lag. This choice also makes sense for future applications, as it would not be possible to forecast baccalà sales (whether mantecato or alla vicentina) for December when monthly consumption data for that month is already available.

kg_std: Represents the standardized version of the kg column. Since the values in kg are quite large, we opted to standardize them to simplify further analysis.

kg: Represents the quantity of salmon sold in Italy, measured in kilograms.

Finally we aggregate the salmon monthly consumption time series to our data.
```{r ASSIGN KG_STD TO DATA}
data$fish_cons <- fish_cons$kg_std[,1]
head(data,3)
```

## Explanatory Analysis ----

First, we visualize the time series of Baccala Mantecato and Baccala Vicentina over time.
This plot helps us compare the trend of sales for both products on a monthly basis.

```{r message=FALSE, warning=FALSE, PLOT BACCALA fig.width=10}
ggplot(data, aes(x = Date)) +
  geom_line(aes(y = Baccala_Mantecato, color = "Baccala Mantecato"), size = 1) +
  geom_line(aes(y = Baccala_Vicentina, color = "Baccala Vicentina"), size = 1) +
  labs(title = "Monthly Time Series of Baccala Mantecato and Baccala Vicentina",
       x = "Date",
       y = "Quantity") +
  theme_minimal()
```

The sales quantities of Baccala Mantecato and Baccala Vicentina show significant differences, with Baccala Mantecato consistently having a much higher volume of sales throughout all periods observed. Additionally, Baccala Mantecato exhibits a much wider range of values, indicating greater variability in sales. In contrast, Baccala Vicentina appears more stable, with sales peaks typically occurring towards the end of the year. A similar trend is also seen for Baccala Mantecato, which experiences an uptick in sales during the final months of each year.

Next, we plot the time series for both products grouped by year.
The goal is to observe yearly trends and patterns for Baccala Mantecato and Baccala Vicentina separately.

```{r PLOT BACCALA GROUP BY YEAR fig.width=10}
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
```
Both graphs highlight the pattern mentioned earlier: for each year, there is an increase in sales during the final months of the year, particularly in September and December. 
Additionally, regarding Baccala Mantecato, it appears that during the earlier years of sales for which we have data, the quantity sold was generally higher in the off-peak months (for example, the orange line for 2021 is higher compared to the other years). However, in the later years, the quantity sold during the peak months has increased.

Finally, we also examined the properties of the time series by plotting the autocorrelation functions (ACF).

```{r ACF TS}
ym = ts(data$Baccala_Mantecato, frequency = 12, start = c(2021, 1))
yv = ts(data$Baccala_Vicentina, frequency = 12, start = c(2021, 1))

if (end(ym)[1] != 2024 || end(ym)[2] != 12) {
  print("Error in ts ym")
}
if (end(yv)[1] != 2024 || end(yv)[2] != 12) {
  print("Error in ts yv")
}

acf(data$Baccala_Mantecato, main = "ACF of Baccala Mantecato", col = "blue", lwd = 2)
acf(data$Baccala_Vicentina, main = "ACF of Baccala Vicentina", col = "red", lwd = 2)
```

We now that autocorrelation occurs when the effect of a avriable is spread over time, in these cases, most of the autocorrelations fall within the confidence bands, indicating that the data does not show significant correlation for most lags. 
However, within the bands, the autocorrelations exhibit a sinusoidal pattern, suggesting the presence of seasonality in the data, where periodic fluctuations occur over time. The peak at lag 12 further supports the idea of a cyclical effect.
We will analyze the residuals of future models to confirm or disprove the presence of this seasonality.


# TRAIN/TEST SPLIT ----

In this section, we perform a train-test split to prepare the data for model training and evaluation. We divide the time series data for both Baccala Mantecato and Baccala Vicentina into training and testing sets, with 90% of the data allocated for training and the remaining 10% for testing.

```{r TRAIN-TEST SPLIT}
prop <- 0.8

n_sample <- floor(nrow(data) * prop)
  
train <- data[1:n_sample, ]
y_trainm <- train[["Baccala_Mantecato"]]
y_trainv <- train[["Baccala_Vicentina"]]
  
test <- data[(n_sample + 1):nrow(data), ]
  
y_testm <- test[["Baccala_Mantecato"]]
y_testv <- test[["Baccala_Vicentina"]]
```

```{r TRAIN-TEST PLOT}
Type = c(rep("Train", dim(train)[1]), rep("Test", dim(test)[1]))
ggplot(data, aes(x = trend, y = Baccala_Mantecato, color = Type)) +
  geom_line() +
  scale_color_manual(values = c("red", "blue")) +
  labs(title = "Train and Test Data for Baccala Mantecato",
       x = "Time",
       y = "Quantity sold of Baccala Mantecato") +
  theme_minimal() +
  theme(legend.title = element_blank())

ggplot(data, aes(x = trend, y = Baccala_Vicentina, color = Type)) +
  geom_line() +
  scale_color_manual(values = c("red", "blue")) +
  labs(title = "Train and Test Data for Baccala Vicentinao",
       x = "Time",
       y = "Quantity sold of Baccala Vicentina") +
  theme_minimal() +
  theme(legend.title = element_blank())
```

## Modelling Phase

### Linear Regression Model

#### Baccala Mantecato
We start by estimating a simple linear regression model with all the possible variables
```{r}
lr_m <- lm(Baccala_Mantecato ~ trend + Month + fish_cons, data = train)
summary(lr_m)
```

The Multiple R-squared value of 0.9178 indicates that approximately 91.78% of the variability in Baccala_Mantecato is explained by the model.
Then we take a closer look to the coefficents:
- the trend variable has a positive coefficient of 0.18, meaning that, on average, Baccala_Mantecato increases by 0.18 units for each time period, assuming all other factors remain constant.
-The coefficient for fish_cons is 7.79, indicating a strong positive relationship. Specifically, for each unit increase in fish consumption, Baccala_Mantecato rises by approximately 7.79 units. In particular we will provide an exemple: if the fish consumption in Italy increases by a unit in October 2021, then keeping all the other variables fixed, the quantity sold in December 2021 will increase of 7.79 kg.
- Regarding the Month variables, they capture seasonal effects by representing deviations from the baseline month (January). Some months show statistically significant effects (e.g., February, August, September, October, and December. For example, December shows a substantial positive effect, indicating a peak in that month, while months like February and October have negative coefficients, reflecting significant declines compared to January.

Now we try removing variables to evaluate their impact on model performance using AIC and adjusted R² values.

```{r}
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
```
As we can expect by the summary of the full model, from the AIC and Adjusted R² results we clearly see that the best model fit belong to the full model with all predictors: trend, monthly seasonality and fish consumption.

Finally we will analyze the residuals.
```{r}
resid_lr <- residuals(lr_m)
plot(resid_lr)
```

```{r}
dwtest(lr_m)
```

When we analyze the residuals of the linear regression model, as shown in the plot below, we observe no particular patterns. The residuals appear to be randomly scattered, indicating that the model has appropriately captured the underlying trends in the data. This suggests that the assumptions of linearity, constant variance, and independence are reasonably satisfied, and the model's fit is adequate for forecasting purposes. 
We also perform the Durbin-Watson test, that is used to check for autocorrelation in the residuals of a regression model.
Since the p-value is smaller than the significance level (0.05), we don't reject the null hypothesis that the autocorrelation of the disturbances is 0.

Finally we plot the predicted values and the actual ones. We also compute the MSE on the test set.

```{r}
ggplot() +
  geom_line(aes(x = data$Date, y = data$Baccala_Mantecato, color = "Actual Values"), size = 1) +
  geom_line(aes(x = train$Date, y = fitted(lr_m), color = "Train Fitted Values"), size = 1) +
  geom_line(aes(x = test$Date, y = predict(lr_m, newdata = test), color = "Test predicted Values"), size = 1) + 
  labs(
    title = "Time Series: Actual vs Predicted Values",
    x = "Date",
    y = "Value",
    color = "Legend"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom", text = element_text(size = 12))
mse_lrm <- mse(predict(lr_m, newdata = test), test$Baccala_Mantecato)
print(mse_lrm)
```

#### Baccala Vicentina

The same analysis is conducted below for the Baccala Vicentina variable.

```{r}
lr_v_full <- lm(Baccala_Vicentina ~ trend + Month + fish_cons, data = train)
summary(lr_v_full)
```

This time the fish consumption seems to be not useful to imporve the model pefromance so we remove it

```{r}
lr_v <- update(lr_v_full, .~. - fish_cons)
summary(lr_v)
```

The adjusted R² increses from 0.8261 to 0.8305 suggesting that the reduced model is better than the full one. 
Furthermore, the trend variable shows a large p-value, suggesting that it can be removed from the model without significantly affecting the results.

```{r}
lr_v <- update(lr_v, .~. - trend)
summary(lr_v)
```


The best model for the Baccala Vicentina series is the one that includes only the monthly seasonality. This model provides the best fit, indicating that the variations in the data are primarily driven by seasonal effects, without the need for additional trend or other predictors.
Moreover, the fitted model is statistically significant, as shown by the F-statistic of 18.21, with a p-value of 1.684e-09, confirming the model's overall significance.

Below, we compare the difference in AIC and adjusted R² between the full model and the one with only the monthly seasonality.

```{r}
cat("Model with month:\n")
cat("  AIC:", AIC(lr_v), "\n")
cat("  Adjusted R²:", summary(lr_v)$adj.r.squared, "\n\n")

cat("Full model:\n")
cat("  AIC:", AIC(lr_v_full), "\n")
cat("  Adjusted R²:", summary(lr_v_full)$adj.r.squared, "\n")
```

The model with only the monthly seasonality performs better than the full model in terms of both AIC and adjusted R².

Finally we will analyze the residuals.

```{r}
resid_lr <- residuals(lr_v)
plot(resid_lr)
```


```{r}
dwtest(lr_v)
```


When we analyze the residuals of the linear regression model, as shown in the plot below, we observe no particular patterns. The residuals appear to be randomly scattered, indicating that the model has appropriately captured the underlying trends in the data. This suggests that the assumptions of linearity, constant variance, and independence are reasonably satisfied, and the model's fit is adequate for forecasting purposes.
On the other hand we perform the Durbin-Watson test, that is used to check for autocorrelation in the residuals of a regression model.mSince the p-value is greater than the significance level (0.05), we reject the null hypothesis.

Finally we plot the predicted values and the actual ones. We also compute the MSE on the test set.

```{r}
ggplot() +
  geom_line(aes(x = data$Date, y = data$Baccala_Vicentina, color = "Actual Values"), size = 1) +
  geom_line(aes(x = train$Date, y = fitted(lr_v), color = "Train Fitted Values"), size = 1) +
  geom_line(aes(x = test$Date, y = predict(lr_v, newdata = test), color = "Test predicted Values"), size = 1) + 
  labs(
    title = "Time Series: Actual vs Predicted Values",
    x = "Date",
    y = "Value",
    color = "Legend"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom", text = element_text(size = 12))
mse_lrv <- mse(predict(lr_v, newdata = test), test$Baccala_Vicentina)
print(mse_lrv)
```


### SARIMA Model

#### Baccala Mantecato

To begin, we first transform the sales data of Baccala Mantecato into a time series object:

```{r message=FALSE, warning=FALSE}
ts_m <- ts(train$Baccala_Mantecato, start = c(2021, 01), frequency = 12)
plot.ts(ts_m)
```
From the plot above and the significance of the trend coefficient in the regression model discussed in the previous section, we might consider taking the first difference to observe the behavior of the ACF and PACF.

```{r}
ts_m1 <- diff(ts_m,1)
Acf(ts_m1)
Pacf(ts_m1)
```


Both the ACF and PACF plots show a sinusoidal pattern, with all values falling within the confidence bands, except for a significant spike at lag 12. 
This suggests that a SARIMA model with a seasonal period s=12, accounting for monthly data with yearly seasonality, might be appropriate for this time series.

```{r}
ts_m_12 <- diff(ts_m, lag = 12)
Acf(ts_m_12)
Pacf(ts_m_12)
```

Now, we will build three SARIMA models. The first model will incorporate a non-seasonal differencing (with d=1), one autoregressive term (AR(1)), and a seasonal differencing with a period of 12 (to account for the yearly seasonality). The second model will instead include a moving average (MA(1)) term along with the differencing. The last one only incorporate the differencing.

```{r}
# SARIMA (1,1,0)(0,1,0)[12]
sarima_model1m <- Arima(ts_m, order=c(1,1,0), seasonal=c(0,1,0))
summary(sarima_model1m)

# SARIMA (0,1,1)(0,1,0)[12]
sarima_model2m <- Arima(ts_m, order=c(0,1,1), seasonal=c(0,1,0))
summary(sarima_model2m)

# SARIMA (0,1,0)(0,1,0)[12]
sarima_model3m <- Arima(ts_m, order=c(0,1,0), seasonal=c(0,1,0))
summary(sarima_model3m)
```

```{r}
mse(test$Baccala_Mantecato, forecast(sarima_model1m, h = length(y_testm))$mean)
mse(test$Baccala_Mantecato, forecast(sarima_model2m, h = length(y_testm))$mean)
mse(test$Baccala_Mantecato, forecast(sarima_model3m, h = length(y_testm))$mean)
```

Based on the AIC values, the SARIMA(0,1,1)(0,1,0)[12] model is the better model. It has a lower AIC compared to the other SARIMA models. By the way, evaluating the performance on the test set, we notice that the MSE is larger for the AIC best model, while the SARIMA(0,1,0)(0,1,0)[12] perform better on the test set. We decide to continue whit the last one.

```{r}
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
```


```{r}
resid3 <- residuals(sarima_model3m)
Acf(resid3)
Pacf(resid3)
```

Residuals does not suggest a really good fit but, as said before, the best test performance are reached by the selected model.

#### Baccala Vicentina

To begin, we first transform the sales data of Baccalá Vicentina into a time series object:

```{r message=FALSE, warning=FALSE}
ts_v <- ts(train$Baccala_Vicentina, start = c(2021, 01), frequency = 12)
plot.ts(ts_v)
```

```{r}
Acf(ts_v)
Pacf(ts_v)
```

From the plot above we clearly notice the presence of seasonality that is confirmed by the pacf and acf functions with a significant spike at lag 12.

```{r}
ts_v_12 <- diff(ts_v, lag = 12)
Acf(ts_v_12)
Pacf(ts_v_12)
```

This suggests that a SARIMA model with a seasonal period s=12, accounting for monthly data with yearly seasonality, might be appropriate for this time series.

Now, we will build a SARIMA model model that incorporate only a seasonal differencing with a period of 12.
The parameter D=2 lead us to lower AIC but a bigger mse, this is a case of overfitting.

```{r}
auto_sarima_modelv <- auto.arima(ts_v)
summary(auto_sarima_modelv)

# SARIMA(0,0,0)(0,1,0)[12]
sarima_model2v <- Arima(ts_v, order=c(0,0,0), seasonal=c(0,1,0))
summary(sarima_model2v)
```

```{r}
resid2 <- residuals(sarima_model2v)
Acf(resid2)
Pacf(resid2)
```

The best model based on AIC is supposed to be the SARIMA(0,0,0)(0,1,0)[12].

```{r}
mse(test$Baccala_Vicentina, forecast(sarima_model2v, h = length(y_testv))$mean)
```

Note: We also tried different configurations, expecially after we saw the residuals plot but at the end, this remain the best model possible.

```{r}
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
```

## GAM Model ----

```{r}

gam_mantecato <- gam(Baccala_Mantecato ~  s(trend) + Month + s(fish_cons), data = train)
summary(gam_mantecato)
gam_vicentina <- gam(Baccala_Vicentina ~  Month, data = train)
summary(gam_vicentina)
```

## ARMAX Model ----

```{r}
armax1 <- Arima(train$Baccala_Mantecato, xreg= train$fish_cons, seasonal=list(order=c(0,1,0), period=12), order=c(0,1,1))
summary(armax1)

res1<- residuals(armax1)
Acf(res1)

fitted(armax1)
plot(train$Baccala_Mantecato)
lines(fitted(armax1), col='red')
AIC(armax2)

ts_m <- ts(train$Baccala_Mantecato, start = c(2021,01), frequency = 12)
armax2 <- Arima(ts_m, xreg=train$fish_cons, order = c(0,1,1), seasonal = c(0,1,0))
summary(armax2)
```

## Exponential Smoothing Model ----

```{r}
fit_ES <- ets(train_series)
summary(fit_ES)
AIC(fit_ES)
```
AIC = 304.7, AICc = 322.5, BIC = 331.1. 
While the AIC is acceptable, higher BIC suggests the model may be overfitting due to complexity.

Training Set Error:
Mean Error (ME): -0.14, which is very close to zero, showing minimal bias in predictions.
RMSE: 3.72 and MAPE: 10.78%. 
Residual ACF (Autocorrelation):
Residual ACF1 = 0.14, indicating a small but present autocorrelation in residuals. 
This hints that the model hasn not fully captured temporal dependencies.

```{r}
forecast_ES <- forecast(fit_ES, h = length(train_testm$y_test))
plot(forecast_ES)
````
### Exponential Smoothing Improvements with Fine-Tuning ----

Include Additive and Multiplicative Seasonality using Holt-Winters

```{r}
fit_hw_mult <- hw(train_series, seasonal = "multiplicative")
forecast_hw_mult <- forecast(fit_hw_mult, h = length(train_testm$y_test))
summary(fit_hw_mult)
```
BAD AIC

Fine-tune Holt-Winters Multiplicative Model

```{r}
fit_hw_mult_tuned <- ets(train_series, model = "MAM")
forecast_hw_mult_tuned <- forecast(fit_hw_mult_tuned, h = length(train_testm$y_test))
summary(fit_hw_mult_tuned)
AIC(fit_hw_mult_tuned)
```
STILL BAD AIC

Combine Results with Fish Consumption using Regression + ETS

```{r}
fit_reg <- lm(y_train_m ~ trainm$Month + trainm$Year + trainm$fish_cons)
residuals_reg <- residuals(fit_reg)
fit_res_ets <- ets(residuals_reg)
summary(fit_reg) # Good R-squared
AIC(fit_reg) # BEST AIC
summary(fit_res_ets)
AIC(fit_res_ets) # Worst AIC compared to the multiple linear regression :(
```


## KNN ----

```{r}
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
```

Split training data into train and validation sets
```{r}
set.seed(123) # For reproducibility
train_indices <- sample(1:nrow(trainm_preprocessed), size = 0.8 * nrow(trainm_preprocessed))
validation_indices <- setdiff(1:nrow(trainm_preprocessed), train_indices)

train_split <- trainm_preprocessed[train_indices, ]
validation_split <- trainm_preprocessed[validation_indices, , drop = FALSE]
```

Step 3: Tune k using validation data (including even k values)
```{r}
k_values <- 1:20 # Include k from 1 to 20

results <- data.frame(k = k_values, Validation_MSE = numeric(length(k_values)))
results_test <- data.frame(k = k_values, Test_MSE = numeric(length(k_values)))

for (k in k_values) {
  # Train KNN model on validation set
  knn_model <- kknn(
    formula = Baccala_Mantecato ~ .,
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
```
# KNN con K = 14 ----
Step 4: Train final KNN model with the best k on full training set

```{r}
knn_final <- kknn(
  formula = Baccala_Mantecato ~ .,
  train = trainm_preprocessed,
  test = testm_preprocessed,
  k = best_k
)
```

Step 5: Predict and evaluate on the test set
```{r}
predictions <- fitted(knn_final)
actual <- testm_preprocessed$Baccala_Mantecato

mse_test <- mean((actual - predictions)^2)
mae_test <- mean(abs(actual - predictions))

cat("Test MSE:", mse_test, "\n")
cat("Test MAE:", mae_test, "\n")
```
K =14 with Test MSE: 0.08237638, Test MAE: 0.2608387 and Validation MSE: 0.01267

## KNN con K = 2 ----
Con K = 2
```{r}
knn_final <- kknn(
  formula = Baccala_Mantecato ~ .,
  train = trainm_preprocessed,
  test = testm_preprocessed,
  k = 2
)
```
Step 5: Predict and evaluate on the test set

```{r}
predictions <- fitted(knn_final)
actual <- testm_preprocessed$Baccala_Mantecato

mse_test <- mean((actual - predictions)^2)
mae_test <- mean(abs(actual - predictions))

cat("Test MSE:", mse_test, "\n")
cat("Test MAE:", mae_test, "\n")
```
Step 6: Plot actual vs predicted
```{r}
plot(actual, predictions, main = "Actual vs Predicted (KNN)",
     xlab = "Actual", ylab = "Predicted", pch = 19, col = "blue")
abline(0, 1, col = "red", lwd = 2)
```
K = 2 with Test MSE: 0.004291991, Test MAE: 0.05627241 and Validation MSE: 0.02047








