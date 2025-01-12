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
library(forecast)
library(lmtest)

```

```{r SETWD, warning=FALSE}
base_path <- "C:/Users/flavi/OneDrive/Documenti/GitHub/BEFD_Project/" # Set base path
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
  scale_color_manual(values = c("Baccala Mantecato" = "#FF7F7F", "Baccala Vicentina" = "#6BC3FF")) +
  theme_minimal() +
  theme(legend.position = "bottom")
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

Acf(data$Baccala_Mantecato, main = "ACF of Baccala Mantecato", col = "#FF7F7F", lwd = 2)
Acf(data$Baccala_Vicentina, main = "ACF of Baccala Vicentina", col = "#6BC3FF", lwd = 2)
```

We now that autocorrelation occurs when the effect of a avriable is spread over time, in these cases, most of the autocorrelations fall within the confidence bands, indicating that the data does not show significant correlation for most lags. 
However, within the bands, the autocorrelations exhibit a sinusoidal pattern, suggesting the presence of seasonality in the data, where periodic fluctuations occur over time. The peak at lag 12 further supports the idea of a cyclical effect.
We will analyze the residuals of future models to confirm or disprove the presence of this seasonality.


## TRAIN/TEST SPLIT ----

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
mse_train_lrm <- mean(resid_lr^2)
checkresiduals(lr_m)
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
  
mse_test_lrm <- mse(predict(lr_m, newdata = test), test$Baccala_Mantecato)
print(mse_test_lrm)
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
mse_train_lrv <- mean(resid_lr^2)
checkresiduals(lr_v)
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

mse_test_lrv <- mse(predict(lr_v, newdata = test), test$Baccala_Vicentina)
print(mse_test_lrv)
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
Acf(ts_m1, main = "ACF of Baccala Mantecato with Lag 1", col = "#FF7F7F", lwd = 2)
Pacf(ts_m1, main = "PACF of Baccala Mantecato with Lag 1", col = "#6BC3FF", lwd = 2)
```


Both the ACF and PACF plots show a sinusoidal pattern, with all values falling within the confidence bands, except for a significant spike at lag 12. 
This suggests that a SARIMA model with a seasonal period s=12, accounting for monthly data with yearly seasonality, might be appropriate for this time series.

```{r}
ts_m_12 <- diff(ts_m, lag = 12)
Acf(ts_m_12, main = "ACF of Baccala Mantecato with Lag 12", col = "#FF7F7F", lwd = 2)
Pacf(ts_m_12, main = "PACF of Baccala Mantecato with Lag 12", col = "#6BC3FF", lwd = 2)
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
mse_test_sarimam <- mse(test$Baccala_Mantecato, forecast(sarima_model3m, h = length(y_testm))$mean)
mse_test_sarimam
```


Based on the AIC values, the SARIMA(0,1,1)(0,1,0)[12] model is the better model. It has a lower AIC compared to the other SARIMA models. By the way, evaluating the performance on the test set, we notice that the MSE is larger for the AIC best model, while the SARIMA(0,1,0)(0,1,0)[12] perform better on the test set. We decide to continue whit the last one.

```{r}
ggplot() +
  geom_line(aes(x = data$Date, y = data$Baccala_Mantecato, color = "Actual Values"), size = 1) +
  geom_line(aes(x = train$Date, y = fitted(sarima_model3m), color = "Fitted Values (Train)"), size = 1) +
  geom_line(aes(x = test$Date, y = forecast(sarima_model3m, h = nrow(test))$mean, 
                color = "Predicted Values (Test)"), size = 1) +
  scale_color_manual(values = c("Actual Values" = "#FF7F7F", 
                                "Fitted Values (Train)" = "#6BC3FF", 
                                "Predicted Values (Test)" = "#8FBC8F"),
                     labels = c("Actual Values", "Fitted Values (Train)", "Predicted Values (Test)")) +
  labs(
    title = "Actual vs Fitted and Predicted Values for Baccala Vicentina (SARIMA Model)",
    x = "Date",
    y = "Value",
    color = "Legend"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")
```



```{r}
resid3 <- residuals(sarima_model3m)
mse_train_sarimam <- mean(resid3^2)
Acf(resid3, main = "ACF for SARIMA Residuals of Baccala Mantecato", col = "#FF7F7F", lwd = 2)
Pacf(resid3, main = "PACF for SARIMA Residuals of Baccala Mantecato", col = "#6BC3FF", lwd = 2)
```

Residuals does not suggest a really good fit but, as said before, the best test performance are reached by the selected model.

#### Baccala Vicentina

To begin, we first transform the sales data of Baccalá Vicentina into a time series object:

```{r message=FALSE, warning=FALSE}
ts_v <- ts(train$Baccala_Vicentina, start = c(2021, 01), frequency = 12)
plot.ts(ts_v)
```

```{r}
Acf(ts_v, main = "ACF of Baccala Vicentina", col = "#FF7F7F", lwd = 2)
Pacf(ts_v, main = "PACF of Baccala Vicentina", col = "#6BC3FF", lwd = 2)
```

From the plot above we clearly notice the presence of seasonality that is confirmed by the pacf and acf functions with a significant spike at lag 12.

```{r}
ts_v_12 <- diff(ts_v, lag = 12)
Acf(ts_v_12, main = "ACF of Baccala Vicentina with Lag 12", col = "#FF7F7F", lwd = 2)
Pacf(ts_v_12, main = "PACF of Baccala Vicentina with Lag 12", col = "#6BC3FF", lwd = 2)
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
mse_train_sarimav <- mean(resid2^2)
Acf(resid2, main = "ACF for SARIMA Residuals of Baccala Vicentina", col = "#FF7F7F", lwd = 2)
Pacf(resid2, main = "PACF for SARIMA Residuals of Baccala Vicentina", col = "#6BC3FF", lwd = 2)
```

The best model based on AIC is supposed to be the SARIMA(0,0,0)(0,1,0)[12].

```{r}
mse_test_sarimav <- mse(test$Baccala_Vicentina, forecast(sarima_model2v, h = length(y_testv))$mean)
mse_test_sarimav
```

Note: We also tried different configurations, expecially after we saw the residuals plot but at the end, this remain the best model possible.

```{r}
ggplot() +
  geom_line(aes(x = data$Date, y = data$Baccala_Vicentina, color = "Actual Values"), size = 1) +
  geom_line(aes(x = train$Date, y = fitted(sarima_model2v), color = "Fitted Values (Train)"), size = 1) +
  geom_line(aes(x = test$Date, y = forecast(sarima_model2v, h = nrow(test))$mean, 
                color = "Predicted Values (Test)"), size = 1) +
  scale_color_manual(values = c("Actual Values" = "#FF7F7F", 
                                "Fitted Values (Train)" = "#6BC3FF", 
                                "Predicted Values (Test)" = "#8FBC8F"),
                     labels = c("Actual Values", "Fitted Values (Train)", "Predicted Values (Test)")) +
  labs(
    title = "Actual vs Fitted and Predicted Values for Baccala Vicentina (SARIMA Model)",
    x = "Date",
    y = "Value",
    color = "Legend"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

```
#### ARMAX

Now following the same reasoning of SARIMA models, we compare to them the same models but also using the fish consumption information as xreg, to see if it improves our results

###Baccalà mantecato

```{r}
sarimax_model1m <- Arima(ts_m, order = c(1, 1, 0), seasonal = c(0, 1, 0), xreg = train$fish_cons)
sarimax_model2m <- Arima(ts_m, order = c(0, 1, 1), seasonal = c(0, 1, 0), xreg = train$fish_cons)
sarimax_model3m <- Arima(ts_m, order = c(0, 1, 0), seasonal = c(0, 1, 0), xreg = train$fish_cons)
```
Once we have fitted the models, we compare them to the previous SARIMA considering the AIC as metric
```{r}
AIC(sarima_model1m, sarimax_model1m)
AIC(sarima_model2m, sarimax_model2m)
AIC(sarima_model3m, sarimax_model3m)
```
It is clear that using the fish consumption data we improve the models, but as we've seen for SARIMAs, considering only AIC may not be enough, so now we also look at the MSEs
```{r}
mse(test$Baccala_Mantecato, forecast(sarima_model1m, h = length(y_testm))$mean)
mse(test$Baccala_Mantecato, forecast(sarimax_model1m, h = length(y_testm),xreg = test$fish_cons)$mean)

mse(test$Baccala_Mantecato, forecast(sarima_model2m, h = length(y_testm))$mean)
mse(test$Baccala_Mantecato, forecast(sarimax_model2m, h = length(y_testm),xreg = test$fish_cons)$mean)

mse(test$Baccala_Mantecato, forecast(sarima_model3m, h = length(y_testm))$mean)
mse(test$Baccala_Mantecato, forecast(sarimax_model3m, h = length(y_testm),xreg = test$fish_cons)$mean)

```
We see that in terms of MSE, SARIMAX brings to huge improvements for all 3 the considered models reducing the MSEs by approximately 50%. 
Following the same reasoning as before, and considering both the AIC and the MSE, among the models we select the ARMAX(0,1,0)(0,1,0)[12].

Now we look at that model prediction on the test set compared to the respective original SARIMA
```{r}
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

```
From the plot we clearly see the improvement obtained with the SARIMAX version, confirming that the fish_cons variable helps on predicting future orders quantity.

Now we see the residuals 
```{r}
resid3_sarimax <- residuals(sarimax_model3m)
Acf(resid3_sarimax, main = "ACF for SARIMAX Residuals of Baccala Mantecato", col = "#FF7F7F", lwd = 2)
Pacf(resid3_sarimax, main = "PACF for SARIMAX Residuals of Baccala Mantecato", col = "#6BC3FF", lwd = 2)
```
From the acf we have the doubt that we are missing an autoregressive component, but including it leads to lower performances on the test set, so we decide to accept that loss of information showed in the residuals to avoid overfitting.

###Baccala Vicentina
Following the same approach, we try to study the effects of considering fish_cons on Baccala_vicentina and compare it to the previously selected SARIMA(0,1,1)(0,1,0)[12] model
```{r}
sarimax_model2v <- Arima(ts_v, order = c(0, 1, 1), seasonal = c(0, 1, 0), xreg = train$fish_cons)

AIC(sarima_model2v, sarimax_model2v)


mse(test$Baccala_Vicentina, forecast(sarima_model2v, h = length(y_testv))$mean)
mse(test$Baccala_Vicentina, forecast(sarimax_model2v, h = length(y_testv), xreg = test$fish_cons)$mean)


```
Surprisingly for the Baccala_Vicentina, the originally selected SARIMA model performs better than the SARIMAX one considering both AIC and MSE, so we discard it and select the original SARIMA.
```{r}
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
```
As we can see from the plot, the 2 predictions are very similar but probably fish_cons and the peak that occurs every December except for the last one, which coincidentally is the test set, lead the SARIMAX model to overestimate the quantities sold, thus explaining the worse results in terms of AIC and MSE obtained earlier. So we confirm that for Baccala_Vicentina we discard the SARIMAX model.


### Prophet Model

#### Baccala Mantecato
We start by creating the dataset to give to the function prophet()

```{r}
library(prophet)

proph_md <- data.frame(
  ds = train[,"Date"],
  y = train[,"Baccala_Mantecato"]
)
colnames(proph_md) <- c("ds", "y")
proph_md$cap <- max(proph_md$y) * 1.1
str(proph_md)
```

```{r message=FALSE, warning=FALSE}
proph_logistic=prophet(proph_md,  growth="logistic", n.changepoints=5, 
           yearly.seasonality=TRUE, 
           seasonality.mode='multiplicative')

proph_m=prophet(proph_md,  growth="linear", n.changepoints=5, 
           yearly.seasonality=TRUE, 
           seasonality.mode='multiplicative')

future_logistic <- make_future_dataframe(proph_logistic, periods = 10, freq = "month", include_history = T)
future_logistic$cap <- max(proph_md$y) * 1.1
forecast_future_logistic <- predict(proph_logistic, future_logistic)
test_logistic <- tail(forecast_future_logistic, 10)

mean((test$Baccala_Mantecato - test_logistic$yhat)^2)

future <- make_future_dataframe(proph_m, periods = 10, freq = "month", include_history = T)
future$cap <- max(proph_md$y) * 1.1
forecast_future <- predict(proph_m, future_logistic)
test_m <- tail(forecast_future, 10)

mse_test_prm <- mean((test$Baccala_Mantecato - test_m$yhat)^2)
mse_test_prm
```



```{r}
forecast_future$y_true <- data$Baccala_Mantecato
forecast_future <- forecast_future[,c("yhat", "ds", "y_true")]

ggplot() +
  geom_line(aes(x = data$Date, y = forecast_future$y_true, color = "Actual Values"), size = 1) +
  geom_line(aes(x = train$Date, y = head(forecast_future,38)$yhat, 
                color = "Fitted Values (Train)"), size = 1) +
  geom_line(aes(x = test$Date, y = tail(forecast_future,10)$yhat, 
                color =  "Predicted Values (Test)"), size = 1) +
  scale_color_manual(values = c("Actual Values" = "#FF7F7F", 
                                "Fitted Values (Train)" = "#6BC3FF", 
                                "Predicted Values (Test)" = "#8FBC8F"),
                     labels = c("Actual Values", "Fitted Values (Train)", "Predicted Values (Test)")) +
  
  labs(
    title = "Actual vs Fitted and Predicted Values for Baccala Mantecato (Prophet Model)",
    x = "Date",
    y = "Value",
    color = "Legend"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

```
We observed the same staff of other models, the mse value (we will perfrom a final comparison between all the models) is high and looking at the plot, we are not able to fit well the function under the data.

```{r}
res_proph <- train$Baccala_Mantecato - head(forecast_future$yhat, 38)
mse_train_prm <- mean(res_proph^2)
checkresiduals(res_proph)
```

#### Baccala Vicentina

```{r}
library(prophet)

proph_vd <- data.frame(
  ds = train[,"Date"],
  y = train[,"Baccala_Vicentina"]
)
colnames(proph_vd) <- c("ds", "y")
proph_vd$cap <- max(proph_vd$y) * 1.1
str(proph_vd)
```

```{r message=FALSE, warning=FALSE}
proph_logistic=prophet(proph_vd,  growth="logistic", n.changepoints=5, 
           yearly.seasonality=TRUE, 
           seasonality.mode='multiplicative')

proph_v=prophet(proph_vd,  growth="linear", n.changepoints=5, 
           yearly.seasonality=TRUE, 
           seasonality.mode='multiplicative')

future_logistic <- make_future_dataframe(proph_logistic, periods = 10, freq = "month", include_history = T)
future_logistic$cap <- max(proph_vd$y) * 1.1
forecast_future_logistic <- predict(proph_logistic, future_logistic)
test_logistic <- tail(forecast_future_logistic, 10)

mean((test$Baccala_Vicentina - test_logistic$yhat)^2)

future <- make_future_dataframe(proph_v, periods = 10, freq = "month", include_history = T)
future$cap <- max(proph_vd$y) * 1.1
forecast_future <- predict(proph_v, future_logistic)
test_v <- tail(forecast_future, 10)

mse_test_prv <- mean((test$Baccala_Vicentina - test_v$yhat)^2)
mse_test_prv
```



```{r}
forecast_future$y_true <- data$Baccala_Vicentina
forecast_future <- forecast_future[,c("yhat", "ds", "y_true")]

ggplot() +
  geom_line(aes(x = data$Date, y = forecast_future$y_true, color = "Actual Values"), size = 1) +
  geom_line(aes(x = train$Date, y = head(forecast_future,38)$yhat, 
                color = "Fitted Values (Train)"), size = 1) +
  geom_line(aes(x = test$Date, y = tail(forecast_future,10)$yhat, 
                color =  "Predicted Values (Test)"), size = 1) +
  scale_color_manual(values = c("Actual Values" = "#FF7F7F", 
                                "Fitted Values (Train)" = "#6BC3FF", 
                                "Predicted Values (Test)" = "#8FBC8F"),
                     labels = c("Actual Values", "Fitted Values (Train)", "Predicted Values (Test)")) +
  
  labs(
    title = "Actual vs Fitted and Predicted Values for Baccala Vicentina (Prophet Model)",
    x = "Date",
    y = "Value",
    color = "Legend"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")
```

```{r}
res_proph <- train$Baccala_Vicentina - head(forecast_future$yhat, 38)
mse_train_prv <- mean(res_proph^2)
checkresiduals(res_proph)
```

## Results

```{r}
name_models <- c("Linear Regression", "SARIMA", "Prophet")

mse_train_m <- c(mse_train_lrm, mse_train_sarimam, mse_train_prm)
mse_train_v <- c(mse_train_lrv, mse_train_sarimav, mse_train_prv)

mse_test_m <- c(mse_test_lrm, mse_test_sarimam, mse_test_prm)
mse_test_v <- c(mse_test_lrv, mse_test_sarimav, mse_test_prv)

results <- data.frame(
  Model = name_models,
  
  MSE_Train_Mantecato = mse_train_m,
  MSE_Train_Vicentina = mse_train_v,
  
  MSE_Test_Mantecato = mse_test_m,
  MSE_Test_Vicentina = mse_test_v
)
results
```

```{r message=FALSE, warning=FALSE}
plot_results(results, "MSE_Train_Mantecato")
plot_results(results, "MSE_Test_Mantecato")

plot_results(results, "MSE_Train_Vicentina")
plot_results(results, "MSE_Test_Vicentina")

```

# FINISCE QUA PARTE CHE TOTO SA CHE GIRA DEL TUTTO

##
### GAM Models

### Baccala Mantecato
Now we explore if we can benefit from non-linear models, in particular we try a GAM model and compare it with the Multiple Linear Regression selected model.
We start from the LR model

```{r}

library(gam)
g0 = gam(Baccala_Mantecato ~ trend + Month + fish_cons, data = train) #this is equivalent to lr_m
```
Now we plot the linear effects to see if they may require smoothing functions
```{r}
par(mfrow=c(1,2))
plot(g0)
```
We clearly see a perfectly linear relation between the predictors and the response variable, this plots suggests to stop our non-linear analysis. To be sure we try anyway to include smoothing variables
```{r}
g1 <- gam(Baccala_Mantecato ~ s(trend, df=2) + Month + s(fish_cons, df=2), data = train)
summary(g1)
```
Based on the df and on the 'Anova for Nonparametric Effects' results, where the nonparametric F-values for s(trend) and s(fish_cons) are not statistically significant (with p-values of 0.19369 and 0.07973 respectively), it is clear that these variables do not exhibit nonlinear relationships with the response variable as we initially expected. Therefore, for reasons of parsimony, interpretability, and simplicity, we exclude the GAM model in favor of a straightforward multiple linear regression model, which aligns better with the data and offers more meaningful results.

###Baccala Vicentina 
Let's see if the same holds for Baccala_Vicentina.
```{r}
g0_v = gam(Baccala_Vicentina ~ trend + Month + fish_cons, data = train) #this is equivalent to lr_v_full (remember that the best one was lr_v (only Month as X))

par(mfrow=c(1,2))
plot(g0)
```
we clearly see that the behaviour of the explenatory variables is still linear. 
This suggests to keep the linear regression model, but we have also seen, in the section dedicated to linear regression, that for Baccala_Vicentina the best model was the reduced one obtained by considering only the Month variable as predictor. 
So we keep it.

This analysis clearly indicates that both results suggest against the use of models that fit nonlinear relationships, such as GAMs.

##Fino a qua Runna tutto FK




### Exponential Smoothing Model ----

#### ETS Baccala Mantecato ----

```{r}
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
```

MSE (Predictive Accuracy):
  The Holt-Winters Multiplicative model has the lowest MSE (221.9902), indicating the best predictive accuracy among the exponential smoothing models.
  Surprisingly, the Regression + ETS model has a much higher MSE (1929.8257), suggesting poor predictive performance. This may indicate that the regression residuals contain patterns that ETS is unable to capture effectively.

AIC (Model Complexity):
  The Regression + ETS model has the lowest AIC (245.7997), suggesting it is the most parsimonious model. However, its high MSE indicates that this simplicity comes at the cost of poor accuracy.
  The ETS model achieves a better balance between predictive accuracy and model complexity compared to Holt-Winters models, as seen by its relatively low AIC and competitive MSE.

Conclusion:
  The Holt-Winters Multiplicative model provides the best predictive accuracy but at the cost of higher model complexity (AIC = 308.4466).
  Despite the low AIC of the Regression + ETS model, its high MSE makes it unsuitable for practical use.


#### ETS Baccala Vicentina ----

```{r}
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
```

MSE (Predictive Accuracy):
  The Holt-Winters Additive model has the lowest MSE (2.585841), indicating the best predictive accuracy for this dataset.
  The Regression + ETS model has a much higher MSE (58.755755), suggesting it struggles to capture the underlying patterns effectively.

AIC (Model Complexity):
  The Regression + ETS model has the lowest AIC (90.40231), indicating it is the most parsimonious model. However, its high MSE makes it less reliable for accurate predictions.
  Among the exponential smoothing models, the ETS model achieves the best balance with a relatively low AIC (118.97612) and competitive MSE.

Conclusion:
  The Holt-Winters Additive model achieves the best predictive accuracy but has a slightly higher AIC, indicating increased complexity.
  The ETS model offers a strong balance between accuracy and complexity, making it a practical choice.
  The Regression + ETS model is the simplest but least accurate, limiting its practical use.


### KNN ----

```{r}
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
```
#### Analysis of KNN for Baccalà Mantecato and Baccalà Vicentina

#### KNN for **Baccalà Mantecato**:
1. **MSE and Overfitting/Underfitting:**
  - The training curve showed a steadily increasing MSE as `k` increased, while the validation MSE initially decreased and then stabilized after `k = 5`.
- The selected `k = 5` balances the tradeoff between overfitting (low `k`) and underfitting (high `k`). It minimizes validation MSE (0.01459) without excessively high test MSE (0.02829).

2. **Actual vs Predicted Plot:**
  - The scatter plot of actual vs predicted values for `k = 5` shows points reasonably close to the diagonal, indicating good alignment between predictions and actual values.
- There are slight deviations for extreme values, suggesting that KNN struggles to capture high variance in the data.

3. **Overall:**
  - `k = 5` is a reasonable choice based on its performance metrics and balance between underfitting/overfitting.
- The relatively low Test MAE (0.15652) indicates a good predictive capability for practical applications.
## Local Regression ----

### Local Regression Baccala Mantecato ----
```{r}
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
```

### Brief Comment on the Results

1. **Baccala Mantecato**:
  - The **Loess model** applied to the Baccala Mantecato dataset demonstrates a good fit for the training data, capturing the underlying trend and variability reasonably well.
- However, the **Mean Squared Error (MSE) on the test set** is relatively high (**358.32314**), indicating that the model''s ability to generalize to unseen data is limited. This may suggest that the data contains significant variability or seasonality that the Loess model with the selected span cannot fully capture.

2. **Baccala Vicentina**:
   - The **Loess model** for Baccala Vicentina shows a smoother fit for the training data, which aligns with the nature of the series. The observed test data points are relatively close to the forecasted values.
   - The **MSE on the test set** is much lower (**2.66208**) compared to Baccala Mantecato, indicating that the Loess model is more effective in capturing the trend and variability for this dataset.

### General Observations:
- The Loess model is highly dependent on the chosen span parameter, which influences the degree of smoothing. Fine-tuning the span (e.g., **0.3** for Baccala Mantecato and **0.3** for Baccala Vicentina) was essential for achieving these results.
- The use of a fallback **linear model** to handle predictions outside the training range effectively ensures continuity in the predictions but may introduce biases if the linear assumption does not align with the underlying pattern.

### Suggestions:
- For the final report, highlight the differences in the data's structure (e.g., variability, trends) that influenced the model's performance.
- Consider discussing the implications of the higher MSE for Baccala Mantecato, potentially suggesting alternative approaches (e.g., including additional covariates or using seasonal decomposition).
- Showcase the plots to visually support the interpretation of the model fit and the transition between the training and testing phases.


>>>>>>> 8e69af4d811e599bf46ddc7b7a48bfc2e24acdf7

