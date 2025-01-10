
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
```

```{r SETWD, warning=FALSE}
base_path <- "D:/Projects_GitHub/BEFD_Project/" # Set base path
```

## Data Loading and Preprocessing

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

## Explanatory Analysis

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

## Train-Test Split

# 3. TRAIN/TEST SPLIT ----

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
From the results above we clearly see that the best model fit belong to the full model with all predictors: trend, monthly seasonality and fish consumption.

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

Finally we will analyze the residuals because...

```{r}
resid_lr <- residuals(lr_v)
plot(resid_lr)
```
```{r}
dwtest(lr_v)
```

When we analyze the residuals of the linear regression model, as shown in the plot below, we observe no particular patterns. The residuals appear to be randomly scattered, indicating that the model has appropriately captured the underlying trends in the data. This suggests that the assumptions of linearity, constant variance, and independence are reasonably satisfied, and the model's fit is adequate for forecasting purposes.
On the other hand we perform the Durbin-Watson test, that is used to check for autocorrelation in the residuals of a regression model.
Since the p-value is greater than the significance level (0.05), we fail to reject the null hypothesis.

## ARIMA Model























