# Analysis of Baccala Mantecato and Baccala alla Vicentina

## Package Loading and General Configuration

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
library(prophet)


base_path <- "D:/Projects_GitHub/BEFD_Project/" # Set base path
source(file.path(base_path, "Code/Functions_Utilies.R"))


## Data Loading and Preprocessing ----

data <- read_csv(file.path(base_path, "Data/data.csv"))
data$Date <- as.Date(data$Date, format = "%Y-%m-%d") # ensure date format
data <- data %>% mutate(Month = format(Date, "%m"),
                        Year = as.factor(format(Date, "%Y"))) # create useful features for the models
data$trend <- 1:nrow(data)

head(data,3)

colSums(is.na(data))


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

data$fish_cons <- fish_cons$kg_std[,1]
head(data,3)

## Explanatory Analysis ----

ggplot(data, aes(x = Date)) +
  geom_line(aes(y = Baccala_Mantecato, color = "Baccala Mantecato"), size = 1) +
  geom_line(aes(y = Baccala_Vicentina, color = "Baccala Vicentina"), size = 1) +
  labs(title = "Monthly Time Series of Baccala Mantecato and Baccala Vicentina",
       x = "Date",
       y = "Quantity") +
  scale_color_manual(values = c("Baccala Mantecato" = "#FF7F7F", "Baccala Vicentina" = "#6BC3FF")) +
  theme_minimal() +
  theme(legend.position = "bottom")

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

ym = ts(data$Baccala_Mantecato, frequency = 12, start = c(2021, 1))
yv = ts(data$Baccala_Vicentina, frequency = 12, start = c(2021, 1))

if (end(ym)[1] != 2024 || end(ym)[2] != 12) {
  print("Error in ts ym")
}
if (end(yv)[1] != 2024 || end(yv)[2] != 12) {
  print("Error in ts yv")
}

tsdisplay(data$Baccala_Mantecato, main="Baccala Mantecato")
tsdisplay(data$Baccala_Vicentina, main="Baccala Vicentina")
#Acf(data$Baccala_Mantecato, main = "ACF of Baccala Mantecato", col = "#FF7F7F", lwd = 2)
#Acf(data$Baccala_Vicentina, main = "ACF of Baccala Vicentina", col = "#6BC3FF", lwd = 2)


## TRAIN/TEST SPLIT ----

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


## Modelling Phase
### Linear Regression Model

#### Baccala Mantecato

lr_m <- lm(Baccala_Mantecato ~ trend + Month + fish_cons, data = train)
summary(lr_m)
#lr_m_<missing_variable>
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

resid_lr <- residuals(lr_m)
mse_train_lrm <- mean(resid_lr^2)
tsdisplay(resid_lr, main="Linear Regression Residuals for Baccala Mantecato")

dwtest(lr_m)

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

#### Baccala Vicentina

lr_v_full <- lm(Baccala_Vicentina ~ trend + Month + fish_cons, data = train)
summary(lr_v_full)

lr_v <- update(lr_v_full, .~. - fish_cons)
summary(lr_v)

lr_v <- update(lr_v, .~. - trend)
summary(lr_v)

cat("Model with month:\n")
cat("  AIC:", AIC(lr_v), "\n")
cat("  Adjusted R²:", summary(lr_v)$adj.r.squared, "\n\n")

cat("Full model:\n")
cat("  AIC:", AIC(lr_v_full), "\n")
cat("  Adjusted R²:", summary(lr_v_full)$adj.r.squared, "\n")

resid_lrv <- residuals(lr_v)
mse_train_lrv <- mean(resid_lrv^2)
tsdisplay(resid_lrv, main="Linear Regression Residuals for Baccala Vicentina")

dwtest(lr_v)

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

### SARIMA Model

#### Baccala Mantecato

ts_m <- ts(train$Baccala_Mantecato, start = c(2021, 01), frequency = 12)
plot.ts(ts_m)

ts_m1 <- diff(ts_m,1)
tsdisplay(diff(ts_m,1), main = "Baccala Mantecato with Lag 1")
#Acf(ts_m1, main = "ACF of Baccala Mantecato with Lag 1", col = "#FF7F7F", lwd = 2)
#Pacf(ts_m1, main = "PACF of Baccala Mantecato with Lag 1", col = "#6BC3FF", lwd = 2)

ts_m_12 <- diff(ts_m, lag = 12)
tsdisplay(diff(ts_m, lag = 12), main = "Baccala Mantecato with Lag 12")
#Acf(ts_m_12, main = "ACF of Baccala Mantecato with Lag 12", col = "#FF7F7F", lwd = 2)
#Pacf(ts_m_12, main = "PACF of Baccala Mantecato with Lag 12", col = "#6BC3FF", lwd = 2)

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
mse_test_sarimam <- mse(test$Baccala_Mantecato, forecast(sarima_model3m, h = length(y_testm))$mean)
mse_test_sarimam

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

resid3 <- residuals(sarima_model3m)
mse_train_sarimam <- mean(resid3^2)
tsdisplay(resid3, main="SARIMA(0,1,0)(0,1,0)[12] Residuals for Baccala Mantecato")
#Acf(resid3, main = "ACF for SARIMA Residuals of Baccala Mantecato", col = "#FF7F7F", lwd = 2)
#Pacf(resid3, main = "PACF for SARIMA Residuals of Baccala Mantecato", col = "#6BC3FF", lwd = 2)

#### Baccala Vicentina

ts_v <- ts(train$Baccala_Vicentina, start = c(2021, 01), frequency = 12)
plot.ts(ts_v)

tsdisplay(ts_v, main = "Baccala Vicentina with Lag 1")
#Acf(ts_v, main = "ACF of Baccala Vicentina", col = "#FF7F7F", lwd = 2)
#Pacf(ts_v, main = "PACF of Baccala Vicentina", col = "#6BC3FF", lwd = 2)

ts_v_12 <- diff(ts_v, lag = 12)
tsdisplay(ts_v_12, main = "Baccala Vicentina with Lag 12")
#Acf(ts_v_12, main = "ACF of Baccala Vicentina with Lag 12", col = "#FF7F7F", lwd = 2)
#Pacf(ts_v_12, main = "PACF of Baccala Vicentina with Lag 12", col = "#6BC3FF", lwd = 2)

auto_sarima_modelv <- auto.arima(ts_v)
summary(auto_sarima_modelv)
# SARIMA(0,0,0)(0,1,0)[12]
sarima_model2v <- Arima(ts_v, order=c(0,0,0), seasonal=c(0,1,0))
summary(sarima_model2v)

resid2 <- residuals(sarima_model2v)
mse_train_sarimav <- mean(resid2^2)
tsdisplay(resid2, main="SARIMA(0,0,0)(0,1,0)[12] Residuals for Baccala Vicentina")
#Acf(resid2, main = "ACF for SARIMA Residuals of Baccala Vicentina", col = "#FF7F7F", lwd = 2)
#Pacf(resid2, main = "PACF for SARIMA Residuals of Baccala Vicentina", col = "#6BC3FF", lwd = 2)

mse_test_sarimav <- mse(test$Baccala_Vicentina, forecast(sarima_model2v, h = length(y_testv))$mean)
mse_test_sarimav

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

### Prophet Model

#### Baccala Mantecato

proph_md <- data.frame(
  ds = train[,"Date"],
  y = train[,"Baccala_Mantecato"]
  )
colnames(proph_md) <- c("ds", "y")
proph_md$cap <- max(proph_md$y) * 1.1
str(proph_md)

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


res_proph <- train$Baccala_Mantecato - head(forecast_future$yhat, 38)
mse_train_prm <- mean(res_proph^2)
#checkresiduals(res_proph)
tsdisplay(res_proph, main="Prophet Residuals for Baccala Mantecato")

aar1 <- Arima(res_proph, order=c(1,0,0))# didn't help-->seasonal=list(order=c(0,1,0),period=12))
res_proph1 <- train$Baccala_Mantecato - (head(forecast_future$yhat, 38) + fitted(aar1))
mse_train_prm1 <- mean(res_proph1^2)
mse_train_prm1
#checkresiduals(res_proph)
tsdisplay(res_proph1)

mean((test$Baccala_Mantecato - (test_m$yhat + as.numeric(forecast(aar1, h=10)$mean)))^2)


#### Baccala Vicentina

proph_vd <- data.frame(
  ds = train[,"Date"],
  y = train[,"Baccala_Vicentina"]
  )
colnames(proph_vd) <- c("ds", "y")
proph_vd$cap <- max(proph_vd$y) * 1.1
str(proph_vd)

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

res_proph <- train$Baccala_Vicentina - head(forecast_future$yhat, 38)
mse_train_prv <- mean(res_proph^2)
tsdisplay(res_proph, main="Prophet Residuals for Baccala Vicentina")

## Results

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


plot_results(results, "MSE_Train_Mantecato")
plot_results(results, "MSE_Test_Mantecato")

plot_results(results, "MSE_Train_Vicentina")
plot_results(results, "MSE_Test_Vicentina")



