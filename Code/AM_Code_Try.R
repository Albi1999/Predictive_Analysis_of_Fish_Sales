# First code, just to see the data plot and fit a couple of models to see how they perform

rm(list=ls())

################################################################################
# Load packages ----
################################################################################

library(readxl)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(forecast)
library(DIMORA) # BASS Model

################################################################################
# USEFUL FUNCTIONS
################################################################################
plot_train_pred <- function(y_train,
                            y_pred,
                            model_name){
  plot_data <- data.frame(
    Time = 1:length(y_train), 
    Observed = y_train,
    Predicted = y_pred
  )
  
  ggplot(plot_data, aes(x = Time)) +
    geom_line(aes(y = Observed),
              color = "blue", linewidth = 1) + 
    geom_line(aes(y = Predicted), color = "red",
              linewidth = 1) + 
    labs(
      title = paste("Observed vs Predicted Values Over Time\nModel:",model_name),
      x = "Time",
      y = "Values"
    ) +
    theme_minimal()
}
################################################################################

################################################################################
# Load Data and Manipulation
################################################################################

# The following analysis will be focused on the CHF/EUR rate

swiss_eu <- read.csv2("Data/Swiss_Euro_daily_2020_2024.csv", 
                      sep = ",",
                      na.strings = c("")
                      )

swiss_eu <- read.csv2("Data/Swiss_Euro_daily_2020_2024.csv", sep = ",") %>%
  mutate(
    DATE = as.Date(DATE, format = "%Y-%m-%d"),
    Rate = as.numeric(Swiss.franc.Euro..EXR.D.CHF.EUR.SP00.A.)
  ) %>%
  select(-Swiss.franc.Euro..EXR.D.CHF.EUR.SP00.A.)

# Check for NA values
colSums(is.na(swiss_eu))

# we have 62 NAs btw the length of the ts is large enoug to consider just a subset 
# i.e. just from year = 2020 
ts_chf_eu = swiss_eu[swiss_eu$DATE>=as.Date("2020-01-01"), ] # N.B.: 01 jan 2020 does not exist
dim(ts_chf_eu) # 12470 obs

rownames(ts_chf_eu) <- NULL
ts_chf_eu[1:5,]

# NON ci sono weekend e giorni festivi
# create the missing dates and impute the Rate value using the previous one

all_dates <- seq(min(ts_chf_eu$DATE), max(ts_chf_eu$DATE), by = "day")
full_dates <- data.frame(DATE = all_dates)
missing_dates <- setdiff(all_dates, ts_chf_eu$DATE)

ts_chf_eu <- full_dates %>%
  left_join(ts_chf_eu, by = "DATE")

ts_chf_eu$TIME.PERIOD <- format(ts_chf_eu$DATE, "%d %b %Y")

# MSA è servito a qualcosa...
ts_chf_eu$Rate <- na.locf(ts_chf_eu$Rate)#imputation method
dim(ts_chf_eu)[1] == length(all_dates)


ts_chf_eu <- ts_chf_eu %>%
  mutate(month = format(DATE, "%m"),
         Year = as.factor(format(DATE, "%Y")),
         DayOfYear = as.numeric(format(DATE, "%j")) # udeful for the plots
         )
         
ts_chf_eu[1:10,]

################################################################################
# Plots
################################################################################

# Plot di tutte le serie temporali separate per anno
ggplot(ts_chf_eu, aes(x = DATE, y = Rate, color = Year, group = Year)) +
  geom_line() +  # Disegna le linee per ogni anno
  labs(x = "Days from Start of Year", y = "EUR/CHF", 
       title = "Time Series for Each Year") +
  theme_minimal() +
  theme(legend.title = element_blank())


ggplot(ts_chf_eu, aes(x = DayOfYear, y = Rate, color = Year, group = Year)) +
  geom_line() +  # Disegna le linee per ogni anno
  labs(x = "Days from Start of Year", y = "EUR/CHF", 
       title = "Time Series for Each Year") +
  theme_minimal() +
  theme(legend.title = element_blank())

# Non mi sembra di vedere alcuna stagionalità precisa 
ts_chf_eu$Year <- format(ts_chf_eu$DATE, "%Y")
yearly_count <- ts_chf_eu %>%
  count(Year)
yearly_count # Number of rows for each year-->mettiamo frequency=365 
#                                             nella funzione ts()


################################################################################
# TS Properties
################################################################################

ts_chf_eu <- ts_chf_eu %>% 
  select(-c("DayOfYear"))

# We observed from the plots that the ts is not stationary
Acf(ts_chf_eu$Rate)

y = ts(ts_chf_eu$Rate, frequency = 365, start = c(2020,01,02))
# then a fist step is to make the first diff
y_diff = diff(y, differences = 1) # now obv the length will be 1247 and no more 1248
# convert y_diff in ts format

plot.ts(y_diff) # mean = 0 YES
           # var = constant ~YES
acf(y_diff) # we solve the stationary issue


################################################################################
# ** TRAIN/TEST SPLIT **
################################################################################

n_sample = length(y_diff)[1]*0.9
train = ts_chf_eu[1:n_sample, ]
y_train = train$Rate
test = ts_chf_eu[(n_sample+1):nrow(ts_chf_eu),]
y_test = test$Rate

y_train_ts <- subset(y_diff, start = 1, end = n_sample)
y_test_ts <- subset(y_diff, start = n_sample + 1)

################################################################################
# ** Linear Regression MODEL **
################################################################################

tt = 1:nrow(train)

fit_LR_month <- lm(y_train ~ tt + train$month)
summary(fit_LR_month)

plot_train_pred(y_train = y_train,
                y_pred = predict(fit_LR_month),
                model_name = "Linear Regression w/ Monthly Seasonality")

##

fit_LR_year <- lm(y_train ~ tt + train$Year)
summary(fit_LR_year)

plot_train_pred(y_train = y_train,
                y_pred = predict(fit_LR_year),
                model_name = "Linear Regression w/ Monthly Seasonality")

##

fit_LR_year_month <- lm(y_train ~ tt + train$Year + train$month)
summary(fit_LR_year_month)

plot_train_pred(y_train = y_train,
                y_pred = predict(fit_LR_year_month),
                model_name = "Linear Regression w/ Monthly Seasonality")

#####

AIC(fit_LR_month)
AIC(fit_LR_year)
AIC(fit_LR_year_mont)

################################################################################
# ** BASS MODEL **
################################################################################

fit_BM <- BM(y_train_ts, display = TRUE)
summary(fit_BM)

plot(y_diff, type = "b", xlab = "XXX", ylab = "YYY",
     pch = 16, lty = 3, xaxt = "n", cex = 0.6)

pred_BM <- predict(fit_BM, newx=c(1:900))
pred.instcas<- make.instantaneous(pred_BM)

lines(pred.instcas, lwd = 2, col = 2) 




