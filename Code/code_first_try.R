# First code, just to see the data plot and fit a couple of models to see how they perform

# Load packages ----

library(readxl)
library(ggplot2)

# Load the data ----

# Exchange rates
us_eur <- read.csv2("Data/Dollar_Euro_daily_1999_2024.csv", sep = ",")
us_eur$DATE <- as.Date(us_eur$DATE, format = "%Y-%m-%d")
us_eur$Rate <- as.numeric(us_eur$US.dollar.Euro..EXR.D.USD.EUR.SP00.A.)

swiss_eu <- read.csv2("Data/Swiss_Euro_daily_2020_2024.csv", sep = ",")
swiss_eu$DATE <- as.Date(swiss_eu$DATE, format = "%Y-%m-%d")
swiss_eu$Rate <- as.numeric(swiss_eu$Swiss.franc.Euro..EXR.D.CHF.EUR.SP00.A.)

# Immigration 
swiss_immigration <- read_excel("Data/Immigration_swiss_2011_2022_cleaned.xlsx")
swiss_immigration$Switzerland <- as.numeric(swiss_immigration$Switzerland)


# Emigration 
swiss_emigration <- read_excel("Data/Emigration_swiss_2011_2022_cleaned.xlsx")
swiss_emigration$Switzerland <- as.numeric(swiss_emigration$Switzerland)

# Plot the data ----

# US Dollar - Euro
plot(us_eur$DATE, us_eur$Rate, type = "l", xlab = "Date",
     ylab = "Exchange Rate", main = "Dollar-Euro Exchange Rate", col = "blue")

# Swiss Franc - Euro
plot(swiss_eu$DATE, swiss_eu$Rate, type = "l", xlab = "Date",
     ylab = "Exchange Rate", main = "Swiss Franc-Euro Exchange Rate", col = "green")

# Preliminary models ----

# Linear model

# US Dollar - Euro
us_eur_lm <- lm(Rate ~ DATE, data = us_eur)
summary(us_eur_lm)
# Very bad R-squared value
# It seems that the linear model is not a good fit since there is no evident trend

# Swiss Franc - Euro
swiss_eu_lm <- lm(Rate ~ DATE, data = swiss_eu)
summary(swiss_eu_lm)
# Good R-squared value
# There is an evident trend in the data, so the linear model is a good fit

# Show the linear trend on the original plot

# US Dollar - Euro
plot(us_eur$DATE, us_eur$Rate, type = "l", xlab = "Date", ylab = "Exchange Rate",
     main = "Dollar-Euro Exchange Rate", col = "blue")
abline(us_eur_lm, col = "orange", lwd = 2)

# Swiss Franc - Euro
plot(swiss_eu$DATE, swiss_eu$Rate, type = "l", xlab = "Date", ylab = "Exchange Rate",
     main = "Swiss Franc-Euro Exchange Rate", col = "green")
abline(swiss_eu_lm, col = "orange", lwd = 2)

################################################################################
# AM 29/12/2024
################################################################################
# The following analysis will be focused on the CHF/EUR rate
library(tidyverse)

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

################################################################################
# NON ci sono weekend

all_dates <- seq(min(ts_chf_eu$DATE), max(ts_chf_eu$DATE), by = "day")
full_dates <- data.frame(DATE = all_dates)
missing_dates <- setdiff(all_dates, ts_chf_eu$DATE)

ts_chf_eu <- full_dates %>%
  left_join(ts_chf_eu, by = "DATE")

ts_chf_eu$TIME.PERIOD <- format(ts_chf_eu$DATE, "%d %b %Y")

# MSA è servito a qualcosa...
library(zoo)
ts_chf_eu$Rate <- na.locf(ts_chf_eu$Rate)#imputation method
dim(ts_chf_eu)[1] == length(all_dates)
ts_chf_eu[1:10, ]

################################################################################
# Plot
library(dplyr)
library(ggplot2)

ggplot(ts_chf_eu, aes(x = DATE, y = Rate)) +
  geom_line(color = "blue") +
  geom_hline(yintercept = 1, color = "red", linetype = "dashed") +
  labs(title = "Serie Storica Swiss Franc/Euro con Linea y=1",
       x = "Data",
       y = "Tasso di Cambio") +
  theme_minimal()

ts_chf_eu <- ts_chf_eu %>%
  mutate(
    Year = as.factor(format(DATE, "%Y")),
    DayOfYear = as.numeric(format(DATE, "%j"))
  )

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

# Non mi sembra di vedere alcuna stagionalità precisa i.e. picchi tutti a Maggio

ts_chf_eu$Year <- format(ts_chf_eu$DATE, "%Y")
yearly_count <- ts_chf_eu %>%
  count(Year)
yearly_count # Number of rows for each year-->mettiamo frequency=365 
#                                             nella funzione ts()



################################################################################

ts_chf_eu <- ts_chf_eu %>% 
  select(-c("Year","DayOfYear","ts_chf_eu"))

# We observed from the plots that the ts is not stationary
y = ts_chf_eu$Rate
Acf(y)

y = ts(ts_chf_eu$Rate, frequency = 365, start = c("2020-01-02"), end = c(""))
# then a fist step is to make the first diff
y_diff = diff(y, differences = 1) # now obv the length will be 1247 and no more 1248
# convert y_diff in ts format
Y = ts(y_diff, frequency = 365, start = c("2020-01"), end = c("")) # .25 bc anno bisestile

plot.ts(Y) # mean = 0 YES
           # var = constant ~YES
acf(Y) # we solve the stationary issue

# we can now proceed with the Modelling phase
# ** TRAIN/TEST SPLIT **
library(forecast)
n_sample = length(Y)[1]*0.9
y_train <- subset(Y, start = 1, end = n_sample)
y_test <- subset(Y, start = n_sample + 1)

acf(y_train)

# ** Linear Regression MODEL **

fit_lr1 <- tslm(y_train~trend)
summary(fit_lr1)  #FA CAGARE
fit_lr2 <- tslm(y_train~trend+season)
summary(fit_lr2)  #ANCHE PEGGIO-->QULCS NON VA

anova(fit_LR1)

##plot of the model
plot(y_train, xlab="XXX", ylab="YYY")
abline(fit_lr1, col=3)

##check the residuals? are they autocorrelated? Test of DW
dwtest(fit_lr1)


# ** BASS MODEL **

library(DIMORA)

fit_BM <- BM(y_train, display = TRUE)
summary(fit_BM)

plot(y_diff, type = "b", xlab = "XXX", ylab = "YYY"
     pch = 16, lty = 3, xaxt = "n", cex = 0.6)
lines(pred.instcas, lwd = 2, col = 2)  # Aggiungi la linea del modello


cat("prova1")









