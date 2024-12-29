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

# Check for NA values
colSums(is.na(swiss_eu))
# we have 62 NAs
swiss_eu[is.na(swiss_eu),] # in 'Rate' column the NAs look like "NA" while in the other cols are "<NA>"
# btw the length of the ts is large enoug to consider just a subset i.e. just from year = 2020 
ts_chf_eu = swiss_eu[swiss_eu$DATE>as.Date("2020-02-01"), ] # 01 jan 2020 does not exist
dim(ts_chf_eu) # 1248 obs

# We observed from the plots that the ts is not stationary
y = ts_chf_eu$Rate
acf(y)
# then a fist step is to make the first diff
y_diff = diff(y, differences = 1) # now obv the length will be 1247 and no more 1248
# convert y_diff in ts format
Y = ts(y_diff, frequency = 365.25) # .25 bc anno bisestile
# TODO: non riesco a mettere data di start

plot.ts(Y) # mean = 0 YES
           # var = constant ~YES
acf(Y) # we solve the stationary issue

# we can now proceed with the Modelling phase
# ** TRAIN/TEST SPLIT **
n_sample = length(Y)[1]*0.9
y_train = Y[1:n_sample]
y_test = Y[(n_sample+1):length(Y)]
acf(y_train)













