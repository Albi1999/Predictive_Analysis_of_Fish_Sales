# First code, just to see the data plot and fit a couple of models to see how they perform

# LINEAR REGRESSION -- RIGA 231 circa
# TODO: GUARDARE CON TEST DI SIGNIFICATIVITÁ SE POSSIAMO RIMUOVERE UNA VARIABILE 
#       E.G.: TOLGO trainm$Year
#       Imo usiamo drop1 e update peró meglio vedere cosa fa prof

# BASS Model  -- RIGA 243 circa
# TODO: NON SO UN TUBO DEL BASS MODEL E MI É DIFFICILE ITERPRETARE I RISULTATI, 
#       IN LINEA DI MASSIMA MI PARE FACCIA CAGARE

# NEXT STEPS
# TODO: INSERIRE VARIABILE DEL FISH CONSUMPTION NEI MODELLI IN CUI É POSSIBILE FARLO
#       PROVARE NUOVI MODELLI-->GUARDARE CODICE PROF E SE POSSIBILE DOCUMENTAZION PERCHE CE ROBA FIGA
#       TIPO QUELLO CHE HO TROVATO SU GAM

rm(list=ls())

################################################################################
# Load packages ----
################################################################################

library(readxl)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(forecast)
library(lubridate)
library(lmtest) # DW test
library(DIMORA) # BASS Model
library(mgcv) # GAM Model

################################################################################
# USEFUL FUNCTIONS
################################################################################

split_train_test = function(data, name_y, prop){
  n_sample = floor(nrow(data)*prop)
  
  train = data[1:n_sample, ]
  y_train = train[[name_y]]
  
  test = data[(n_sample+1):nrow(data),]
  y_test = test[[name_y]]
  
  
  return(list(train = train, y_train = y_train, test = test, y_test = y_test))
}

plot_train_test = function(train_test, name_y){
  train_length <- length(train_test$y_train)
  test_length <- length(train_test$y_test)
  time_index <- c(1:(train_length + test_length))
  
  data_plot <- data.frame(
    Time = time_index,
    Value = c(as.numeric(train_test$y_train), as.numeric(train_test$y_test)),
    Type = c(rep("Train", train_length), rep("Test", test_length))
  )
  
  ggplot(data_plot, aes(x = Time, y = Value, color = Type)) +
    geom_line() +
    labs(title = paste(name_y, ": Train vs Test"),
         x = "Tempo",
         y = "Valore") +
    scale_color_manual(values = c("Train" = "blue", "Test" = "red")) +
    theme_minimal()
}

compute_AIC <- function(n, RSS, k) {
  
  # GUARDA DOCUMENTAZIONE AIC: ?AIC
  logLik <- -n / 2 * (log(2 * pi) + log(RSS / n) + 1)
  AIC <- -2 * logLik + 2 * k
  
  return(AIC)
}


plot_train_pred <- function(y_train,
                            y_pred,
                            model_name){
  plot_data = data.frame(
    Time = 1:length(y_train), 
    Observed = y_train,
    Predicted = y_pred)
  
  ggplot(plot_data, aes(x = Time)) +
    geom_line(aes(y = Observed),
              color = "blue", linewidth = 1) + 
    geom_line(aes(y = Predicted), color = "red",
              linewidth = 1) + 
    labs(
      title = paste("Observed and Predicted Values\nModel:",model_name),
      x = "Time",
      y = "Values"
    ) +
    theme_minimal()
}
################################################################################

################################################################################
# Load Data and Manipulation
################################################################################

data <- read_csv("Data/data.csv")

# Trasforma la colonna 'Date' in formato Date
data$Date <- as.Date(data$Date, format = "%Y-%m-%d")

# Check for NA values
colSums(is.na(data))

data <- data %>%
  mutate(Month = format(Date, "%m"),
         Year = as.factor(format(Date, "%Y"))
  )

head(data)

################################################################################
# LOAD DATA 2 --> variabile esplicativa
################################################################################

monthly_consumption <- read_excel("Data/Fish_consumption_ita_raw.xlsx")

monthly_consumption <- monthly_consumption %>% filter(CG == "Salmon")

monthly_consumption <- monthly_consumption %>%
  mutate(kg = as.numeric(`volume(Kg)`)) 
str(monthly_consumption)

monthly_time_series <- monthly_consumption %>%
  group_by(year, month) %>%
  summarise(kg = sum(kg)) %>%
  ungroup()

monthly_time_series <- monthly_time_series %>%
  filter(year > 2020 | (year == 2020 & month %in% c(11, 12)))

# Std
monthly_time_series <- monthly_time_series %>%
  mutate(kg_std = as.vector(scale(kg)))

monthly_time_series$Date <- as.Date(paste(monthly_time_series$year, monthly_time_series$month, "01", sep = "-"))

head(monthly_time_series)

ggplot(monthly_time_series, aes(x = Date, y = kg_std)) +
  geom_line(color = 'blue') +
  labs(title = 'Time Series of Consumed Kg (Salmon)', 
       x = 'Data', 
       y = 'Kg Consumed') +
  theme_minimal() +
  scale_x_date(labels = scales::date_format("%b %Y"), breaks = "3 months") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

################################################################################
# Plots
################################################################################

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

################################################################################
# TS Properties
################################################################################

ym = ts(data$Baccala_Mantecato, frequency = 12, start = c(2020,01))
yv = ts(data$Baccala_Vicentina, frequency = 12, start = c(2020,01))

plot.ts(yv)
plot.ts(ym)

# Stationary check
acf(data$Baccala_Mantecato)
acf(data$Baccala_Vicentina)

################################################################################
# ** TRAIN/TEST SPLIT **
################################################################################


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

################################################################################
# ** Linear Regression MODEL **
################################################################################

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

res_LR_year_month <- residuals(fit_LR_year_month)
plot(res_LR_year_month, ylab="residuals")
dwtest(fit_LR_year_month)

#####

AIC(fit_LR_month)
AIC(fit_LR_year)
AIC(fit_LR_year_month) # lower AIC-->best model
# TODO: GUARDARE CON TEST DI SIGNIFICATIVITÁ SE POSSIAMO RIMUOVERE UNA VARIABILE 
#       E.G.: TOLGO trainm$Year

################################################################################
# ** BASS MODEL **
################################################################################

y_train_tsm = ts(y_train_m, start = c(2021, 01), frequency = 12)
end(y_train_tsm)

fit_BM <- BM(y_train_tsm, display = TRUE)
summary(fit_BM)

AIC_BM <- compute_AIC(n = length(fit_BM$data),
                      RSS = fit_BM$RSS,
                      k = length(fit_BM$coefficients))
AIC_BM
# TODO: NON SO UN TUBO DEL BASS MODEL E MI É DIFFICILE ITERPRETARE I RISULTATI, 
#       IN LINEA DI MASSIMA MI PARE FACCIA CAGARE

################################################################################
# ** GAM MODEL **
################################################################################

trainm$Month <- as.numeric(trainm$Month)
gam_model <- gam(Baccala_Mantecato ~ s(Month), data = trainm)

summary(gam_model)
# leggendo documentazione, sembra figo -> https://www.rdocumentation.org/packages/mgcv/versions/1.9-1/topics/gam
plot(gam_model, pages=1, seWithMean=TRUE) 
gam.check(gam_model)
#

gam_forecast <- predict(gam_model, newdata = trainm)
AIC(gam_model)


plot_train_pred(y_train = trainm$Baccala_Mantecato,
                y_pred = gam_forecast,
                model_name = "GAM Model")

res_GAM <- residuals(gam_model)
plot(res_GAM, ylab="residuals")
dwtest(gam_model)