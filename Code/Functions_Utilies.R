
mse = function(pred, real){
  mse = mean((real-pred)^2)
  return(mse)
}

plot_train_test <- function(train_test, name_y) {
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
         x = "Time",
         y = "Value") +
    scale_color_manual(values = c("Train" = "blue", "Test" = "red")) +
    theme_minimal()
}

compute_AIC <- function(n, RSS, k) {
  # GUARDA DOCUMENTAZIONE AIC: ?AIC
  logLik <- -n / 2 * (log(2 * pi) + log(RSS / n) + 1)
  AIC <- -2 * logLik + 2 * k
  return(AIC)
}

plot_train_pred <- function(y_train, y_pred, model_name) {
  plot_data <- data.frame(
    Time = 1:length(y_train),
    Observed = y_train,
    Predicted = y_pred
  )
  ggplot(plot_data, aes(x = Time)) +
    geom_line(aes(y = Observed), color = "blue", linewidth = 1) +
    geom_line(aes(y = Predicted), color = "red", linewidth = 1) +
    labs(
      title = paste("Observed and Predicted Values\nModel:", model_name),
      x = "Time",
      y = "Values"
    ) +
    theme_minimal()
}

plot_actual_vs_forecast <- function(actual, forecast, model_name) {
  forecast_data <- data.frame(
    Time = 1:length(actual),
    Actual = actual,
    Forecast = forecast
  )
  ggplot(forecast_data, aes(x = Time)) +
    geom_line(aes(y = Actual, color = "Actual"), linewidth = 1) +
    geom_line(aes(y = Forecast, color = "Forecast"), linewidth = 1, linetype = "dashed") +
    labs(
      title = paste("Actual vs Forecasted Values\nModel:", model_name),
      x = "Time",
      y = "Values"
    ) +
    scale_color_manual(values = c("Actual" = "blue", "Forecast" = "red")) +
    theme_minimal()
}

compute_metrics <- function(actual, predicted) {
  mae <- mean(abs(actual - predicted))
  rmse <- sqrt(mean((actual - predicted)^2))
  return(list(MAE = mae, RMSE = rmse))
}

analyze_residuals <- function(residuals, model_name) {
  par(mfrow = c(1, 3))
  plot(residuals, main = paste("Residuals of", model_name), ylab = "Residuals")
  acf(residuals, main = "ACF of Residuals")
  hist(residuals, main = "Histogram of Residuals", breaks = 10, col = "gray")
  par(mfrow = c(1, 1))
}

time_series_cv <- function(series, model_func, h = 12) {
  n <- length(series)
  errors <- numeric()
  for (i in seq(n - h, by = 1)) {
    train <- series[1:i]
    test <- series[(i + 1):(i + h)]
    model <- model_func(train)
    pred <- forecast(model, h = h)$mean
    errors <- c(errors, mean((test - pred)^2))
  }
  mean(errors)
}

grid_search <- function(model_func, param_grid, data) {
  results <- list()
  for (params in param_grid) {
    model <- do.call(model_func, c(list(data), params))
    results <- append(results, list(list(params = params, model = model)))
  }
  results
}

ensemble_forecast <- function(models, h = 12) {
  forecasts <- lapply(models, function(model) forecast(model, h = h)$mean)
  ensemble <- rowMeans(do.call(cbind, forecasts))
  return(ensemble)
}

decompose_series <- function(series, type = "additive") {
  decomposition <- decompose(ts(series, frequency = 12), type = type)
  plot(decomposition)
  return(decomposition)
}