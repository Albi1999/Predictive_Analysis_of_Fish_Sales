# Baccalà Time Series Analysis in Treviso

## Overview

This repository contains the work for a Business, Economic, and Financial Data (BEFD) course project. The project focuses on analyzing the time series data of Baccalà production in Treviso, Italy, using advanced statistical and machine learning methods to identify trends, seasonality, and optimize forecasting models.

------------------------------------------------------------------------

## Objectives

-   Explore and preprocess data on Baccalà Mantecato and Baccalà alla Vicentina sales.
-   Perform exploratory data analysis (EDA) to uncover trends, seasonality, and anomalies.
-   Develop and compare models for short-term and long-term forecasting, such as:
    -   **Linear Regression Models**: Including trend and seasonality.
    -   **SARIMA Models**: Seasonal autoregressive integrated moving average configurations.
    -   **SARIMAX Models**: Incorporating external regressors like salmon consumption.
    -   **Exponential Smoothing Methods**: Including Holt-Winters for trend and seasonality.
    -   **Bass and Generalized Bass Models**: For modeling product adoption curves.
    -   **Generalized Additive Models (GAM)**: Flexible trend modeling using splines.
    -   **Prophet Models**: Robust automatic seasonality detection and trend analysis.
-   Evaluate model performance using metrics like Mean Squared Error (MSE) and visualize results to provide actionable insights for inventory management and production planning.

------------------------------------------------------------------------

## Project Structure

### Files

-   **`BEFD_Report.pdf`**: Final report detailing the analysis, methodology, and results.
-   **`FINAL_CODE_NEW.Rmd`**: RMarkdown file containing the complete workflow, from data processing to model evaluation and forecasting.
-   **Code:**
    -   
-   **Data:**
    -   **`Business_Notes.pdf`**: Notes and theoretical insights on time series modeling.

------------------------------------------------------------------------

## Key Features

### 1. **Exploratory Data Analysis (EDA)**

-   Visualizations to understand data distribution, trends, and seasonality.
-   Autocorrelation and Partial Autocorrelation plots.

### 2. **Modeling Techniques**

#### **Linear Models**:

-   Simple Linear Regression with trend and seasonality components.
-   **SARIMA** and **SARIMAX** for modeling seasonal patterns and incorporating external regressors.

#### **Diffusion Models**:

-   Bass diffusion models for growth prediction of Baccalà variants.
-   Generalized Bass Models (GBM) to incorporate external shocks and dynamic market potentials.

#### **Exponential Smoothing**:

-   Simple Exponential Smoothing.
-   Holt-Winters method for additive and multiplicative seasonality.

#### Advanced Forecasting:

-   **Prophet:** Automatic seasonality detection and robust forecasting with changepoints.

------------------------------------------------------------------------

## Instructions

### 1. **Setup**

To run the analysis, you need the following dependencies installed:

``` r
install.packages(c("readxl", "forecast", "fpp2", "gam", "DIMORA", "prophet", "gbm"))
```

### 2. **Execution**

-   Clone the repository:

    ``` bash
    git clone https://github.com/Albi1999/Time_Series_Analysis_in_Treviso.git
    ```

-   Open the RMarkdown file (`FINAL_CODE_NEW.Rmd`) in RStudio.

-   Knit the file to generate the HTML report.

-   Explore individual scripts for in-depth analysis of specific methods.

### 3. **Data**

Ensure the required datasets are in the appropriate directories: - Example: `Lab/Data/` - Datasets used include: - Monthly sales data for Baccalà Mantecato and Baccalà alla Vicentina. - External data on salmon consumption as an explanatory variable.

------------------------------------------------------------------------

## Results

### Performance Comparison

| Model                 | Training MSE | Testing MSE |
|-----------------------|--------------|-------------|
| Linear Regression     | x.xxx        | x.xxx       |
| SARIMA                | x.xxx        | x.xxx       |
| SARIMAX               | x.xxx        | x.xxx       |
| Exponential Smoothing | x.xxx        | x.xxx       |
| Bass Model            | x.xxx        | x.xxx       |


-   Full comparison and insights are detailed in the report.
-   Visualizations highlight the model fit and prediction intervals.

------------------------------------------------------------------------

## Contributions

-   **Team Members:**
    -   Flavio Kaci
    -   Antonio Mattesco
    -   Alberto Calabrese

------------------------------------------------------------------------

## License

This project is licensed under the MIT License. See `LICENSE` for details.

------------------------------------------------------------------------

## Acknowledgments

-   Course: Business, Economic, and Financial Data (BEFD)
-   Instructor: Mariangela Guidolin
-   University: University of Padua
-   MCs: Data Science
For further questions, feel free to open an issue or contact the contributors!

</div>

<p>
  <img alt="R" src="https://img.shields.io/badge/R-276DC3?logo=r&logoColor=white&style=plastic" height="25"/>
</p>