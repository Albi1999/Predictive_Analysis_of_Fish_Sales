# Predictive Analysis of Baccalà Sales: Insights and Challenges for Arte Culinaria da Jhonny

## Overview

This repository contains the work for a Business, Economic, and Financial Data (BEFD) course project. The project focuses on analyzing the time series data of Baccalà production in Treviso, Italy, using advanced statistical and machine learning methods to identify trends, seasonality, and optimize forecasting models.

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

## Project Structure

### Files

-   **`BEFD_Report.pdf`**: Final report detailing the analysis, methodology, and results.

-   **`FINAL_CODE.Rmd`**: RMarkdown file containing the complete workflow, from data processing to model evaluation and forecasting.

-   **BEFD_Report:**

    -   PlotsBEFD: Folder containing plots generated for the report.

    -   `main.tex`: LaTeX file for the report.

-   **Code:**

    -   `Main_Code.R`: Main code for the project.
    
    -   `Functions_Utilies.R`: Utility functions for data processing and model evaluation.

-   **Data:**

    -   `data.csv`: Raw data file containing monthly sales data for Baccalà Mantecato and Baccalà alla Vicentina.

    -   `Fish_consumption_ita_raw.xlsx`: External data file containing fish consumption data.

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

## Instructions

### 1. **Setup**

To run the analysis, you need the following dependencies installed:

``` r
install.packages(c("readxl", "forecast", "fpp2", "gam", "DIMORA", "prophet", "gbm", "tidyverse"))
```

### 2. **Execution**

-   Clone the repository:

    ``` bash
    git clone https://github.com/Albi1999/Predictive_Analysis_of_Fish_Sales.git
    ```

-   Open the RMarkdown file (`FINAL_CODE_NEW.Rmd`) in RStudio.

-   Knit the file to generate the HTML report.

-   Explore individual files for in-depth analysis of specific methods.

### 3. **Data**

Ensure the required datasets are in the appropriate directories: - Example: `Data/` - `data.csv` - `Fish_consumption_ita_raw.xlsx`

## Results

### Performance Comparison

| Model   | Train_Mantecato | Train_Vicentina | Test_Mantecato | Test_Vicentina |
|---------|-----------------|-----------------|----------------|----------------|
| LR      | 6.7755          | 0.1879          | 45.5953        | 1.5124         |
| SARIMA  | 29.7579         | 0.3605          | 104.5050       | 2.2650         |
| SARIMAX | 14.2973         | 0.3683          | 64.0557        | 2.3494         |
| ETS     | 13.2462         | 0.1936          | 111.7995       | 1.4348         |
| Prophet | 9.2290          | 0.1130          | 119.0730       | 1.8423         |

-   Full comparison and insights are detailed in the report.
-   Visualizations highlight the model fit and prediction intervals.

## Contributions

-   **Team Members:**
    -   [Flavio Kaci](https://github.com/Flavio1912)
    -   [Antonio Mattesco](https://github.com/antnmttsc)
    -   [Alberto Calabrese](https://github.com/Albi1999)

## License

This project is licensed under the MIT License. See `LICENSE` for details.

## Acknowledgments

-   Course: Business, Economic, and Financial Data (BEFD)
-   Professor: Mariangela Guidolin
-   University: University of Padua
-   MCs: Data Science

For further questions, feel free to open an issue or contact the contributors!

</div>

<p><img src="https://img.shields.io/badge/R-276DC3?logo=r&amp;logoColor=white&amp;style=plastic" alt="R" height="25"/></p>
