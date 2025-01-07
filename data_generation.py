import os
import pandas as pd
import numpy as np
import json
import matplotlib.pyplot as plt
from statsmodels.tsa.statespace.sarimax import SARIMAX
from sklearn.linear_model import LinearRegression
from sklearn.metrics import mean_squared_error, r2_score

# File paths
prec_files = ["Data/New_raw/prec2021.json", "Data/New_raw/prec2022.json", "Data/New_raw/prec2023.json"]
rad_files = ["Data/New_raw/sol2021.json", "Data/New_raw/sol2022.json", "Data/New_raw/sol2023.json"]
temp_files = ["Data/New_raw/temp2021.json", "Data/New_raw/temp2022.json", "Data/New_raw/temp2023.json"]
ppa_file = "Data/New_raw/DCSP_PREZZIAGR_02012025184621931.csv"

# Load and debug ppa file
ppa = pd.read_csv(ppa_file)
print("PPA Columns:", ppa.columns)  # Debug: Inspect column names

# Rename or adjust the column name for date processing
if 'Anno_Mese' not in ppa.columns:
    if 'TIME' in ppa.columns:  # Check if 'TIME' is the actual date column
        ppa.rename(columns={'TIME': 'Anno_Mese'}, inplace=True)
    else:
        raise ValueError("Column 'Anno_Mese' or equivalent not found in PPA file.")

# Ensure required columns exist
required_columns = ['PPA_ortive']
for col in required_columns:
    if col not in ppa.columns:
        print(f"Column '{col}' not found in PPA. Adding default values.")
        ppa[col] = np.random.uniform(0.2, 0.8, size=len(ppa))  # Example default values

# Process 'Anno_Mese' column
ppa['Anno_Mese'] = pd.to_datetime(ppa['Anno_Mese']).dt.to_period('M').dt.to_timestamp()

# Function to process JSON files and calculate monthly means
def process_json(files, value_key):
    data_frames = []
    for file in files:
        with open(file, 'r') as f:
            content = json.load(f)['data']
            df = pd.json_normalize(content)
            df['dataora'] = pd.to_datetime(df['dataora'])
            df['Anno_Mese'] = df['dataora'].dt.to_period('M')
            df[value_key] = pd.to_numeric(df['valore'], errors='coerce')
            monthly_mean = df.groupby('Anno_Mese')[value_key].mean().reset_index()
            monthly_mean['Anno_Mese'] = monthly_mean['Anno_Mese'].dt.to_timestamp()
            data_frames.append(monthly_mean)
    return pd.concat(data_frames, ignore_index=True)

# Process each dataset
monthly_prec = process_json(prec_files, "prec")
monthly_rad = process_json(rad_files, "rad")
monthly_temp = process_json(temp_files, "temp_mean")

# Merge datasets
merged_df = pd.merge(monthly_prec, monthly_rad, on="Anno_Mese", how="inner")
merged_df = pd.merge(merged_df, monthly_temp, on="Anno_Mese", how="inner")
merged_df = pd.merge(merged_df, ppa, on="Anno_Mese", how="inner")

# Functions for sales generation
def create_ortaggio(merged_df, seasonal_factor, trend_factors, base_sales, event_factors=None, noise_factor=0.05):
    seasonal_component = seasonal_factor * np.sin(2 * np.pi * (merged_df['Anno_Mese'].dt.month - 1) / 12)
    trend_component = sum(trend_factors[factor] * merged_df[factor] for factor in trend_factors)
    sales = base_sales + seasonal_component + trend_component
    if event_factors:
        for event_month, factor in event_factors.items():
            sales[merged_df['Anno_Mese'].dt.month == event_month] *= factor
    noise = np.random.normal(scale=noise_factor * base_sales, size=len(merged_df))
    return sales + noise

def fix_qty_ortaggio(row, peak_value, best_months, border_months, max_all, ortaggio,
                      min_unif_best=2.5, max_unif_best=3.5, min_unif_border=1.5,
                      max_unif_border=2.0, min_unif_offseason=0.5, max_unif_offseason=1.0,
                      noise_factor_best=0.05, noise_factor_border=0.1, noise_factor_offseason=0.15):
    month = pd.to_datetime(row['Anno_Mese']).month
    value = row[ortaggio]
    base_value = value * peak_value / max_all
    if month in best_months:
        seasonal_value = base_value * np.random.uniform(min_unif_best, max_unif_best)
        noise_factor_month = noise_factor_best
    elif month in border_months:
        seasonal_value = base_value * np.random.uniform(min_unif_border, max_unif_border)
        noise_factor_month = noise_factor_border
    else:
        seasonal_value = base_value * np.random.uniform(min_unif_offseason, max_unif_offseason)
        noise_factor_month = noise_factor_offseason
    seasonal_value += seasonal_value * np.random.normal(0, noise_factor_month)
    return int(seasonal_value)

# Generate sales data for each product
np.random.seed(123)

# Radicchio
trend_factors_radicchio = {'prec': 1, 'rad': 0.8, 'temp_mean': 1.2, 'PPA_ortive': 0.5}
merged_df['Radicchio'] = create_ortaggio(merged_df, seasonal_factor=80, trend_factors=trend_factors_radicchio, base_sales=28000)

# Carrots
trend_factors_carrots = {'prec': 0.8, 'rad': 1.0, 'temp_mean': 1.0, 'PPA_ortive': 0.5}
merged_df['Carrots'] = create_ortaggio(merged_df, seasonal_factor=50, trend_factors=trend_factors_carrots, base_sales=400000)

# Peaches
trend_factors_peaches = {'prec': 1, 'rad': 0.8, 'temp_mean': 1.5, 'PPA_ortive': 0.5}
merged_df['Peaches'] = create_ortaggio(merged_df, seasonal_factor=100, trend_factors=trend_factors_peaches, base_sales=50000)

# Save the generated dataset
output_file_path = "Data/generated_sales_data.csv"
merged_df.to_csv(output_file_path, index=False)

print(f"Generated data has been saved to {output_file_path}")
