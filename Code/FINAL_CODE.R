
Upload only the sections of code that you are confident work correctly and are ready for submission in the exam. Include comments where possible for better clarity and understanding.


# Analysis of Baccala Mantecato and Baccala alla Vicentina

## Package Loading and General Configuration

```{r LIBRARIES, message=FALSE, warning=FALSE}
rm(list=ls())
library(readxl)
library(readr)
library(dplyr)
```

```{r SETWD, warning=FALSE}
base_path <- "D:/Projects_GitHub/BEFD_Project/Data" # Set base path
```

## Data Loading and Preprocessing

```{r DATA LOAD, message=FALSE, warning=FALSE}
data <- read_csv(file.path(base_path, "data.csv"))
data$Date <- as.Date(data$Date, format = "%Y-%m-%d") # ensure date format
data <- data %>% mutate(Month = format(Date, "%m")) # create useful features for the models
data$trend <- 1:nrow(data)

head(data,4)
```

Check for missing values
```{r NAN CHECK}
cat("NA counts in dataset:\n")
cat(paste(names(data), colSums(is.na(data)), sep=": "), sep="\n")
```

Then we load a dataset found on [EUMOFA](https://eumofa.eu/web/guest/bulk-download).
Here we focus on the salmon consumption only beacuse we have seen that other fishes or their aggregate value lead worst results.


















































