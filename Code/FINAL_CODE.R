
Upload only the sections of code that you are confident work correctly and are ready for submission in the exam. Include comments where possible for better clarity and understanding.


# Analysis of Baccala Mantecato and Baccala alla Vicentina

## Package Loading and General Configuration

```{r LIBRARIES}
rm(list=ls())
library(readxl)
library(readr)
```

```{r SETWD, warning=FALSE}
# Set base path
base_path <- "D:/Projects_GitHub/BEFD_Project/Data"
```

## Data Loading and Preprocessing

```{r DATA LOAD, message=FALSE, warning=FALSE}
data <- read_csv(file.path(base_path, "data.csv"))
data$Date <- as.Date(data$Date, format = "%Y-%m-%d") # ensure date format
data <- data %>% mutate(Month = format(Date, "%m"),
                        Year = as.factor(format(Date, "%Y"))) # create useful features for the models
data$trend <- 1:nrow(data)
head(data)
```

Check for missing values
```{r NAN CHECK}
cat("NA counts in dataset:\n")
cat(paste(names(data), colSums(is.na(data)), sep=": "), sep="\n")
```

Then we load a dataset found on [EUMOFA](https://eumofa.eu/web/guest/bulk-download).
Here we focus onthe salmon consumption only beacusa we have seen that other fishes or thei aggregate value lead worst results.


















































