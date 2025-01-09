
Upload only the sections of code that you are confident work correctly and are ready for submission in the exam. Include comments where possible for better clarity and understanding.


# Analysis of Baccala Mantecato and Baccala alla Vicentina

## Package Loading and General Configuration

```{r LIBRARIES}
library(readxl)
library(readr)
```

```{r SETWD}
setwd("D:/Projects_GitHub/BEFD_Project")
```

## Data Loading and Preprocessing

```{r DATA LOAD}
data <- read_csv("Data/data.csv")
data$Date <- as.Date(data$Date, format = "%Y-%m-%d") # ensure dt format
data <- data %>% mutate(Month = format(Date, "%m"),
                        Year = as.factor(format(Date, "%Y"))) # create useful features for the models
head(data)
```

Check for missing values
```{r NAN CHECK}
cat("NA counts in dataset:\n")
cat(paste(names(data), colSums(is.na(data)), sep=": "), sep="\n")
```

Then we load a dataset found on [EUMOFA](https://eumofa.eu/web/guest/bulk-download)
```{r}

```


















































