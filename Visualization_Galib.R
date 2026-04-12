# Load necessary libraries
library(car)
library(caret)
library(corrplot)
library(dplyr)
library(fastDummies)
library(ggcorrplot)
library(GGally)
library(ggExtra)
library(ggplot2)
library(ggpubr)
library(Hmisc)
library(iml)
library(lmtest)
library(lubridate)
library(MASS)
library(mice)
library(mlr3)
#library(mlr3automl)
library(mlr3benchmark)
library(mlr3learners)
library(mlr3pipelines)
library(mlr3tuning)
library(paradox)
library(pROC)
library(plotly)
library(plm)
library(psych)
library(randomForest)
library(reader)
library(readr)
library(reshape2)
library(sandwich)
library(sf)
library(stargazer)
library(strucchange)
library(tidyverse)
library(tidyr)
library(vip)
library(xgboost)
library(xtable)



#######################
#### Data Filtration ###
#######################

# Load the dataset
setwd("D:/SoSe 2024/ADA/Project Work/Rental Price ML")

data <- read.csv("Immo_data.csv", header = TRUE, sep = ",")

# Convert the 'date' column from "MonYY" to Date format
data$date <- as.Date(paste0("01", data$date), format = "%d%b%y")

# Filter the data set for September 2018 and May 2019
clean.data1 <- data %>%
  filter(format(date, "%Y-%m") == "2018-09" | format(date, "%Y-%m") == "2019-05")


# Remove rows with NA values
clean.data1 <- clean.data1 %>%
  drop_na()

# Check dimensions after removing NA
cat("Data dimensions after removing NA: ", dim(clean.data1), "\n")


# Display the first few rows and summary
head(clean.data1)
summary(clean.data1)
str(clean.data1)

# Save clean.data1 as "filtered_rental_data.csv"
write.csv(clean.data1, file = "filtered_rental_data.csv", row.names = FALSE)

############################
## Descriptive statistics ##
############################

# Read the CSV file
data1 <- read.csv("filtered_rental_data.csv", header = TRUE, sep = ",")
# Check the structure and first few rows
str(data1)
head(data1)

# Check for any NA values in the important columns
colSums(is.na(data1))

# Summary statistics for numeric columns
numeric_cols <- sapply(data1, is.numeric)
numeric_data <- data1[, numeric_cols]
numeric_summary <- summary(numeric_data)
print(numeric_summary)

# Summary of non-numeric columns
non_numeric_cols <- sapply(data1, function(x) !is.numeric(x))
non_numeric_data <- data1[, non_numeric_cols]
summary(non_numeric_data)

# Table of descriptive statistics for numeric and categorical features
stargazer(data1, type = "text", title = "Descriptive Statistics", digits = 1, out = "table1.html", median = TRUE)

#######################
#### Data Cleaning ####
#######################

# Create a copy of the data for cleaning
rental_data_copy <- data1

# Function to calculate missing values
missing_values <- function(df, norows) {
  total <- sort(colSums(is.na(df)), decreasing = TRUE)
  percent <- sort((colSums(is.na(df)) / nrow(df)) * 100, decreasing = TRUE)
  missing_data <- data.frame(Total = total, Percent = percent)
  return(head(missing_data, norows))
}

# Calculate and handle missing values
missing_data <- missing_values(rental_data_copy, ncol(rental_data_copy))
columns_to_drop <- rownames(missing_data[missing_data$Percent > 30, ])
rental_data_copy <- rental_data_copy[, !(names(rental_data_copy) %in% columns_to_drop)]
rental_data_copy <- rental_data_copy[!is.na(rental_data_copy$totalRent), ]

# Further clean the data
columns_to_drop <- c('serviceCharge', 'baseRentRange', 'pricetrend', 'livingSpaceRange', 'street', 'description', 'facilities', 
                     'geo_krs', 'geo_plz', 'scoutId', 'regio1', 'telekomUploadSpeed', 'baseRent', 'regio3', 'geo_bln', 'date', 'houseNumber', 'streetPlain')
rental_data_copy <- rental_data_copy[, !(names(rental_data_copy) %in% columns_to_drop)]

# Define the get_mode function
get_mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# Process missing values in the remaining columns
rental_data_copy$condition[is.na(rental_data_copy$condition)] <- "Other"
rental_data_copy$typeOfFlat[is.na(rental_data_copy$typeOfFlat)] <- get_mode(rental_data_copy$typeOfFlat)
rental_data_copy$heatingType[is.na(rental_data_copy$heatingType)] <- get_mode(rental_data_copy$heatingType)
rental_data_copy$yearConstructed[is.na(rental_data_copy$yearConstructed)] <- median(rental_data_copy$yearConstructed, na.rm = TRUE)
rental_data_copy$yearConstructedRange[is.na(rental_data_copy$yearConstructedRange)] <- median(rental_data_copy$yearConstructedRange, na.rm = TRUE)

# Handle outliers
rental_data_copy <- rental_data_copy[rental_data_copy$baseRent > 100 & rental_data_copy$baseRent < 4000, ]
rental_data_copy <- rental_data_copy[rental_data_copy$totalRent > 200 & rental_data_copy$totalRent < 5000, ]
rental_data_copy <- rental_data_copy[rental_data_copy$totalRent > rental_data_copy$baseRent, ]
rental_data_copy <- rental_data_copy[rental_data_copy$livingSpace > 10 & rental_data_copy$livingSpace < 400,  ]

# Remove any remaining rows with NA values in the columns of interest
rental_data_copy <- rental_data_copy[complete.cases(rental_data_copy$balcony, rental_data_copy$totalRent), ]

########################
## Data Visualization ##
########################

# Convert 'balcony' to a numeric binary variable (0/1)
# TRUE should map to 1 and FALSE should map to 0
rental_data_copy$balcony_numeric <- ifelse(rental_data_copy$balcony, 1, 0)

# Check the distribution of balcony_numeric to ensure it has variability
table(rental_data_copy$balcony_numeric)

# Check if 'balcony_numeric' and 'totalRent' are indeed numeric and have enough data
str(rental_data_copy$balcony_numeric)
str(rental_data_copy$totalRent)

# Ensure there are no NA values and enough finite observations
if (all(is.finite(rental_data_copy$balcony_numeric)) & all(is.finite(rental_data_copy$totalRent))) {
  # Perform the Point-Biserial Correlation test
  cor_bis <- cor.test(rental_data_copy$balcony_numeric, rental_data_copy$totalRent, method = "pearson")
  # Print the results
  print(cor_bis)
} else {
  print("Data contains non-finite values or lacks variability, unable to perform correlation test.")
}
