install.packages("grid")
library(vcd)
library(grid) 

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

print(dim(data1))
print(summary(data1))
#######################
#### Data Cleaning ####
#######################

# Create a copy of the data for cleaning
rental_data_copy <- data1
print(dim(rental_data_copy))  # Check initial dimensions

# Drop columns with more than 30% missing data
missing_data <- colSums(is.na(rental_data_copy))
columns_to_drop <- names(missing_data[missing_data / nrow(rental_data_copy) > 0.3])
rental_data_copy <- rental_data_copy[, !(names(rental_data_copy) %in% columns_to_drop)]
print(dim(rental_data_copy))  # Check dimensions after dropping columns

# Remove rows with NA in key columns
rental_data_copy <- rental_data_copy[!is.na(rental_data_copy$totalRent) & !is.na(rental_data_copy$balcony), ]
print(dim(rental_data_copy))  # Check dimensions after removing rows with NA

# Further clean the data by removing unwanted columns
columns_to_drop <- c('serviceCharge', 'baseRentRange', 'pricetrend', 'livingSpaceRange', 
                     'street', 'description', 'facilities', 'geo_krs', 'geo_plz', 'scoutId', 
                     'regio1', 'telekomUploadSpeed', 'baseRent', 'regio3', 'geo_bln', 'date', 
                     'houseNumber', 'streetPlain')
rental_data_copy <- rental_data_copy[, !(names(rental_data_copy) %in% columns_to_drop)]
print(dim(rental_data_copy))  # Check dimensions after further cleaning

# Define the get_mode function
get_mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# Process missing values in the remaining columns using mode or median
rental_data_copy$condition[is.na(rental_data_copy$condition)] <- "Other"
rental_data_copy$typeOfFlat[is.na(rental_data_copy$typeOfFlat)] <- get_mode(rental_data_copy$typeOfFlat)
rental_data_copy$heatingType[is.na(rental_data_copy$heatingType)] <- get_mode(rental_data_copy$heatingType)
rental_data_copy$yearConstructed[is.na(rental_data_copy$yearConstructed)] <- median(rental_data_copy$yearConstructed, na.rm = TRUE)
rental_data_copy$yearConstructedRange[is.na(rental_data_copy$yearConstructedRange)] <- median(rental_data_copy$yearConstructedRange, na.rm = TRUE)

# Handle outliers in numeric columns
rental_data_copy <- rental_data_copy[rental_data_copy$totalRent > 200 & rental_data_copy$totalRent < 5000, ]
rental_data_copy <- rental_data_copy[rental_data_copy$livingSpace > 10 & rental_data_copy$livingSpace < 400,  ]
print(dim(rental_data_copy))  # Check dimensions after handling outliers

# Remove any remaining rows with NA values
rental_data_copy <- rental_data_copy[complete.cases(rental_data_copy), ]
print(dim(rental_data_copy))  # Final check

# Final check on the data structure and summary
str(rental_data_copy)
summary(rental_data_copy)


# Convert 'balcony' to a numeric binary variable (0/1)
rental_data_copy$balcony_numeric <- ifelse(rental_data_copy$balcony, 1, 0)

# Check if 'balcony_numeric' and 'totalRent' are indeed numeric and have enough data
str(rental_data_copy$balcony_numeric)
str(rental_data_copy$totalRent)

# Remove rows with NA or infinite values in 'balcony_numeric' or 'totalRent'
rental_data_copy <- rental_data_copy[is.finite(rental_data_copy$balcony_numeric) & is.finite(rental_data_copy$totalRent), ]
print(dim(rental_data_copy))  # Check dimensions after removing invalid data

# Perform the Point-Biserial Correlation test
cor_bis <- cor.test(rental_data_copy$balcony_numeric, rental_data_copy$totalRent, method = "pearson")
print(cor_bis)

# Handle Binning Issue with 'totalRent'
# Remove rows with NA or infinite values in 'totalRent'
rental_data_copy <- rental_data_copy[!is.na(rental_data_copy$totalRent) & is.finite(rental_data_copy$totalRent), ]

# Bin the 'totalRent' column into 5 categories
rental_data_copy$totalRent_binned <- cut(rental_data_copy$totalRent, breaks = 5)

# Check if 'totalRent_binned' was created correctly
str(rental_data_copy$totalRent_binned)

# Perform the Chi-Square Test of Independence
chisq_test <- chisq.test(table(rental_data_copy$typeOfFlat, rental_data_copy$totalRent_binned))
print(chisq_test)


########################
## Data Visualization ##
########################

# 1. Boxplots
boxplot_heatingType <- ggplot(rental_data_copy, aes(x = heatingType, y = totalRent)) +
  geom_boxplot(fill = "skyblue") +
  theme_minimal() +
  labs(title = "Boxplot of Total Rent by Heating Type",
       x = "Heating Type",
       y = "Total Rent (€)")

boxplot_typeOfFlat <- ggplot(rental_data_copy, aes(x = typeOfFlat, y = totalRent)) +
  geom_boxplot(fill = "lightgreen") +
  theme_minimal() +
  labs(title = "Boxplot of Total Rent by Type of Flat",
       x = "Type of Flat",
       y = "Total Rent (€)")

# 2. Violin Plots
violin_heatingType <- ggplot(rental_data_copy, aes(x = heatingType, y = totalRent)) +
  geom_violin(fill = "lightpink", alpha = 0.7) +
  theme_minimal() +
  labs(title = "Violin Plot of Total Rent by Heating Type",
       x = "Heating Type",
       y = "Total Rent (€)")

violin_typeOfFlat <- ggplot(rental_data_copy, aes(x = typeOfFlat, y = totalRent)) +
  geom_violin(fill = "lightcoral", alpha = 0.7) +
  theme_minimal() +
  labs(title = "Violin Plot of Total Rent by Type of Flat",
       x = "Type of Flat",
       y = "Total Rent (€)")

# 3. Bar Plots with Error Bars
barplot_regio2 <- ggplot(rental_data_copy, aes(x = regio2, y = totalRent)) +
  stat_summary(fun = mean, geom = "bar", fill = "lightblue", alpha = 0.7) +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2, color = "blue") +
  theme_minimal() +
  labs(title = "Bar Plot of Average Total Rent by Region with Error Bars",
       x = "Region (Regio2)",
       y = "Average Total Rent (€)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 4. Convert 'balcony' to a numeric binary variable (0/1)
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



# 5. Chi-Square Test of Independence (Example with 'typeOfFlat' and binned 'totalRent')
# Step 1: Remove rows with NA or infinite values in 'totalRent'
rental_data_copy <- rental_data_copy[!is.na(rental_data_copy$totalRent) & is.finite(rental_data_copy$totalRent), ]

# Step 2: Check the structure of 'totalRent' to ensure there are no more missing or infinite values
str(rental_data_copy$totalRent)

# Step 3: Bin the 'totalRent' column into 5 categories
rental_data_copy$totalRent_binned <- cut(rental_data_copy$totalRent, breaks = 5)

# Step 4: Perform the Chi-Square Test of Independence between 'typeOfFlat' and binned 'totalRent'
chisq_test <- chisq.test(table(rental_data_copy$typeOfFlat, rental_data_copy$totalRent_binned))
cat("Chi-Square Test of Independence between Type of Flat and Binned Total Rent: \n")
print(chisq_test)


# 6. Enhanced Mosaic Plot with Better Visualization
# Create the Mosaic Plot with enhanced visualization
png(filename = "enhanced_mosaic_plot.png", width = 1200, height = 800)

# Enhanced Mosaic Plot with Improved Text and Labeling Visibility
mosaic(~ typeOfFlat + totalRent_binned, data = rental_data_copy, shade = TRUE, legend = TRUE, 
       main = "Mosaic Plot of Type of Flat by Binned Total Rent (€)", 
       gp = shading_hcl,  # Use HCL colors for better visual distinction
       labeling_args = list(
         set_varnames = c(typeOfFlat = "Flat Type", totalRent_binned = "Total Rent (Binned) (€)"),
         gp_labels = gpar(col = "white", fontsize = 10, fontface = "bold"),  # Text color set to white with bold font
         gp_varnames = gpar(fontsize = 12, fontface = "bold", col = "black"),  # Variable names outside the plot
         gp_varlevels = gpar(fontsize = 10, col = "black")  # Levels of variables outside the plot
       ),
       labeling = labeling_border( # Additional labeling options to enhance visibility
         gp_labels = gpar(col = "white", fontsize = 10, fontface = "bold"),
         gp_varnames = gpar(fontsize = 12, fontface = "bold", col = "black"),
         gp_varlevels = gpar(fontsize = 10, col = "black")
       ))

# Close the PNG device
dev.off()

# Display plots
print(boxplot_heatingType)
print(boxplot_typeOfFlat)
print(violin_heatingType)
print(violin_typeOfFlat)
print(barplot_regio2)
