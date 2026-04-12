rm(list=ls(all=T))
graphics.off()

library(reshape2)
library(readr)
library(ggplot2)
library(stargazer)
library(xtable)
library(plm)
library(psych)
library(sf)
library(lmtest)
library(car)
library(sandwich)
library(ggcorrplot)
library(MASS)
library(strucchange)
library(dplyr)
library(tidyverse)
library(tidyr)

############################
## Descriptive statistics ##
############################

# Read the CSV file
data1 <- read.csv("filtered_rental_data.csv", header = TRUE, sep = ",")

# Check the head and tail
head(data1)
tail(data1)

# Check the dimmension
dim(data1)

# View class of data
class(data1)

# View first 6 rows of data
head(data1)

# View structure of data
str(data1)

# Select numeric columns
numeric_cols <- sapply(data1, is.numeric)
numeric_data <- data1[, numeric_cols]

# Number of numeric columns
length(colnames(numeric_data))

# Summary statistics for numeric columns
numeric_summary <- summary(numeric_data)
numeric_summary

# Select non-numeric columns
non_numeric_cols <- sapply(data1, function(x) !is.numeric(x))
non_numeric_data <- data1[, non_numeric_cols]

# Number of non-numeric columns
length(colnames(non_numeric_data))

# Summary of non-numeric columns
summary(non_numeric_data)

#table of descriptive statistics(for numeric and catagorical features)
stargazer(data1,type="text", title= "Descriptive Statistics", digits=1, out="table1.html", median = T)

#######################
#### Data Cleaning ####
#######################

### delete unrelated, unreasonable and data insufficient features with too many missing values ###

# Create a copy of the data
rental_data_copy <- data1

# Function to calculate missing values
missing_values <- function(df, norows) {
  total <- sort(colSums(is.na(df)), decreasing = TRUE)
  percent <- sort((colSums(is.na(df)) / nrow(df)) * 100, decreasing = TRUE)
  missing_data <- data.frame(Total = total, Percent = percent)
  return(head(missing_data, norows))
}

# Calculate missing values for all columns
missing_values(rental_data_copy, ncol(rental_data_copy))

# Calculate missing values and drop columns with > 30% missing data
missing_data <- missing_values(rental_data_copy, ncol(rental_data_copy))
columns_to_drop <- rownames(missing_data[missing_data$Percent > 30, ])
rental_data_copy <- rental_data_copy[, !(names(rental_data_copy) %in% columns_to_drop)]

# Recalculate missing values after dropping columns
missing_values(rental_data_copy, ncol(rental_data_copy))

# Remove rows with NA in totalRent column (Because our target is total rent)
rental_data_copy <- rental_data_copy[!is.na(rental_data_copy$totalRent), ]

# Display the first few rows
head(rental_data_copy)

# Drop specified columns
columns_to_drop <- c('livingSpaceRange', 'street', 'description', 'facilities', 
                     'geo_krs', 'geo_plz', 'scoutId', 'regio1', 'telekomUploadSpeed',
                      'regio3', 'geo_bln', 'date', 'houseNumber', 'streetPlain')

rental_data_copy <- rental_data_copy[, !(names(rental_data_copy) %in% columns_to_drop)]

### process missing value ###

# Display missing values
print(missing_values(rental_data_copy, 11))

# Display value counts for 'condition' column
print(table(rental_data_copy$condition))

# Replace NAs in 'condition' column with "Other"
rental_data_copy$condition[is.na(rental_data_copy$condition)] <- "Other"

# Replace "negotiable", "need_of_renovation", "ripe_for_demolition" with "Other" 
last_three <- c("negotiable", "need_of_renovation", "ripe_for_demolition" )
rental_data_copy$condition <- ifelse(rental_data_copy$condition %in% last_three, "Other", rental_data_copy$condition)

# Display updated value counts for 'condition' column
print(table(rental_data_copy$condition))

# Display missing values
print(missing_values(rental_data_copy, 6))

# Group by 'condition' and calculate mean and std of 'yearConstructed'
library(dplyr)
yearConstructed_summary <- rental_data_copy %>%
  group_by(condition) %>%
  summarise(mean = round(mean(yearConstructed, na.rm = TRUE), 0),
            std = round(sd(yearConstructed, na.rm = TRUE), 0))
print(yearConstructed_summary)

# replace NA in yearConstructed column with median 
rental_data_copy$yearConstructed[is.na(rental_data_copy$yearConstructed)]<- median(rental_data_copy$yearConstructed, na.rm = TRUE)
# replace NA in yearConstructedRange column with median 
rental_data_copy$yearConstructedRange[is.na(rental_data_copy$yearConstructedRange)]<- median(rental_data_copy$yearConstructedRange, na.rm = TRUE)

# Display missing values
print(missing_values(rental_data_copy, 6))

# Display value counts for 'typeOfFlat' column
print(table(rental_data_copy$typeOfFlat))

# Describe 'heatingType' column
print(table(rental_data_copy$heatingType))

# Fill NA values in 'typeOfFlat' and 'heatingType' with mode
get_mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

rental_data_copy$typeOfFlat[is.na(rental_data_copy$typeOfFlat)] <- get_mode(rental_data_copy$typeOfFlat)
rental_data_copy$heatingType[is.na(rental_data_copy$heatingType)] <- get_mode(rental_data_copy$heatingType)

# Display missing values
print(missing_values(rental_data_copy, ncol(rental_data_copy)))

# For floor and serviceCharge we just simply remove NA
print(table(rental_data_copy$floor))
rental_data_copy$floor[is.na(rental_data_copy$floor)]<- median(rental_data_copy$floor, na.rm = TRUE)
rental_data_copy <- rental_data_copy[!is.na(rental_data_copy$serviceCharge), ]
rental_data_copy <- rental_data_copy[!is.na(rental_data_copy$firingTypes), ]
rental_data_copy <- rental_data_copy[!is.na(rental_data_copy$telekomTvOffer), ]
rental_data_copy <- rental_data_copy[!is.na(rental_data_copy$pricetrend), ]
missing_values(rental_data_copy, ncol(rental_data_copy))

### Outliers ###

# We've already addressed outliers in the condition column by adding the least common conditions to Other. 
# Now, let's focus on the remaining columns, beginning with regio2.

# outliers with regio2


top_30_regio2 <- head(sort(table(rental_data_copy$regio2), decreasing = TRUE), 30)
print(top_30_regio2)
top_40_regio2 <- head(sort(table(rental_data_copy$regio2), decreasing = TRUE), 40)
print(top_40_regio2)
top_60_regio2 <- head(sort(table(rental_data_copy$regio2), decreasing = TRUE), 60)
print(top_60_regio2)

total_regio2 <- length(unique(rental_data_copy$regio2))
print(total_regio2)

top_30_regio2_count <- sum(head(sort(table(rental_data_copy$regio2), decreasing = TRUE), 30))
total_regio2_count <- sum(table(rental_data_copy$regio2))
ratio_top_30 <- top_30_regio2_count / total_regio2_count
print(ratio_top_30)

top_40_regio2_count <- sum(head(sort(table(rental_data_copy$regio2), decreasing = TRUE), 40))
total_regio2_count <- sum(table(rental_data_copy$regio2))
ratio_top_40 <- top_40_regio2_count / total_regio2_count
print(ratio_top_40)

top_50_regio2_count <- sum(head(sort(table(rental_data_copy$regio2), decreasing = TRUE), 50))
total_regio2_count <- sum(table(rental_data_copy$regio2))
ratio_top_50 <- top_50_regio2_count / total_regio2_count
print(ratio_top_50)

top_60_regio2_count <- sum(head(sort(table(rental_data_copy$regio2), decreasing = TRUE), 60))
total_regio2_count <- sum(table(rental_data_copy$regio2))
ratio_top_60 <- top_60_regio2_count / total_regio2_count
print(ratio_top_60)

other_region <- names(sort(table(rental_data_copy$regio2), decreasing = TRUE)[61:length(table(rental_data_copy$regio2))])

edit_region <- function(dflist) {
  if (dflist %in% other_region) {
    return("Other")
  } else {
    return(dflist)
  }
}

rental_data_copy$regio2 <- sapply(rental_data_copy$regio2, edit_region)

sort(table(rental_data_copy$regio2),decreasing = TRUE ) %>% print()

# Outlier with total rent and base rent

# Filter data based on baseRent and totalRent

#check the distribution of baseRent and totalRent
ggplot(data = rental_data_copy, aes(x = baseRent)) + 
  geom_histogram(bins = 100, fill = 'skyblue', color = 'black', alpha = 0.7) +
  scale_x_continuous(limits = c(0, 5000), breaks = seq(0, 5000, by = 500)) +
  scale_y_continuous(limits = c(0, 12000), breaks = seq(0, 12000, by = 500)) +
  labs(title = "Distribution of Base Rent",
       x = "Base Rent (€)",
       y = "Frequency") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank())

ggplot(data = rental_data_copy, aes(x = totalRent)) + 
  geom_histogram(bins = 100, fill = 'skyblue', color = 'black', alpha = 0.7) +
  scale_x_continuous(limits = c(0, 5000), breaks = seq(0, 5000, by = 500)) +
  scale_y_continuous(limits = c(0, 10000), breaks = seq(0, 10000, by = 500)) +
  labs(title = "Distribution of Total Rent",
       x = "Total Rent (€)",
       y = "Frequency") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank())

# eliminate illogical data

rental_data_copy <- rental_data_copy[rental_data_copy$baseRent > 100 & rental_data_copy$baseRent < 4000, ]
rental_data_copy <- rental_data_copy[rental_data_copy$totalRent > 200 & rental_data_copy$totalRent < 5000, ]
rental_data_copy <- rental_data_copy[rental_data_copy$totalRent > rental_data_copy$baseRent, ]

# Create scatter plot of totalRent vs baseRent
library(plotly)
fig <- plot_ly(rental_data_copy, x = ~totalRent, y = ~baseRent, type = 'scatter', mode = 'markers')
fig

# Filter data based on livingSpace
ggplot(data = rental_data_copy, aes(x = livingSpace)) + 
  geom_histogram(bins = 100, fill = 'skyblue', color = 'black', alpha = 0.7) +
  scale_x_continuous(limits = c(0, 500), breaks = seq(0, 500, by = 50)) +
  scale_y_continuous(limits = c(0, 12000), breaks = seq(0, 12000, by = 1000)) +
  labs(title = "Distribution of livingSpace",
       x = "livingSpace (m^2)",
       y = "Frequency") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank())

rental_data_copy <- rental_data_copy[rental_data_copy$livingSpace > 10 & rental_data_copy$livingSpace < 400,  ]

# Create scatter plot of baseRent vs livingSpace
fig <- plot_ly(rental_data_copy, x = ~baseRent, y = ~livingSpace, type = 'scatter', mode = 'markers')
fig

# Calculate new columns
rental_data_copy$Pricepm2 <- rental_data_copy$baseRent / rental_data_copy$livingSpace
rental_data_copy$additioncost <- rental_data_copy$totalRent - rental_data_copy$baseRent
rental_data_copy$livingspace_per_room <- rental_data_copy$livingSpace / rental_data_copy$noRooms


# Drop baseRent column
rental_data_copy$baseRent <- NULL

# Drop column of living space and number of room
rental_data_copy$livingSpace <- NULL
rental_data_copy$noRooms<- NULL

# Remove outliers for numeric columns
for (col in names(rental_data_copy)) {
  if (is.numeric(rental_data_copy[[col]])) {
    upper_range <- mean(rental_data_copy[[col]], na.rm = TRUE) + 3 * sd(rental_data_copy[[col]], na.rm = TRUE)
    lower_range <- mean(rental_data_copy[[col]], na.rm = TRUE) - 3 * sd(rental_data_copy[[col]], na.rm = TRUE)
    
    rental_data_copy <- rental_data_copy[rental_data_copy[[col]] <= upper_range & rental_data_copy[[col]] >= lower_range, ]
  }
}


########################
## Data Visualization ##
########################

numeric_data
# Select numeric columns
num_columns <- sapply(rental_data_copy, is.numeric)
numeric_data <- rental_data_copy[, num_columns]
numeric_data

num_columns

# Calculate correlation matrix
cor_matrix <- cor(numeric_data, use = "complete.obs")
cor_matrix

# Sort correlation matrix by totalRent column
cor_matrix_sorted <- cor_matrix[order(cor_matrix[,"totalRent"], decreasing = TRUE), ]

# Melt the correlation matrix for ggplot
melted_cor_matrix <- reshape2::melt(cor_matrix_sorted)

# Create heatmap
ggplot(data = melted_cor_matrix, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  geom_text(aes(label = sprintf("%.2f", value)), vjust = 1) +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "", y = "", fill = "Correlation") +
  coord_fixed()


# Select numeric and binary columns
num_bin_columns <- sapply(rental_data_copy, function(x) is.numeric(x) | is.logical(x) | all(x %in% c(0, 1, NA)))
selected_data <- rental_data_copy[, num_bin_columns]

# Calculate correlation matrix
cor_matrix <- cor(selected_data, use = "pairwise.complete.obs")

# Sort correlation matrix by totalRent column
cor_matrix_sorted <- cor_matrix[order(cor_matrix[,"totalRent"], decreasing = TRUE), ]

# Melt the correlation matrix for ggplot
melted_cor_matrix <- reshape2::melt(cor_matrix_sorted)

# Create heatmap
ggplot(data = melted_cor_matrix, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  geom_text(aes(label = sprintf("%.2f", value)), vjust = 1, size = 2.5) +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limits = c(0, 1)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        axis.text.y = element_text(hjust = 1)) +
  labs(x = "", y = "", fill = "Correlation") +
  coord_fixed()

# Save the plot if needed
ggsave("correlation_heatmap.png", width = 16, height = 16)

# save the document

write.csv(rental_data_copy, "clean_data3.csv", row.names = FALSE)



# Adjusted function to create scatter and density plots with available columns
create_plots <- function(data, dataset_name) {
  
  # Calculate missing columns if needed
  if ("Pricepm2" %in% colnames(data) == FALSE && "baseRent" %in% colnames(data) && "livingSpace" %in% colnames(data)) {
    data$Pricepm2 <- data$baseRent / data$livingSpace
  }
  
  if ("additioncost" %in% colnames(data) == FALSE && "totalRent" %in% colnames(data) && "baseRent" %in% colnames(data)) {
    data$additioncost <- data$totalRent - data$baseRent
  }
  
  if ("livingspace_per_room" %in% colnames(data) == FALSE && "livingSpace" %in% colnames(data) && "noRooms" %in% colnames(data)) {
    data$livingspace_per_room <- data$livingSpace / data$noRooms
  }
  
  # Check if the required columns now exist
  required_columns <- c("totalRent", "Pricepm2", "additioncost", "livingspace_per_room")
  
  if (!all(required_columns %in% colnames(data))) {
    stop(paste("Dataset", dataset_name, "does not contain the required columns, and they could not be calculated."))
  }
  
  # Scatter Plot: totalRent vs Pricepm2
  scatter_plot <- ggplot(data, aes(x = Pricepm2, y = totalRent)) +
    geom_point(alpha = 0.5, color = 'darkblue') +
    geom_smooth(method = 'lm', col = 'red', se = FALSE) +
    theme_minimal() +
    labs(title = paste("Total Rent vs Price per m²:", dataset_name),
         x = "Price per m² (€)",
         y = "Total Rent (€)")
  
  # Density Plot: totalRent
  density_plot_rent <- ggplot(data, aes(x = totalRent)) +
    geom_density(fill = "blue", alpha = 0.5) +
    theme_minimal() +
    labs(title = paste("Density of Total Rent:", dataset_name),
         x = "Total Rent (€)",
         y = "Density")
  
  # Density Plot: Pricepm2
  density_plot_pricepm2 <- ggplot(data, aes(x = Pricepm2)) +
    geom_density(fill = "red", alpha = 0.5) +
    theme_minimal() +
    labs(title = paste("Density of Price per m²:", dataset_name),
         x = "Price per m² (€)",
         y = "Density")
  
  # Print the plots
  print(scatter_plot)
  print(density_plot_rent)
  print(density_plot_pricepm2)
  
  # Save the plots as images
  ggsave(paste0("scatter_plot_", dataset_name, ".png"), plot = scatter_plot)
  ggsave(paste0("density_plot_rent_", dataset_name, ".png"), plot = density_plot_rent)
  ggsave(paste0("density_plot_pricepm2_", dataset_name, ".png"), plot = density_plot_pricepm2)
}

# Example of usage with filtered_rental_data
create_plots(data1, "filtered_rental_data")

