#Environment empty
rm(list = ls())
## ==========================================================================================
##          Economic effect of climatic factors on the Health Sector in Brazil -
##                                  Scientific Essay
## ==========================================================================================
## Goal: aimed to estimate cases of cardiovascular disease in a Brazilian region 
## based on climatic factors with the construction of a linear regression model, 
## survey treatment costs, forecast national population growth and future climate 
## change scenarios.
## About this file
## "weather_station_INMET_Midwest.R"
## This file is a Historical data meticulously mined from 30 meteorological stations (active)
## (2010-2019) of Midwest Region of Brazil and lays the groundwork for robust analysis.
## ==========================================================================================
# Load the packages
library(dplyr)
library(ggplot2)
library(tidyr)
library(readr)

# List of file paths for the 30 datasets
file_paths_1 <- c(
  "dados_82976.csv", "dados_83007.csv", 
  "dados_83214.csv", "dados_83264.csv",
  "dados_83267.csv", "dados_83270.csv",
  "dados_83309.csv", "dados_83319.csv",
  "dados_83332.csv", "dados_83358.csv",
  "dados_83361.csv", "dados_83364.csv",
  "dados_83368.csv", "dados_83374.csv",
  "dados_83376.csv", "dados_83377.csv",
  "dados_83379.csv", "dados_83405.csv",
  "dados_83410.csv", "dados_83423.csv",
  "dados_83464.csv", "dados_83470.csv",
  "dados_83513.csv", "dados_83522.csv",
  "dados_83523.csv", "dados_83526.csv",
  "dados_83552.csv", "dados_83565.csv",
  "dados_83702.csv", "dados_83704.csv"
)

# Function to convert categorical columns to numeric
convert_to_numeric <- function(data) {
  numeric_cols <- c("INSOLACAO_TOTAL", "PRECIPITACAO_TOTAL", 
                    "TEMPERATURA_MAXIMA", "TEMPERATURA_MEDIA")
  
  colnames(data) <- c("Data", "INSOLACAO_TOTAL", "PRECIPITACAO_TOTAL", 
                      "TEMPERATURA_MAXIMA", "TEMPERATURA_MEDIA")
  
  for (col in numeric_cols) {
    data[[col]] <- as.numeric(data[[col]])
  }
  return(data)
}

# Read and combine datasets, filtering out datasets with less than 120 rows
#deleted_datasets <- 0

#Combined_data
combined_data <- lapply(file_paths_1, function(file) {
  read_data <- read_delim(file, delim = ";", skip = 11, col_names = FALSE)
  colnames(read_data) <- c("Data", "INSOLACAO_TOTAL", "PRECIPITACAO_TOTAL", 
                           "TEMPERATURA_MAXIMA", "TEMPERATURA_MEDIA")
  read_data$Data <- as.factor(read_data$Data)
  numeric_data <- convert_to_numeric(read_data)
})

# Remove the sixth column from each tibble in combined_data
combined_data <- lapply(combined_data, function(data) {
  data[, -6, drop = FALSE]  # Remove the sixth column
})

# Show the modified structure with the first 5 columns only
head(combined_data, 5)

# Assuming combined_data has been created
sample_datasets <- combined_data

# Assuming combined_data is a list with 74 elements
for (i in 1:93) {
  assign(paste0("dataset_", sprintf("%02d", i)), combined_data[[i]])
}

sample_datasets <- list()
for (i in 1:93) {
  sample_datasets[[i]] <- get(paste0("dataset_", sprintf("%02d", i)))
}

# Create vectors for each cell in the same location across datasets
cell_values <- lapply(2:5, function(col) {
  sapply(sample_datasets, function(dataset) dataset[, col])
})
# Adjust the output format for the vector
cell_values_formatted <- unlist(cell_values)

# Define the number of rows
num_rows <- nrow(sample_datasets[[1]])

# Store the vectors in a list
vectors <- list()
for (col in 2:5) {
  for (row in 1:num_rows) {
    vector_name <- paste(colnames(sample_datasets[[1]])[col], row, sep = "_")
    values <- sapply(sample_datasets, function(dataset) dataset[row, col])
    vectors[[vector_name]] <- as.numeric(values) # Ensure numeric conversion
  }
}

# Impute NA values in temperature-related vectors using mean
for (i in 1:length(vectors)) {
  vectors[[i]][is.na(vectors[[i]])] <- mean(vectors[[i]], na.rm = TRUE)
}

# Extract the "Data" column from the first dataset
Data <- sample_datasets[[1]]$Data

# Create vectors for each type of average
INSOLACAO_TOTAL <- sapply(1:120, function(i) mean(vectors[[paste0("INSOLACAO_TOTAL_", i)]]))
PRECIPITACAO_TOTAL <- sapply(1:120, function(i) mean(vectors[[paste0("PRECIPITACAO_TOTAL_", i)]]))
TEMPERATURA_MAXIMA <- sapply(1:120, function(i) mean(vectors[[paste0("TEMPERATURA_MAXIMA_", i)]]))
TEMPERATURA_MEDIA <- sapply(1:120, function(i) mean(vectors[[paste0("TEMPERATURA_MEDIA_", i)]]))

# Create the new data frame
new_df_centro_oeste <- data.frame(Data, INSOLACAO_TOTAL, PRECIPITACAO_TOTAL, TEMPERATURA_MAXIMA, TEMPERATURA_MEDIA)

# Show the structure of the new data frame
str(new_df_centro_oeste)
head(new_df_centro_oeste)

# Save as CSV with semicolon delimiter
write.table(new_df_centro_oeste, file = "proj_ipea_CO.csv", sep = ";", row.names = FALSE)

# Correctly specify column names for temperature values
temp_max_col_name <- "TEMPERATURA_MAXIMA"  # Replace with the actual column name
temp_med_col_name <- "TEMPERATURA_MEDIA"   # Replace with the actual column name

# Extract temperature values directly using column names
temp_max_values <- lapply(sample_datasets, function(dataset) dataset[, temp_max_col_name])
temp_med_values <- lapply(sample_datasets, function(dataset) dataset[, temp_med_col_name])

# Inspect intermediate results (temperature values)
head(temp_max_values)  # View the first few entries
summary(temp_max_values)  # Get summary statistics
boxplot(temp_max_values)  # Create a boxplot to visualize distribution

# Review imputation method
if (any(is.na(temp_max_values[[1]])) | any(is.na(temp_med_values[[1]]))) {  # Check for NAs
  # Calculate means for imputation (adjust as needed)
  temp_max_mean <- mean(unlist(temp_max_values), na.rm = TRUE)
  temp_med_mean <- mean(unlist(temp_med_values), na.rm = TRUE)
  
  # Inspect calculated means
  print(paste("Mean for TEMPERATURA_MAXIMA:", temp_max_mean))
  print(paste("Mean for TEMPERATURA_MEDIA:", temp_med_mean))
  
  # Consider alternative imputation methods if needed
  # For example, using medians:
  # temp_max_median <- median(unlist(temp_max_values), na.rm = TRUE)
  # temp_med_median <- median(unlist(temp_med_values), na.rm = TRUE)
  
  # Choose the appropriate imputation method and apply it
  imputed_temp_max_values <- lapply(temp_max_values, function(x) replace(x, is.na(x), temp_max_mean))
  imputed_temp_med_values <- lapply(temp_med_values, function(x) replace(x, is.na(x), temp_med_mean))
  
  # Proceed with the rest of the code using imputed_temp_max_values and imputed_temp_med_values
} else {
  # No imputation needed, proceed with original values
  # ...
}

# Assuming combined_data has been created
dataset_01 <- combined_data[[1]]
dataset_02 <- combined_data[[2]]
dataset_03 <- combined_data[[3]]
dataset_04 <- combined_data[[4]]
dataset_05 <- combined_data[[5]]
dataset_06 <- combined_data[[6]]
dataset_07 <- combined_data[[7]]
dataset_08 <- combined_data[[8]]
dataset_09 <- combined_data[[9]]
dataset_10 <- combined_data[[10]]
dataset_11 <- combined_data[[11]]
dataset_12 <- combined_data[[12]]
dataset_13 <- combined_data[[13]]
dataset_14 <- combined_data[[14]]
dataset_15 <- combined_data[[15]]
dataset_16 <- combined_data[[16]]
dataset_17 <- combined_data[[17]]
dataset_18 <- combined_data[[18]]
dataset_19 <- combined_data[[19]]
dataset_20 <- combined_data[[20]]
dataset_21 <- combined_data[[21]]
dataset_22 <- combined_data[[22]]
dataset_23 <- combined_data[[23]]
dataset_24 <- combined_data[[24]]
dataset_25 <- combined_data[[25]]
dataset_26 <- combined_data[[26]]
dataset_27 <- combined_data[[27]]
dataset_28 <- combined_data[[28]]
dataset_29 <- combined_data[[29]]
dataset_30 <- combined_data[[30]]

# Create a sample_datasets list
sample_datasets <- list(
  dataset_01, dataset_02, dataset_03, dataset_04, dataset_05, 
  dataset_06, dataset_07, dataset_08, dataset_09, dataset_10,
  dataset_11, dataset_12, dataset_13, dataset_14, dataset_15,
  dataset_16, dataset_17, dataset_18, dataset_19, dataset_20,
  dataset_21, dataset_22, dataset_23, dataset_24, dataset_25,
  dataset_26, dataset_27, dataset_28, dataset_29, dataset_30
)

# Create vectors for each cell in the same location across datasets
cell_values <- lapply(2:5, function(col) {
  sapply(sample_datasets, function(dataset) dataset[, col])
})
# Adjust the output format for the vector
cell_values_formatted <- unlist(cell_values)

# Print the first 6 values of specific types of vectors before imputation
print("INSOLACAO_TOTAL")
for (i in 1:6) {
  vector_name <- paste("INSOLACAO_TOTAL", i, sep = "_")
  print(head(vectors[[vector_name]]))
}

print("PRECIPITACAO_TOTAL")
for (i in 1:6) {
  vector_name <- paste("PRECIPITACAO_TOTAL", i, sep = "_")
  print(head(vectors[[vector_name]]))
}

print("TEMPERATURA_MAXIMA")
for (i in 1:6) {
  vector_name <- paste("TEMPERATURA_MAXIMA", i, sep = "_")
  print(head(vectors[[vector_name]]))
}

print("TEMPERATURA_MEDIA")
for (i in 1:6) {
  vector_name <- paste("TEMPERATURA_MEDIA", i, sep = "_")
  print(head(vectors[[vector_name]]))
}

# Impute NA values in temperature-related vectors using mean
for (i in 1:length(vectors)) {
  vectors[[i]][is.na(vectors[[i]])] <- mean(vectors[[i]], na.rm = TRUE)
}

# Print the first 6 values of specific vector types after imputation
print("INSOLACAO_TOTAL")
for (i in 1:6) {
  vector_name <- paste("INSOLACAO_TOTAL", i, sep = "_")
  print(head(vectors[[vector_name]]))
}

print("PRECIPITACAO_TOTAL")
for (i in 1:6) {
  vector_name <- paste("PRECIPITACAO_TOTAL", i, sep = "_")
  print(head(vectors[[vector_name]]))
}

print("TEMPERATURA_MAXIMA")
for (i in 1:6) {
  vector_name <- paste("TEMPERATURA_MAXIMA", i, sep = "_")
  print(head(vectors[[vector_name]]))
}

print("TEMPERATURA_MEDIA")
for (i in 1:6) {
  vector_name <- paste("TEMPERATURA_MEDIA", i, sep = "_")
  print(head(vectors[[vector_name]]))
}

# Define the number of rows
num_rows <- nrow(sample_datasets[[1]])

# Store the vectors in a list
vectors <- list()
for (col in 2:5) {
  for (row in 1:num_rows) {
    vector_name <- paste(colnames(sample_datasets[[1]])[col], row, sep = "_")
    values <- sapply(sample_datasets, function(dataset) dataset[row, col])
    vectors[[vector_name]] <- as.numeric(values) # Ensure numeric conversion
  }
}

# Impute NA values in temperature-related vectors using mean
for (i in 1:length(vectors)) {
  vectors[[i]][is.na(vectors[[i]])] <- mean(vectors[[i]], na.rm = TRUE)
}

# Print the mean of the first 6 values for each specific vector type
print("INSOLACAO_TOTAL")
for (i in 1:6) {
  vector_name <- paste("INSOLACAO_TOTAL", i, sep = "_")
  print(mean(vectors[[vector_name]]))
}

print("PRECIPITACAO_TOTAL")
for (i in 1:6) {
  vector_name <- paste("PRECIPITACAO_TOTAL", i, sep = "_")
  print(mean(vectors[[vector_name]]))
}

print("TEMPERATURA_MAXIMA")
for (i in 1:6) {
  vector_name <- paste("TEMPERATURA_MAXIMA", i, sep = "_")
  print(mean(vectors[[vector_name]]))
}

print("TEMPERATURA_MEDIA")
for (i in 1:6) {
  vector_name <- paste("TEMPERATURA_MEDIA", i, sep = "_")
  print(mean(vectors[[vector_name]]))
}





     



