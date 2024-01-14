#Environment empty
rm(list = ls())
## ==========================================================================================
##          Economic effect of climatic factors on the Health Sector in Brazil -
##                                  Scientific Essay
## ==========================================================================================
## Main Goal: aimed to estimate cases of cardiovascular disease in a Brazilian region 
## based on climatic factors with the construction of a linear regression model, 
## survey treatment costs, forecast national population growth and future climate 
## change scenarios.
## About this file
## "weather_station_INMET_South.R"
## This file is a Historical data meticulously mined from 40 meteorological stations (active)
## (2010-2019) of South Region of Brazil and lays the groundwork for robust analysis.
## ==========================================================================================
# Load the packages
library(dplyr)
library(ggplot2)
library(tidyr)
library(readr)

# List of file paths for the 40 datasets
file_paths_1 <- c("dados_82684.csv", "dados_83766.csv", "dados_83767.csv",
                  "dados_83783.csv", "dados_83811.csv", "dados_83813.csv",
                  "dados_83836.csv", "dados_83841.csv", "dados_83842.csv", 
                  "dados_83844.csv", "dados_83872.csv", "dados_83881.csv", 
                  "dados_83883.csv", "dados_83887.csv", "dados_83891.csv", 
                  "dados_83897.csv", "dados_83907.csv", "dados_83912.csv",
                  "dados_83914.csv", "dados_83916.csv", "dados_83919.csv",
                  "dados_83920.csv", "dados_83923.csv", "dados_83927.csv", 
                  "dados_83932.csv", "dados_83936.csv", "dados_83941.csv", 
                  "dados_83942.csv", "dados_83946.csv", "dados_83948.csv", 
                  "dados_83953.csv", "dados_83954.csv", "dados_83961.csv", 
                  "dados_83964.csv", "dados_83967.csv", "dados_83972.csv", 
                  "dados_83980.csv", "dados_83985.csv", "dados_83995.csv", 
                  "dados_83997.csv")

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
deleted_datasets <- 0

combined_data <- lapply(file_paths_1, function(file) {
  read_data <- read_delim(file, delim = ";", skip = 11, col_names = FALSE)
  colnames(read_data) <- c("Data", "INSOLACAO_TOTAL", "PRECIPITACAO_TOTAL", 
                           "TEMPERATURA_MAXIMA", "TEMPERATURA_MEDIA")
  read_data$Data <- as.factor(read_data$Data)
  numeric_data <- convert_to_numeric(read_data)
  
  if (nrow(numeric_data) >= 120) {
    return(numeric_data)
  } else {
    deleted_datasets <<- deleted_datasets + 1
    return(NULL)
  }
})

# Filter out NULL entries and keep datasets with at least 120 rows
combined_data <- Filter(Negate(is.null), combined_data)

# Show the number of datasets deleted
cat("Number of datasets deleted (less than 120 rows):", deleted_datasets, "\n")

#combined_data
combined_data <- lapply(combined_data, function(data) {
  if (ncol(data) >= 6) {
    data <- data[, -6, drop = FALSE]  # Remove the sixth column if it exists
  } else {
    # If fewer than 6 columns exist, return the data as is
    data
  }
})

# Show the modified structure with the first 5 columns only
head(combined_data, 5)

# Assuming combined_data has been created
sample_datasets <- combined_data

# Assuming combined_data is a list with 74 elements
for (i in 1:23) {
  assign(paste0("dataset_", sprintf("%02d", i)), combined_data[[i]])
}

sample_datasets <- list()
for (i in 1:23) {
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
new_df_sul <- data.frame(Data, INSOLACAO_TOTAL, PRECIPITACAO_TOTAL, TEMPERATURA_MAXIMA, TEMPERATURA_MEDIA)

# Show the structure of the new data frame
str(new_df_sul$Data)
head(new_df_sul)

#Summary for each variable
summary(new_df_sul$PRECIPITACAO_TOTAL)
summary(new_df_sul$TEMPERATURA_MEDIA)
summary(new_df_sul$TEMPERATURA_MAXIMA)# Get summary statistics
boxplot(new_df_sul$TEMPERATURA_MEDIA)
boxplot(new_df_sul$TEMPERATURA_MAXIMA) # Create a boxplot to visualize distribution

# Save as CSV with semicolon delimiter
write.table(new_df_sul, file = "proj_ipea_Sul.csv", sep = ";", row.names = FALSE)
