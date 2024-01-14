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
## "weather_station_INMET_North.R"
## This file is a Historical data meticulously mined from 41 meteorological stations (active)
## (2010-2019) of North Region of Brazil and lays the groundwork for robust analysis.
## ==========================================================================================

# Load the packages
library(dplyr)
library(ggplot2)
library(tidyr)
library(readr)

# List of file paths for the 41 datasets
file_paths_1 <- c("dados_82024.csv", "dados_82042.csv", "dados_82067.csv",
                  "dados_82098.csv", "dados_82106.csv", "dados_82113.csv", 
                  "dados_82141.csv", "dados_82145.csv", "dados_82178.csv",
                  "dados_82181.csv", "dados_82184.csv", "dados_82188.csv",
                  "dados_82191.csv", "dados_82212.csv", "dados_82240.csv",
                  "dados_82246.csv", "dados_82263.csv", "dados_82317.csv",
                  "dados_82326.csv", "dados_82331.csv", "dados_82336.csv",
                  "dados_82353.csv", "dados_82361.csv", "dados_82410.csv", 
                  "dados_82425.csv", "dados_82445.csv", "dados_82533.csv", 
                  "dados_82562.csv", "dados_82610.csv", "dados_82659.csv",
                  "dados_82668.csv", "dados_82704.csv", "dados_82723.csv",
                  "dados_82807.csv", "dados_82861.csv", "dados_82863.csv",
                  "dados_82915.csv", "dados_83033.csv", "dados_83064.csv",
                  "dados_83228.csv", "dados_83235.csv")

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

#combined_data
combined_data <- lapply(file_paths_1, function(file) {
  read_data <- read_delim(file, delim = ";", skip = 11, col_names = FALSE)
  colnames(read_data) <- c("Data", "INSOLACAO_TOTAL", "PRECIPITACAO_TOTAL", 
                           "TEMPERATURA_MAXIMA", "TEMPERATURA_MEDIA")
  read_data$Data <- as.factor(read_data$Data)
  numeric_data <- convert_to_numeric(read_data)
  
  #if (nrow(numeric_data) >= 120) {
   # return(numeric_data)
  #} else {
  #  deleted_datasets <<- deleted_datasets + 1
   # return(NULL)
 # }
})

# Filter out NULL entries and keep datasets with at least 120 rows
#combined_data <- Filter(Negate(is.null), combined_data)

# Show the number of datasets deleted
#cat("Number of datasets deleted (less than 120 rows):", deleted_datasets, "\n")

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
for (i in 1:41) {
  assign(paste0("dataset_", sprintf("%02d", i)), combined_data[[i]])
}

sample_datasets <- list()
for (i in 1:41) {
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
new_df_norte <- data.frame(Data, INSOLACAO_TOTAL, PRECIPITACAO_TOTAL, TEMPERATURA_MAXIMA, TEMPERATURA_MEDIA)

# Show the structure of the new data frame
str(new_df_norte)
head(new_df_norte)

# Save as CSV with semicolon delimiter
write.table(new_df_norte, file = "proj_ipea_Norte.csv", sep = ";", row.names = FALSE)
