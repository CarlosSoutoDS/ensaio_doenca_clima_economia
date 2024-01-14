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
## "weather_station_INMET_Southeast.R"
## This file is a Historical data meticulously mined from 90 meteorological stations (active)
## (2010-2019) of Southeast Region of Brazil and lays the groundwork for robust analysis.
## ==========================================================================================
# Load the packages
library(dplyr)
library(ggplot2)
library(tidyr)
library(readr)

# List of file paths for the 90 datasets
file_paths_1 <- c("dados_82689.csv", "dados_83013.csv", "dados_83015.csv",
                  "dados_83032.csv", "dados_83034.csv", "dados_83048.csv",
                  "dados_83049.csv", "dados_83075.csv", "dados_83089.csv",
                  "dados_83114.csv", "dados_83334.csv", "dados_83338.csv",
                  "dados_83363.csv", "dados_83373.csv", "dados_83384.csv",
                  "dados_83386.csv", "dados_83388.csv", "dados_83389.csv",
                  "dados_83393.csv", "dados_83395.csv", "dados_83428.csv",
                  "dados_83437.csv", "dados_83441.csv", "dados_83442.csv",
                  "dados_83452.csv", "dados_83479.csv", "dados_83481.csv", 
                  "dados_83483.csv", "dados_83485.csv", "dados_83488.csv", 
                  "dados_83492.csv", "dados_83514.csv", "dados_83521.csv",
                  "dados_83531.csv", "dados_83533.csv", "dados_83536.csv",
                  "dados_83538.csv", "dados_83546.csv", "dados_83550.csv",
                  "dados_83557.csv", "dados_83570.csv", "dados_83574.csv",
                  "dados_83577.csv", "dados_83579.csv", "dados_83581.csv",
                  "dados_83582.csv", "dados_83586.csv", "dados_83587.csv",
                  "dados_83589.csv", "dados_83591.csv", "dados_83592.csv",
                  "dados_83595.csv", "dados_83596.csv", "dados_83623.csv", 
                  "dados_83630.csv", "dados_83631.csv", "dados_83632.csv",
                  "dados_83635.csv", "dados_83637.csv", "dados_83639.csv", 
                  "dados_83642.csv", "dados_83648.csv", "dados_83669.csv",
                  "dados_83676.csv", "dados_83681.csv", "dados_83683.csv",
                  "dados_83687.csv", "dados_83689.csv", "dados_83692.csv",
                  "dados_83695.csv", "dados_83696.csv", "dados_83698.csv",
                  "dados_83714.csv", "dados_83716.csv", "dados_83718.csv",
                  "dados_83726.csv", "dados_83736.csv", "dados_83737.csv",
                  "dados_83738.csv", "dados_83743.csv", "dados_83744.csv",
                  "dados_83773.csv", "dados_83781.csv", "dados_83784.csv",
                  "dados_83788.csv", "dados_83789.csv", "dados_83801.csv",
                  "dados_83805.csv", "dados_83807.csv", "dados_83851.csv")

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
  
  #if (nrow(numeric_data) >= 120) {
  # return(numeric_data)
  #} else {
   # deleted_datasets <<- deleted_datasets + 1
   # return(NULL)
#  }
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
for (i in 1:90) {
  assign(paste0("dataset_", sprintf("%02d", i)), combined_data[[i]])
}

sample_datasets <- list()
for (i in 1:90) {
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
new_df_sudeste <- data.frame(Data, INSOLACAO_TOTAL, PRECIPITACAO_TOTAL, TEMPERATURA_MAXIMA, TEMPERATURA_MEDIA)

# Show the structure of the new data frame
str(new_df_sudeste)
head(new_df_sudeste)

# Save as CSV with semicolon delimiter
write.table(new_df_sudeste, file = "proj_ipea_Sudeste.csv", sep = ";", row.names = FALSE)