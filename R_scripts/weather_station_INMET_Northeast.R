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
## "weather_station_INMET_Northeast.R"
## This file is a Historical data meticulously mined from 93 meteorological stations (active)
## (2010-2019) of Northeas Region of Brazil and lays the groundwork for robust analysis.
## ==========================================================================================
# Load the packages
library(dplyr)
library(ggplot2)
library(tidyr)
library(readr)

# List of file paths for the 93 datasets
file_paths_1 <- c(
  "dados_82198.csv","dados_82280.csv", "dados_82287.csv", "dados_82294.csv", 
  "dados_82296.csv", "dados_82298.csv", "dados_82376.csv", "dados_82382.csv", 
  "dados_82392.csv", "dados_82397.csv", "dados_82460.csv", "dados_82474.csv",
  "dados_82476.csv", "dados_82480.csv", "dados_82487.csv", "dados_82493.csv",
  "dados_82564.csv", "dados_82571.csv", "dados_82578.csv", "dados_82583.csv",
  "dados_82586.csv", "dados_82588.csv", "dados_82590.csv", "dados_82594.csv",
  "dados_82596.csv", "dados_82598.csv", "dados_82676.csv", "dados_82678.csv",
  "dados_82683.csv", "dados_82686.csv", "dados_82690.csv", "dados_82691.csv",
  "dados_82693.csv", "dados_82696.csv", "dados_82753.csv", "dados_82765.csv",
  "dados_82768.csv", "dados_82777.csv", "dados_82780.csv", "dados_82784.csv",
  "dados_82789.csv", "dados_82791.csv", "dados_82792.csv", "dados_82795.csv",
  "dados_82797.csv", "dados_82798.csv", "dados_82870.csv", "dados_82879.csv",
  "dados_82882.csv", "dados_82886.csv", "dados_82890.csv", "dados_82892.csv",
  "dados_82893.csv", "dados_82900.csv", "dados_82970.csv", "dados_82975.csv",
  "dados_82979.csv", "dados_82983.csv", "dados_82986.csv", "dados_82989.csv",
  "dados_82990.csv", "dados_82992.csv", "dados_82994.csv", "dados_82996.csv",
  "dados_83076.csv", "dados_83088.csv", "dados_83090.csv", "dados_83096.csv",
  "dados_83097.csv", "dados_83179.csv", "dados_83182.csv", "dados_83184.csv",
  "dados_83186.csv", "dados_83190.csv", "dados_83192.csv", "dados_83195.csv",
  "dados_83221.csv", "dados_83222.csv", "dados_83229.csv", "dados_83236.csv",
  "dados_83242.csv", "dados_83244.csv", "dados_83249.csv", "dados_83286.csv",
  "dados_83288.csv", "dados_83292.csv", "dados_83295.csv", "dados_83339.csv",
  "dados_83344.csv", "dados_83398.csv", "dados_83408.csv", "dados_83446.csv",
  "dados_83498.csv"
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

#combined_data
combined_data <- lapply(file_paths_1, function(file) {
  read_data <- read_delim(file, delim = ";", skip = 11, col_names = FALSE)
  colnames(read_data) <- c("Data", "INSOLACAO_TOTAL", "PRECIPITACAO_TOTAL", 
                           "TEMPERATURA_MAXIMA", "TEMPERATURA_MEDIA")
  read_data$Data <- as.factor(read_data$Data)
  numeric_data <- convert_to_numeric(read_data)
  
  #if (nrow(numeric_data) >= 120) {
  #  return(numeric_data)
  #} else {
    #deleted_datasets <<- deleted_datasets + 1
   # return(NULL)
  #}
})

# Filter out NULL entries and keep datasets with at least 120 rows
#combined_data <- Filter(Negate(is.null), combined_data)

# Show the number of datasets deleted
#cat("Number of datasets deleted (less than 120 rows):", deleted_datasets, "\n")

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
new_df_nordeste <- data.frame(Data, INSOLACAO_TOTAL, PRECIPITACAO_TOTAL, TEMPERATURA_MAXIMA, TEMPERATURA_MEDIA)

# Show the structure of the new data frame
str(new_df_nordeste)
head(new_df_nordeste)

# Save as CSV with semicolon delimiter
write.table(new_df_nordeste, file = "proj_ipea_Nordeste.csv", sep = ";", row.names = FALSE)




