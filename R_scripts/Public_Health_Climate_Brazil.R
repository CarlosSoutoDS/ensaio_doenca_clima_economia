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

#####################################################################
## Correlation analysis between diseases and climate - Regions Brazil 
#####################################################################
## Information sources
## The information sources are publicly accessible and divided by area (sector):
##  ▪ Public Health - Brazil: available at https://datasus.saude.gov.br/informacoes-de-saude-tabnet/
## Files in .csv format were obtained from the DATASUS database on:
## ▪ Hospital Morbidity - by place of hospitalization
## >> Admissions by Year/month of processing and Federation Unit:
## >> Period: 2010-2019
## >> Pathologies:
##  - Chapter (ICD-10 F) Mental disorders
##  - Chapter (ICD-10 M) Diseases of the musculoskeletal system and connective tissue
##  - Chapter ICD-10 Neoplasms (tumors)
##  - Chapter ICD-10 Diseases of the respiratory system
##  - Categories Causes: Traffic accident
##  ▪ Mortality
## >> Deaths by Residence by Year / month of Death and Federation Unit
##  - Group: Ischemic heart diseases, Cerebrovascular diseases (CVD)
##  - Traffic Accident Category
## ▪ DENGUE VIRUS (Notification)
## >> Probable Cases by Month Notification and State of notification
## >> Period: 2010-2019
## About this file
## "Public_Health_Climate_Brazil.R"
## This file is a Historical data meticulously mined from Department of Informatics of the 
## Unified Health System (DATASUS) (2010-2019) of Regions of Brazil and lays the groundwork 
## for robust analysis.
## ==========================================================================================

# Install and Load the necessary packages
install.packages("mice")
install.packages("GGally")
install.packages("zoo")
#carregas as bibliotecas
library(dplyr)
library(ggplot2)
library(tidyr)
library(readr)
library(mice)
library(Hmisc)
library(plyr)
library(dplyr)
library(forcats)
library(tibble)
library(purrr)
library(GGally)
library(ggplot2)
library(zoo)  # For working with yearmon objects
library(lubridate)  # For easy date manipulation

########################################
## Cardiovascular disease - South Region 
########################################

# Replace 'file_path.csv' with your CSV file path
file_path <- "dcv_nacional.csv"

# Read CSV file starting from row 5 with delimiter ';', specifying decimal and grouping marks
data_dcv <- read_delim(file_path, delim = ";", skip = 4)
class(data_dcv)
head(data_dcv)
dim(data_dcv)

#missing data
#Use md.pattern() from mice to visualize the missing data pattern
md.pattern(data_dcv, rotate.names = TRUE)

#Use describe() from Hmisc
Hmisc::describe(data_dcv)

# Remove the column at a specific index (e.g., 1)
str(new_df_sul)
new_df_sul <- new_df_sul[, -1]

# Create a new column 'Data' in 'new_df_sul' and assign values from 'data_dcv'
new_df_sul$Data_new <- as.character(data_dcv$Data)

# Display the updated 'new_df_sul' dataset
head(new_df_sul)
str(new_df_sul$Data_new)

#Join Data sets
# Selecting only necessary columns from data_dcv
data_dcv_subset <- data_dcv[, c("Sul")]
head(data_dcv_subset)

# Combine 'data_dcv_subset' as a new column in 'new_df_sul' and rename it as 'new_data_dcv'
new_data_dcv <- cbind(new_df_sul, new_data_dcv = data_dcv_subset)

# Show the structure of the new dataset
str(new_data_dcv)

# Renaming columns in 'new_data_dcv'
colnames(new_data_dcv)[colnames(new_data_dcv) == "Data_new"] <- "Data"
# ... Repeat this pattern for other columns you want to rename

# Show the updated column names
colnames(new_data_dcv)

# Create a sequence of years from 2010 to 2019 and repeat each year 10 times
ano <- rep(2010:2019, each = 12)

# Add the 'year' column to 'merged_data'
new_data_dcv <- transform(new_data_dcv, ano = as.factor(ano))

# Show the updated dataset with the new 'year' column
head(new_data_dcv)
str(new_data_dcv)

# Save as CSV with semicolon delimiter
write.table(new_data_dcv, file = "data_dcv_clima.csv", sep = ";", row.names = FALSE)

#to test "load file"

######################################
## Cardiovascular disease and climate 
######################################

# Replace 'file_path.csv' with your CSV file path
file_path2 <- "data_dcv_clima.csv"

# Read CSV file starting from row 5 with delimiter ';', specifying decimal and grouping marks
data_dcv_clima <- read_delim(file_path2, delim = ";")
str(data_dcv_clima) #ano (numerico)
class(data_dcv_clima)
head(data_dcv_clima)

#Several Scatterplot (South)
# Select the desired columns
data_subset_plot <- data_dcv_clima[, c("Sul", "INSOLACAO_TOTAL", "PRECIPITACAO_TOTAL", "TEMPERATURA_MAXIMA", "TEMPERATURA_MEDIA")]

# Rename the columns for better visualization
colnames(data_subset_plot) <- c("DCV", "insolacao", "pp", "t_max", "t_med")

# Create the scatter plot matrix using GGally
ggpairs(data_subset_plot,
        title = "Scatter Plot Matrix for DCV and Climate Variables",
        upper = list(continuous = wrap("cor", size = 3)),  # Add correlation coefficients
        lower = list(continuous = "smooth"))  # Add smoothers

#Times Series from Montly Panel

# Convert the "Data" column to yearmon objects
data_dcv_clima$Data <- as.yearmon(paste0("01-", data_dcv_clima$Data), format = "%d-%b_%Y")  # Pad with "01-" for consistency
str(data_dcv_clima)
head(data_dcv_clima)

# Select the relevant variables
data_subset <- data_dcv_clima[, c("INSOLACAO_TOTAL", "PRECIPITACAO_TOTAL", 
            "TEMPERATURA_MAXIMA", "TEMPERATURA_MEDIA", "Data", "Sul")]

# Reshape the data into a long format using tidyr::pivot_longer
data_long <- pivot_longer(data_subset, cols = -Data, names_to = "variable", values_to = "value")

# Now use the reshaped data for plotting
ggplot(data_long, aes(x = Data, y = value)) +
  facet_wrap(~ variable, scales = "free_y", ncol = 2) +
  geom_line() +
  labs(title = "Figure 2. CVD-Climate Interactions in South Brazil: 
       A Monthly Data Approach (2010-2019)",
       y = "Values") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        strip.text = element_text(size = 10)) +
  scale_x_yearmon(format = "%b %Y", n = 12, limits = c(as.yearmon("jan 2010"), as.yearmon("dez 2019")))

############################################
## Cardiovascular disease - Southeast Region 
############################################

# Replace 'file_path.csv' with your CSV file path
file_path <- "dcv_nacional.csv"

# Read CSV file starting from row 5 with delimiter ';', specifying decimal and grouping marks
data_dcv <- read_delim(file_path, delim = ";", skip = 4)
class(data_dcv)
head(data_dcv)
dim(data_dcv)

#missing data
#Use md.pattern() from mice to visualize the missing data pattern
md.pattern(data_dcv, rotate.names = TRUE)

#Use describe() from Hmisc
Hmisc::describe(data_dcv)

# Replace 'file_path.csv' with your CSV file path
file_path2 <- "proj_ipea_Sudeste.csv"

# Read CSV file starting from row 5 with delimiter ';', specifying decimal and grouping marks
new_df_sudeste <- read_delim(file_path2, delim = ";")
str(new_df_sudeste)

# Remove the column at a specific index (e.g., 1)
new_df_sudeste <- new_df_sudeste[, -1]

# Create a new column 'Data' in 'new_df_sul' and assign values from 'data_dcv'
new_df_sudeste$Data_new <- as.character(data_dcv$Data)

# Display the updated 'new_df_sudeste' dataset
head(new_df_sudeste)
str(new_df_sudeste$Data_new)

#Join Data sets
# Selecting only necessary columns from data_dcv
data_dcv_subset <- data_dcv[, c("Sudeste")]
head(data_dcv_subset)

# Combine 'data_dcv_subset' as a new column in 'new_df_sul' and rename it as 'new_data_dcv'
new_data_dcv <- cbind(new_df_sudeste, new_data_dcv = data_dcv_subset)

# Show the structure of the new dataset
str(new_data_dcv)

# Renaming columns in 'new_data_dcv'
colnames(new_data_dcv)[colnames(new_data_dcv) == "Data_new"] <- "Data"
# ... Repeat this pattern for other columns you want to rename

# Show the updated column names
colnames(new_data_dcv)

# Create a sequence of years from 2010 to 2019 and repeat each year 10 times
ano <- rep(2010:2019, each = 12)

# Add the 'year' column to 'merged_data'
new_data_dcv <- transform(new_data_dcv, ano = as.factor(ano))

# Show the updated dataset with the new 'year' column
head(new_data_dcv)
str(new_data_dcv)

# Save as CSV with semicolon delimiter
write.table(new_data_dcv, file = "data_dcv_clima_sud.csv", sep = ";", row.names = FALSE)

#to test"load file"

######################################
## Cardiovascular disease and climate 
######################################

# Replace 'file_path.csv' with your CSV file path
file_path_sud <- "data_dcv_clima_sud.csv"

# Read CSV file starting from row 5 with delimiter ';', specifying decimal and grouping marks
data_dcv_clima <- read_delim(file_path_sud, delim = ";")
str(data_dcv_clima) #ano (numerico)
class(data_dcv_clima)
head(data_dcv_clima)

#Several Scatterplot (Southeast)
# Select the desired columns
data_subset_plot <- data_dcv_clima[, c("Sudeste", "INSOLACAO_TOTAL", "PRECIPITACAO_TOTAL", "TEMPERATURA_MAXIMA", "TEMPERATURA_MEDIA")]

# Rename the columns for better visualization
colnames(data_subset_plot) <- c("DCV", "insolacao", "pp", "t_max", "t_med")

# Create the scatter plot matrix using GGally
ggpairs(data_subset_plot,
        title = "Scatter Plot Matrix for DCV and Climate Variables",
        upper = list(continuous = wrap("cor", size = 3)),  # Add correlation coefficients
        lower = list(continuous = "smooth"))  # Add smoothers

#Times Series from Montly Panel

# Convert the "Data" column to yearmon objects
data_dcv_clima$Data <- as.yearmon(paste0("01-", data_dcv_clima$Data), format = "%d-%b_%Y")  # Pad with "01-" for consistency
str(data_dcv_clima)
head(data_dcv_clima)

# Select the relevant variables
data_subset <- data_dcv_clima[, c("INSOLACAO_TOTAL", "PRECIPITACAO_TOTAL", 
                                  "TEMPERATURA_MAXIMA", "TEMPERATURA_MEDIA", "Data", "Sudeste")]

# Reshape the data into a long format using tidyr::pivot_longer
data_long <- pivot_longer(data_subset, cols = -Data, names_to = "variable", values_to = "value")

# Now use the reshaped data for plotting
ggplot(data_long, aes(x = Data, y = value)) +
  facet_wrap(~ variable, scales = "free_y", ncol = 2) +
  geom_line() +
  labs(title = "Figure. CVD-Climate Interactions in Southeast Brazil: 
                       A Monthly Data Approach (2010-2019)",
       y = "Values") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        strip.text = element_text(size = 10)) +
  scale_x_yearmon(format = "%b %Y", n = 12, limits = c(as.yearmon("jan 2010"), as.yearmon("dez 2019")))

############################################
## Cardiovascular disease - Northeast Region 
############################################

# Replace 'file_path.csv' with your CSV file path
file_path <- "dcv_nacional.csv"

# Read CSV file starting from row 5 with delimiter ';', specifying decimal and grouping marks
data_dcv <- read_delim(file_path, delim = ";", skip = 4)
class(data_dcv)
head(data_dcv)
dim(data_dcv)

#missing data
#Use md.pattern() from mice to visualize the missing data pattern
md.pattern(data_dcv, rotate.names = TRUE)

#Use describe() from Hmisc
Hmisc::describe(data_dcv)

# Replace 'file_path.csv' with your CSV file path
file_path_nord <- "proj_ipea_Nordeste.csv"

# Read CSV file starting from row 5 with delimiter ';', specifying decimal and grouping marks
new_df_nordeste <- read_delim(file_path_nord, delim = ";")
str(new_df_nordeste)

# Remove the column at a specific index (e.g., 1)
new_df_nordeste <- new_df_nordeste[, -1]

# Create a new column 'Data' in 'new_df_sul' and assign values from 'data_dcv'
new_df_nordeste$Data_new <- as.character(data_dcv$Data)

# Display the updated 'new_df_sudeste' dataset
head(new_df_nordeste)
str(new_df_nordeste$Data_new)

#Join Data sets
# Selecting only necessary columns from data_dcv
data_dcv_subset <- data_dcv[, c("Nordeste")]
head(data_dcv_subset)

# Combine 'data_dcv_subset' as a new column in 'new_df_sul' and rename it as 'new_data_dcv'
new_data_dcv <- cbind(new_df_nordeste, new_data_dcv = data_dcv_subset)

# Show the structure of the new dataset
str(new_data_dcv)

# Renaming columns in 'new_data_dcv'
colnames(new_data_dcv)[colnames(new_data_dcv) == "Data_new"] <- "Data"
# ... Repeat this pattern for other columns you want to rename

# Show the updated column names
colnames(new_data_dcv)

# Create a sequence of years from 2010 to 2019 and repeat each year 10 times
ano <- rep(2010:2019, each = 12)

# Add the 'year' column to 'merged_data'
new_data_dcv <- transform(new_data_dcv, ano = as.factor(ano))

# Show the updated dataset with the new 'year' column
head(new_data_dcv)
str(new_data_dcv)

# Save as CSV with semicolon delimiter
write.table(new_data_dcv, file = "data_dcv_clima_nord.csv", sep = ";", row.names = FALSE)

#to test "load file"

######################################
## Cardiovascular disease and climate 
######################################

# Replace 'file_path.csv' with your CSV file path
file_path_nord2 <- "data_dcv_clima_nord.csv"

# Read CSV file starting from row 5 with delimiter ';', specifying decimal and grouping marks
data_dcv_clima <- read_delim(file_path_nord2, delim = ";")
str(data_dcv_clima) #ano (numerico)
class(data_dcv_clima)
head(data_dcv_clima)

# Several Scatterplot (Northeast)
# Select the desired columns
data_subset_plot <- data_dcv_clima[, c("Nordeste", "INSOLACAO_TOTAL", "PRECIPITACAO_TOTAL", "TEMPERATURA_MAXIMA", "TEMPERATURA_MEDIA")]

# Rename the columns for better visualization
colnames(data_subset_plot) <- c("DCV", "insolacao", "pp", "t_max", "t_med")

# Create the scatter plot matrix using GGally
ggpairs(data_subset_plot,
        title = "Scatter Plot Matrix for DCV and Climate Variables",
        upper = list(continuous = wrap("cor", size = 3)),  # Add correlation coefficients
        lower = list(continuous = "smooth"))  # Add smoothers

#Times Series from Montly Panel

# Convert the "Data" column to yearmon objects
data_dcv_clima$Data <- as.yearmon(paste0("01-", data_dcv_clima$Data), format = "%d-%b_%Y")  # Pad with "01-" for consistency
str(data_dcv_clima)
head(data_dcv_clima)

# Select the relevant variables
data_subset <- data_dcv_clima[, c("INSOLACAO_TOTAL", "PRECIPITACAO_TOTAL", 
                                  "TEMPERATURA_MAXIMA", "TEMPERATURA_MEDIA", "Data", "Nordeste")]

# Reshape the data into a long format using tidyr::pivot_longer
data_long <- pivot_longer(data_subset, cols = -Data, names_to = "variable", values_to = "value")

# Now use the reshaped data for plotting
ggplot(data_long, aes(x = Data, y = value)) +
  facet_wrap(~ variable, scales = "free_y", ncol = 2) +
  geom_line() +
  labs(title = "Figure. CVD-Climate Interactions in Northeast Brazil: 
                       A Monthly Data Approach (2010-2019)",
       y = "Values") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        strip.text = element_text(size = 10)) +
  scale_x_yearmon(format = "%b %Y", n = 12, limits = c(as.yearmon("jan 2010"), as.yearmon("dez 2019")))

############################################
## Cardiovascular disease - Northeast Region 
############################################

# Replace 'file_path.csv' with your CSV file path
file_path <- "dcv_nacional.csv"

# Read CSV file starting from row 5 with delimiter ';', specifying decimal and grouping marks
data_dcv <- read_delim(file_path, delim = ";", skip = 4)
class(data_dcv)
head(data_dcv)
dim(data_dcv)

#missing data
#Use md.pattern() from mice to visualize the missing data pattern
md.pattern(data_dcv, rotate.names = TRUE)

#Use describe() from Hmisc
Hmisc::describe(data_dcv)

# Replace 'file_path.csv' with your CSV file path
file_path_norte <- "proj_ipea_Norte.csv"

# Read CSV file starting from row 5 with delimiter ';', specifying decimal and grouping marks
new_df_norte <- read_delim(file_path_norte, delim = ";")
str(new_df_norte)

# Remove the column at a specific index (e.g., 1)
new_df_norte <- new_df_norte[, -1]

# Create a new column 'Data' in 'new_df_sul' and assign values from 'data_dcv'
new_df_norte$Data_new <- as.character(data_dcv$Data)

# Display the updated 'new_df_sudeste' dataset
head(new_df_norte)
str(new_df_norte$Data_new)

#Join Data sets
# Selecting only necessary columns from data_dcv
data_dcv_subset <- data_dcv[, c("Norte")]
head(data_dcv_subset)

# Combine 'data_dcv_subset' as a new column in 'new_df_sul' and rename it as 'new_data_dcv'
new_data_dcv <- cbind(new_df_norte, new_data_dcv = data_dcv_subset)

# Show the structure of the new dataset
str(new_data_dcv)

# Renaming columns in 'new_data_dcv'
colnames(new_data_dcv)[colnames(new_data_dcv) == "Data_new"] <- "Data"
# ... Repeat this pattern for other columns you want to rename

# Show the updated column names
colnames(new_data_dcv)

# Create a sequence of years from 2010 to 2019 and repeat each year 10 times
ano <- rep(2010:2019, each = 12)

# Add the 'year' column to 'merged_data'
new_data_dcv <- transform(new_data_dcv, ano = as.factor(ano))

# Show the updated dataset with the new 'year' column
head(new_data_dcv)
str(new_data_dcv)

# Save as CSV with semicolon delimiter
write.table(new_data_dcv, file = "data_dcv_clima_norte.csv", sep = ";", row.names = FALSE)

#to test "load file"

######################################
## Cardiovascular disease and climate 
######################################

# Replace 'file_path.csv' with your CSV file path
file_path_norte2 <- "data_dcv_clima_norte.csv"

# Read CSV file starting from row 5 with delimiter ';', specifying decimal and grouping marks
data_dcv_clima <- read_delim(file_path_norte2, delim = ";")
str(data_dcv_clima)
class(data_dcv_clima)
head(data_dcv_clima)

# Several Scatterplot (Northeast)
# Select the desired columns
data_subset_plot <- data_dcv_clima[, c("Norte", "INSOLACAO_TOTAL", "PRECIPITACAO_TOTAL", "TEMPERATURA_MAXIMA", "TEMPERATURA_MEDIA")]

# Rename the columns for better visualization
colnames(data_subset_plot) <- c("DCV", "insolacao", "pp", "t_max", "t_med")

# Create the scatter plot matrix using GGally
ggpairs(data_subset_plot,
        title = "Scatter Plot Matrix for DCV and Climate Variables",
        upper = list(continuous = wrap("cor", size = 3)),  # Add correlation coefficients
        lower = list(continuous = "smooth"))  # Add smoothers

#Times Series from Montly Panel

# Convert the "Data" column to yearmon objects
data_dcv_clima$Data <- as.yearmon(paste0("01-", data_dcv_clima$Data), format = "%d-%b_%Y")  # Pad with "01-" for consistency
str(data_dcv_clima)
head(data_dcv_clima)

# Select the relevant variables
data_subset <- data_dcv_clima[, c("INSOLACAO_TOTAL", "PRECIPITACAO_TOTAL", 
                                  "TEMPERATURA_MAXIMA", "TEMPERATURA_MEDIA", "Data", "Norte")]

# Reshape the data into a long format using tidyr::pivot_longer
data_long <- pivot_longer(data_subset, cols = -Data, names_to = "variable", values_to = "value")

# Now use the reshaped data for plotting
ggplot(data_long, aes(x = Data, y = value)) +
  facet_wrap(~ variable, scales = "free_y", ncol = 2) +
  geom_line() +
  labs(title = "Figure. CVD-Climate Interactions in North Brazil: 
                       A Monthly Data Approach (2010-2019)",
       y = "Values") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        strip.text = element_text(size = 10)) +
  scale_x_yearmon(format = "%b %Y", n = 12, limits = c(as.yearmon("jan 2010"), as.yearmon("dez 2019")))

###########################################
## Cardiovascular disease - Midwest Region 
###########################################

# Replace 'file_path.csv' with your CSV file path
file_path <- "dcv_nacional.csv"

# Read CSV file starting from row 5 with delimiter ';', specifying decimal and grouping marks
data_dcv <- read_delim(file_path, delim = ";", skip = 4)
class(data_dcv)
head(data_dcv)
dim(data_dcv)

#missing data
#Use md.pattern() from mice to visualize the missing data pattern
md.pattern(data_dcv, rotate.names = TRUE)

#Use describe() from Hmisc
Hmisc::describe(data_dcv)

# Replace 'file_path.csv' with your CSV file path
file_path_co <- "proj_ipea_CO.csv"

# Read CSV file starting from row 5 with delimiter ';', specifying decimal and grouping marks
new_df_centro_oeste <- read_delim(file_path_co, delim = ";")
str(new_df_centro_oeste)

# Remove the column at a specific index (e.g., 1)
new_df_centro_oeste <- new_df_centro_oeste[, -1]

# Create a new column 'Data' in 'new_df_sul' and assign values from 'data_dcv'
new_df_centro_oeste$Data_new <- as.character(data_dcv$Data)

# Display the updated 'new_df_sudeste' dataset
head(new_df_centro_oeste)
str(new_df_centro_oeste$Data_new)

#Join Data sets
# Selecting only necessary columns from data_dcv
data_dcv_subset <- data_dcv[, c("Centro_Oeste")]
head(data_dcv_subset)

# Combine 'data_dcv_subset' as a new column in 'new_df_sul' and rename it as 'new_data_dcv'
new_data_dcv <- cbind(new_df_centro_oeste, new_data_dcv = data_dcv_subset)

# Show the structure of the new dataset
str(new_data_dcv)

# Renaming columns in 'new_data_dcv'
colnames(new_data_dcv)[colnames(new_data_dcv) == "Data_new"] <- "Data"
# ... Repeat this pattern for other columns you want to rename

# Show the updated column names
colnames(new_data_dcv)

# Create a sequence of years from 2010 to 2019 and repeat each year 10 times
ano <- rep(2010:2019, each = 12)

# Add the 'year' column to 'merged_data'
new_data_dcv <- transform(new_data_dcv, ano = as.factor(ano))

# Show the updated dataset with the new 'year' column
head(new_data_dcv)
str(new_data_dcv)

# Save as CSV with semicolon delimiter
write.table(new_data_dcv, file = "data_dcv_clima_centro.csv", sep = ";", row.names = FALSE)

#to test "load file"

######################################
## Cardiovascular disease and climate 
######################################

# Replace 'file_path.csv' with your CSV file path
file_path_co2 <- "data_dcv_clima_centro.csv"

# Read CSV file starting from row 5 with delimiter ';', specifying decimal and grouping marks
data_dcv_clima <- read_delim(file_path_co2, delim = ";")
str(data_dcv_clima) #ano (numerico)
class(data_dcv_clima)
head(data_dcv_clima)

# Several Scatterplot (Midwest)
# Select the desired columns
data_subset_plot <- data_dcv_clima[, c("Centro_Oeste", "INSOLACAO_TOTAL", "PRECIPITACAO_TOTAL", "TEMPERATURA_MAXIMA", "TEMPERATURA_MEDIA")]

# Rename the columns for better visualization
colnames(data_subset_plot) <- c("DCV", "insolacao", "pp", "t_max", "t_med")

# Create the scatter plot matrix using GGally
ggpairs(data_subset_plot,
        title = "Scatter Plot Matrix for DCV and Climate Variables",
        upper = list(continuous = wrap("cor", size = 3)),  # Add correlation coefficients
        lower = list(continuous = "smooth"))  # Add smoothers

#Times Series from Montly Panel

# Convert the "Data" column to yearmon objects
data_dcv_clima$Data <- as.yearmon(paste0("01-", data_dcv_clima$Data), format = "%d-%b_%Y")  # Pad with "01-" for consistency
str(data_dcv_clima)
head(data_dcv_clima)

# Select the relevant variables
data_subset <- data_dcv_clima[, c("INSOLACAO_TOTAL", "PRECIPITACAO_TOTAL", 
                                  "TEMPERATURA_MAXIMA", "TEMPERATURA_MEDIA", "Data", "Centro_Oeste")]


# Reshape the data into a long format using tidyr::pivot_longer
data_long <- pivot_longer(data_subset, cols = -Data, names_to = "variable", values_to = "value")

# Now use the reshaped data for plotting
ggplot(data_long, aes(x = Data, y = value)) +
  facet_wrap(~ variable, scales = "free_y", ncol = 2) +
  geom_line() +
  labs(title = "Figure. CVD-Climate Interactions in Central-West Brazil: 
                       A Monthly Data Approach (2010-2019)",
       y = "Values") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        strip.text = element_text(size = 10)) +
  scale_x_yearmon(format = "%b %Y", n = 12, limits = c(as.yearmon("jan 2010"), as.yearmon("dez 2019")))

#########################################################
## Traffic Accident Mortality (ICD-10 V) Southeast Region 
#########################################################

# Replace 'file_path.csv' with your CSV file path
file_path <- "cid_v_nacional.csv"

# Read CSV file starting from row 5 with delimiter ';', specifying decimal and grouping marks
data_acidente <- read_delim(file_path, delim = ";", skip = 4)
class(data_acidente)
head(data_acidente)
dim(data_acidente)

#missing data
#Use md.pattern() from mice to visualize the missing data pattern
md.pattern(data_acidente, rotate.names = TRUE)

#Use describe() from Hmisc
Hmisc::describe(data_acidente)

# Replace 'file_path.csv' with your CSV file path
file_path2 <- "proj_ipea_Sudeste.csv"

# Read CSV file starting from row 5 with delimiter ';', specifying decimal and grouping marks
new_df_sudeste <- read_delim(file_path2, delim = ";")
class(new_df_sudeste)
str(new_df_sudeste)

# Remove the column at a specific index (e.g., 1)
new_df_sudeste <- new_df_sudeste[, -1]

# Create a new column 'Data' in 'new_df_sul' and assign values from 'data_dcv'
new_df_sudeste$Data_new <- as.character(data_acidente$Data)

# Display the updated 'new_df_sudeste' dataset
head(new_df_sudeste)
str(new_df_sudeste$Data_new)

#Join Data sets
# Selecting only necessary columns from data_dcv
data_acidente_subset <- data_acidente[, c("Sudeste")]
head(data_acidente_subset)

# Combine 'data_dcv_subset' as a new column in 'new_df_sul' and rename it as 'new_data_dcv'
new_data_acidente <- cbind(new_df_sudeste, new_data_acidente = data_acidente_subset)

# Show the structure of the new dataset
str(new_data_acidente)

# Renaming columns in 'new_data_dcv'
colnames(new_data_acidente)[colnames(new_data_acidente) == "Data_new"] <- "Data"
# ... Repeat this pattern for other columns you want to rename

# Show the updated column names
colnames(new_data_acidente)

# Create a sequence of years from 2010 to 2019 and repeat each year 10 times
ano <- rep(2010:2019, each = 12)

# Add the 'year' column to 'merged_data'
new_data_acidente <- transform(new_data_acidente, ano = as.factor(ano))

# Show the updated dataset with the new 'year' column
head(new_data_acidente)
str(new_data_acidente)

# Save as CSV with semicolon delimiter
write.table(new_data_acidente, file = "data_acidente_clima.csv", sep = ";", row.names = FALSE)

#to test "load file"

####################################################
## Traffic Accident Mortality (ICD-10 V) and climate 
####################################################

# Replace 'file_path.csv' with your CSV file path
file_path3 <- "data_acidente_clima.csv"

# Read CSV file starting from row 5 with delimiter ';', specifying decimal and grouping marks
data_acidente_clima <- read_delim(file_path3, delim = ";")
str(data_acidente_clima) #ano (numerico)
class(data_acidente_clima)
head(data_acidente_clima)

# Several Scatterplot (Southeast)
# Select the desired columns
data_subset_plot2 <- data_acidente_clima[, c("Sudeste", "INSOLACAO_TOTAL", "PRECIPITACAO_TOTAL", "TEMPERATURA_MAXIMA", "TEMPERATURA_MEDIA")]

# Rename the columns for better visualization
colnames(data_subset_plot2) <- c("Cid-10 V", "insolacao", "pp", "t_max", "t_med")

# Create the scatter plot matrix using GGally
ggpairs(data_subset_plot2,
        title = "Scatter Plot Matrix for Accidents (ICD-10 V) and Climate Variables",
        upper = list(continuous = wrap("cor", size = 3)),  # Add correlation coefficients
        lower = list(continuous = "smooth"))  # Add smoothers

#Times Series from Montly Panel

# Convert the "Data" column to yearmon objects
data_acidente_clima$Data <- as.yearmon(paste0("01-", data_acidente_clima$Data), format = "%d-%b_%Y")  # Pad with "01-" for consistency
str(data_acidente_clima)
head(data_acidente_clima)

# Select the relevant variables
data_subset1 <- data_acidente_clima[, c("INSOLACAO_TOTAL", "PRECIPITACAO_TOTAL", 
                                  "TEMPERATURA_MAXIMA", "TEMPERATURA_MEDIA", "Data", "Sudeste")]

# Reshape the data into a long format using tidyr::pivot_longer
data_long1 <- pivot_longer(data_subset1, cols = -Data, names_to = "variable", values_to = "value")

# Now use the reshaped data for plotting
ggplot(data_long1, aes(x = Data, y = value)) +
  facet_wrap(~ variable, scales = "free_y", ncol = 2) +
  geom_line() +
  labs(title = "Figure 4. Temporal Analysis of Accident Mortality (ICD-10 V) and Climate 
                          in Southeast Brazil (2010-2019)",
       y = "Values") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        strip.text = element_text(size = 10)) +
  scale_x_yearmon(format = "%b %Y", n = 12, limits = c(as.yearmon("jan 2010"), as.yearmon("dez 2019")))

##############################################################
## Traffic Accident Inpatient Care (ICD-10 V) Southeast Region 
##############################################################

# Replace 'file_path.csv' with your CSV file path
file_path3 <- "cid_v_sih.csv"

# Read CSV file starting from row 5 with delimiter ';', specifying decimal and grouping marks
data_acidente_sih <- read_delim(file_path3, delim = ";", skip = 4)
class(data_acidente_sih)
head(data_acidente_sih)
dim(data_acidente_sih)

#missing data
#Use md.pattern() from mice to visualize the missing data pattern
md.pattern(data_acidente_sih, rotate.names = TRUE)

#Use describe() from Hmisc
Hmisc::describe(data_acidente_sih)

#Join Data sets
# Selecting only necessary columns from data_dcv
data_acidente_sih_subset <- data_acidente_sih[, c("Sudeste")]
head(data_acidente_sih_subset)

# Combine 'data_dcv_subset' as a new column in 'new_df_sul' and rename it as 'new_data_dcv'
new_data_acidente_sih <- cbind(new_df_sudeste, new_data_acidente_sih = data_acidente_sih_subset)

# Show the structure of the new dataset
str(new_data_acidente_sih)

# Renaming columns in 'new_data_dcv'
colnames(new_data_acidente_sih)[colnames(new_data_acidente_sih) == "Data_new"] <- "Data"
# ... Repeat this pattern for other columns you want to rename

# Show the updated column names
colnames(new_data_acidente_sih)

# Create a sequence of years from 2010 to 2019 and repeat each year 10 times
ano <- rep(2010:2019, each = 12)

# Add the 'year' column to 'merged_data'
new_data_acidente_sih <- transform(new_data_acidente_sih, ano = as.factor(ano))

# Show the updated dataset with the new 'year' column
head(new_data_acidente_sih)
str(new_data_acidente_sih)

# Save as CSV with semicolon delimiter
write.table(new_data_acidente_sih, file = "data_acidente_sih_clima.csv", sep = ";", row.names = FALSE)

#to test "load file"

#########################################################
## Traffic Accident Inpatient Care (ICD-10 V) and climate 
#########################################################

# Replace 'file_path.csv' with your CSV file path
file_path4 <- "data_acidente_sih_clima.csv"

# Read CSV file starting from row 5 with delimiter ';', specifying decimal and grouping marks
data_acidente_sih_clima <- read_delim(file_path4, delim = ";")
str(data_acidente_sih_clima) #ano (numerico)
class(data_acidente_sih_clima)
head(data_acidente_sih_clima)

# Several Scatterplot (Southeast) (Traffic Accident Inpatient Care and Climate)
# Select the desired columns
data_subset_plot3 <- data_acidente_sih_clima[, c("Sudeste", "INSOLACAO_TOTAL", "PRECIPITACAO_TOTAL", "TEMPERATURA_MAXIMA", "TEMPERATURA_MEDIA")]

# Rename the columns for better visualization
colnames(data_subset_plot3) <- c("Cid-10 V (SIH)", "insolacao", "pp", "t_max", "t_med")

# Create the scatter plot matrix using GGally
ggpairs(data_subset_plot2,
        title = "Scatter Plot Matrix Analysis of Accident, Climate & Inpatient Care 
                                in Southeast Brazil (2010-2019)",
        upper = list(continuous = wrap("cor", size = 3)),  # Add correlation coefficients
        lower = list(continuous = "smooth"))  # Add smoothers

#Times Series from Montly Panel

# Convert the "Data" column to yearmon objects
data_acidente_sih_clima$Data <- as.yearmon(paste0("01-", data_acidente_sih_clima$Data), format = "%d-%b_%Y")  # Pad with "01-" for consistency
str(data_acidente_sih_clima)
head(data_acidente_sih_clima)

# Select the relevant variables
data_subset2 <- data_acidente_sih_clima[, c("INSOLACAO_TOTAL", "PRECIPITACAO_TOTAL", 
                                        "TEMPERATURA_MAXIMA", "TEMPERATURA_MEDIA", "Data", "Sudeste")]

# Reshape the data into a long format using tidyr::pivot_longer
data_long2 <- pivot_longer(data_subset2, cols = -Data, names_to = "variable", values_to = "value")

# Now use the reshaped data for plotting
ggplot(data_long2, aes(x = Data, y = value)) +
  facet_wrap(~ variable, scales = "free_y", ncol = 2) +
  geom_line() +
  labs(title = "Figure 4. Temporal Analysis of Accident (ICD-10 V) and Climate 
                            in Southeast Brazil (2010-2019)",
       y = "Values") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        strip.text = element_text(size = 10)) +
  scale_x_yearmon(format = "%b %Y", n = 12, limits = c(as.yearmon("jan 2010"), as.yearmon("dez 2019")))

#################################################################################################
## Diseases of the musculoskeletal system and connective tissue Inpatient Care - Southeast Region 
#################################################################################################

# Replace 'file_path.csv' with your CSV file path
file_path_m <- "cid_m_sih.csv"

# Read CSV file starting from row 5 with delimiter ';', specifying decimal and grouping marks
data_cid_m <- read_delim(file_path_m, delim = ";", skip = 4)
class(data_cid_m)
head(data_cid_m)
dim(data_cid_m)

#missing data
#Use md.pattern() from mice to visualize the missing data pattern
md.pattern(data_cid_m, rotate.names = TRUE)

#Use describe() from Hmisc
Hmisc::describe(data_cid_m)

str(new_df_sudeste)

#Join Data sets
# Selecting only necessary columns from data_dcv
data_cid_m_subset <- data_cid_m[, c("Sudeste")]
head(data_cid_m_subset)

# Combine 'data_dcv_subset' as a new column in 'new_df_sul' and rename it as 'new_data_dcv'
new_data_cid_m <- cbind(new_df_sudeste, new_data_cid_m = data_cid_m_subset)

# Show the structure of the new dataset
str(new_data_cid_m)

# Renaming columns in 'new_data_dcv'
colnames(new_data_cid_m)[colnames(new_data_cid_m) == "Data_new"] <- "Data"
# ... Repeat this pattern for other columns you want to rename

# Show the updated column names
colnames(new_data_cid_m)

# Create a sequence of years from 2010 to 2019 and repeat each year 10 times
ano <- rep(2010:2019, each = 12)

# Add the 'year' column to 'merged_data'
new_data_cid_m <- transform(new_data_cid_m, ano = as.factor(ano))

# Show the updated dataset with the new 'year' column
head(new_data_cid_m)
str(new_data_cid_m)

# Save as CSV with semicolon delimiter
write.table(new_data_cid_m, file = "data_cid_m_clima.csv", sep = ";", row.names = FALSE)

#to test "load file"

##########################################################################
## musculoskeletal system and connective tissue Inpatient Care and climate 
##########################################################################

# Replace 'file_path.csv' with your CSV file path
file_path_cid_m <- "data_cid_m_clima.csv"

# Read CSV file starting from row 5 with delimiter ';', specifying decimal and grouping marks
data_cid_m_clima <- read_delim(file_path_cid_m, delim = ";")
str(data_cid_m_clima) #ano (numerico)
class(data_cid_m_clima)
head(data_cid_m_clima)

# Several Scatterplot (Southeast) (musculoskeletal system and connective tissue Inpatient Care and Climate)
# Select the desired columns
data_subset_plot_m <- data_cid_m_clima[, c("Sudeste", "INSOLACAO_TOTAL", "PRECIPITACAO_TOTAL", "TEMPERATURA_MAXIMA", "TEMPERATURA_MEDIA")]

# Rename the columns for better visualization
colnames(data_subset_plot_m) <- c("Cid-10 M (SIH)", "insolacao", "pp", "t_max", "t_med")

# Create the scatter plot matrix using GGally
ggpairs(data_subset_plot_m,
        title = "Scatter Plot Matrix Analysis of Diseases of the musculoskeletal system and connective tissue, 
                            Climate & Inpatient Care in Southeast Brazil (2010-2019)",
        upper = list(continuous = wrap("cor", size = 3)),  # Add correlation coefficients
        lower = list(continuous = "smooth"))  # Add smoothers

#Times Series from Montly Panel
# Convert the "Data" column to yearmon objects
data_cid_m_clima$Data <- as.yearmon(paste0("01-", data_cid_m_clima$Data), format = "%d-%b_%Y")  # Pad with "01-" for consistency
str(data_cid_m_clima)
head(data_cid_m_clima)

# Select the relevant variables
data_subset_cidm <- data_cid_m_clima[, c("INSOLACAO_TOTAL", "PRECIPITACAO_TOTAL", 
                                            "TEMPERATURA_MAXIMA", "TEMPERATURA_MEDIA", "Data", "Sudeste")]

# Reshape the data into a long format using tidyr::pivot_longer
data_long_m <- pivot_longer(data_subset_cidm, cols = -Data, names_to = "variable", values_to = "value")

# Now use the reshaped data for plotting
ggplot(data_long_m, aes(x = Data, y = value)) +
  facet_wrap(~ variable, scales = "free_y", ncol = 2) +
  geom_line() +
  labs(title = "Figure 6. Temporal Analysis of Diseases of the musculoskeletal system and connective tissue 
                                    and Climate in Southeast Brazil (2010-2019)",
       y = "Values") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        strip.text = element_text(size = 10)) +
  scale_x_yearmon(format = "%b %Y", n = 12, limits = c(as.yearmon("jan 2010"), as.yearmon("dez 2019")))

#####################################################
## Mental disorders Inpatient Care - Southeast Region 
#####################################################

# Replace 'file_path.csv' with your CSV file path
file_path_f <- "cid_F_sih.csv"

# Read CSV file starting from row 5 with delimiter ';', specifying decimal and grouping marks
data_cid_f <- read_delim(file_path_f, delim = ";", skip = 4)
class(data_cid_f)
head(data_cid_f)
dim(data_cid_f)

#missing data
#Use md.pattern() from mice to visualize the missing data pattern
md.pattern(data_cid_f, rotate.names = TRUE)

#Use describe() from Hmisc
Hmisc::describe(data_cid_f)

#Join Data sets
# Selecting only necessary columns from data_dcv
data_cid_f_subset <- data_cid_f[, c("Sudeste")]
head(data_cid_f_subset)

# Combine 'data_dcv_subset' as a new column in 'new_df_sul' and rename it as 'new_data_dcv'
new_data_cid_f <- cbind(new_df_sudeste, new_data_cid_f = data_cid_f_subset)

# Show the structure of the new dataset
str(new_data_cid_f)

# Renaming columns in 'new_data_dcv'
colnames(new_data_cid_f)[colnames(new_data_cid_f) == "Data_new"] <- "Data"
# ... Repeat this pattern for other columns you want to rename

# Show the updated column names
colnames(new_data_cid_f)

# Create a sequence of years from 2010 to 2019 and repeat each year 10 times
ano <- rep(2010:2019, each = 12)

# Add the 'year' column to 'merged_data'
new_data_cid_f <- transform(new_data_cid_f, ano = as.factor(ano))

# Show the updated dataset with the new 'year' column
head(new_data_cid_f)
str(new_data_cid_f)

# Save as CSV with semicolon delimiter
write.table(new_data_cid_f, file = "data_cid_f_clima.csv", sep = ";", row.names = FALSE)

#to test "load file"

##############################################
## Mental disorders Inpatient Care and climate 
##############################################

# Replace 'file_path.csv' with your CSV file path
file_path_cid_f <- "data_cid_f_clima.csv"

# Read CSV file starting from row 5 with delimiter ';', specifying decimal and grouping marks
data_cid_f_clima <- read_delim(file_path_cid_f, delim = ";")
str(data_cid_f_clima) #ano (numerico)
class(data_cid_f_clima)
head(data_cid_f_clima)

# Several Scatterplot (Southeast) (Mental disorders Inpatient Care and climate)
# Select the desired columns
data_subset_plot_f <- data_cid_f_clima[, c("Sudeste", "INSOLACAO_TOTAL", "PRECIPITACAO_TOTAL", "TEMPERATURA_MAXIMA", "TEMPERATURA_MEDIA")]

# Rename the columns for better visualization
colnames(data_subset_plot_f) <- c("Cid-10 F (SIH)", "insolacao", "pp", "t_max", "t_med")

# Create the scatter plot matrix using GGally
ggpairs(data_subset_plot_f,
        title = "Scatter Plot Matrix Analysis of Diseases of Mental Disorders, Climate & Inpatient Care 
                                    in Southeast Brazil (2010-2019)",
        upper = list(continuous = wrap("cor", size = 3)),  # Add correlation coefficients
        lower = list(continuous = "smooth"))  # Add smoothers

#Times Series from Montly Panel

# Convert the "Data" column to yearmon objects
data_cid_f_clima$Data <- as.yearmon(paste0("01-", data_cid_f_clima$Data), format = "%d-%b_%Y")  # Pad with "01-" for consistency
str(data_cid_f_clima)
head(data_cid_f_clima)

# Select the relevant variables
data_subset_cidf <- data_cid_f_clima[, c("INSOLACAO_TOTAL", "PRECIPITACAO_TOTAL", 
                                         "TEMPERATURA_MAXIMA", "TEMPERATURA_MEDIA", "Data", "Sudeste")]

# Reshape the data into a long format using tidyr::pivot_longer
data_long_f <- pivot_longer(data_subset_cidf, cols = -Data, names_to = "variable", values_to = "value")

# Now use the reshaped data for plotting
ggplot(data_long_f, aes(x = Data, y = value)) +
  facet_wrap(~ variable, scales = "free_y", ncol = 2) +
  geom_line() +
  labs(title = "Figure 8. Temporal Analysis of Diseases of Mental Disorders and Climate 
                              in Southeast Brazil (2010-2019)",
       y = "Values") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        strip.text = element_text(size = 10)) +
  scale_x_yearmon(format = "%b %Y", n = 12, limits = c(as.yearmon("jan 2010"), as.yearmon("dez 2019")))

#######################################################
## Neoplasms (tumors) Inpatient Care - Southeast Region 
#######################################################

# Replace 'file_path.csv' with your CSV file path
file_path_onco <- "cid_onco_sih.csv"

# Read CSV file starting from row 5 with delimiter ';', specifying decimal and grouping marks
data_cid_onco <- read_delim(file_path_onco, delim = ";", skip = 4)
class(data_cid_onco)
head(data_cid_onco)
dim(data_cid_onco)

#missing data
#Use md.pattern() from mice to visualize the missing data pattern
md.pattern(data_cid_onco, rotate.names = TRUE)

#Use describe() from Hmisc
Hmisc::describe(data_cid_onco)

#Join Data sets
# Selecting only necessary columns from data_dcv
data_cid_onco_subset <- data_cid_onco[, c("Sudeste")]
head(data_cid_onco_subset)

# Combine 'data_dcv_subset' as a new column in 'new_df_sul' and rename it as 'new_data_dcv'
new_data_cid_onco <- cbind(new_df_sudeste, new_data_cid_onco = data_cid_onco_subset)

# Show the structure of the new dataset
str(new_data_cid_onco)

# Renaming columns in 'new_data_dcv'
colnames(new_data_cid_onco)[colnames(new_data_cid_onco) == "Data_new"] <- "Data"
# ... Repeat this pattern for other columns you want to rename

# Show the updated column names
colnames(new_data_cid_onco)

# Create a sequence of years from 2010 to 2019 and repeat each year 10 times
ano <- rep(2010:2019, each = 12)

# Add the 'year' column to 'merged_data'
new_data_cid_onco <- transform(new_data_cid_onco, ano = as.factor(ano))

# Show the updated dataset with the new 'year' column
head(new_data_cid_onco)
str(new_data_cid_onco)

# Save as CSV with semicolon delimiter
write.table(new_data_cid_onco, file = "data_cid_onco_clima.csv", sep = ";", row.names = FALSE)

#to test "load file"

##############################################
## Mental disorders Inpatient Care and climate 
##############################################

# Replace 'file_path.csv' with your CSV file path
file_path_cid_onco <- "data_cid_onco_clima.csv"

# Read CSV file starting from row 5 with delimiter ';', specifying decimal and grouping marks
data_cid_onco_clima <- read_delim(file_path_cid_onco, delim = ";")
str(data_cid_onco_clima) #ano (numerico)
class(data_cid_onco_clima)
head(data_cid_onco_clima)

# Several Scatterplot (Sudeste) clima SIH cid Onco
# Select the desired columns
data_subset_plot_onco <- data_cid_onco_clima[, c("Sudeste", "INSOLACAO_TOTAL", "PRECIPITACAO_TOTAL", "TEMPERATURA_MAXIMA", "TEMPERATURA_MEDIA")]

# Rename the columns for better visualization
colnames(data_subset_plot_onco) <- c("Neoplasias (SIH)", "insolacao", "pp", "t_max", "t_med")

# Create the scatter plot matrix using GGally
ggpairs(data_subset_plot_onco,
        title = "Scatter Plot Matrix Analysis of Neoplasms (tumors), Climate & Inpatient Care 
                                    in Southeast Brazil (2010-2019)",
        upper = list(continuous = wrap("cor", size = 3)),  # Add correlation coefficients
        lower = list(continuous = "smooth"))  # Add smoothers

#Times Series from Montly Panel

# Convert the "Data" column to yearmon objects
data_cid_onco_clima$Data <- as.yearmon(paste0("01-", data_cid_onco_clima$Data), format = "%d-%b_%Y")  # Pad with "01-" for consistency
str(data_cid_onco_clima)
head(data_cid_onco_clima)

# Select the relevant variables
data_subset_cid_onco <- data_cid_onco_clima[, c("INSOLACAO_TOTAL", "PRECIPITACAO_TOTAL", 
                                         "TEMPERATURA_MAXIMA", "TEMPERATURA_MEDIA", "Data", "Sudeste")]

# Reshape the data into a long format using tidyr::pivot_longer
data_long_onco <- pivot_longer(data_subset_cid_onco, cols = -Data, names_to = "variable", values_to = "value")

# Now use the reshaped data for plotting
ggplot(data_long_onco, aes(x = Data, y = value)) +
  facet_wrap(~ variable, scales = "free_y", ncol = 2) +
  geom_line() +
  labs(title = "Figure 10. Temporal Analysis of Neoplasms (tumors) and Climate 
                              in Southeast Brazil (2010-2019)",
       y = "Values") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        strip.text = element_text(size = 10)) +
  scale_x_yearmon(format = "%b %Y", n = 12, limits = c(as.yearmon("jan 2010"), as.yearmon("dez 2019")))

#################################################
## DENGUE VIRUS (Notification) - Southeast Region 
#################################################

# Replace 'file_path.csv' with your CSV file path
file_path_dengue <- "dengue_nacional.csv"

# Read CSV file starting from row 5 with delimiter ';', specifying decimal and grouping marks
data_dengue <- read_delim(file_path_dengue, delim = ";", skip = 4)
class(data_dengue)
head(data_dengue)
dim(data_dengue)

#missing data
#Use md.pattern() from mice to visualize the missing data pattern
md.pattern(data_dengue, rotate.names = TRUE)

#Use describe() from Hmisc
Hmisc::describe(data_dengue)

# Replace 'file_path.csv' with your CSV file path
file_path2 <- "proj_ipea_Sudeste.csv"

# Read CSV file starting from row 5 with delimiter ';', specifying decimal and grouping marks
new_df_sudeste <- read_delim(file_path2, delim = ";")
str(new_df_sudeste)

# Remove the column at a specific index (e.g., 1)
new_df_sudeste <- new_df_sudeste[, -1]

# Create a new column 'Data' in 'new_df_sul' and assign values from 'data_dcv'
new_df_sudeste$Data_new <- as.character(data_dengue$Data)

# Display the updated 'new_df_sudeste' dataset
head(new_df_sudeste)
str(new_df_sudeste$Data_new)

#Join Data sets
# Selecting only necessary columns from data_dcv
data_dengue_subset <- data_dengue[, c("Sudeste")]
head(data_dengue_subset)

# Combine 'data_dcv_subset' as a new column in 'new_df_sul' and rename it as 'new_data_dcv'
new_data_dengue <- cbind(new_df_sudeste, new_data_dengue = data_dengue_subset)

# Show the structure of the new dataset
str(new_data_dengue)

# Renaming columns in 'new_data_dcv'
colnames(new_data_dengue)[colnames(new_data_dengue) == "Data_new"] <- "Data"
# ... Repeat this pattern for other columns you want to rename

# Show the updated column names
colnames(new_data_dengue)

# Create a sequence of years from 2010 to 2019 and repeat each year 10 times
ano <- rep(2010:2019, each = 12)

# Add the 'year' column to 'merged_data'
new_data_dengue <- transform(new_data_dengue, ano = as.factor(ano))

# Show the updated dataset with the new 'year' column
head(new_data_dengue)
str(new_data_dengue)

# Save as CSV with semicolon delimiter
write.table(new_data_dengue, file = "data_dengue_clima.csv", sep = ";", row.names = FALSE)

#to test "load file"

##########################################
## DENGUE VIRUS (Notification) and climate 
##########################################

# Replace 'file_path.csv' with your CSV file path
file_path_dengue2 <- "data_dengue_clima.csv"

# Read CSV file starting from row 5 with delimiter ';', specifying decimal and grouping marks
data_dengue_clima <- read_delim(file_path_dengue2, delim = ";")
str(data_dengue_clima) #ano (numerico)
class(data_dengue_clima)
head(data_dengue_clima)

# Several Scatterplot (Southeast) (DENGUE VIRUS and climate)
# Select the desired columns
data_subset_plot_dengue <- data_dengue_clima[, c("Sudeste", "INSOLACAO_TOTAL", "PRECIPITACAO_TOTAL", "TEMPERATURA_MAXIMA", "TEMPERATURA_MEDIA")]

# Rename the columns for better visualization
colnames(data_subset_plot_dengue) <- c("Dengue", "insolacao", "pp", "t_max", "t_med")

# Create the scatter plot matrix using GGally
ggpairs(data_subset_plot_dengue,
        title = "Scatter Plot Matrix Analysis of Dengue & Climate in Southeast Brazil (2010-2019)",
        upper = list(continuous = wrap("cor", size = 3)),  # Add correlation coefficients
        lower = list(continuous = "smooth"))  # Add smoothers

#Times Series from Montly Panel

# Convert the "Data" column to yearmon objects
data_dengue_clima$Data <- as.yearmon(paste0("01-", data_dengue_clima$Data), format = "%d-%b_%Y")  # Pad with "01-" for consistency
str(data_dengue_clima)
head(data_dengue_clima)

# Select the relevant variables
data_subset_ts_dengue <- data_dengue_clima[, c("INSOLACAO_TOTAL", "PRECIPITACAO_TOTAL", 
                                                "TEMPERATURA_MAXIMA", "TEMPERATURA_MEDIA", "Data", "Sudeste")]

# Reshape the data into a long format using tidyr::pivot_longer
data_long_dengue <- pivot_longer(data_subset_ts_dengue, cols = -Data, names_to = "variable", values_to = "value")

# Now use the reshaped data for plotting
ggplot(data_long_dengue, aes(x = Data, y = value)) +
  facet_wrap(~ variable, scales = "free_y", ncol = 2) +
  geom_line() +
  labs(title = "Figure 12. Temporal Analysis of Dengue and Climate in Southeast Brazil (2010-2019)",
       y = "Values") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        strip.text = element_text(size = 10)) +
  scale_x_yearmon(format = "%b %Y", n = 12, limits = c(as.yearmon("jan 2010"), as.yearmon("dez 2019")))

#############################################
## DENGUE VIRUS (Notification) - South Region 
#############################################

# Replace 'file_path.csv' with your CSV file path
file_path_dengue <- "dengue_nacional.csv"

# Read CSV file starting from row 5 with delimiter ';', specifying decimal and grouping marks
data_dengue <- read_delim(file_path_dengue, delim = ";", skip = 4)
class(data_dengue)
head(data_dengue)
dim(data_dengue)

#missing data
#Use md.pattern() from mice to visualize the missing data pattern
md.pattern(data_dengue, rotate.names = TRUE)

#Use describe() from Hmisc
Hmisc::describe(data_dengue)

# Replace 'file_path.csv' with your CSV file path
file_path_sul <- "proj_ipea_Sul.csv"

# Read CSV file starting from row 5 with delimiter ';', specifying decimal and grouping marks
new_df_sul <- read_delim(file_path_sul, delim = ";")
str(new_df_sul)

# Remove the column at a specific index (e.g., 1)
new_df_sul <- new_df_sul[, -1]

# Create a new column 'Data' in 'new_df_sul' and assign values from 'data_dcv'
new_df_sul$Data_new <- as.character(data_dengue$Data)

# Display the updated 'new_df_sudeste' dataset
head(new_df_sul)
str(new_df_sul$Data_new)

#Join Data sets
# Selecting only necessary columns from data_dcv
data_dengue_subset <- data_dengue[, c("Sul")]
head(data_dengue_subset)

# Combine 'data_dcv_subset' as a new column in 'new_df_sul' and rename it as 'new_data_dcv'
new_data_dengue <- cbind(new_df_sul, new_data_dengue = data_dengue_subset)

# Show the structure of the new dataset
str(new_data_dengue)

# Renaming columns in 'new_data_dcv'
colnames(new_data_dengue)[colnames(new_data_dengue) == "Data_new"] <- "Data"
# ... Repeat this pattern for other columns you want to rename

# Show the updated column names
colnames(new_data_dengue)

# Create a sequence of years from 2010 to 2019 and repeat each year 10 times
ano <- rep(2010:2019, each = 12)

# Add the 'year' column to 'merged_data'
new_data_dengue <- transform(new_data_dengue, ano = as.factor(ano))

# Show the updated dataset with the new 'year' column
head(new_data_dengue)
str(new_data_dengue)

# Save as CSV with semicolon delimiter
write.table(new_data_dengue, file = "data_dengue_clima_sul.csv", sep = ";", row.names = FALSE)

#to test "load file"

##########################################
## DENGUE VIRUS (Notification) and climate 
##########################################

# Replace 'file_path.csv' with your CSV file path
file_path_dengue_sul <- "data_dengue_clima_sul.csv"

# Read CSV file starting from row 5 with delimiter ';', specifying decimal and grouping marks
data_dengue_clima <- read_delim(file_path_dengue_sul, delim = ";")
str(data_dengue_clima) #ano (numerico)
class(data_dengue_clima)
head(data_dengue_clima)

# Several Scatterplot (South) (DENGUE VIRUS and climate)
# Select the desired columns
data_subset_plot_dengue <- data_dengue_clima[, c("Sul", "INSOLACAO_TOTAL", "PRECIPITACAO_TOTAL", "TEMPERATURA_MAXIMA", "TEMPERATURA_MEDIA")]

# Rename the columns for better visualization
colnames(data_subset_plot_dengue) <- c("Dengue", "insolacao", "pp", "t_max", "t_med")

# Create the scatter plot matrix using GGally
ggpairs(data_subset_plot_dengue,
        title = "Scatter Plot Matrix Analysis of Dengue & Climate in South Brazil (2010-2019)",
        upper = list(continuous = wrap("cor", size = 3)),  # Add correlation coefficients
        lower = list(continuous = "smooth"))  # Add smoothers

#Times Series from Montly Panel

# Convert the "Data" column to yearmon objects
data_dengue_clima$Data <- as.yearmon(paste0("01-", data_dengue_clima$Data), format = "%d-%b_%Y")  # Pad with "01-" for consistency
str(data_dengue_clima)
head(data_dengue_clima)

# Select the relevant variables
data_subset_ts_dengue <- data_dengue_clima[, c("INSOLACAO_TOTAL", "PRECIPITACAO_TOTAL", 
                                               "TEMPERATURA_MAXIMA", "TEMPERATURA_MEDIA", "Data", "Sul")]

# Reshape the data into a long format using tidyr::pivot_longer
data_long_dengue <- pivot_longer(data_subset_ts_dengue, cols = -Data, names_to = "variable", values_to = "value")

# Now use the reshaped data for plotting
ggplot(data_long_dengue, aes(x = Data, y = value)) +
  facet_wrap(~ variable, scales = "free_y", ncol = 2) +
  geom_line() +
  labs(title = "Figure 12. Temporal Analysis of Dengue and Climate in South Brazil (2010-2019)",
       y = "Values") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        strip.text = element_text(size = 10)) +
  scale_x_yearmon(format = "%b %Y", n = 12, limits = c(as.yearmon("jan 2010"), as.yearmon("dez 2019")))

#################################################
## DENGUE VIRUS (Notification) - Northeast Region 
#################################################

# Replace 'file_path.csv' with your CSV file path
file_path_dengue <- "dengue_nacional.csv"

# Read CSV file starting from row 5 with delimiter ';', specifying decimal and grouping marks
data_dengue <- read_delim(file_path_dengue, delim = ";", skip = 4)
class(data_dengue)
head(data_dengue)
dim(data_dengue)

#missing data
#Use md.pattern() from mice to visualize the missing data pattern
md.pattern(data_dengue, rotate.names = TRUE)

#Use describe() from Hmisc
Hmisc::describe(data_dengue)

# Replace 'file_path.csv' with your CSV file path
file_path_nordeste <- "proj_ipea_Nordeste.csv"

# Read CSV file starting from row 5 with delimiter ';', specifying decimal and grouping marks
new_df_nordeste <- read_delim(file_path_nordeste, delim = ";")
str(new_df_nordeste)

# Remove the column at a specific index (e.g., 1)
new_df_nordeste <- new_df_nordeste[, -1]

# Create a new column 'Data' in 'new_df_sul' and assign values from 'data_dcv'
new_df_nordeste$Data_new <- as.character(data_dengue$Data)

# Display the updated 'new_df_sudeste' dataset
head(new_df_nordeste)
str(new_df_nordeste$Data_new)

#Join Data sets
# Selecting only necessary columns from data_dcv
data_dengue_subset <- data_dengue[, c("Nordeste")]
head(data_dengue_subset)

# Combine 'data_dcv_subset' as a new column in 'new_df_sul' and rename it as 'new_data_dcv'
new_data_dengue <- cbind(new_df_nordeste, new_data_dengue = data_dengue_subset)

# Show the structure of the new dataset
str(new_data_dengue)

# Renaming columns in 'new_data_dcv'
colnames(new_data_dengue)[colnames(new_data_dengue) == "Data_new"] <- "Data"
# ... Repeat this pattern for other columns you want to rename

# Show the updated column names
colnames(new_data_dengue)

# Create a sequence of years from 2010 to 2019 and repeat each year 10 times
ano <- rep(2010:2019, each = 12)

# Add the 'year' column to 'merged_data'
new_data_dengue <- transform(new_data_dengue, ano = as.factor(ano))

# Show the updated dataset with the new 'year' column
head(new_data_dengue)
str(new_data_dengue)

# Save as CSV with semicolon delimiter
write.table(new_data_dengue, file = "data_dengue_clima_nord.csv", sep = ";", row.names = FALSE)

#to test "load file"

##########################################
## DENGUE VIRUS (Notification) and climate 
##########################################

# Replace 'file_path.csv' with your CSV file path
file_path_dengue_nordeste <- "data_dengue_clima_nord.csv"

# Read CSV file starting from row 5 with delimiter ';', specifying decimal and grouping marks
data_dengue_clima <- read_delim(file_path_dengue_nordeste, delim = ";")
str(data_dengue_clima) #ano (numerico)
class(data_dengue_clima)
head(data_dengue_clima)

# Several Scatterplot (Northeast) (DENGUE VIRUS and climate)
# Select the desired columns
data_subset_plot_dengue <- data_dengue_clima[, c("Nordeste", "INSOLACAO_TOTAL", "PRECIPITACAO_TOTAL", "TEMPERATURA_MAXIMA", "TEMPERATURA_MEDIA")]

# Rename the columns for better visualization
colnames(data_subset_plot_dengue) <- c("Dengue", "insolacao", "pp", "t_max", "t_med")

# Create the scatter plot matrix using GGally
ggpairs(data_subset_plot_dengue,
        title = "Scatter Plot Matrix Analysis of Dengue & Climate in Northeast Brazil (2010-2019)",
        upper = list(continuous = wrap("cor", size = 3)),  # Add correlation coefficients
        lower = list(continuous = "smooth"))  # Add smoothers

#Times Series from Montly Panel

# Convert the "Data" column to yearmon objects
data_dengue_clima$Data <- as.yearmon(paste0("01-", data_dengue_clima$Data), format = "%d-%b_%Y")  # Pad with "01-" for consistency
str(data_dengue_clima)
head(data_dengue_clima)

# Select the relevant variables
data_subset_ts_dengue <- data_dengue_clima[, c("INSOLACAO_TOTAL", "PRECIPITACAO_TOTAL", 
                                               "TEMPERATURA_MAXIMA", "TEMPERATURA_MEDIA", "Data", "Nordeste")]

# Reshape the data into a long format using tidyr::pivot_longer
library(tidyr)
data_long_dengue <- pivot_longer(data_subset_ts_dengue, cols = -Data, names_to = "variable", values_to = "value")

# Now use the reshaped data for plotting
ggplot(data_long_dengue, aes(x = Data, y = value)) +
  facet_wrap(~ variable, scales = "free_y", ncol = 2) +
  geom_line() +
  labs(title = "Figure 12. Temporal Analysis of Dengue and Climate in Northeast Brazil (2010-2019)",
       y = "Values") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        strip.text = element_text(size = 10)) +
  scale_x_yearmon(format = "%b %Y", n = 12, limits = c(as.yearmon("jan 2010"), as.yearmon("dez 2019")))

#############################################
## DENGUE VIRUS (Notification) - North Region 
#############################################

# Replace 'file_path.csv' with your CSV file path
file_path_dengue <- "dengue_nacional.csv"

# Read CSV file starting from row 5 with delimiter ';', specifying decimal and grouping marks
data_dengue <- read_delim(file_path_dengue, delim = ";", skip = 4)
class(data_dengue)
head(data_dengue)
dim(data_dengue)

#missing data
#Use md.pattern() from mice to visualize the missing data pattern
md.pattern(data_dengue, rotate.names = TRUE)

#Use describe() from Hmisc
Hmisc::describe(data_dengue)

# Replace 'file_path.csv' with your CSV file path
file_path_norte <- "proj_ipea_Norte.csv"

# Read CSV file starting from row 5 with delimiter ';', specifying decimal and grouping marks
new_df_norte <- read_delim(file_path_norte, delim = ";")
str(new_df_norte)

# Remove the column at a specific index (e.g., 1)
new_df_norte <- new_df_norte[, -1]

# Create a new column 'Data' in 'new_df_sul' and assign values from 'data_dcv'
new_df_norte$Data_new <- as.character(data_dengue$Data)

# Display the updated 'new_df_sudeste' dataset
head(new_df_norte)
str(new_df_norte$Data_new)

#Join Data sets
# Selecting only necessary columns from data_dcv
data_dengue_subset <- data_dengue[, c("Norte")]
head(data_dengue_subset)

# Combine 'data_dcv_subset' as a new column in 'new_df_sul' and rename it as 'new_data_dcv'
new_data_dengue <- cbind(new_df_norte, new_data_dengue = data_dengue_subset)

# Show the structure of the new dataset
str(new_data_dengue)

# Renaming columns in 'new_data_dcv'
colnames(new_data_dengue)[colnames(new_data_dengue) == "Data_new"] <- "Data"
# ... Repeat this pattern for other columns you want to rename

# Show the updated column names
colnames(new_data_dengue)

# Create a sequence of years from 2010 to 2019 and repeat each year 10 times
ano <- rep(2010:2019, each = 12)

# Add the 'year' column to 'merged_data'
new_data_dengue <- transform(new_data_dengue, ano = as.factor(ano))

# Show the updated dataset with the new 'year' column
head(new_data_dengue)
str(new_data_dengue)

# Save as CSV with semicolon delimiter
write.table(new_data_dengue, file = "data_dengue_clima_norte.csv", sep = ";", row.names = FALSE)

#to test "load file"

##########################################
## DENGUE VIRUS (Notification) and climate 
##########################################

# Replace 'file_path.csv' with your CSV file path
file_path_dengue_norte <- "data_dengue_clima_norte.csv"

# Read CSV file starting from row 5 with delimiter ';', specifying decimal and grouping marks
data_dengue_clima <- read_delim(file_path_dengue_norte, delim = ";")
str(data_dengue_clima) #ano (numerico)
class(data_dengue_clima)
head(data_dengue_clima)

# Several Scatterplot (North) (DENGUE VIRUS and climate)
# Select the desired columns
data_subset_plot_dengue <- data_dengue_clima[, c("Norte", "INSOLACAO_TOTAL", "PRECIPITACAO_TOTAL", "TEMPERATURA_MAXIMA", "TEMPERATURA_MEDIA")]

# Rename the columns for better visualization
colnames(data_subset_plot_dengue) <- c("Dengue", "insolacao", "pp", "t_max", "t_med")

# Create the scatter plot matrix using GGally
ggpairs(data_subset_plot_dengue,
        title = "Scatter Plot Matrix Analysis of Dengue & Climate in North Brazil (2010-2019)",
        upper = list(continuous = wrap("cor", size = 3)),  # Add correlation coefficients
        lower = list(continuous = "smooth"))  # Add smoothers

#Times Series from Montly Panel

# Convert the "Data" column to yearmon objects
data_dengue_clima$Data <- as.yearmon(paste0("01-", data_dengue_clima$Data), format = "%d-%b_%Y")  # Pad with "01-" for consistency
str(data_dengue_clima)
head(data_dengue_clima)

# Select the relevant variables
data_subset_ts_dengue <- data_dengue_clima[, c("INSOLACAO_TOTAL", "PRECIPITACAO_TOTAL", 
                                               "TEMPERATURA_MAXIMA", "TEMPERATURA_MEDIA", "Data", "Norte")]

# Reshape the data into a long format using tidyr::pivot_longer
data_long_dengue <- pivot_longer(data_subset_ts_dengue, cols = -Data, names_to = "variable", values_to = "value")

# Now use the reshaped data for plotting
ggplot(data_long_dengue, aes(x = Data, y = value)) +
  facet_wrap(~ variable, scales = "free_y", ncol = 2) +
  geom_line() +
  labs(title = "Figure 12. Temporal Analysis of Dengue and Climate in North Brazil (2010-2019)",
       y = "Values") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        strip.text = element_text(size = 10)) +
  scale_x_yearmon(format = "%b %Y", n = 12, limits = c(as.yearmon("jan 2010"), as.yearmon("dez 2019")))

###############################################
## DENGUE VIRUS (Notification) - Midwest Region 
###############################################

# Replace 'file_path.csv' with your CSV file path
file_path_dengue <- "dengue_nacional.csv"

# Read CSV file starting from row 5 with delimiter ';', specifying decimal and grouping marks
data_dengue <- read_delim(file_path_dengue, delim = ";", skip = 4)
class(data_dengue)
head(data_dengue)
dim(data_dengue)

#missing data
#Use md.pattern() from mice to visualize the missing data pattern
md.pattern(data_dengue, rotate.names = TRUE)

#Use describe() from Hmisc
Hmisc::describe(data_dengue)

# Replace 'file_path.csv' with your CSV file path
file_path_CO <- "proj_ipea_CO.csv"

# Read CSV file starting from row 5 with delimiter ';', specifying decimal and grouping marks
new_df_centro_oeste <- read_delim(file_path_CO, delim = ";")
str(new_df_centro_oeste)

# Remove the column at a specific index (e.g., 1)
new_df_centro_oeste <- new_df_centro_oeste[, -1]

# Create a new column 'Data' in 'new_df_sul' and assign values from 'data_dcv'
new_df_centro_oeste$Data_new <- as.character(data_dengue$Data)

# Display the updated 'new_df_sudeste' dataset
head(new_df_centro_oeste)
str(new_df_centro_oeste$Data_new)

#Join Data sets
# Selecting only necessary columns from data_dcv
data_dengue_subset <- data_dengue[, c("Centro_Oeste")]
head(data_dengue_subset)

# Combine 'data_dcv_subset' as a new column in 'new_df_sul' and rename it as 'new_data_dcv'
new_data_dengue <- cbind(new_df_centro_oeste, new_data_dengue = data_dengue_subset)

# Show the structure of the new dataset
str(new_data_dengue)

# Renaming columns in 'new_data_dcv'
colnames(new_data_dengue)[colnames(new_data_dengue) == "Data_new"] <- "Data"
# ... Repeat this pattern for other columns you want to rename

# Show the updated column names
colnames(new_data_dengue)

# Create a sequence of years from 2010 to 2019 and repeat each year 10 times
ano <- rep(2010:2019, each = 12)

# Add the 'year' column to 'merged_data'
new_data_dengue <- transform(new_data_dengue, ano = as.factor(ano))

# Show the updated dataset with the new 'year' column
head(new_data_dengue)
str(new_data_dengue)

# Save as CSV with semicolon delimiter
write.table(new_data_dengue, file = "data_dengue_clima_Centro.csv", sep = ";", row.names = FALSE)

#to test "load file"

##########################################
## DENGUE VIRUS (Notification) and climate 
##########################################

# Replace 'file_path.csv' with your CSV file path
file_path_dengue_CO <- "data_dengue_clima_Centro.csv"

# Read CSV file starting from row 5 with delimiter ';', specifying decimal and grouping marks
data_dengue_clima <- read_delim(file_path_dengue_CO, delim = ";")
str(data_dengue_clima)
class(data_dengue_clima)
head(data_dengue_clima)

# Several Scatterplot (Midwest) (DENGUE VIRUS and climate)
# Select the desired columns
data_subset_plot_dengue <- data_dengue_clima[, c("Centro_Oeste", "INSOLACAO_TOTAL", "PRECIPITACAO_TOTAL", "TEMPERATURA_MAXIMA", "TEMPERATURA_MEDIA")]

# Rename the columns for better visualization
colnames(data_subset_plot_dengue) <- c("Dengue", "insolacao", "pp", "t_max", "t_med")

# Create the scatter plot matrix using GGally
ggpairs(data_subset_plot_dengue,
        title = "Scatter Plot Matrix Analysis of Dengue & Climate in Central-West Brazil (2010-2019)",
        upper = list(continuous = wrap("cor", size = 3)),  # Add correlation coefficients
        lower = list(continuous = "smooth"))  # Add smoothers

#Times Series from Montly Panel

# Convert the "Data" column to yearmon objects
data_dengue_clima$Data <- as.yearmon(paste0("01-", data_dengue_clima$Data), format = "%d-%b_%Y")  # Pad with "01-" for consistency
str(data_dengue_clima)
head(data_dengue_clima)

# Select the relevant variables
data_subset_ts_dengue <- data_dengue_clima[, c("INSOLACAO_TOTAL", "PRECIPITACAO_TOTAL", 
                                               "TEMPERATURA_MAXIMA", "TEMPERATURA_MEDIA", "Data", "Centro_Oeste")]

# Reshape the data into a long format using tidyr::pivot_longer
data_long_dengue <- pivot_longer(data_subset_ts_dengue, cols = -Data, names_to = "variable", values_to = "value")

# Now use the reshaped data for plotting
ggplot(data_long_dengue, aes(x = Data, y = value)) +
  facet_wrap(~ variable, scales = "free_y", ncol = 2) +
  geom_line() +
  labs(title = "Figure 14. Temporal Analysis of Dengue and Climate in Central-West Brazil (2010-2019)",
       y = "Values") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        strip.text = element_text(size = 10)) +
  scale_x_yearmon(format = "%b %Y", n = 12, limits = c(as.yearmon("jan 2010"), as.yearmon("dez 2019")))

######################################################
## Diseases of the respiratory system - Midwest Region 
######################################################

# Replace 'file_path.csv' with your CSV file path
file_path_resp <- "cid_resp_sih.csv"

# Read CSV file starting from row 5 with delimiter ';', specifying decimal and grouping marks
data_resp <- read_delim(file_path_resp, delim = ";", skip = 4)
class(data_resp)
head(data_resp)
dim(data_resp)

#missing data
#Use md.pattern() from mice to visualize the missing data pattern
md.pattern(data_resp, rotate.names = TRUE)

#Use describe() from Hmisc
Hmisc::describe(data_resp)

# Replace 'file_path.csv' with your CSV file path
file_path_CO <- "proj_ipea_CO.csv"

# Read CSV file starting from row 5 with delimiter ';', specifying decimal and grouping marks
new_df_centro_oeste <- read_delim(file_path_CO, delim = ";")
str(new_df_centro_oeste)

# Remove the column at a specific index (e.g., 1)
new_df_centro_oeste <- new_df_centro_oeste[, -1]

# Create a new column 'Data' in 'new_df_sul' and assign values from 'data_dcv'
new_df_centro_oeste$Data_new <- as.character(data_resp$Data)

# Display the updated 'new_df_sudeste' dataset
head(new_df_centro_oeste)
str(new_df_centro_oeste$Data_new)

#Join Data sets
# Selecting only necessary columns from data_dcv
data_resp_subset <- data_resp[, c("Centro_Oeste")]
head(data_resp_subset)

# Combine 'data_dcv_subset' as a new column in 'new_df_sul' and rename it as 'new_data_dcv'
new_data_resp <- cbind(new_df_centro_oeste, new_data_resp = data_resp_subset)

# Show the structure of the new dataset
str(new_data_resp)

# Renaming columns in 'new_data_dcv'
colnames(new_data_resp)[colnames(new_data_resp) == "Data_new"] <- "Data"
# ... Repeat this pattern for other columns you want to rename

# Show the updated column names
colnames(new_data_resp)

# Create a sequence of years from 2010 to 2019 and repeat each year 10 times
ano <- rep(2010:2019, each = 12)

# Add the 'year' column to 'merged_data'
new_data_resp <- transform(new_data_resp, ano = as.factor(ano))

# Show the updated dataset with the new 'year' column
head(new_data_resp)
str(new_data_resp)

# Save as CSV with semicolon delimiter
write.table(new_data_resp, file = "data_resp_clima_Centro.csv", sep = ";", row.names = FALSE)

#to test "load file"

#################################################
## Diseases of the respiratory system and climate 
#################################################

# Replace 'file_path.csv' with your CSV file path
file_path_dengue_resp <- "data_resp_clima_Centro.csv"

# Read CSV file starting from row 5 with delimiter ';', specifying decimal and grouping marks
data_resp_clima <- read_delim(file_path_dengue_resp, delim = ";")
str(data_resp_clima) #ano (numerico)
class(data_resp_clima)
head(data_resp_clima)

# Several Scatterplot (Midwest) (Diseases of the respiratory system and climate) 
# Select the desired columns
data_subset_plot_resp <- data_resp_clima[, c("Centro_Oeste", "INSOLACAO_TOTAL", "PRECIPITACAO_TOTAL", "TEMPERATURA_MAXIMA", "TEMPERATURA_MEDIA")]

# Rename the columns for better visualization
colnames(data_subset_plot_resp) <- c("Dça_Respiratoria", "insolacao", "pp", "t_max", "t_med")

# Create the scatter plot matrix using GGally
ggpairs(data_subset_plot_resp,
        title = "Scatter Plot Matrix Analysis of Respiratory disorders, Climate &
                      Inpatient Care in Central-West Brazil (2010-2019)",
        upper = list(continuous = wrap("cor", size = 3)),  # Add correlation coefficients
        lower = list(continuous = "smooth"))  # Add smoothers

#Times Series from Montly Panel

# Convert the "Data" column to yearmon objects
data_resp_clima$Data <- as.yearmon(paste0("01-", data_resp_clima$Data), format = "%d-%b_%Y")  # Pad with "01-" for consistency
str(data_resp_clima)
head(data_resp_clima)

# Select the relevant variables
data_subset_ts_resp <- data_resp_clima[, c("INSOLACAO_TOTAL", "PRECIPITACAO_TOTAL", 
                                               "TEMPERATURA_MAXIMA", "TEMPERATURA_MEDIA", "Data", "Centro_Oeste")]

# Reshape the data into a long format using tidyr::pivot_longer
data_long_resp <- pivot_longer(data_subset_ts_resp, cols = -Data, names_to = "variable", values_to = "value")

# Now use the reshaped data for plotting (Midwest)
ggplot(data_long_resp, aes(x = Data, y = value)) +
  facet_wrap(~ variable, scales = "free_y", ncol = 2) +
  geom_line() +
  labs(title = "Figure 14. Temporal Analysis of Respiratory disorders and Climate 
                                 in Central-West Brazil (2010-2019)",
       y = "Values") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        strip.text = element_text(size = 10)) +
  scale_x_yearmon(format = "%b %Y", n = 12, limits = c(as.yearmon("jan 2010"), as.yearmon("dez 2019")))

########################################################
## Diseases of the respiratory system - Northeast Region 
########################################################

# Replace 'file_path.csv' with your CSV file path
file_path_resp <- "cid_resp_sih.csv"

# Read CSV file starting from row 5 with delimiter ';', specifying decimal and grouping marks
data_resp <- read_delim(file_path_resp, delim = ";", skip = 4)
class(data_resp)
head(data_resp)
dim(data_resp)

#missing data
#Use md.pattern() from mice to visualize the missing data pattern
md.pattern(data_resp, rotate.names = TRUE)

#Use describe() from Hmisc
Hmisc::describe(data_resp)

# Replace 'file_path.csv' with your CSV file path
file_path_nord <- "proj_ipea_Nordeste.csv"

# Read CSV file starting from row 5 with delimiter ';', specifying decimal and grouping marks
new_df_nordeste <- read_delim(file_path_nord, delim = ";")
str(new_df_nordeste)

# Remove the column at a specific index (e.g., 1)
new_df_nordeste <- new_df_nordeste[, -1]

# Create a new column 'Data' in 'new_df_sul' and assign values from 'data_dcv'
new_df_nordeste$Data_new <- as.character(data_resp$Data)

# Display the updated 'new_df_sudeste' dataset
head(new_df_nordeste)
str(new_df_nordeste$Data_new)

#Join Data sets
# Selecting only necessary columns from data_dcv
data_resp_subset <- data_resp[, c("Nordeste")]
head(data_resp_subset)

# Combine 'data_dcv_subset' as a new column in 'new_df_sul' and rename it as 'new_data_dcv'
new_data_resp <- cbind(new_df_nordeste, new_data_resp = data_resp_subset)

# Show the structure of the new dataset
str(new_data_resp)

# Renaming columns in 'new_data_dcv'
colnames(new_data_resp)[colnames(new_data_resp) == "Data_new"] <- "Data"
# ... Repeat this pattern for other columns you want to rename

# Show the updated column names
colnames(new_data_resp)

# Create a sequence of years from 2010 to 2019 and repeat each year 10 times
ano <- rep(2010:2019, each = 12)

# Add the 'year' column to 'merged_data'
new_data_resp <- transform(new_data_resp, ano = as.factor(ano))

# Show the updated dataset with the new 'year' column
head(new_data_resp)
str(new_data_resp)

# Save as CSV with semicolon delimiter
write.table(new_data_resp, file = "data_resp_clima_Nord.csv", sep = ";", row.names = FALSE)

#to test "load file"

#################################################
## Diseases of the respiratory system and climate 
#################################################

# Replace 'file_path.csv' with your CSV file path
file_path_resp <- "data_resp_clima_Nord.csv"

# Read CSV file starting from row 5 with delimiter ';', specifying decimal and grouping marks
data_resp_clima <- read_delim(file_path_resp, delim = ";")
str(data_resp_clima) #ano (numerico)
class(data_resp_clima)
head(data_resp_clima)

# Several Scatterplot (Nordeste) (Diseases of the respiratory system and climate)
# Select the desired columns
data_subset_plot_resp <- data_resp_clima[, c("Nordeste", "INSOLACAO_TOTAL", "PRECIPITACAO_TOTAL", "TEMPERATURA_MAXIMA", "TEMPERATURA_MEDIA")]

# Rename the columns for better visualization
colnames(data_subset_plot_resp) <- c("Dça_Respiratoria", "insolacao", "pp", "t_max", "t_med")

# Create the scatter plot matrix using GGally
ggpairs(data_subset_plot_resp,
        title = "Scatter Plot Matrix Analysis of Respiratory disorders, Climate &
                      Inpatient Care in Northeast Brazil (2010-2019)",
        upper = list(continuous = wrap("cor", size = 3)),  # Add correlation coefficients
        lower = list(continuous = "smooth"))  # Add smoothers

#Times Series from Montly Panel

# Convert the "Data" column to yearmon objects
data_resp_clima$Data <- as.yearmon(paste0("01-", data_resp_clima$Data), format = "%d-%b_%Y")  # Pad with "01-" for consistency
str(data_resp_clima)
head(data_resp_clima)

# Select the relevant variables
data_subset_ts_resp <- data_resp_clima[, c("INSOLACAO_TOTAL", "PRECIPITACAO_TOTAL", 
                                           "TEMPERATURA_MAXIMA", "TEMPERATURA_MEDIA", "Data", "Nordeste")]

# Reshape the data into a long format using tidyr::pivot_longer
data_long_resp <- pivot_longer(data_subset_ts_resp, cols = -Data, names_to = "variable", values_to = "value")

# Now use the reshaped data for plotting (Northeast)
ggplot(data_long_resp, aes(x = Data, y = value)) +
  facet_wrap(~ variable, scales = "free_y", ncol = 2) +
  geom_line() +
  labs(title = "Figure 16. Temporal Analysis of Respiratory disorders and Climate 
                                 in Northeast Brazil (2010-2019)",
       y = "Values") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        strip.text = element_text(size = 10)) +
  scale_x_yearmon(format = "%b %Y", n = 12, limits = c(as.yearmon("jan 2010"), as.yearmon("dez 2019")))

####################################################
## Diseases of the respiratory system - North Region 
####################################################

# Replace 'file_path.csv' with your CSV file path
file_path_resp <- "cid_resp_sih.csv"

# Read CSV file starting from row 5 with delimiter ';', specifying decimal and grouping marks
data_resp <- read_delim(file_path_resp, delim = ";", skip = 4)
class(data_resp)
head(data_resp)
dim(data_resp)

#missing data
#Use md.pattern() from mice to visualize the missing data pattern
md.pattern(data_resp, rotate.names = TRUE)

#Use describe() from Hmisc
Hmisc::describe(data_resp)

# Replace 'file_path.csv' with your CSV file path
file_path_norte <- "proj_ipea_Norte.csv"

# Read CSV file starting from row 5 with delimiter ';', specifying decimal and grouping marks
new_df_norte <- read_delim(file_path_norte, delim = ";")
str(new_df_norte)

# Remove the column at a specific index (e.g., 1)
new_df_norte <- new_df_norte[, -1]

# Create a new column 'Data' in 'new_df_sul' and assign values from 'data_dcv'
new_df_norte$Data_new <- as.character(data_resp$Data)

# Display the updated 'new_df_sudeste' dataset
head(new_df_norte)
str(new_df_norte$Data_new)

#Join Data sets
# Selecting only necessary columns from data_dcv
data_resp_subset <- data_resp[, c("Norte")]
head(data_resp_subset)

# Combine 'data_dcv_subset' as a new column in 'new_df_sul' and rename it as 'new_data_dcv'
new_data_resp <- cbind(new_df_norte, new_data_resp = data_resp_subset)

# Show the structure of the new dataset
str(new_data_resp)

# Renaming columns in 'new_data_dcv'
colnames(new_data_resp)[colnames(new_data_resp) == "Data_new"] <- "Data"
# ... Repeat this pattern for other columns you want to rename

# Show the updated column names
colnames(new_data_resp)

# Create a sequence of years from 2010 to 2019 and repeat each year 10 times
ano <- rep(2010:2019, each = 12)

# Add the 'year' column to 'merged_data'
new_data_resp <- transform(new_data_resp, ano = as.factor(ano))

# Show the updated dataset with the new 'year' column
head(new_data_resp)
str(new_data_resp)

# Save as CSV with semicolon delimiter
write.table(new_data_resp, file = "data_resp_clima_Norte.csv", sep = ";", row.names = FALSE)

#to test "load file"

#################################################
## Diseases of the respiratory system and climate 
#################################################

# Replace 'file_path.csv' with your CSV file path
file_path_resp <- "data_resp_clima_Norte.csv"

# Read CSV file starting from row 5 with delimiter ';', specifying decimal and grouping marks
data_resp_clima <- read_delim(file_path_resp, delim = ";")
str(data_resp_clima) #ano (numerico)
class(data_resp_clima)
head(data_resp_clima)

# Several Scatterplot (North) (Diseases of the respiratory system and climate)
# Select the desired columns
data_subset_plot_resp <- data_resp_clima[, c("Norte", "INSOLACAO_TOTAL", "PRECIPITACAO_TOTAL", "TEMPERATURA_MAXIMA", "TEMPERATURA_MEDIA")]

# Rename the columns for better visualization
colnames(data_subset_plot_resp) <- c("Dça_Respiratoria", "insolacao", "pp", "t_max", "t_med")

# Create the scatter plot matrix using GGally
ggpairs(data_subset_plot_resp,
        title = "Scatter Plot Matrix Analysis of Respiratory disorders, Climate &
                      Inpatient Care in North Brazil (2010-2019)",
        upper = list(continuous = wrap("cor", size = 3)),  # Add correlation coefficients
        lower = list(continuous = "smooth"))  # Add smoothers

#Times Series from Montly Panel

# Convert the "Data" column to yearmon objects
data_resp_clima$Data <- as.yearmon(paste0("01-", data_resp_clima$Data), format = "%d-%b_%Y")  # Pad with "01-" for consistency
str(data_resp_clima)
head(data_resp_clima)

# Select the relevant variables
data_subset_ts_resp <- data_resp_clima[, c("INSOLACAO_TOTAL", "PRECIPITACAO_TOTAL", 
                                           "TEMPERATURA_MAXIMA", "TEMPERATURA_MEDIA", "Data", "Norte")]

# Reshape the data into a long format using tidyr::pivot_longer
data_long_resp <- pivot_longer(data_subset_ts_resp, cols = -Data, names_to = "variable", values_to = "value")

# Now use the reshaped data for plotting (North)
ggplot(data_long_resp, aes(x = Data, y = value)) +
  facet_wrap(~ variable, scales = "free_y", ncol = 2) +
  geom_line() +
  labs(title = "Figure 18. Temporal Analysis of Respiratory disorders and Climate 
                                 in North Brazil (2010-2019)",
       y = "Values") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        strip.text = element_text(size = 10)) +
  scale_x_yearmon(format = "%b %Y", n = 12, limits = c(as.yearmon("jan 2010"), as.yearmon("dez 2019")))

########################################################
## Diseases of the respiratory system - Southeast Region 
########################################################

# Replace 'file_path.csv' with your CSV file path
file_path_resp <- "cid_resp_sih.csv"

# Read CSV file starting from row 5 with delimiter ';', specifying decimal and grouping marks
data_resp <- read_delim(file_path_resp, delim = ";", skip = 4)
class(data_resp)
head(data_resp)
dim(data_resp)

#missing data
#Use md.pattern() from mice to visualize the missing data pattern
md.pattern(data_resp, rotate.names = TRUE)

#Use describe() from Hmisc
Hmisc::describe(data_resp)

# Replace 'file_path.csv' with your CSV file path
file_path_sudeste <- "proj_ipea_Sudeste.csv"

# Read CSV file starting from row 5 with delimiter ';', specifying decimal and grouping marks
new_df_sudeste <- read_delim(file_path_sudeste, delim = ";")
str(new_df_sudeste)

# Remove the column at a specific index (e.g., 1)
new_df_sudeste <- new_df_sudeste[, -1]

# Create a new column 'Data' in 'new_df_sul' and assign values from 'data_dcv'
new_df_sudeste$Data_new <- as.character(data_resp$Data)

# Display the updated 'new_df_sudeste' dataset
head(new_df_sudeste)
str(new_df_sudeste$Data_new)

#Join Data sets
# Selecting only necessary columns from data_dcv
data_resp_subset <- data_resp[, c("Sudeste")]
head(data_resp_subset)

# Combine 'data_dcv_subset' as a new column in 'new_df_sul' and rename it as 'new_data_dcv'
new_data_resp <- cbind(new_df_sudeste, new_data_resp = data_resp_subset)

# Show the structure of the new dataset
str(new_data_resp)

# Renaming columns in 'new_data_dcv'
colnames(new_data_resp)[colnames(new_data_resp) == "Data_new"] <- "Data"
# ... Repeat this pattern for other columns you want to rename

# Show the updated column names
colnames(new_data_resp)

# Create a sequence of years from 2010 to 2019 and repeat each year 10 times
ano <- rep(2010:2019, each = 12)

# Add the 'year' column to 'merged_data'
new_data_resp <- transform(new_data_resp, ano = as.factor(ano))

# Show the updated dataset with the new 'year' column
head(new_data_resp)
str(new_data_resp)

# Save as CSV with semicolon delimiter
write.table(new_data_resp, file = "data_resp_clima_Sud.csv", sep = ";", row.names = FALSE)

#to test "load file"

#################################################
## Diseases of the respiratory system and climate 
#################################################

# Replace 'file_path.csv' with your CSV file path
file_path_resp <- "data_resp_clima_Sud.csv"

# Read CSV file starting from row 5 with delimiter ';', specifying decimal and grouping marks
data_resp_clima <- read_delim(file_path_resp, delim = ";")
str(data_resp_clima) 
class(data_resp_clima)
head(data_resp_clima)

# Several Scatterplot (Southeast) (Diseases of the respiratory system and climate)
# Select the desired columns
data_subset_plot_resp <- data_resp_clima[, c("Sudeste", "INSOLACAO_TOTAL", "PRECIPITACAO_TOTAL", "TEMPERATURA_MAXIMA", "TEMPERATURA_MEDIA")]

# Rename the columns for better visualization
colnames(data_subset_plot_resp) <- c("Dça_Respiratoria", "insolacao", "pp", "t_max", "t_med")

# Create the scatter plot matrix using GGally
ggpairs(data_subset_plot_resp,
        title = "Scatter Plot Matrix Analysis of Respiratory disorders, Climate &
                      Inpatient Care in Southeast Brazil (2010-2019)",
        upper = list(continuous = wrap("cor", size = 3)),  # Add correlation coefficients
        lower = list(continuous = "smooth"))  # Add smoothers

#Times Series from Montly Panel

# Convert the "Data" column to yearmon objects
data_resp_clima$Data <- as.yearmon(paste0("01-", data_resp_clima$Data), format = "%d-%b_%Y")  # Pad with "01-" for consistency
str(data_resp_clima)
head(data_resp_clima)

# Select the relevant variables
data_subset_ts_resp <- data_resp_clima[, c("INSOLACAO_TOTAL", "PRECIPITACAO_TOTAL", 
                                           "TEMPERATURA_MAXIMA", "TEMPERATURA_MEDIA", "Data", "Sudeste")]

# Reshape the data into a long format using tidyr::pivot_longer
data_long_resp <- pivot_longer(data_subset_ts_resp, cols = -Data, names_to = "variable", values_to = "value")

# Now use the reshaped data for plotting (Southeast)
ggplot(data_long_resp, aes(x = Data, y = value)) +
  facet_wrap(~ variable, scales = "free_y", ncol = 2) +
  geom_line() +
  labs(title = "Figure 20. Temporal Analysis of Respiratory disorders and Climate 
                                 in Southeast Brazil (2010-2019)",
       y = "Values") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        strip.text = element_text(size = 10)) +
  scale_x_yearmon(format = "%b %Y", n = 12, limits = c(as.yearmon("jan 2010"), as.yearmon("dez 2019")))

####################################################
## Diseases of the respiratory system - South Region 
####################################################

# Replace 'file_path.csv' with your CSV file path
file_path_resp <- "cid_resp_sih.csv"

# Read CSV file starting from row 5 with delimiter ';', specifying decimal and grouping marks
data_resp <- read_delim(file_path_resp, delim = ";", skip = 4)
class(data_resp)
head(data_resp)
dim(data_resp)

#missing data
#Use md.pattern() from mice to visualize the missing data pattern
md.pattern(data_resp, rotate.names = TRUE)

#Use describe() from Hmisc
Hmisc::describe(data_resp)

# Replace 'file_path.csv' with your CSV file path
file_path_sul <- "proj_ipea_Sul.csv"

# Read CSV file starting from row 5 with delimiter ';', specifying decimal and grouping marks
new_df_sul <- read_delim(file_path_sul, delim = ";")
str(new_df_sul)

# Remove the column at a specific index (e.g., 1)
new_df_sul <- new_df_sul[, -1]

# Create a new column 'Data' in 'new_df_sul' and assign values from 'data_dcv'
new_df_sul$Data_new <- as.character(data_resp$Data)

# Display the updated 'new_df_sudeste' dataset
head(new_df_sul)
str(new_df_sul$Data_new)

#Join Data sets
# Selecting only necessary columns from data_dcv
data_resp_subset <- data_resp[, c("Sul")]
head(data_resp_subset)

# Combine 'data_dcv_subset' as a new column in 'new_df_sul' and rename it as 'new_data_dcv'
new_data_resp <- cbind(new_df_sul, new_data_resp = data_resp_subset)

# Show the structure of the new dataset
str(new_data_resp)

# Renaming columns in 'new_data_dcv'
colnames(new_data_resp)[colnames(new_data_resp) == "Data_new"] <- "Data"
# ... Repeat this pattern for other columns you want to rename

# Show the updated column names
colnames(new_data_resp)

# Create a sequence of years from 2010 to 2019 and repeat each year 10 times
ano <- rep(2010:2019, each = 12)

# Add the 'year' column to 'merged_data'
new_data_resp <- transform(new_data_resp, ano = as.factor(ano))

# Show the updated dataset with the new 'year' column
head(new_data_resp)
str(new_data_resp)

# Save as CSV with semicolon delimiter
write.table(new_data_resp, file = "data_resp_clima_Sul.csv", sep = ";", row.names = FALSE)

#to test "load file"

#################################################
## Diseases of the respiratory system and climate 
#################################################

# Replace 'file_path.csv' with your CSV file path
file_path_resp <- "data_resp_clima_Sul.csv"

# Read CSV file starting from row 5 with delimiter ';', specifying decimal and grouping marks
data_resp_clima <- read_delim(file_path_resp, delim = ";")
str(data_resp_clima) #ano (numerico)
class(data_resp_clima)
head(data_resp_clima)

# Several Scatterplot (Sputh) (Diseases of the respiratory system and climate)
# Select the desired columns
data_subset_plot_resp <- data_resp_clima[, c("Sul", "INSOLACAO_TOTAL", "PRECIPITACAO_TOTAL", "TEMPERATURA_MAXIMA", "TEMPERATURA_MEDIA")]

# Rename the columns for better visualization
colnames(data_subset_plot_resp) <- c("Dça_Respiratoria", "insolacao", "pp", "t_max", "t_med")

# Create the scatter plot matrix using GGally
ggpairs(data_subset_plot_resp,
        title = "Scatter Plot Matrix Analysis of Respiratory disorders, Climate &
                      Inpatient Care in South Brazil (2010-2019)",
        upper = list(continuous = wrap("cor", size = 3)),  # Add correlation coefficients
        lower = list(continuous = "smooth"))  # Add smoothers

#Times Series from Montly Panel

# Convert the "Data" column to yearmon objects
data_resp_clima$Data <- as.yearmon(paste0("01-", data_resp_clima$Data), format = "%d-%b_%Y")  # Pad with "01-" for consistency
str(data_resp_clima)
head(data_resp_clima)

# Select the relevant variables
data_subset_ts_resp <- data_resp_clima[, c("INSOLACAO_TOTAL", "PRECIPITACAO_TOTAL", 
                                           "TEMPERATURA_MAXIMA", "TEMPERATURA_MEDIA", "Data", "Sul")]

# Reshape the data into a long format using tidyr::pivot_longer
data_long_resp <- pivot_longer(data_subset_ts_resp, cols = -Data, names_to = "variable", values_to = "value")

# Now use the reshaped data for plotting (South)
ggplot(data_long_resp, aes(x = Data, y = value)) +
  facet_wrap(~ variable, scales = "free_y", ncol = 2) +
  geom_line() +
  labs(title = "Figure 22. Temporal Analysis of Respiratory disorders and Climate 
                                 in South Brazil (2010-2019)",
       y = "Values") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        strip.text = element_text(size = 10)) +
  scale_x_yearmon(format = "%b %Y", n = 12, limits = c(as.yearmon("jan 2010"), as.yearmon("dez 2019")))


# Convert 'Data' to yearmon format
data_ts_sudeste$Data <- as.yearmon(paste0("01_", data_ts_sudeste$Data), "%d_%b_%Y")

# Subset historical and future data
historical_data <- subset(data_ts_sudeste, as.numeric(format(Data, "%Y")) < 2020)
future_data <- subset(data_ts_sudeste, as.numeric(format(Data, "%Y")) >= 2020)

# Plotting
ggplot() +
  geom_line(data = historical_data, aes(x = Data, y = TEMPERATURA_MAXIMA), color = "black") +
  geom_line(data = future_data, aes(x = Data, y = TEMPERATURA_MAXIMA), color = "blue") +
  labs(title = "Future Seasonal maximum temperature changes for RCP4.5 (GFDL scenario) to Southeast region Brazil",
       subtitle = "In black (historical) and in color (future)",
       x = "Date", y = "Maximum Temperature") +
  scale_x_yearmon(format = "%b %Y", n = 12, limits = c(as.yearmon("jan 2010"), as.yearmon("dez 2100"))) +
  theme_minimal()

