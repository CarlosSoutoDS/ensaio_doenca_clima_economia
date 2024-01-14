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
## ==========================================================================================

############################################################################################
## supplementary analysis for study
############################################################################################

## ==========================================================================================
## Information sources
## The information sources are publicly accessible and divided by area:
##  ▪ Climate:
## >> CLIMBra (CABra), available at: https://hess.copernicus.org/articles/25/3105/2021/ 
## & https://doi.org/10.57760/sciencedb.02316
## ▪ Average Annual Growth Rate
## >> Wittgenstein Center for Demography and Global Human Capital (2018). Wittgenstein 
## Center Data Explorer, available at: https://dataexplorer.wittgensteincentre.org/wcde-v2/
## ▪ Brazilian Deprivation Index (IBP)
## >> Cidacs, available at: 
## https://docs.google.com/forms/d/e/1FAIpQLScG7fffQVuZE_yCSweeQ06iNr2jZx7AG4kuX8iziPY7Gee18w/viewform
## About this file
## "suppl._analysis_study_climate.R"
## This file is a Historical data and future data meticulously mined from Center Data.
## ==========================================================================================

# Load the packages
library(readr)
library(aplpack)
library(dplyr)
library(ggplot2)
library(tidyr)
library(tidyverse)
library(reshape2)
library(corrgram)
library(car)
library(lmtest)
library(pacman)
library(psych)
library(corrplot)
library(nortest)
library(caret)
library(predictmeans)
library(lubridate)

####################################
## Brazilian Deprivation Index (IBP)
####################################

# Replace 'file_path.csv' with your CSV file path
file_path_sud2 <- "BDI_Municipalities.csv"

# Read CSV file with delimiter ',' specifying decimal and grouping marks
data_privacao <- read_delim(file_path_sud2, delim = ",", locale = 
                              locale(decimal_mark = ".", grouping_mark = ","))
str(data_privacao) 
class(data_privacao)
head(data_privacao)
names(data_privacao)

#####################################
## IBP of the Southeast Region (2010)
#####################################

# Filter the data for "Sudeste" in "nome_grande_regiao"
data_sudeste <- data_privacao %>%
  filter(nome_grande_regiao == "Região Sudeste")

# Convert q_measure_1f_12 to factor to plot properly
data_sudeste$q_measure_1f_12 <- factor(data_sudeste$q_measure_1f_12)

# Calculate the percentage for each value of "q_measure_1f_12" and add labels
percentage <- data_sudeste %>%
  group_by(q_measure_1f_12) %>%
  summarize(percentage = (n() / nrow(data_sudeste)) * 100) %>%
  mutate(q_measure_label = paste0("Q", q_measure_1f_12))  # Adding labels

# Plotting the bar plot with labels
ggplot(percentage, aes(x = q_measure_label, y = percentage)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(
    x = "Classificação de privação",
    y = "Percentagem",
    title = "Índice de Privação no Sudeste do Brasil em 2010"
  ) +
  theme_minimal()

#Boxplot IBP Southeast
# Filter the data for "Região Sudeste" in "nome_grande_regiao"
data_sudeste <- data_privacao %>%
  filter(nome_grande_regiao == "Região Sudeste")

# Plotting the boxplot
ggplot(data_sudeste, aes(y = measure_1f_12)) +
  geom_boxplot(fill = "skyblue") +
  labs(
    y = "Medida de Privação",
    title = "Índice de Privacão na Região Sudeste do Brasil em 2010"
  ) +
  theme_minimal()

# Creating the boxplot with title
boxplot(data_sudeste$measure_1f_12, 
        main = "Índice de Privacão na Região Sudeste do Brasil em 2010",
        ylab = "IBP")

# Correlation between PPI and CVD_UF 2010
# first step: create new dataset with IBP by Federation Unit
# Calculate the sum of "measure_1f_12" for each "nome_da_uf"
sum_by_uf <- aggregate(measure_1f_12 ~ nome_da_uf, data = data_privacao, FUN = sum)

# Calculate the average using counts from the original dataset
average_by_uf <- sum_by_uf %>%
  mutate(IBP = measure_1f_12 / data_privacao %>% count(nome_da_uf) %>% pull(n)) %>%
  select(UF = nome_da_uf, IBP)

# Create the new dataframe
new_dataframe <- average_by_uf

# View the corrected dataframe
print(new_dataframe)
print(data_morte_dcv_uf)

# Join the datasets using a merge with careful ordering
merged_data <- merge(new_dataframe, data_morte_dcv_uf, by = "UF", all = TRUE)

# Arrange the "UF" column in alphabetical order to ensure consistency
merged_IBP_DCV <- merged_data %>% arrange(UF)

# Print the merged data with three columns
print(merged_IBP_DCV)

# Correlations
cor.test(merged_IBP_DCV$DCV, merged_IBP_DCV$IBP, method="pearson")

# Linear Regression model (simple)
mod_DCV_IBP=lm(DCV~IBP, data=merged_IBP_DCV)
summary(mod_DCV_IBP)

#Initially check scatter plot between Y and X
plot(merged_IBP_DCV$IBP, merged_IBP_DCV$DCV, pch=20, xlab="IBP", ylab="DCV (Sudeste) 2010", col="darkblue")
abline(lm(merged_IBP_DCV$DCV~merged_IBP_DCV$IBP), lwd=2, col="red")

# Residual Plots
par(mfrow=c(2,2))
plot(mod_DCV_IBP)

#########################################################
## average annual growth rate of the Brazilian population
#########################################################

# Replace 'file_path.csv' with your CSV file path
file_path_wcde <- "wcde_data.csv"

# Read the CSV file using read_delim() with explicit column names and delimiter
data_taxa_cresc <- read_csv2(file_path_wcde, skip = 5)
class(data_taxa_cresc)
head(data_taxa_cresc)
dim(data_taxa_cresc)

# Assuming data_taxa_cresc is your data frame
data_taxa_cresc <- data_taxa_cresc %>%
  mutate(Rate = as.numeric(Rate))
head(data_taxa_cresc)
tail(data_taxa_cresc)
plot(data_taxa_cresc)

############################################################################
## time series plot - average annual growth rate of the Brazilian population
############################################################################

# Convert 'Period' to factors to ensure proper ordering on x-axis
data_taxa_cresc$Period <- factor(data_taxa_cresc$Period, 
                                 levels = unique(data_taxa_cresc$Period))
# Create the plot with adjusted line thickness
ggplot(data_taxa_cresc, aes(x = Period, y = Rate, color = Scenario, group = Scenario)) +
  geom_line(size = 1.1) + # Adjust the size as needed
  labs(
    x = "Period",
    y = "Average Growth Rate",
    title = "Average annual population growth rates over a period of one year - Brazil"
  ) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

data_sem_influente <- subset(data_dcv_clima, (data_dcv_clima$ident!=4 & data_dcv_clima$ident!=28 &
                                                data_dcv_clima$ident!=61 & data_dcv_clima$ident!=78 &
                                                data_dcv_clima$ident!=91))
data_sem_influente.fit <- lm(Sudeste~INSOLACAO_TOTAL+TEMPERATURA_MEDIA,data=data_sem_influente)

####################################################################################
## Future seasonal maximum temperature for RCP4.5 scenario (Southeast region Brazil)
####################################################################################

# Replace 'file_path.csv' with your CSV file path
file_path_sud4 <- "GFDL-CM4-tasmax-ssp245-interp_modif.csv"

# Read CSV file with delimiter ',' specifying decimal and grouping marks
data_gfdl <- read_delim(file_path_sud4, delim = ",", locale = 
                          locale(decimal_mark = ".", grouping_mark = ","))
str(data_gfdl) 
class(data_gfdl)
head(data_gfdl)
names(data_gfdl)

# Selecting variables of interest for the Southeast region
# Extracting specific columns from the original dataset
selected_columns <- c("time", "year", "month", "CABra_158", "CABra_159", "CABra_160", "CABra_161", 
                      "CABra_162", "CABra_163", "CABra_164", "CABra_165", "CABra_166", 
                      "CABra_167", "CABra_168", "CABra_169", "CABra_170", "CABra_171", 
                      "CABra_172", "CABra_173", "CABra_174", "CABra_175", "CABra_176", 
                      "CABra_178", "CABra_227", "CABra_228", "CABra_229", "CABra_230", 
                      "CABra_231", "CABra_232", "CABra_233", "CABra_234", "CABra_235", 
                      "CABra_236", "CABra_237", "CABra_238", "CABra_239", "CABra_240", 
                      "CABra_241", "CABra_250", "CABra_251", "CABra_252", "CABra_253", 
                      "CABra_254", "CABra_255", "CABra_256", "CABra_258", "CABra_259", 
                      "CABra_260", "CABra_261", "CABra_262", "CABra_263", "CABra_264", 
                      "CABra_265", "CABra_266", "CABra_267", "CABra_268", "CABra_269", 
                      "CABra_270", "CABra_271", "CABra_272", "CABra_273", "CABra_274", 
                      "CABra_275", "CABra_276", "CABra_277", "CABra_278", "CABra_279", 
                      "CABra_280", "CABra_281", "CABra_282", "CABra_283", "CABra_284", 
                      "CABra_285", "CABra_286", "CABra_287", "CABra_288", "CABra_289", 
                      "CABra_290", "CABra_291", "CABra_292", "CABra_293", "CABra_294", 
                      "CABra_295", "CABra_296", "CABra_297", "CABra_298", "CABra_299", 
                      "CABra_300", "CABra_301", "CABra_302", "CABra_303", "CABra_304", 
                      "CABra_305", "CABra_306", "CABra_307", "CABra_308", "CABra_309", 
                      "CABra_310", "CABra_311", "CABra_312", "CABra_313", "CABra_314", 
                      "CABra_315", "CABra_316", "CABra_317", "CABra_318", "CABra_319", 
                      "CABra_320", "CABra_321", "CABra_322", "CABra_323", "CABra_324", 
                      "CABra_325", "CABra_326", "CABra_327", "CABra_328", "CABra_329", 
                      "CABra_330", "CABra_331", "CABra_332", "CABra_333", "CABra_334", 
                      "CABra_335", "CABra_336", "CABra_337", "CABra_338", "CABra_339", 
                      "CABra_340", "CABra_341", "CABra_342", "CABra_343", "CABra_344", 
                      "CABra_345", "CABra_346", "CABra_347", "CABra_348", "CABra_349", 
                      "CABra_350", "CABra_351", "CABra_352", "CABra_353", "CABra_354", 
                      "CABra_355", "CABra_356", "CABra_357", "CABra_358", "CABra_359", 
                      "CABra_360", "CABra_361", "CABra_362", "CABra_363", "CABra_364", 
                      "CABra_365", "CABra_366", "CABra_367", "CABra_368", "CABra_369", 
                      "CABra_370", "CABra_371", "CABra_372", "CABra_373", "CABra_374", 
                      "CABra_375", "CABra_376", "CABra_377", "CABra_379", "CABra_380", 
                      "CABra_381", "CABra_382", "CABra_383", "CABra_384", "CABra_385", 
                      "CABra_386", "CABra_387", "CABra_389", "CABra_415", "CABra_420", 
                      "CABra_421", "CABra_422", "CABra_423", "CABra_424", "CABra_425", 
                      "CABra_426", "CABra_427", "CABra_428", "CABra_429", "CABra_430", 
                      "CABra_431", "CABra_432", "CABra_433", "CABra_434", "CABra_435", 
                      "CABra_436", "CABra_437", "CABra_438", "CABra_439", "CABra_440", 
                      "CABra_441", "CABra_442", "CABra_443", "CABra_444", "CABra_445", 
                      "CABra_446", "CABra_447", "CABra_448", "CABra_449", "CABra_450", 
                      "CABra_451", "CABra_452", "CABra_453", "CABra_454", "CABra_455", 
                      "CABra_456", "CABra_457", "CABra_458", "CABra_459", "CABra_460", 
                      "CABra_461", "CABra_462", "CABra_463", "CABra_464", "CABra_465", 
                      "CABra_466", "CABra_467", "CABra_468", "CABra_472", "CABra_633", 
                      "CABra_634", "CABra_635", "CABra_636", "CABra_637", "CABra_638", 
                      "CABra_639", "CABra_640", "CABra_641", "CABra_642", "CABra_643", 
                      "CABra_644", "CABra_645", "CABra_646", "CABra_647", "CABra_648", 
                      "CABra_649", "CABra_650", "CABra_651", "CABra_652", "CABra_653", 
                      "CABra_654", "CABra_655", "CABra_656", "CABra_657", "CABra_658", 
                      "CABra_659", "CABra_660", "CABra_661", "CABra_662", "CABra_663", 
                      "CABra_664", "CABra_665", "CABra_666", "CABra_667", "CABra_668", 
                      "CABra_669", "CABra_670", "CABra_671", "CABra_672", "CABra_673", 
                      "CABra_703", "CABra_704", "CABra_705", "CABra_706", "CABra_707", 
                      "CABra_708", "CABra_709", "CABra_710", "CABra_711", "CABra_712", 
                      "CABra_713", "CABra_714", "CABra_730", "CABra_731")

# Create a new dataset with selected columns
new_data_gfdl <- data_gfdl[, selected_columns]
head(new_data_gfdl)

# Create an empty list to store results
results_list <- list()

# Loop through each year (2020 to 2100)
for (year in 2020:2100) {
  # Loop through each month (1 to 12)
  for (month in 1:12) {
    # Subset data for each month and year
    monthly_year_data <- new_data_gfdl[new_data_gfdl$month == month & new_data_gfdl$year == year, ]
    
    # Extract columns that exist within the specified range
    existing_columns <- intersect(colnames(monthly_year_data), paste0("CABra_", 158:731))
    
    # Check if data exists for the given month and year and columns are present
    if (nrow(monthly_year_data) > 0 && length(existing_columns) > 0) {
      # Calculate averages for existing columns
      averages <- colMeans(monthly_year_data[, existing_columns], na.rm = TRUE)
      
      # Store the averages in the results list
      results_list[[paste("Year", year, "Month", month)]] <- averages
    }
  }
}

# Access specific averages (e.g., January 2020)
print(results_list[["Year 2100 Month 11"]])

# Create an empty data frame to store results
result_dataset <- data.frame(year = integer(), month = integer())
num_columns <- length(paste("CABra_", 158:731, sep = ""))

# Loop through each year (2020 to 2100)
for (year in 2020:2100) {
  # Loop through each month (1 to 12)
  for (month in 1:12) {
    # Access specific averages from the results_list
    averages <- results_list[[paste("Year", year, "Month", month)]]
    
    # Create a row with year, month, and averages
    new_row <- c(year, month, averages)
    
    # Combine with existing results or create a new dataset
    if (nrow(result_dataset) == 0) {
      result_dataset <- data.frame(matrix(new_row, nrow = 1, ncol = (2 + num_columns)))
      colnames(result_dataset) <- c("year", "month", paste("CABra_", 158:731, sep = ""))
    } else {
      result_dataset <- rbind(result_dataset, new_row)
    }
  }
}

# Display the head of the resulting dataset
head(result_dataset)

# Save as CSV with semicolon delimiter
write.table(result_dataset, file = "gauges_means_sudeste.csv", sep = ";", row.names = FALSE)
str(result_dataset)

# Calculate row sums for the specified columns
#result_dataset$sum <- rowSums(result_dataset[, paste0("CABra_", 158:731)], na.rm = TRUE)

# Calculate row-wise averages
result_dataset$average_row <- rowMeans(result_dataset[, paste0("CABra_", 158:731)], na.rm = TRUE)
result_dataset$average_row

# Create a sequence of months and years from January 2020 to December 2100
# Months in Portuguese abbreviations
month_names_pt <- c("jan", "fev", "mar", "abr", "mai", "jun", "jul", "ago", "set", 
                    "out", "nov", "dez")
# Create a sequence of months and years from January 2020 to December 2100 with Portuguese month names
months_pt <- rep(month_names_pt, 81)  # 81 years from 2020 to 2100
years <- rep(2020:2100, each = 12)
time_sequence_pt <- paste0(months_pt, "_", years)

# Create a new dataset with time and Temp_Max columns
new_dataset <- data.frame(time = time_sequence_pt, Temp_Max = result_dataset$average_row)

# Display the head of the new dataset
head(new_dataset)
str(new_dataset)
# Save as CSV with semicolon delimiter
write.table(new_dataset, file = "data_ts_sud.csv", sep = ";", row.names = FALSE)

# Display the head of the time sequence with Portuguese month names
head(time_sequence_pt)
time_sequence_pt

###############################################################
## time series plot - future forecast temp max southeast region
###############################################################

# Selecting relevant columns from data_dcv_clima
data_dcv_clima_selected <- data_dcv_clima[, c("Data", "TEMPERATURA_MAXIMA")]

# Renaming columns in new_dataset to match data_dcv_clima_selected
names(new_dataset) <- c("Data", "TEMPERATURA_MAXIMA")

# Combine the datasets
data_ts_sudeste <- rbind(data_dcv_clima_selected, new_dataset)
str(data_ts_sudeste)

# Save as CSV with semicolon delimiter
write.table(data_ts_sudeste, file = "time_series_sudeste.csv", sep = ";", row.names = FALSE)

# Generating dates from 2010-01-01 to 2100-12-01
time <- seq(as.Date("2010-01-01"), as.Date("2100-12-01"), by = "1 month")

# Format the dates to "yyyy-mm-01"
time <- format(time, "%Y-%m-01")

# Display the vector 'time'
print(time)

# Adding the 'time' vector as a new column named "time"
data_ts_sudeste <- cbind(time, data_ts_sudeste)

# Renaming the columns
colnames(data_ts_sudeste)[1] <- "time"

# Displaying the modified dataset
print(data_ts_sudeste)
str(data_ts_sudeste)

#plot time series
# Convert 'time' column to Date class
data_ts_sudeste$time <- ymd(data_ts_sudeste$time)

# Filter historical and future data
historical_data <- subset(data_ts_sudeste, time >= "2010-01-01" & time <= "2019-12-01")
future_data <- subset(data_ts_sudeste, time >= "2020-01-01" & time <= "2100-12-01")

# Create a time series plot
ggplot() +
  # Historical data (in black)
  geom_line(data = historical_data, aes(x = time, y = TEMPERATURA_MAXIMA), color = "black") +
  # Future scenario data (in another color)
  geom_line(data = future_data, aes(x = time, y = TEMPERATURA_MAXIMA), color = "blue") +
  # Customize x-axis labels for 20-year intervals
  scale_x_date(date_breaks = "20 years", date_labels = "%Y", limits = c(ymd("2010-01-01"), ymd("2100-12-01"))) +
  # Add title and labels
  labs(title = "Future seasonal maximum temperature changes for RCP4.5 (GFDL scenario) for the Southeast region of Brazil",
       subtitle = "In black (historical) and blue (future)",
       x = "Year",
       y = "Max_Temp") +
  # Adjust theme (optional)
  theme_minimal()
