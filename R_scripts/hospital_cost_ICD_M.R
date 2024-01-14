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

#############################################################################################
## Hospital Cost analysis - Regions Brazil 
#############################################################################################
## Information sources
## The information sources are publicly accessible and divided by area:
##  ▪ Public Health - Brazil: available at https://datasus.saude.gov.br/informacoes-de-saude-tabnet/
## Files in .csv format were obtained from the DATASUS database on:
## ▪ Hospital Cost - Inpatient Care
## >> Total value per Year/month processing and Federation Unit
##   - Chapter (ICD-10 M) Musculoskeletal system and connective tissue diseases
## >> Period: 2010-2019
## About this file
## "hospital_cost_ICD_M.R"
## This file is a Historical data meticulously mined from DATASUS (2010-2019) of Regions 
## of Brazil and lays the groundwork for robust analysis.
## ==========================================================================================

# Install and Load the necessary packages
installed.packages("fBasics")
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
library(fBasics)
library(gdata)

# Replace 'file_path.csv' with your CSV file path
file_path_custo1 <- "sih_cid_m_custo_total.csv"

# Read CSV file starting from row 5 with delimiter ';', specifying decimal and grouping marks
data_cidm_custo <- read_delim(file_path_custo1, delim = ";",skip = 4)
str(data_cidm_custo) 
class(data_cidm_custo)
head(data_cidm_custo)
summary(data_cidm_custo$Norte)
summary(data_cidm_custo$Nordeste)
summary(data_cidm_custo$Sudeste)
summary(data_cidm_custo$Sul)
summary(data_cidm_custo$Centro_Oeste)

# Selecting necessary columns
regions_data <- data_cidm_custo[, c("Norte", "Nordeste", "Sudeste", "Sul", "Centro_Oeste")]

# Reshaping data for plotting
regions_data_long <- tidyr::gather(regions_data, key = "Region", value = "Cost")

# Creating strip plots with horizontal medians for each region
plots <- lapply(unique(regions_data_long$Region), function(region) {
  ggplot(subset(regions_data_long, Region == region), aes(x = Region, y = Cost)) +
    geom_point(position = position_jitter(width = 0.3), alpha = 0.7) +
    stat_summary(
      fun = "median",
      geom = "crossbar",
      width = 0.7,  # Adjust the width to span across the entire data points
      color = "blue"
    ) +
    labs(subtitle = region) +
    theme_minimal() +
    theme(axis.text.x = element_blank())
})

# Combine strip plots using patchwork
combined_plots <- wrap_plots(plots, nrow = 1)

# Add a single title
combined_plots <- combined_plots +
  plot_annotation(
    title = "Total cost of hospital admissions for diseases of the musculoskeletal system and connective tissue in Brazilian regions (2010-2019)"
  )

# Display the combined plot
combined_plots

# Replace 'file_path.csv' with your CSV file path
file_path_custo2 <- "cid_m_sih.csv"

# Read CSV file starting from row 5 with delimiter ';', specifying decimal and grouping marks
data_cidm_sih <- read_delim(file_path_custo2, delim = ";",skip = 4)
str(data_cidm_sih) 
class(data_cidm_sih)
head(data_cidm_sih)

# Select the relevant columns
selected_columns <- c("Norte", "Nordeste", "Sudeste", "Sul", "Centro_Oeste")

# Initialize a matrix to store the sums
interval_sums <- matrix(0, nrow = 10, ncol = length(selected_columns), 
                        dimnames = list(NULL, selected_columns))

# Loop through each selected column and calculate the sum for each interval
for (j in 1:length(selected_columns)) {
  col <- selected_columns[j]
  for (i in 1:10) {
    start_index <- (i - 1) * 12 + 1
    end_index <- i * 12
    interval_sums[i, j] <- sum(data_cidm_sih[start_index:end_index, col])
  }
}

# Print the results
cat("Sum of each interval of 12 values for each column:\n")
print(interval_sums)

# Select and print the first value in the "Norte" column
first_norte_sum <- interval_sums[1, "Norte"]
cat("First value (sum of the first range of 12 values) in the 'Norte' column:\n")
print(first_norte_sum)

# Extract the first 12 values of the "Norte" column
norte_first_12_values <- data_cidm_sih[1:12, "Norte"]

# Retain only up to 12 values if there are less than 12 in the column
norte_first_12_values <- norte_first_12_values[1:min(12, length(norte_first_12_values))]

# Calculate the proportions by dividing each value by the first_norte_sum
norte_proportions <- norte_first_12_values / first_norte_sum

# Print the proportions
print(norte_proportions)

# Select and print the sum of the next 12 values in the "Norte" column
next_12_norte_sum <- interval_sums[2, "Norte"]  # Use the value from row 2, as row 1 represents the first 12 values
cat("Sum of the next 12 values (13th to 24th) in the 'Norte' column:\n")
print(next_12_norte_sum)

# Extract the 13th to 24th values of the "Norte" column
norte_next_12_values <- data_cidm_sih[13:24, "Norte"]

# Retain only up to 12 values if there are less than 12 remaining
norte_next_12_values <- norte_next_12_values[1:min(12, length(norte_next_12_values))]

# Calculate the proportions by dividing each value by the next_12_norte_sum
norte_next_proportions <- norte_next_12_values / next_12_norte_sum

# Print the proportions
print(norte_next_proportions)

# Define the number of intervals (assuming 120 values in the column)
num_intervals <- 120 %/% 12  # Integer division to get the number of whole intervals

# Loop through each interval
for (i in 1:num_intervals) {
  start_index <- (i - 1) * 12 + 1
  end_index <- min(i * 12, nrow(data_cidm_sih))  # Ensure end_index doesn't exceed data length
  
  # Get the sum of the current interval
  current_sum <- interval_sums[i, "Norte"]
  
  # Extract the values for the current interval
  current_values <- data_cidm_sih[start_index:end_index, "Norte"]
  
  # Calculate proportions
  current_proportions <- current_values / current_sum
  
  # Print the results
  cat("Interval", i, "(values", start_index, ":", end_index, "):\n")
  print(current_proportions)
}

# Define the columns to process
selected_columns <- c("Norte", "Nordeste", "Sudeste", "Sul", "Centro_Oeste")

# Define the number of intervals (assuming 120 values in each column)
num_intervals <- 120 %/% 12

# Loop through each column
for (col in selected_columns) {
  cat("Results for column:", col, "\n")
  
  # Loop through each interval within the column
  for (i in 1:num_intervals) {
    start_index <- (i - 1) * 12 + 1
    end_index <- min(i * 12, nrow(data_cidm_sih))  # Ensure end_index doesn't exceed data length
    
    # Get the sum of the current interval
    current_sum <- interval_sums[i, col]
    
    # Extract the values for the current interval
    current_values <- data_cidm_sih[start_index:end_index, col]
    
    # Calculate proportions
    current_proportions <- current_values / current_sum
    
    # Print the results
    cat("Interval", i, "(values", start_index, ":", end_index, "):\n")
    print(current_proportions)
  }
  
  cat("\n")  # Add an empty line between columns for visual clarity
}

# Define the columns to process
selected_columns <- c("Norte", "Nordeste", "Sudeste", "Sul", "Centro_Oeste")

# Define the number of intervals (assuming 120 values in each column)
num_intervals <- 120 %/% 12

# Initialize lists to store results for each column
result_lists <- lapply(selected_columns, function(col) list())

# Loop through each column
for (col_index in seq_along(selected_columns)) {
  col <- selected_columns[col_index]
  cat("Results for column:", col, "\n")
  
  # Loop through each interval within the column
  for (i in 1:num_intervals) {
    start_index <- (i - 1) * 12 + 1
    end_index <- min(i * 12, nrow(data_cidm_sih))  # Ensure end_index doesn't exceed data length
    
    # Get the sum of the current interval
    current_sum <- interval_sums[i, col]
    
    # Extract the values for the current interval
    current_values <- data_cidm_sih[start_index:end_index, col]
    
    # Calculate proportions
    current_proportions <- current_values / current_sum
    
    # Print the results
    cat("Interval", i, "(values", start_index, ":", end_index, "):\n")
    print(current_proportions)
    
    # Append the current_proportions to the corresponding result list
    result_lists[[col_index]][[i]] <- current_proportions
  }
  
  cat("\n")  # Add an empty line between columns for visual clarity
}

# Create a vector of dates in the format "dd/mm/yyyy"
dates <- c(
  "01/01/2010", "01/02/2010", "01/03/2010", "01/04/2010", "01/05/2010", "01/06/2010",
  "01/07/2010", "01/08/2010", "01/09/2010", "01/10/2010", "01/11/2010", "01/12/2010",
  "01/01/2011", "01/02/2011", "01/03/2011", "01/04/2011", "01/05/2011", "01/06/2011",
  "01/07/2011", "01/08/2011", "01/09/2011", "01/10/2011", "01/11/2011", "01/12/2011",
  "01/01/2012", "01/02/2012", "01/03/2012", "01/04/2012", "01/05/2012", "01/06/2012",
  "01/07/2012", "01/08/2012", "01/09/2012", "01/10/2012", "01/11/2012", "01/12/2012",
  "01/01/2013", "01/02/2013", "01/03/2013", "01/04/2013", "01/05/2013", "01/06/2013",
  "01/07/2013", "01/08/2013", "01/09/2013", "01/10/2013", "01/11/2013", "01/12/2013",
  "01/01/2014", "01/02/2014", "01/03/2014", "01/04/2014", "01/05/2014", "01/06/2014",
  "01/07/2014", "01/08/2014", "01/09/2014", "01/10/2014", "01/11/2014", "01/12/2014",
  "01/01/2015", "01/02/2015", "01/03/2015", "01/04/2015", "01/05/2015", "01/06/2015",
  "01/07/2015", "01/08/2015", "01/09/2015", "01/10/2015", "01/11/2015", "01/12/2015",
  "01/01/2016", "01/02/2016", "01/03/2016", "01/04/2016", "01/05/2016", "01/06/2016",
  "01/07/2016", "01/08/2016", "01/09/2016", "01/10/2016", "01/11/2016", "01/12/2016",
  "01/01/2017", "01/02/2017", "01/03/2017", "01/04/2017", "01/05/2017", "01/06/2017",
  "01/07/2017", "01/08/2017", "01/09/2017", "01/10/2017", "01/11/2017", "01/12/2017",
  "01/01/2018", "01/02/2018", "01/03/2018", "01/04/2018", "01/05/2018", "01/06/2018",
  "01/07/2018", "01/08/2018", "01/09/2018", "01/10/2018", "01/11/2018", "01/12/2018",
  "01/01/2019", "01/02/2019", "01/03/2019", "01/04/2019", "01/05/2019", "01/06/2019",
  "01/07/2019", "01/08/2019", "01/09/2019", "01/10/2019", "01/11/2019", "01/12/2019"
)

# Convert the dates to actual date format
dates <- as.Date(dates, format = "%d/%m/%Y")
dates

#Values of the Midwest
# Placeholder for the output structure
output_structure <- "
Interval 1 (values 1 : 12 ):
   Centro_Oeste
1    0.07548198
2    0.07766197
3    0.08249881
4    0.08672253
5    0.08488317
6    0.08508754
7    0.08740377
8    0.08856189
9    0.08331630
10   0.08910689
11   0.07956945
12   0.07970570
Interval 2 (values 13 : 24 ):
   Centro_Oeste
1    0.07482714
2    0.07722325
3    0.08386390
4    0.08906689
5    0.08037242
6    0.08742384
7    0.08790306
8    0.08872458
9    0.08865612
10   0.08475389
11   0.08126241
12   0.07592250
Interval 3 (values 25 : 36 ):
   Centro_Oeste
1    0.07057275
2    0.07959184
3    0.08341014
4    0.08755760
5    0.08420013
6    0.07807768
7    0.07992100
8    0.09802502
9    0.08630678
10   0.08538512
11   0.08630678
12   0.08064516
Interval 4 (values 37 : 48 ):
   Centro_Oeste
1    0.07074333
2    0.08116061
3    0.08098405
4    0.08475075
5    0.08686952
6    0.08286740
7    0.09234301
8    0.08787005
9    0.08480961
10   0.08116061
11   0.08475075
12   0.08169031
Interval 5 (values 49 : 60 ):
   Centro_Oeste
1    0.07129235
2    0.07852816
3    0.08166928
4    0.07662105
5    0.07751851
6    0.07847207
7    0.08598833
8    0.09058784
9    0.09064393
10   0.09597263
11   0.08705407
12   0.08565178
Interval 6 (values 61 : 72 ):
   Centro_Oeste
1    0.07510742
2    0.08442609
3    0.09458178
4    0.08409129
5    0.08721612
6    0.08146867
7    0.08822052
8    0.08794152
9    0.08442609
10   0.07678143
11   0.07605602
12   0.07968305
Interval 7 (values 73 : 84 ):
   Centro_Oeste
1    0.07496631
2    0.07592093
3    0.08557951
4    0.08951033
5    0.08075022
6    0.08894879
7    0.08737646
8    0.09237421
9    0.09035265
10   0.07597709
11   0.07597709
12   0.08226640
Interval 8 (values 85 : 96 ):
   Centro_Oeste
1    0.07239229
2    0.08304989
3    0.08395692
4    0.08764172
5    0.08696145
6    0.07874150
7    0.08730159
8    0.08531746
9    0.08741497
10   0.08628118
11   0.08367347
12   0.07726757
Interval 9 (values 97 : 108 ):
   Centro_Oeste
1    0.08450206
2    0.08049499
3    0.08438421
4    0.07967001
5    0.08432528
6    0.08049499
7    0.08479670
8    0.09027696
9    0.08550383
10   0.08161461
11   0.08550383
12   0.07843253
Interval 10 (values 109 : 120 ):
   Centro_Oeste
1    0.07267704
2    0.07543978
3    0.08643437
4    0.08575778
5    0.08305142
6    0.08203654
7    0.08322057
8    0.08964817
9    0.08141633
10   0.09291836
11   0.08553225
12   0.08186739
"
# Extract the values from the output structure
values <- as.numeric(unlist(strsplit(output_structure, "\\s+")))
values <- values[!is.na(values) & !values %in% 1:12]  # Exclude intervals and 1 to 12
values <- values[values %% 1 != 0]  # Exclude non-decimal numbers

# Create a data frame using the values vector
num_rows <- length(values) / 2
df_centro <- data.frame(
  Value = values
)

# Print the new data frame
print(df_centro)

#Values of the North
# Placeholder for the output structure
output_structure <- "
Interval 1 (values 1 : 12 ):
  Norte
1  0.08164593
2  0.07749858
3  0.07823046
4  0.08058876
5  0.09449459
6  0.08693177
7  0.08636253
8  0.09075384
9  0.08172725
10 0.08075140
11 0.08514272
12 0.07587216
Interval 2 (values 13 : 24 ):
  Norte
1  0.09244486
2  0.08437353
3  0.08446739
4  0.08606288
5  0.07921164
6  0.08690756
7  0.08681370
8  0.07339277
9  0.08108869
10 0.08305960
11 0.07968090
12 0.08249648
Interval 3 (values 25 : 36 ):
  Norte
1  0.08177254
2  0.08544120
3  0.08466885
4  0.08939950
5  0.08853060
6  0.09625410
7  0.08119328
8  0.07221471
9  0.07308361
10 0.08235181
11 0.08186909
12 0.08322070
Interval 4 (values 37 : 48 ):
  Norte
1  0.07267491
2  0.07775707
3  0.08097578
4  0.08309334
5  0.08622734
6  0.08783669
7  0.08156869
8  0.08283923
9  0.08512621
10 0.08944604
11 0.08927664
12 0.08317805
Interval 5 (values 49 : 60 ):
  Norte
1  0.07364545
2  0.08030861
3  0.08250044
4  0.08048396
5  0.08627038
6  0.08434157
7  0.08451692
8  0.09398562
9  0.08767315
10 0.08872523
11 0.08241277
12 0.07513589
Interval 6 (values 61 : 72 ):
  Norte
1  0.08439068
2  0.07927609
3  0.08635782
4  0.08557096
5  0.08822662
6  0.08802990
7  0.08311203
8  0.08429232
9  0.08350546
10 0.08694797
11 0.08065309
12 0.06963706
Interval 7 (values 73 : 84 ):
  Norte
1  0.06362533
2  0.07626947
3  0.08223751
4  0.08952053
5  0.08547441
6  0.08972284
7  0.08648594
8  0.09285859
9  0.09053207
10 0.08962169
11 0.08294558
12 0.07070605
Interval 8 (values 85 : 96 ):
  Norte
1  0.07654642
2  0.07434796
3  0.07714600
4  0.08434096
5  0.08733886
6  0.08304187
7  0.08923753
8  0.09243530
9  0.08484061
10 0.09323474
11 0.07584691
12 0.08164285
Interval 9 (values 97 : 108 ):
  Norte
1  0.07492166
2  0.07796031
3  0.08527205
4  0.09723673
5  0.08574684
6  0.08831070
7  0.08527205
8  0.08688634
9  0.08223341
10 0.08679138
11 0.07378217
12 0.07558636
Interval 10 (values 109 : 120 ):
  Norte
1  0.08144885
2  0.08252570
3  0.08379834
4  0.08125306
5  0.08693098
6  0.08614782
7  0.08076358
8  0.08272149
9  0.07978463
10 0.08487518
11 0.09133627
12 0.07841410
"
# Extract the values from the output structure
values1 <- as.numeric(unlist(strsplit(output_structure, "\\s+")))
values1 <- values1[!is.na(values1) & !values1 %in% 1:12]  # Exclude intervals and 1 to 12
values1 <- values1[values1 %% 1 != 0]  # Exclude non-decimal numbers

# Create a data frame using the values vector
num_rows <- length(values1) / 2
df_norte <- data.frame(
  Value1 = values1
)

# Print the new data frame
print(df_norte)

#Values of the Northeast
# Placeholder for the output structure
output_structure <-"
Interval 1 (values 1 : 12 ):
     Nordeste
1  0.07498543
2  0.07287213
3  0.08732511
4  0.08202973
5  0.08691216
6  0.07785173
7  0.08297707
8  0.09021570
9  0.08819957
10 0.08858822
11 0.08426448
12 0.08377866
Interval 2 (values 13 : 24 ):
     Nordeste
1  0.07275504
2  0.07952810
3  0.08080883
4  0.08679375
5  0.07906014
6  0.08292695
7  0.08573469
8  0.08901039
9  0.09068519
10 0.08726171
11 0.08157234
12 0.08386286
Interval 3 (values 25 : 36 ):
     Nordeste
1  0.07444879
2  0.07713140
3  0.08410618
4  0.08385069
5  0.08630336
6  0.08420837
7  0.08569020
8  0.09161749
9  0.08351856
10 0.08431057
11 0.08494929
12 0.07986510
Interval 4 (values 37 : 48 ):
     Nordeste
1  0.06894418
2  0.07478648
3  0.08275777
4  0.08416883
5  0.08803070
6  0.08699096
7  0.08486199
8  0.08899616
9  0.08788216
10 0.08758510
11 0.08345092
12 0.08154475
Interval 5 (values 49 : 60 ):
     Nordeste
1  0.07892589
2  0.07621430
3  0.07855733
4  0.08295380
5  0.08566539
6  0.08595498
7  0.08292747
8  0.08550744
9  0.08771884
10 0.08790312
11 0.08735027
12 0.08032118
Interval 6 (values 61 : 72 ):
     Nordeste
1  0.07723132
2  0.07127401
3  0.08447092
4  0.08385649
5  0.08543264
6  0.08129191
7  0.09021452
8  0.09272567
9  0.08535250
10 0.08666150
11 0.08433735
12 0.07715118
Interval 7 (values 73 : 84 ):
     Nordeste
1  0.07895090
2  0.07470940
3  0.08488363
4  0.08053475
5  0.08482994
6  0.08179646
7  0.08689700
8  0.09183646
9  0.08641379
10 0.08644063
11 0.08160855
12 0.08109849
Interval 8 (values 85 : 96 ):
     Nordeste
1  0.07564721
2  0.07711456
3  0.08122838
4  0.07876533
5  0.08513259
6  0.08130699
7  0.08547322
8  0.08584006
9  0.08741222
10 0.09267896
11 0.08489676
12 0.08450372
Interval 9 (values 97 : 108 ):
     Nordeste
1  0.07660167
2  0.07554510
3  0.08260494
4  0.08515032
5  0.08190856
6  0.08279704
7  0.08471809
8  0.08947267
9  0.08274902
10 0.09132168
11 0.08656709
12 0.08056383
Interval 10 (values 109 : 120 ):
     Nordeste
1  0.07814167
2  0.07771480
3  0.08084521
4  0.08556454
5  0.08608628
6  0.08283729
7  0.08551711
8  0.08919297
9  0.08625228
10 0.08584912
11 0.08361989
12 0.07837883
"
# Extract the values from the output structure
values2 <- as.numeric(unlist(strsplit(output_structure, "\\s+")))
values2 <- values2[!is.na(values2) & !values2 %in% 1:12]  # Exclude intervals and 1 to 12
values2 <- values2[values2 %% 1 != 0]  # Exclude non-decimal numbers

# Create a data frame using the values vector
num_rows <- length(values2) / 2
df_nordeste <- data.frame(
  Value2 = values2
)

# Print the new data frame
print(df_nordeste)

#Values of the Southeast
# Placeholder for the output structure
output_structure <-"
Interval 1 (values 1 : 12 ):
      Sudeste
1  0.07304306
2  0.06954216
3  0.08470140
4  0.08198226
5  0.08857619
6  0.08414624
7  0.08452013
8  0.09009438
9  0.08931262
10 0.08469007
11 0.08609497
12 0.08329651
Interval 2 (values 13 : 24 ):
      Sudeste
1  0.07293567
2  0.07818534
3  0.08215897
4  0.08003224
5  0.08511400
6  0.08212539
7  0.08837126
8  0.08841603
9  0.09020696
10 0.08612140
11 0.08466627
12 0.08166646
Interval 3 (values 25 : 36 ):
      Sudeste
1  0.07493555
2  0.07416444
3  0.09051883
4  0.08332566
5  0.08743440
6  0.08280775
7  0.08644462
8  0.08721573
9  0.08492542
10 0.08758402
11 0.08319906
12 0.07744453
Interval 4 (values 37 : 48 ):
      Sudeste
1  0.07520917
2  0.07266181
3  0.07841317
4  0.08551179
5  0.08315691
6  0.08354184
7  0.08585144
8  0.08807047
9  0.08588540
10 0.09448979
11 0.08687038
12 0.08033784
Interval 5 (values 49 : 60 ):
      Sudeste
1  0.07430556
2  0.08247354
3  0.07601411
4  0.07818563
5  0.08203263
6  0.07730379
7  0.08647487
8  0.08764330
9  0.09036596
10 0.09510582
11 0.08998016
12 0.08011464
Interval 6 (values 61 : 72 ):
      Sudeste
1  0.07291589
2  0.07264989
3  0.08318349
4  0.08281109
5  0.08515189
6  0.08486461
7  0.08974836
8  0.08499229
9  0.08829068
10 0.09419588
11 0.08363037
12 0.07756557
Interval 7 (values 73 : 84 ):
      Sudeste
1  0.07406632
2  0.07394514
3  0.08424590
4  0.08095186
5  0.08398149
6  0.09009585
7  0.08845434
8  0.08906026
9  0.08660350
10 0.08456539
11 0.08399251
12 0.08003746
Interval 8 (values 85 : 96 ):
      Sudeste
1  0.07521294
2  0.07458405
3  0.08385189
4  0.07761816
5  0.08386292
6  0.08230725
7  0.08654398
8  0.09127720
9  0.08687497
10 0.08893817
11 0.08619092
12 0.08273754
Interval 9 (values 97 : 108 ):
      Sudeste
1  0.08151287
2  0.07552835
3  0.08640929
4  0.08302992
5  0.08245449
6  0.08152333
7  0.08361582
8  0.08644068
9  0.08453651
10 0.09006068
11 0.08309270
12 0.08179535
Interval 10 (values 109 : 120 ):
      Sudeste
1  0.07626469
2  0.08144297
3  0.07855253
4  0.08224984
5  0.08604929
6  0.07812356
7  0.08515050
8  0.09019600
9  0.08240305
10 0.09070668
11 0.08743834
12 0.08142255
"
# Extract the values from the output structure
values3 <- as.numeric(unlist(strsplit(output_structure, "\\s+")))
values3 <- values3[!is.na(values3) & !values3 %in% 1:12]  # Exclude intervals and 1 to 12
values3 <- values3[values3 %% 1 != 0]  # Exclude non-decimal numbers

# Create a data frame using the values vector
num_rows <- length(values3) / 2
df_sudeste <- data.frame(
  Value3 = values3
)

# Print the new data frame
print(df_sudeste)

#Values of the South
# Placeholder for the output structure
output_structure <-"
Results for column: Sul 
Interval 1 (values 1 : 12 ):
          Sul
1  0.07611422
2  0.07075309
3  0.08152136
4  0.08543292
5  0.09008076
6  0.08002577
7  0.08674444
8  0.09100113
9  0.08577805
10 0.08363820
11 0.08561699
12 0.08329307
Interval 2 (values 13 : 24 ):
          Sul
1  0.07255557
2  0.07538633
3  0.08350736
4  0.07919161
5  0.08538679
6  0.08469070
7  0.08728943
8  0.08329853
9  0.08441227
10 0.09025941
11 0.09097870
12 0.08304330
Interval 3 (values 25 : 36 ):
          Sul
1  0.07186388
2  0.07902580
3  0.08763345
4  0.08349644
5  0.08876779
6  0.08605427
7  0.08576512
8  0.08743327
9  0.08274021
10 0.08380783
11 0.08872331
12 0.07468861
Interval 4 (values 37 : 48 ):
          Sul
1  0.07183182
2  0.06957786
3  0.08790445
4  0.08516599
5  0.08131109
6  0.08232221
7  0.08409168
8  0.08811510
9  0.08455511
10 0.09395012
11 0.08794658
12 0.08322801
Interval 5 (values 49 : 60 ):
          Sul
1  0.07410249
2  0.07456689
3  0.07662642
4  0.08114930
5  0.08028106
6  0.08066470
7  0.08593466
8  0.08900376
9  0.09271898
10 0.09273917
11 0.09275936
12 0.07945322
Interval 6 (values 61 : 72 ):
          Sul
1  0.07489964
2  0.07703790
3  0.08631715
4  0.08676094
5  0.08482440
6  0.08125391
7  0.08873782
8  0.08746697
9  0.08625663
10 0.08661973
11 0.08458233
12 0.07524257
Interval 7 (values 73 : 84 ):
          Sul
1  0.07042971
2  0.07361273
3  0.08335279
4  0.08123077
5  0.08740584
6  0.08882759
7  0.08816976
8  0.09063130
9  0.08411671
10 0.08976127
11 0.08456233
12 0.07789920
Interval 8 (values 85 : 96 ):
          Sul
1  0.07025893
2  0.07208761
3  0.07861269
4  0.07842567
5  0.08887827
6  0.08692490
7  0.08827563
8  0.09209925
9  0.08603134
10 0.08677944
11 0.08592743
12 0.08569885
Interval 9 (values 97 : 108 ):
          Sul
1  0.07070342
2  0.07496532
3  0.07930764
4  0.08620308
5  0.08421285
6  0.08346903
7  0.08401182
8  0.08875621
9  0.08795207
10 0.09090726
11 0.08847476
12 0.08103653
Interval 10 (values 109 : 120 ):
          Sul
1  0.07024123
2  0.07824983
3  0.07803496
4  0.08131653
5  0.08805547
6  0.08393398
7  0.08959859
8  0.08672722
9  0.08586776
10 0.09262623
11 0.08801641
12 0.07733177
"
# Extract the values from the output structure
values4 <- as.numeric(unlist(strsplit(output_structure, "\\s+")))
values4 <- values4[!is.na(values4) & !values4 %in% 1:12]  # Exclude intervals and 1 to 12
values4 <- values4[values4 %% 1 != 0]  # Exclude non-decimal numbers

# Create a data frame using the values vector
num_rows <- length(values4) / 2
df_sul <- data.frame(
  Value4 = values4
)

# Print the new data frame
print(df_sul)

# Create a data frame
df_prop_cidm <- data.frame(
  Data = dates,
  prop_norte = df_norte$Value1,
  prop_nordeste = df_nordeste$Value2,
  prop_sudeste = df_sudeste$Value3,
  prop_sul = df_sul$Value4,
  prop_centro = df_centro$Value
)
print(df_prop_cidm)

# Replace 'file_path.csv' with your CSV file path
file_path_custo_cidm <- "sih_cid_m_custo_total2.csv"

# Read CSV file starting from row 5 with delimiter ';', specifying decimal and grouping marks
cidm_custo <- read_delim(file_path_custo_cidm, delim = ";",skip = 4)
str(cidm_custo) 
head(cidm_custo)

######################################
## total cost for each region per year
######################################

# Select the relevant columns
selected_columns1 <- c("Norte", "Nordeste", "Sudeste", "Sul", "Centro_Oeste")

# Initialize a matrix to store the sums
interval_sums1 <- matrix(0, nrow = 10, ncol = length(selected_columns1), 
                        dimnames = list(NULL, selected_columns1))

# Loop through each selected column and calculate the sum for each interval
for (j in 1:length(selected_columns1)) {
  col <- selected_columns1[j]
  for (i in 1:10) {
    start_index <- (i - 1) * 12 + 1
    end_index <- i * 12
    interval_sums1[i, j] <- sum(cidm_custo[start_index:end_index, col])
  }
}

# Print the results
cat("Sum of each interval of 12 values for each column:\n")
print(interval_sums1)

####################################################################
## Total Cases musculoskeletal system and connective tissue per year
####################################################################

#North
norte_2010 <- interval_sums[1, "Norte"]
norte_2011 <- interval_sums[2, "Norte"]
norte_2012 <- interval_sums[3, "Norte"]
norte_2013 <- interval_sums[4, "Norte"]
norte_2014 <- interval_sums[5, "Norte"]
norte_2015 <- interval_sums[6, "Norte"]
norte_2016 <- interval_sums[7, "Norte"]
norte_2017 <- interval_sums[8, "Norte"]
norte_2018 <- interval_sums[9, "Norte"]
norte_2019 <- interval_sums[10, "Norte"]

## Northeast
nord_2010 <- interval_sums[1, "Nordeste"]
nord_2011 <- interval_sums[2, "Nordeste"]
nord_2012 <- interval_sums[3, "Nordeste"]
nord_2013 <- interval_sums[4, "Nordeste"]
nord_2014 <- interval_sums[5, "Nordeste"]
nord_2015 <- interval_sums[6, "Nordeste"]
nord_2016 <- interval_sums[7, "Nordeste"]
nord_2017 <- interval_sums[8, "Nordeste"]
nord_2018 <- interval_sums[9, "Nordeste"]
nord_2019 <- interval_sums[10, "Nordeste"]

## Southeast
sud_2010 <- interval_sums[1, "Sudeste"]
sud_2011 <- interval_sums[2, "Sudeste"]
sud_2012 <- interval_sums[3, "Sudeste"]
sud_2013 <- interval_sums[4, "Sudeste"]
sud_2014 <- interval_sums[5, "Sudeste"]
sud_2015 <- interval_sums[6, "Sudeste"]
sud_2016 <- interval_sums[7, "Sudeste"]
sud_2017 <- interval_sums[8, "Sudeste"]
sud_2018 <- interval_sums[9, "Sudeste"]
sud_2019 <- interval_sums[10, "Sudeste"]

## South
sul_2010 <- interval_sums[1, "Sul"]
sul_2011 <- interval_sums[2, "Sul"]
sul_2012 <- interval_sums[3, "Sul"]
sul_2013 <- interval_sums[4, "Sul"]
sul_2014 <- interval_sums[5, "Sul"]
sul_2015 <- interval_sums[6, "Sul"]
sul_2016 <- interval_sums[7, "Sul"]
sul_2017 <- interval_sums[8, "Sul"]
sul_2018 <- interval_sums[9, "Sul"]
sul_2019 <- interval_sums[10, "Sul"]

## Midwest
centro_2010 <- interval_sums[1, "Centro_Oeste"]
centro_2011 <- interval_sums[2, "Centro_Oeste"]
centro_2012 <- interval_sums[3, "Centro_Oeste"]
centro_2013 <- interval_sums[4, "Centro_Oeste"]
centro_2014 <- interval_sums[5, "Centro_Oeste"]
centro_2015 <- interval_sums[6, "Centro_Oeste"]
centro_2016 <- interval_sums[7, "Centro_Oeste"]
centro_2017 <- interval_sums[8, "Centro_Oeste"]
centro_2018 <- interval_sums[9, "Centro_Oeste"]
centro_2019 <- interval_sums[10, "Centro_Oeste"]

############################################################################
## Total Hospital Cost musculoskeletal system and connective tissue per year
############################################################################

## North
total_2010_norte <- interval_sums1[1, "Norte"]
total_2011_norte <- interval_sums1[2, "Norte"]
total_2012_norte <- interval_sums1[3, "Norte"]
total_2013_norte <- interval_sums1[4, "Norte"]
total_2014_norte <- interval_sums1[5, "Norte"]
total_2015_norte <- interval_sums1[6, "Norte"]
total_2016_norte <- interval_sums1[7, "Norte"]
total_2017_norte <- interval_sums1[8, "Norte"]
total_2018_norte <- interval_sums1[9, "Norte"]
total_2019_norte <- interval_sums1[10, "Norte"]

#Northeast
total_2010_nord <- interval_sums1[1, "Nordeste"]
total_2011_nord <- interval_sums1[2, "Nordeste"]
total_2012_nord <- interval_sums1[3, "Nordeste"]
total_2013_nord <- interval_sums1[4, "Nordeste"]
total_2014_nord <- interval_sums1[5, "Nordeste"]
total_2015_nord <- interval_sums1[6, "Nordeste"]
total_2016_nord <- interval_sums1[7, "Nordeste"]
total_2017_nord <- interval_sums1[8, "Nordeste"]
total_2018_nord <- interval_sums1[9, "Nordeste"]
total_2019_nord <- interval_sums1[10, "Nordeste"]

## Southeast
total_2010_sud <- interval_sums1[1, "Sudeste"]
total_2011_sud <- interval_sums1[2, "Sudeste"]
total_2012_sud <- interval_sums1[3, "Sudeste"]
total_2013_sud <- interval_sums1[4, "Sudeste"]
total_2014_sud <- interval_sums1[5, "Sudeste"]
total_2015_sud <- interval_sums1[6, "Sudeste"]
total_2016_sud <- interval_sums1[7, "Sudeste"]
total_2017_sud <- interval_sums1[8, "Sudeste"]
total_2018_sud <- interval_sums1[9, "Sudeste"]
total_2019_sud <- interval_sums1[10, "Sudeste"]

## South
total_2010_sul <- interval_sums1[1, "Sul"]
total_2011_sul <- interval_sums1[2, "Sul"]
total_2012_sul <- interval_sums1[3, "Sul"]
total_2013_sul <- interval_sums1[4, "Sul"]
total_2014_sul <- interval_sums1[5, "Sul"]
total_2015_sul <- interval_sums1[6, "Sul"]
total_2016_sul <- interval_sums1[7, "Sul"]
total_2017_sul <- interval_sums1[8, "Sul"]
total_2018_sul <- interval_sums1[9, "Sul"]
total_2019_sul <- interval_sums1[10, "Sul"]

## Midwest
total_2010_centro <- interval_sums1[1, "Centro_Oeste"]
total_2011_centro <- interval_sums1[2, "Centro_Oeste"]
total_2012_centro <- interval_sums1[3, "Centro_Oeste"]
total_2013_centro <- interval_sums1[4, "Centro_Oeste"]
total_2014_centro <- interval_sums1[5, "Centro_Oeste"]
total_2015_centro <- interval_sums1[6, "Centro_Oeste"]
total_2016_centro <- interval_sums1[7, "Centro_Oeste"]
total_2017_centro <- interval_sums1[8, "Centro_Oeste"]
total_2018_centro <- interval_sums1[9, "Centro_Oeste"]
total_2019_centro <- interval_sums1[10, "Centro_Oeste"]

#################################################################################
## Total Hospital Cost musculoskeletal system and connective tissue per case/year
#################################################################################

## North
(custo_2010 <- (total_2010_norte <- interval_sums1[1, "Norte"])/norte_2010)
(custo_2011 <- (total_2011_norte <- interval_sums1[2, "Norte"])/norte_2011)
(custo_2012 <- (total_2012_norte <- interval_sums1[3, "Norte"])/norte_2012)
(custo_2013 <- (total_2013_norte <- interval_sums1[4, "Norte"])/norte_2013)
(custo_2014 <- (total_2014_norte <- interval_sums1[5, "Norte"])/norte_2014)
(custo_2015 <- (total_2015_norte <- interval_sums1[6, "Norte"])/norte_2015)
(custo_2016 <- (total_2016_norte <- interval_sums1[7, "Norte"])/norte_2016)
(custo_2017 <- (total_2017_norte <- interval_sums1[8, "Norte"])/norte_2017)
(custo_2018 <- (total_2018_norte <- interval_sums1[9, "Norte"])/norte_2018)
(custo_2019 <- (total_2019_norte <- interval_sums1[10, "Norte"])/norte_2019)

## Northeast
(custo1_2010 <- (total_2010_nord <- interval_sums1[1, "Nordeste"])/nord_2010)
(custo1_2011 <- (total_2011_nord <- interval_sums1[2, "Nordeste"])/nord_2011)
(custo1_2012 <- (total_2012_nord <- interval_sums1[3, "Nordeste"])/nord_2012)
(custo1_2013 <- (total_2013_nord <- interval_sums1[4, "Nordeste"])/nord_2013)
(custo1_2014 <- (total_2014_nord <- interval_sums1[5, "Nordeste"])/nord_2014)
(custo1_2015 <- (total_2015_nord <- interval_sums1[6, "Nordeste"])/nord_2015)
(custo1_2016 <- (total_2016_nord <- interval_sums1[7, "Nordeste"])/nord_2016)
(custo1_2017 <- (total_2017_nord <- interval_sums1[8, "Nordeste"])/nord_2017)
(custo1_2018 <- (total_2018_nord <- interval_sums1[9, "Nordeste"])/nord_2018)
(custo1_2019 <- (total_2019_nord <- interval_sums1[10, "Nordeste"])/nord_2019)

## Southeast
(custo2_2010 <- (total_2010_sud <- interval_sums1[1, "Sudeste"])/sud_2010)
(custo2_2011 <- (total_2011_sud <- interval_sums1[2, "Sudeste"])/sud_2011)
(custo2_2012 <- (total_2012_sud <- interval_sums1[3, "Sudeste"])/sud_2012)
(custo2_2013 <- (total_2013_sud <- interval_sums1[4, "Sudeste"])/sud_2013)
(custo2_2014 <- (total_2014_sud <- interval_sums1[5, "Sudeste"])/sud_2014)
(custo2_2015 <- (total_2015_sud <- interval_sums1[6, "Sudeste"])/sud_2015)
(custo2_2016 <- (total_2016_sud <- interval_sums1[7, "Sudeste"])/sud_2016)
(custo2_2017 <- (total_2017_sud <- interval_sums1[8, "Sudeste"])/sud_2017)
(custo2_2018 <- (total_2018_sud <- interval_sums1[9, "Sudeste"])/sud_2018)
(custo2_2019 <- (total_2019_sud <- interval_sums1[10, "Sudeste"])/sud_2019)

## South
(custo3_2010 <- (total_2010_sul <- interval_sums1[1, "Sul"])/sul_2010)
(custo3_2011 <- (total_2011_sul <- interval_sums1[2, "Sul"])/sul_2011)
(custo3_2012 <- (total_2012_sul <- interval_sums1[3, "Sul"])/sul_2012)
(custo3_2013 <- (total_2013_sul <- interval_sums1[4, "Sul"])/sul_2013)
(custo3_2014 <- (total_2014_sul <- interval_sums1[5, "Sul"])/sul_2014)
(custo3_2015 <- (total_2015_sul <- interval_sums1[6, "Sul"])/sul_2015)
(custo3_2016 <- (total_2016_sul <- interval_sums1[7, "Sul"])/sul_2016)
(custo3_2017 <- (total_2017_sul <- interval_sums1[8, "Sul"])/sul_2017)
(custo3_2018 <- (total_2018_sul <- interval_sums1[9, "Sul"])/sul_2018)
(custo3_2019 <- (total_2019_sul <- interval_sums1[10, "Sul"])/sul_2019)

## Midwest
(custo4_2010 <- (total_2010_centro <- interval_sums1[1, "Centro_Oeste"])/centro_2010)
(custo4_2011 <- (total_2011_centro <- interval_sums1[2, "Centro_Oeste"])/centro_2011)
(custo4_2012 <- (total_2012_centro <- interval_sums1[3, "Centro_Oeste"])/centro_2012)
(custo4_2013 <- (total_2013_centro <- interval_sums1[4, "Centro_Oeste"])/centro_2013)
(custo4_2014 <- (total_2014_centro <- interval_sums1[5, "Centro_Oeste"])/centro_2014)
(custo4_2015 <- (total_2015_centro <- interval_sums1[6, "Centro_Oeste"])/centro_2015)
(custo4_2016 <- (total_2016_centro <- interval_sums1[7, "Centro_Oeste"])/centro_2016)
(custo4_2017 <- (total_2017_centro <- interval_sums1[8, "Centro_Oeste"])/centro_2017)
(custo4_2018 <- (total_2018_centro <- interval_sums1[9, "Centro_Oeste"])/centro_2018)
(custo4_2019 <- (total_2019_centro <- interval_sums1[10, "Centro_Oeste"])/centro_2019)

#####################################################################
## time series plot - percentage of costs per month / year (ICD-10 M) 
#####################################################################

# Replace 'file_path.csv' with your CSV file path
file_path_prop <- "prop_custo_atual.csv"

# Read CSV file starting from row 5 with delimiter ';', specifying decimal and grouping marks
data_cidm_prop <- read_delim(file_path_prop, delim = ";")
str(data_cidm_prop) 
class(data_cidm_prop)
head(data_cidm_prop)

# Rename columns
colnames(data_cidm_prop) <- c("Time", "North", "Northeast", "Southeast", "South", "Center_West")

# Create a time series plot
plot(data_cidm_prop$Time, data_cidm_prop$North, type = "l", col = "blue", xlab = "", ylab = "percentage of Cost",
     main = "Financial Burden of Musculoskeletal & Connective Tissue Diseases: A Regional Analysis in Brazil (2009-2019)", ylim = c(0.05, 0.11),
     xlim = c(min(data_cidm_prop$Time), max(data_cidm_prop$Time)))

# Add lines for other regions
lines(data_cidm_prop$Time, data_cidm_prop$Northeast, col = "red")
lines(data_cidm_prop$Time, data_cidm_prop$Southeast, col = "green")
lines(data_cidm_prop$Time, data_cidm_prop$South, col = "purple")
lines(data_cidm_prop$Time, data_cidm_prop$Center_West, col = "orange")

# Adjust margins and plot region
par(mar = c(5, 4, 2, 2) + 0.1)

# Improved legend placement and formatting
legend("bottomright", legend = c("North", "Northeast", "Southeast", "South", "Center_West"),
       col = c("blue", "red", "green", "purple", "orange"), lty = 1, cex = 0.6, bty = "n")

# Display the plot
# Rename columns
colnames(df_prop_cidm) <- c("Time", "North", "Northeast", "Southeast", "South", "Center_West")

# Create a time series plot
plot(df_prop_cidm$Time, df_prop_cidm$North, type = "l", col = "blue", xlab = "", ylab = "Proportion",
     main = "Proportions of Musculoskeletal & Connective Tissue Diseases in Brazilian Regions: A 10-Year Analysis", ylim = c(0.06, 0.10),
     xlim = c(min(df_prop_cidm$Time), max(df_prop_cidm$Time)))

# Add lines for other regions
lines(df_prop_cidm$Time, df_prop_cidm$Northeast, col = "red")
lines(df_prop_cidm$Time, df_prop_cidm$Southeast, col = "green")
lines(df_prop_cidm$Time, df_prop_cidm$South, col = "purple")
lines(df_prop_cidm$Time, df_prop_cidm$Center_West, col = "orange")

# Adjust margins and plot region
par(mar = c(5, 4, 2, 2) + 0.1)

# Improved legend placement and formatting
legend("bottomright", legend = c("North", "Northeast", "Southeast", "South", "Center_West"),
       col = c("blue", "red", "green", "purple", "orange"), lty = 1, cex = 0.6, bty = "n")
# Display the plot

#######################################
## correlation cost vs cases (ICD-10 M) 
#######################################

# Replace 'file_path.csv' with your CSV file path
file_path_m <- "sih_cid_m_custo_total2.csv"

# Read CSV file starting from row 5 with delimiter ';', specifying decimal and grouping marks
cidm_custo_total <- read_delim(file_path_m, delim = ";", skip = 4)
str(cidm_custo_total) 
class(cidm_custo_total)
head(cidm_custo_total)

#join data sets
# Select the specified columns from each dataset
cidm_custo_total_selected <- cidm_custo_total %>%
  select(Data, Norte, Nordeste, Sudeste, Sul, Centro_Oeste)

data_cidm_sih_selected <- data_cidm_sih %>%
  select(Data, Norte, Nordeste, Sudeste, Sul, Centro_Oeste)

# Perform the join based on the common column "Data"
new_data_cidm <- inner_join(cidm_custo_total_selected, data_cidm_sih_selected, by = "Data")

# Print or save the resulting dataset
print(new_data_cidm)
# write.csv(new_data_cidm, "path/to/new_data_cidm.csv")  # Uncomment and replace with the actual path if you want to save the result

# Rename columns
colnames(new_data_cidm) <- c("Time", "Norte_custo", "Nordeste_custo", "Sudeste_custo", 
                            "Sul_custo", "Centro_Oeste_custo", "Norte_casos", "Nordeste_casos",
                            "Sudeste_casos", "Sul_casos", "Centro_Oeste_casos")
print(new_data_cidm)

# Correlations
cor.test(new_data_cidm$Norte_custo, new_data_cidm$Norte_casos, method="pearson")
cor.test(new_data_cidm$Nordeste_custo, new_data_cidm$Nordeste_casos, method="pearson")
cor.test(new_data_cidm$Sudeste_custo, new_data_cidm$Sudeste_casos, method="pearson")
cor.test(new_data_cidm$Sul_custo, new_data_cidm$Sul_casos, method="pearson")
cor.test(new_data_cidm$Centro_Oeste_custo, new_data_cidm$Centro_Oeste_casos, method="pearson")

###############################################
## PLOTS - correlation cost vs cases (ICD-10 M) 
##############################################

new_data_cidm_norte <- select(new_data_cidm, Norte_custo, Norte_casos)
pairs.panels(new_data_cidm_norte) #plot1
cor(new_data_cidm_norte,method = "pearson")

new_data_cidm_nordeste <- select(new_data_cidm, Nordeste_custo, Nordeste_casos)
pairs.panels(new_data_cidm_nordeste) #plot1
cor(new_data_cidm_nordeste,method = "pearson")

new_data_cidm_sudeste <- select(new_data_cidm, Sudeste_custo, Sudeste_casos)
pairs.panels(new_data_cidm_sudeste) #plot1
cor(new_data_cidm_sudeste,method = "pearson")

new_data_cidm_sul <- select(new_data_cidm, Sul_custo, Sul_casos)
pairs.panels(new_data_cidm_sul) #plot1
cor(new_data_cidm_sul,method = "pearson")

new_data_cidm_centro <- select(new_data_cidm, Centro_Oeste_custo, Centro_Oeste_casos)
pairs.panels(new_data_cidm_centro) #plot1
cor(new_data_cidm_centro,method = "pearson")
