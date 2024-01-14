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
## "regression_model_CVD_climate.R"
## This file is a linear regression model (cardiovascular disease in Southeast Region Brazil 
## and climatic factors and lays the groundwork for robust analysis.
## ==========================================================================================

# Install and Load the packages
install.packages("corrgram")
install.packages("pacman")
install.packages("psych")
install.packages("predictmeans")
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

# Replace 'file_path.csv' with your CSV file path
file_path_sud <- "data_dcv_clima_sud.csv"

# Read CSV file starting from row 5 with delimiter ';', specifying decimal and grouping marks
data_dcv_clima <- read_delim(file_path_sud, delim = ";")
str(data_dcv_clima) 
class(data_dcv_clima)
head(data_dcv_clima)

# Replace 'file_path.csv' with your CSV file path
file_path_sud3 <- "sim_dcv_uf_2010.csv"

# Read CSV file starting from row 5 with delimiter ';', specifying decimal and grouping marks
data_morte_dcv_uf <- read_delim(file_path_sud3, delim = ";", skip = 4)
str(data_morte_dcv_uf) 
class(data_morte_dcv_uf)
head(data_morte_dcv_uf)

#################################
## Simple Linear Regression Model
#################################

# model checking - outlier detection
# Selecting the desired independent variables
selected_vars <- c("INSOLACAO_TOTAL", "PRECIPITACAO_TOTAL", "TEMPERATURA_MAXIMA",
"TEMPERATURA_MEDIA")

# Calculating Z scores for selected variables
matriz <- data_dcv_clima %>%
  mutate_at(vars(selected_vars), ~scale(., center = TRUE, scale = TRUE))

# Melt the data for boxplot representation
matriz2 <- matriz %>%
  select(all_of(selected_vars)) %>%
  melt()

# Creating boxplots
jpeg("boxplot_outliers_dcv.jpeg", quality = 75)
matriz2 %>%
  ggplot(aes(x = variable, y = value, group = variable)) +
  geom_boxplot() +
  scale_y_continuous(limits = c(-5, 5), breaks = seq(-5, 5, 1)) +
  theme_bw() +
  labs(y = 'Z Score', x = "") +
  scale_x_discrete(labels = c('INSOLACAO_TOTAL', 'PRECIPITACAO_TOTAL', 'TEMP_MAXIMA',
                              'TEMP_MEDIA'))
dev.off()
summary(data_dcv_clima)

## Correlation matrix
matriz_correlacoes <- cor(data_dcv_clima[c("Sudeste", "INSOLACAO_TOTAL", "PRECIPITACAO_TOTAL", 
                                           "TEMPERATURA_MAXIMA", "TEMPERATURA_MEDIA")])
corrplot(matriz_correlacoes, method = "shade")

data_dcv_clima2 <- select(data_dcv_clima, Sudeste, INSOLACAO_TOTAL, PRECIPITACAO_TOTAL, TEMPERATURA_MAXIMA, 
                          TEMPERATURA_MEDIA)
pairs.panels(data_dcv_clima2) #plot1
cor(data_dcv_clima2,method = "pearson")

# Simple Linear Regression Model
par(mfrow=c(2,2))
mod1=lm(Sudeste~INSOLACAO_TOTAL, data=data_dcv_clima)
mod2=lm(Sudeste~PRECIPITACAO_TOTAL, data=data_dcv_clima)
mod3=lm(Sudeste~TEMPERATURA_MAXIMA, data=data_dcv_clima)
mod4=lm(Sudeste~TEMPERATURA_MEDIA, data=data_dcv_clima) #melhor R^2
summary(mod1) 
summary(mod2) 
summary(mod3) 
summary(mod4) 

#https://online.stat.psu.edu/stat462/node/146/ (Evaluating model assumptions)
#Residuals vs. Fits Plot
#Initially check scatter plot between Y and X
plot(data_dcv_clima$TEMPERATURA_MEDIA, data_dcv_clima$Sudeste, pch=20, xlab="Temp_Média", ylab="DCV (Sudeste)", col="darkblue")
abline(lm(data_dcv_clima$Sudeste~data_dcv_clima$TEMPERATURA_MEDIA), lwd=2, col="red")

# Get standardized residuals
std_resid <- rstandard(mod4)

# Create a scatterplot with the regression line
plot(data_dcv_clima$TEMPERATURA_MEDIA, data_dcv_clima$Sudeste, pch = 20, 
     xlab = "Temp_Média", ylab = "DCV (Sudeste)", col = "darkblue")
abline(lm(data_dcv_clima$Sudeste ~ data_dcv_clima$TEMPERATURA_MEDIA), lwd = 2, col = "red")

# Identify potential outliers using standardized residuals
outliers <- which(abs(std_resid) > 2)  # Using a threshold of 2 for example

# Mark outliers on the plot
points(data_dcv_clima$TEMPERATURA_MEDIA[outliers], data_dcv_clima$Sudeste[outliers], col = "red", pch = 20)

# Optional: Label the outliers with their observation numbers
text(data_dcv_clima$TEMPERATURA_MEDIA[outliers], data_dcv_clima$Sudeste[outliers], labels = outliers, pos = 4, col = "red")

#Residuals vs. Predictor Plot (investigate other predictors)
# Obtain residuals - temp_maxima
residuals_mod3 <- residuals(mod3)

# Create a data frame for plotting
plot_data <- data.frame(TEMPERATURA_MAXIMA = data_dcv_clima$TEMPERATURA_MAXIMA, Residuos = residuals_mod3)

# Create scatterplot with residuals vs. predictor
ggplot(plot_data, aes(x = TEMPERATURA_MAXIMA, y = Residuos)) +
  geom_point(color = "darkblue", size = 3) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(x = "Tem_Máxima", y = "Resíduos") +
  ggtitle("Resíduos vs. Temperatura Máxima")

# Identify potential outliers
outliers <- which(abs(residuals_mod3) > 2 * sd(residuals_mod3))  # Using a threshold of 2 standard deviations for example

# Mark outliers on the plot
ggplot(plot_data, aes(x = TEMPERATURA_MAXIMA, y = Residuos)) +
  geom_point(color = "darkblue", size = 3) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_point(data = plot_data[outliers, ], aes(x = TEMPERATURA_MAXIMA, y = Residuos), color = "red", size = 3) +
  labs(x = "Temp_Máxima", y = "Resíduos") +
  ggtitle("Residuals vs. TEMPERATURA_MAXIMA (with outliers)")

# Optional: Label the outliers with their observation numbers
ggplot(plot_data, aes(x = TEMPERATURA_MAXIMA, y = Residuos)) +
  geom_point(color = "darkblue", size = 3) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_point(data = plot_data[outliers, ], aes(x = TEMPERATURA_MAXIMA, y = Residuos), color = "red", size = 3) +
  geom_text(data = plot_data[outliers, ], aes(x = TEMPERATURA_MAXIMA, y = Residuos, label = outliers), color = "red", size = 3, vjust = -1) +
  labs(x = "Temp_Máxima", y = "Resíduos") +
  ggtitle("Residuals vs. TEMPERATURA_MAXIMA (with outliers labeled)")

# Obtain residuals - preciptacao_total
residuals_mod2 <- residuals(mod2)

# Create a data frame for plotting
plot_data1 <- data.frame(PRECIPITACAO_TOTAL = data_dcv_clima$PRECIPITACAO_TOTAL, Residuos = residuals_mod2)

# Create scatterplot with residuals vs. predictor
ggplot(plot_data1, aes(x = PRECIPITACAO_TOTAL, y = Residuos)) +
  geom_point(color = "darkblue", size = 3) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(x = "PRECIPITACAO TOTAL", y = "Resíduos") +
  ggtitle("Resíduos vs. PRECIPITACAO TOTAL")

# Identify potential outliers
outliers <- which(abs(residuals_mod2) > 2 * sd(residuals_mod2))  # Using a threshold of 2 standard deviations for example

# Mark outliers on the plot
ggplot(plot_data1, aes(x = PRECIPITACAO_TOTAL, y = Residuos)) +
  geom_point(color = "darkblue", size = 3) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_point(data = plot_data1[outliers, ], aes(x = PRECIPITACAO_TOTAL, y = Residuos), color = "red", size = 3) +
  labs(x = "PRECIPITACAO_TOTAL", y = "Resíduos") +
  ggtitle("Residuals vs. PRECIPITACAO_TOTAL (com outliers)")

# Optional: Label the outliers with their observation numbers
ggplot(plot_data1, aes(x = PRECIPITACAO_TOTAL, y = Residuos)) +
  geom_point(color = "darkblue", size = 3) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_point(data = plot_data1[outliers, ], aes(x = PRECIPITACAO_TOTAL, y = Residuos), color = "red", size = 3) +
  geom_text(data = plot_data1[outliers, ], aes(x = PRECIPITACAO_TOTAL, y = Residuos, label = outliers), color = "red", size = 3, vjust = -1) +
  labs(x = "PRECIPITACAO TOTAL", y = "Resíduos") +
  ggtitle("Residuals vs. PRECIPITACAO TOTAL (com outliers rotulados)")

#Residuals vs. Predictor Plot (investigate other predictors)
# Obtain residuals - temp_maxima
residuals_mod3 <- residuals(mod3)

# Create a data frame for plotting
plot_data <- data.frame(TEMPERATURA_MAXIMA = data_dcv_clima$TEMPERATURA_MAXIMA, Residuos = residuals_mod3)

# Create scatterplot with residuals vs. predictor
ggplot(plot_data, aes(x = TEMPERATURA_MAXIMA, y = Residuos)) +
  geom_point(color = "darkblue", size = 3) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(x = "Tem_Máxima", y = "Resíduos") +
  ggtitle("Resíduos vs. Temperatura Máxima")

# Identify potential outliers
outliers <- which(abs(residuals_mod3) > 2 * sd(residuals_mod3))  # Using a threshold of 2 standard deviations for example

# Mark outliers on the plot
ggplot(plot_data, aes(x = TEMPERATURA_MAXIMA, y = Residuos)) +
  geom_point(color = "darkblue", size = 3) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_point(data = plot_data[outliers, ], aes(x = TEMPERATURA_MAXIMA, y = Residuos), color = "red", size = 3) +
  labs(x = "Temp_Máxima", y = "Resíduos") +
  ggtitle("Residuals vs. TEMPERATURA_MAXIMA (with outliers)")

# Optional: Label the outliers with their observation numbers
ggplot(plot_data, aes(x = TEMPERATURA_MAXIMA, y = Residuos)) +
  geom_point(color = "darkblue", size = 3) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_point(data = plot_data[outliers, ], aes(x = TEMPERATURA_MAXIMA, y = Residuos), color = "red", size = 3) +
  geom_text(data = plot_data[outliers, ], aes(x = TEMPERATURA_MAXIMA, y = Residuos, label = outliers), color = "red", size = 3, vjust = -1) +
  labs(x = "Temp_Máxima", y = "Resíduos") +
  ggtitle("Residuals vs. TEMPERATURA_MAXIMA (with outliers labeled)")

# Obtain residuals - insolacao_total variable
residuals_mod1 <- residuals(mod1)

# Create a data frame for plotting
plot_data2 <- data.frame(INSOLACAO_TOTAL = data_dcv_clima$INSOLACAO_TOTAL, Residuos = residuals_mod1)

# Create scatterplot with residuals vs. predictor
ggplot(plot_data2, aes(x = INSOLACAO_TOTAL, y = Residuos)) +
  geom_point(color = "darkblue", size = 3) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(x = "INSOLAÇÃO TOTAL", y = "Resíduos") +
  ggtitle("Resíduos vs. INSOLACAO TOTAL")

# Identify potential outliers
outliers <- which(abs(residuals_mod1) > 2 * sd(residuals_mod1))  # Using a threshold of 2 standard deviations for example

# Mark outliers on the plot
ggplot(plot_data2, aes(x = INSOLACAO_TOTAL, y = Residuos)) +
  geom_point(color = "darkblue", size = 3) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_point(data = plot_data2[outliers, ], aes(x = INSOLACAO_TOTAL, y = Residuos), color = "red", size = 3) +
  labs(x = "INSOLACAO_TOTAL", y = "Resíduos") +
  ggtitle("Residuals vs. INSOLACAO_TOTAL (com outliers)")

# Optional: Label the outliers with their observation numbers
ggplot(plot_data2, aes(x = INSOLACAO_TOTAL, y = Residuos)) +
  geom_point(color = "darkblue", size = 3) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_point(data = plot_data2[outliers, ], aes(x = INSOLACAO_TOTAL, y = Residuos), color = "red", size = 3) +
  geom_text(data = plot_data2[outliers, ], aes(x = INSOLACAO_TOTAL, y = Residuos, label = outliers), color = "red", size = 3, vjust = -1) +
  labs(x = "INSOLAÇÃO TOTAL", y = "Resíduos") +
  ggtitle("Residuals vs. INSOLAÇÃO TOTAL (com outliers rotulados)")

# SUMMARY: IT’S WORTH ADDING TOTAL RASH TO THE MODEL
# adjusting the model by removing outliers (61,98,115 data points)
# Indices of influential points
influential_points_1 <- c(61, 98, 115)

# Exclude influential points by row indices
data_sem_influente <- data_dcv_clima[-influential_points_1, ]

# Fit the adjusted multiple regression model
mod4_sem_influente <- lm(Sudeste ~ TEMPERATURA_MEDIA, 
                                 data = data_sem_influente)
summary(mod4_sem_influente)

#Residuals vs. Fits Plot (without outliers)
#Initially check scatter plot between Y and X
plot(data_sem_influente$TEMPERATURA_MEDIA, data_sem_influente$Sudeste, pch=20, xlab="Temp_Média", ylab="DCV (Sudeste)", col="darkblue")
abline(lm(data_sem_influente$Sudeste~data_sem_influente$TEMPERATURA_MEDIA), lwd=2, col="red")

# Get standardized residuals
std_resid_sem <- rstandard(mod4_sem_influente)

# Create a scatterplot with the regression line
plot(data_sem_influente$TEMPERATURA_MEDIA, data_sem_influente$Sudeste, pch = 20, 
     xlab = "Temp_Média", ylab = "DCV (Sudeste)", col = "darkblue")
abline(lm(data_sem_influente$Sudeste~data_sem_influente$TEMPERATURA_MEDIA), lwd = 2, col = "red")

# Identify potential outliers using standardized residuals
outliers <- which(abs(std_resid_sem) > 3)  # Using a threshold of 2 for example

# Mark outliers on the plot
points(data_sem_influente$TEMPERATURA_MEDIA[outliers], data_sem_influente$Sudeste[outliers], col = "red", pch = 20)

# Residual Plots
par(mfrow=c(2,2))
plot(mod4_sem_influente)

# Conclusion: there is a violation of the independence of errors (see plot)
# don't follow a simple model.

# To calculate the value of y^(??) - predict function or estimate the value accordingly
# with parameter estimates:
# fit16_1$coefficients gives us the fitted coefficients, with fit16_1$coefficients[1] 
# being the intercept and fit16_1$coefficients[2] being the slope.
y_28=mod4$coefficients[1]+mod4$coefficients[2]*28
y_28

predict(object = mod4)

newdata=data.frame(TEMPERATURA_MEDIA=28) # `data.frame` contém os dados das variáveis explicativas utilizados para calcular as previsões.
predict(mod4, newdata)

residuos <- resid(mod4)
SQRes=sum(residuos^2)
S2e=SQRes/(120-2)

SQRes
S2e
sqrt(S2e)
2*sqrt(S2e)
s2<-var(data_dcv_clima$Sudeste)
s2

# Variance Analysis
anova(mod4)

# Residual Plots
par(mfrow=c(2,2))
plot(mod4)

par(mfrow=c(1,2))
residuos <- resid(mod4)
plot(data_dcv_clima2$TEMPERATURA_MEDIA, residuos)
title("Resíduos vs Temperatura Média")
plot(data_dcv_clima2$TEMPERATURA_MAXIMA, residuos)
title("Resíduos vs TEMPERATURA_MAXIMA")

# IC for coefficients with the confint function
confint(mod4)
# Prediction IC using interval="predict"
newdata=data.frame(TEMPERATURA_MEDIA=28)  # `data.frame` contém os dados das variáveis explicativas utilizados para calcular as previsões.
predict(mod4, newdata, interval="predict") 

zi=residuos/sqrt(S2e)
par(mfrow=c(1,2))
plot(data_dcv_clima2$TEMPERATURA_MEDIA,residuos, cex=2, col="darkblue", pch=20, 
     xlab="Temp_Média", ylab="Resíduos",main="(a)")
abline(h=0)
plot(data_dcv_clima2$TEMPERATURA_MEDIA,zi, cex=2, col="darkblue", pch=20, 
     xlab="Temp_Média", ylab="Resíduos padronizados",main="(b)")
abline(h=0)

par(mfrow=c(1,1))
hist(residuos, xlab="Resíduos", ylab="", col="lightblue3", border="white", main="")

qqnorm(residuos, cex=1.5, col="darkblue", xlab="Quantis da normal padrão", 
       ylab="Quantis dos resíduos", pch=20)
qqline(residuos, col="darkred")

#QQ plot with confidence band
# Obtain standardized residuals
residuals_std <- rstandard(mod4)

# Create QQ plot with confidence bands
qqPlot(residuals_std, main = "QQ Plot de Resíduos Padronizados",
       xlab = "Quantis da distribuição normal",
       ylab = "Quantis dos resíduos padronizados")

# Add confidence bands
qqline(residuals_std, distribution = qnorm, probs = c(0.25, 0.75))


#Check if the errors are independent with constant variance
plot(fitted(mod4), residuals(mod4),xlab = "valores preditos", ylab = "resíduos" , pch=20)
abline(h=0, lty=2, lwd=1.5)
lines(smooth.spline(fitted(mod4), residuals(mod4)), col="red", lwd=2)

#Hypotheses tests for adjustment to normal distribution
#Anderson-Darling Test
# Assuming you have your multiple regression model fitted and residuals calculated
residuals_mod4 <- residuals(mod4)

# Perform Anderson-Darling Test for residual normality
ad_test <- ad.test(residuals_mod4)
ad_test

# Shapiro-Wilk's Test
shapiro.test(mod4$residuals)

#KS's test with Lilliefors correction.
lillie.test(mod4$residuals)

# model training
# Set the seed for reproducibility
set.seed(0)

# Create an index for splitting data into training and testing sets
index <- createDataPartition(data_dcv_clima$Sudeste, p = 0.8, list = FALSE)

# Split the data into training and testing sets based on the index
train_data <- data_dcv_clima[index, ]
test_data <- data_dcv_clima[-index, ]

# Create a linear regression model using 'peso' to predict 'DCV - sUDESTE'
fit_1 <- lm(Sudeste ~ TEMPERATURA_MEDIA, data = train_data)

# Predict 'DCV' values using the test data
predictions <- predict(fit_1, newdata = test_data)

# Evaluate model performance (you can use various metrics here)
# For example, calculate RMSE (Root Mean Squared Error)
rmse <- sqrt(mean((test_data$Sudeste - predictions)^2))
# View the RMSE
print(paste("Root Mean Squared Error (RMSE):", rmse))
MAE <- mean((test_data$Sudeste - predictions)^2)
MAE

###################################
## Multiple Linear Regression Model
###################################

mod_multiplo1=lm(Sudeste~INSOLACAO_TOTAL+PRECIPITACAO_TOTAL+TEMPERATURA_MAXIMA
                 +TEMPERATURA_MEDIA, data=data_dcv_clima)
mod_multiplo2=lm(Sudeste~INSOLACAO_TOTAL+PRECIPITACAO_TOTAL+TEMPERATURA_MAXIMA, 
                 data=data_dcv_clima)
mod_multiplo3=lm(Sudeste~INSOLACAO_TOTAL+PRECIPITACAO_TOTAL+TEMPERATURA_MEDIA,
                 data=data_dcv_clima)
mod_multiplo4=lm(Sudeste~INSOLACAO_TOTAL+TEMPERATURA_MAXIMA,data=data_dcv_clima)
mod_multiplo5=lm(Sudeste~INSOLACAO_TOTAL+TEMPERATURA_MEDIA,data=data_dcv_clima)
mod_multiplo6=lm(Sudeste~PRECIPITACAO_TOTAL+TEMPERATURA_MEDIA,data=data_dcv_clima)
summary(mod_multiplo1) 
summary(mod_multiplo2) 
summary(mod_multiplo3) 
summary(mod_multiplo4) 
summary(mod_multiplo5) # the best
summary(mod_multiplo6) 

# Variance Analysis
anova(mod_multiplo5) 

# Residual Plots
par(mfrow=c(2,2))
plot(mod_multiplo1)

par(mfrow=c(2,2))
plot(mod_multiplo2)

par(mfrow=c(2,2))
plot(mod_multiplo3)

par(mfrow=c(2,2))
plot(mod_multiplo4)

par(mfrow=c(2,2))
plot(mod_multiplo5)

par(mfrow=c(2,2))
plot(mod_multiplo6)

par(mfrow=c(1,1))
#QQ plot with confidence band
# Obtain standardized residuals
residuals_std <- rstandard(mod_multiplo5)

# Create QQ plot with confidence bands
qqPlot(residuals_std, main = "QQ Plot de Resíduos Padronizados",
       xlab = "Quantis da distribuição normal",
       ylab = "Quantis dos resíduos padronizados")

# Add confidence bands
qqline(residuals_std, distribution = qnorm, probs = c(0.25, 0.75))

# Calculate Cook's distance
cooksd <- cooks.distance(mod_multiplo5)

# Create plot for Cook's distance
plot(cooksd, pch = 20, main = "Gráfico da Distância de Cook",
     xlab = "Observação", ylab = "Distância de Cook")

# Identify influential points (optional)
influential_points <- which(cooksd2 > 4 / length(cooksd2)) # threshold for influential points
if (length(influential_points) > 0) {
  points(influential_points, cooksd2[influential_points], col = "red", pch = 20)
}

#Hypotheses tests for adjustment to normal distribution
# Shapiro-Wilk test
shapiro.test(mod_multiplo5$residuals)

#KS test with Lilliefors correction.
lillie.test(mod_multiplo5$residuals)

#adjustment with exclusion of influential points
# Indices of influential points
influential_points <- c(4,28,61,78)

# Exclude influential points by row indices
data_sem_influente <- data_dcv_clima[-influential_points, ]

# Fit the adjusted multiple regression model
mod_multiplo_sem_influente <- lm(Sudeste ~ INSOLACAO_TOTAL + TEMPERATURA_MEDIA, 
                                 data = data_sem_influente)
summary(mod_multiplo_sem_influente)

# Residual plots
par(mfrow=c(2,2))
plot(mod_multiplo_sem_influente)

#QQ plot with confidence band (with influential points)
# Obtain standardized residuals
residuals_std2 <- rstandard(mod_multiplo_sem_influente)

# Create QQ plot with confidence bands
qqPlot(residuals_std2, main = "QQ Plot de Resíduos Padronizados",
       xlab = "Quantis da distribuição normal",
       ylab = "Quantis dos resíduos padronizados")

# Add confidence bands
qqline(residuals_std2, distribution = qnorm, probs = c(0.25, 0.75))


# Calculate Cook's distance
cooksd2 <- cooks.distance(mod_multiplo_sem_influente)

# Create plot for Cook's distance (sem pontos influentes)
plot(cooksd2, pch = 20, main = "Gráfico da Distância de Cook",
     xlab = "Observação", ylab = "Distância de Cook")

# Identify influential points (optional)
influential_points <- which(cooksd2 > 4 / length(cooksd2)) # threshold for influential points
if (length(influential_points) > 0) {
  points(influential_points, cooksd2[influential_points], col = "red", pch = 20)
}

# Assuming you have your multiple regression model fitted and residuals calculated
residuals_mod5 <- residuals(mod_multiplo_sem_influente)

#histogram - residuals (without outliers)
par(mfrow=c(1,1))
hist(residuals_mod5, xlab="Resíduos", ylab="", col="lightblue3", border="white", main="")

#Anderson-Darling Test
# Perform Anderson-Darling Test for residual normality
ad_test <- ad.test(residuals_mod5)
ad_test

# Shapiro-Wilk's test
shapiro.test(residuals_mod5)

#KS test with Lilliefors correction.
lillie.test(residuals_mod5)

#Tests for Constant Error Variance
#F-test
# Obtain residuals from the model
residuals_mod <- residuals(mod_multiplo_sem_influente)

# Sort residuals based on predictor variable (e.g., INSOLACAO_TOTAL)
sorted_resid <- residuals_mod[order(data_sem_influente$INSOLACAO_TOTAL)]

# Determine the split point (median or any other quantile)
split_point <- median(data_sem_influente$INSOLACAO_TOTAL)

# Divide residuals into two groups based on predictor variable
low_resid <- sorted_resid[data_sem_influente$INSOLACAO_TOTAL <= split_point]
high_resid <- sorted_resid[data_sem_influente$INSOLACAO_TOTAL > split_point]

# Perform F-test for constant error variance
var_low <- var(low_resid)
var_high <- var(high_resid)

# Calculate F-statistic and p-value
f_statistic <- var_high / var_low
df1 <- length(high_resid) - 1
df2 <- length(low_resid) - 1
p_value <- pf(f_statistic, df1, df2, lower.tail = FALSE)

# Display results
cat("F-statistic:", f_statistic, "\n")
cat("Degrees of freedom (numerator):", df1, "\n")
cat("Degrees of freedom (denominator):", df2, "\n")
cat("p-value:", p_value, "\n")

#perform Breusch-Pagan Test
bptest(mod_multiplo_sem_influente)

#Confidence Interval for the Mean Response
# New observations: Values for predictor variables
new_data <- data.frame(
  INSOLACAO_TOTAL = 202.17,TEMPERATURA_MEDIA = 22.70)  # Insert the value for TEMPERATURA_MEDIA

# Calculate confidence interval for mean response
mean_response_ci <- predict(mod_multiplo_sem_influente, newdata = new_data, interval = "confidence", level = 0.95)

# Calculate prediction interval for new observations
prediction_interval <- predict(mod_multiplo_sem_influente, newdata = new_data, interval = "prediction", level = 0.95)

# Display results
cat("Values of Predictors for New Observations:\n")
print(new_data)

cat("\n95% Confidence Interval for Mean Response:\n")
print(mean_response_ci)

cat("\nPredicted Values for New Observations (Sudeste):\n")
print(prediction_interval)
