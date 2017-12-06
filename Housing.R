# Set the working directory in order to pull in data
setwd("C:/Users/cpelkey/Desktop/Data/Housing")

# Load the appropriate libraries
library(readr)
library(dplyr)
library(ggplot2)
library(corrplot)

# Load the training data
train_big <- read_csv("C:/Users/cpelkey/Desktop/Data/Housing/train.csv")

# Split the larger training data into training and evaluation data
smp_size <- floor(0.7 * nrow(train_big))

set.seed(1127)
train_ind <- sample(seq_len(nrow(train_big)), size = smp_size)

train <- train_big[train_ind, ]
test <- train_big[-train_ind, ]

# Begin the exploratory data analysis 
na_count <- sapply(train, function(x) sum(is.na(x)))
sum(na_count > 0)
colnames(train)[colSums(is.na(train)) > 0]

# # This reveals there are 18 variables with missing data
# "LotFrontage"  "Alley"        "MasVnrType"   "MasVnrArea"   "BsmtQual"    
# "BsmtCond"     "BsmtExposure" "BsmtFinType1" "BsmtFinType2" "FireplaceQu" 
# "GarageType"   "GarageYrBlt"  "GarageFinish" "GarageQual"   "GarageCond"  
# "PoolQC"       "Fence"        "MiscFeature"

summary(train)

class_variables <- sapply(train, function(x) class(x))
categorical_variables <- colnames(train)[class_variables == 'character']
continuous_variables <- colnames(train)[class_variables == 'integer']

# Make the appropriate variable into factors
sapply(train[categorical_variables], as.factor)
train$YearBuilt <- as.factor(train$YearBuilt)
train$YearRemodAdd <- as.factor(train$YearRemodAdd)

# Visualize the dependent variable to check for outliers
train %>% select(Neighborhood, SalePrice) %>% ggplot(aes(factor(Neighborhood), SalePrice)) + geom_boxplot() + theme(axis.text.x = element_text(angle = 90, hjust =1)) + xlab('Neighborhoods')

# Correlations of continuous variables
continuous_variables_zero <- train[continuous_variables]
continuous_variables_zero[is.na(continuous_variables_zero)] <- 0
continuous_variables_zero <- sapply(continuous_variables_zero, function(x) as.numeric(x))

corr_matrix <- cor(continuous_variables_zero, use="pairwise.complete.obs")
corrplot(corr_matrix, method = 'square')
