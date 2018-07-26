rm(list=ls())
library(tidyverse)

# Objective : Predict the sales price for each house. SalePrice variable
# Evaluation Metric : RMSE log(predictedSalePrice) v log(SalePrice)

# Figure out what to do about missing data
## Choose a strategy - delete or impute ? 
## 
# Look for correlation with the numeric variables
## choose only those with a reasonable correlatio threshold (> 0.4 ?)
## Plot those distributions & relationships with target variable
## Look for outliers

# Determine the categorical variables
# Encode them as factors
# Change some numeric variables to factors
# Re-check correlations


# Use random forrect for importance ?


test_data <- read_csv("house_prices/data/train.csv")
dim(test_data)
dim(test_data)
glimpse(test_data)

# Examine Missing Data
## What % of rows missing data
sum(!complete.cases(test_data)) / nrow(test_data)


## What % of columns are missing data?
missing_values <- test_data %>% 
  summarise_all(function(x) round(mean(is.na(x) * 100),1)) %>%
  select_if(function(x) sum(x) > 0) %>% 
  t() %>%
  as.tibble(rownames = "Variable") %>%
  rename(Missing=V1) %>%
  arrange(desc(Missing))

# Assumption 1:  Just blindly delete the missing data columns for now
test_data <- test_data %>% select(-one_of(missing_values$Variable))
sum(!complete.cases(test_data)) / nrow(test_data)
nrow(test_data)
dim(test_data)




# EDA on variables
## EDA Numeric Variables
# Find the correlation between all numeric variables and the output variable
# Proceed only with those above a certain threshold


# Select the numeric values
test_data.numeric_variables <- test_data %>% select_if(is.numeric) %>% select(-Id)
glimpse(test_data.numeric_variables)
sort(names(test_data.numeric_variables))

dim(test_data.numeric_variables)

summary(test_data.numeric_variables)
library(reshape2)
highly_correlated_variables <- melt(cor(test_data.numeric_variables)) %>% 
  filter(Var1 == "SalePrice", value > 0.5) %>%
  arrange(desc(value)) %>%
  .$Var2 %>%
  as.character()

library(GGally)
test_data.numeric_variables %>% 
  select(one_of(highly_correlated_variables)) %>% 
  ggpairs()


test_data.numeric_variables$OverallQual <- factor(test_data.numeric_variables$OverallQual)
model <- lm(log(SalePrice) ~ OverallQual + GrLivArea, test_data.numeric_variables)

summary(model)

library(modelr)
ss <- add_residuals(test_data.numeric_variables, model)


View(ss)
ggplot(ss, aes(log(SalePrice), resid)) + geom_point()
# Examine the Sale Price - output variable - Is it normally distributed
ggplot(test_data, aes(SalePrice)) + 
  geom_histogram(bins = 100) +
  geom_freqpoly(bins = 50)

ggplot(test_data, aes(sample = SalePrice)) + 
  geom_qq() +
  geom_qq_line()




# Slightly Right Skewed
test_data$lSalePrice <- log(test_data$SalePrice)
ggplot(test_data, aes(lSalePrice)) + 
  geom_histogram(bins = 100) +
  geom_freqpoly(bins = 50)

ggplot(test_data, aes(sample = lSalePrice)) + 
  geom_qq() +
  geom_qq_line()

