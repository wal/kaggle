# WIP
## Questions
## Q: What is a conversion ?
## Q: Are users represented multiple times ? 
## Q: What defines a unique user ? repeat user ? 


# QUESTION

## Q: What specifically is the research objective / question ?
### A: To analyse the results of an A/B test for an e-commerce website. 

## Q: Why? - Do you understand the scientific / business application ?
### A: New website layout designed, want to understand its impact on conversions..

## Q: What type of analysis is required ?
### Descriptive, Exploratory, Inferential, Predictive, Causal, Mechanistic
### A: Inferential - What is the impact of the new design ?

## Q: What is the objective for successful project ? 
### A: Decision : Flip to new design, remain on old design, run another test ?

## Q: What is the metric for success ? - What does the metric mean ? 
### A: Conversions


# DATA
## Q: What / Where is the data ?
## Q: What kind of data is it ? (tabular, text, binary ?)
### A: Tabular data, conversions control/treatment represented

## Q: How was the data collected / Who collected it ?
### A: Collected using usage tool

## Q: Is it sampled / complete / within subjects - between subjects ?
##### A: Represents all traffic in time period

## Q: Is there a code book / description of the data anywhere ?
##### A: No, but its simple (5 variabes, obvious names)

## Q: Is it possible to answer the question using this data ?
##### A: Believe so


library(tidyverse)
library(plyr)

# IMPORT DATA
## Q: How to read in the data ? (Direct / Indirect )
### A: Data Supplied as CSV file


data <- read_csv("ab_testing/data/raw/ab_data.csv")
## Q: What are the dimensions of the data ?
dim(data)

## Q: Variables summary
glimpse(data)

## Q: Create a variables table (Name, type, #unique_values, #missing, %missing)
create_variables_table <- function(data) {
  types <- data %>% summarise_all(function(x) class(x)[[1]]) %>% gather(Name, class)
  unique_values <- data %>% summarise_all(function(x) length(unique(x))) %>% gather(Name, `Unique Values`)
  missing_count   <- data %>% summarise_all(function(x) sum(is.na(x))) %>% gather(Name, `Missing Data Count`)
  missing_percent <- data %>% summarise_all(function(x) round(mean(is.na(x)) * 100, 1)) %>% gather(Name, `Missing Data %`)
  
  join_all(list(types, unique_values, missing_count, missing_percent), by='Name', type='left')  
}

create_variables_table(data) %>% arrange(Name)

## Q: Is the data tidy ?
### A: Yes!


# EDA

## Response Variable
### Variable: converted

### Description & Unit
#### A: Boolean: Did that user convert ?
#### A: Should be a factor
data$converted <- factor(data$converted)

### Distribution
ggplot(data, aes(converted)) + geom_bar()

#### Intuitive, Outliers, Errors, Patterns ?
table(data$converted)

##### A: 
#### Transformations


## NUMERIC VARIABLES

### Q: Missing Data ? - A: No missing data!

### Q: Correlation to Target Variable ?
#### A: There are no numeric variables to correlate against the target variable

#### Per Variable EDA (in order of correlation)


##### Variable: user_id
##### Description & Unit
###### A: Represents the unique user id ?
##### Distribution v Target Variable
summary(data$user_id)
length(unique(data$user_id))/ length(data$user_id) #### 98.6% unique values - some users repeated?, what does non unique user_id mean ?

ggplot(data, aes(converted, user_id)) + 
  geom_boxplot() +
  geom_point(position="jitter", aes(color = converted), alpha = 0.025)
  # converted / not converted distributed similarly across user_ids

###### Intuitive, Outliers, Errors, Patterns ?
summary(data$user_id)
max(data$user_id) - min(data$user_id)
###### A: Range of user_ids from 630000 - 945999 

##### Transformations
##### A: N/A

###### Binnable ?
###### A: N/A - No data other than id to determine bins



## Non Numeric Data
### Q: Missing Data ?
### Per Variable EDA (in order of correlation)
### Variable: group
#### Description & Unit
##### A: Represents the treatment and control groups - factor
data$group <- factor(data$group)

#### Distribution v Target Variable
table(data$group, data$converted)

data %>% 
  group_by(converted, group) %>% 
  dplyr::summarise(n = n()) %>%
  ggplot(aes(converted, group)) + 
  geom_tile(aes(fill = n), color = "white") +
  scale_fill_gradient(low = "white", high = "steelblue") # Most data is unonverted treatment * control

##### Intuitive, Outliers, Errors, Patterns ? - A: N/A

#### Transformations - A: N/A
##### Binnable ? - A: N/A
#### Reduce number of Bins - A: N/A

### Variable: landing_page
#### Description & Unit
##### Represents the landing page for the user - factor
data$landing_page <- factor(data$landing_page)

#### Distribution v Target Variable
table(data$landing_page, data$converted)

##### Intuitive, Outliers, Errors, Patterns ? - A: N/A
#### Transformations - A: N/A
#### Reduce number of Bins


### Variable: timestamp
#### Description & Unit
##### Timestamp of the user interaction - Date
#### Distribution v Target Variable
ggplot(data, aes(timestamp)) + 
  geom_histogram(bins = 50) +
  facet_grid(group ~ converted) # Seems a reasonably constant rate of conversions / not converted per group

##### Intuitive, Outliers, Errors, Patterns ? - A: N/A
#### Transformations - A: N/A
##### Binnable ? - A: N/A
#### Reduce number of Bins - A: N/A

# INFERENCE / ANALYSIS

## Are the proportions in the treatment and control groups the same, or statistically significantly different ?
prop.test(table(data$group, data$converted))  # p = .2182

## How did this p.value change over the course of the experiment ?

time_sorted_data <- data %>% arrange(timestamp)

expiriment_over_time <- purrr::map_df(seq(1000, nrow(data), by = 500), function(x) {
  data_to_test <- time_sorted_data %>% head(x)
  conversion_rates = data_to_test %>% group_by(group) %>% dplyr::summarise(conv_rate = mean(converted == 1))

  tibble(
    observation = x,
    control_conversion_rate = conversion_rates %>% filter(group == "control") %>% .$conv_rate,
    treatment_conversion_rate = conversion_rates %>% filter(group == "treatment") %>% .$conv_rate,
    p_value = prop.test(table(data_to_test$group, data_to_test$converted))$p.value
  )
})

expiriment_over_time %>%
  ggplot(aes(observation, p_value)) + 
  geom_point() +
  geom_line() +
  geom_line(aes(y = control_conversion_rate), color = "red") +
  geom_line(aes(y = treatment_conversion_rate), color = "green") +
  geom_hline(yintercept = 0.05, color = "red")
