##-------------------------
## load libraries
##-------------------------

library(readstata13)  #imports .dta files from Stata 13 thru 15
library(tidyverse)
library(dplyr)
library(ggplot2)
library(broom)
## -----------------------------------------------------------------------------
## directory paths
## -----------------------------------------------------------------------------

getwd()

data3_1990 <- read.dta13("1990_v3.dta")
colnames(data3_1990)
data3_1990 <- data3_1990 %>% rename(birthyr = cn1990a_birthy)
data3_1990 <- data3_1990 %>% rename(age = cn1990a_age)
##data3 <- data3 %>% rename(sex = cn1990a_sex)


# Find unique non-numeric values in edattain
unique(data3_1990$edattain)
unique(data3_1990$edattain_numeric)
data3_1990 <- subset(data3_1990, edattain != "niu (not in universe)")

data3_1990$edattain_numeric <- as.character(data3_1990$edattain)

unique(data3_1990$edattain_numeric)

#Create a mapping from categories to numeric values;treating university completed as also secondary completed
levels_mapping <- c(
  "less than primary completed" = 1,
  "primary completed" = 2,
  "secondary completed" = 3,
  "university completed" = 3
)

# Ensure 'edattain' is a factor and use its levels to map to numeric values
data3_1990$edattain_n <- sapply(data3_1990$edattain_numeric, function(x) levels_mapping[x])

unique(data3_1990$edattain_n)
#Convert birthyear to Numeric:
data3_1990$birthyr <- as.numeric(as.character(data3_1990$birthyr))

#############
#assume that people are from the province they residue in 1985. show unique value of province 
data3_1990 <- data3_1990 %>% rename(provin = geo1_cn1990)
unique_provin <- unique(data3_1990$provin)
print(unique_provin)

##exclude the birthplace equals 'zhejiang', 'tibet', 'chongqing', 'hong kong, macau, taiwan, or foreign country', which do not have implement year info
excluded_locations <- c('zhejiang', 'tibet', 'chongqing', 'hong kong, macau, taiwan, or foreign country')

data4_1990 <- data3_1990 %>%
  filter(!provin %in% excluded_locations)

unique_provin4 <- unique(data4_1990$provin)
print(unique_provin4)

##mapping province implementation year 
policy_imp_years <- list(
  'beijing' = 1971,
  'liaoning' = 1971, 
  'hubei' = 1972, 
  'shandong' = 1972, 
  'zhejiang' = 1973,
  'shanxi' = 1973, 
  'hebei' = 1972, 
  'jiangsu' = 1970, 
  'hunan' = 1974, 
  'tianjin' = 1972, 
  'shanghai' = 1973, 
  'guangdong' = 1970, 
  'sichuan' = 1971, 
  'henan' = 1974, 
  'ningxia' = 1973, 
  'anhui'= 1974, 
  'jilin'= 1971,
  'heilongjiang' = 1972,
  'shaanxi'= 1973, 
  'inner mongolia' = 1979, 
  'guizhou' = 1971, 
  'gansu'= 1971,
  'guangxi'= 1971, 
  'fujian' = 1971, 
  'yunnan' = 1972, 
  'qinghai'= 1972, 
  'jiangxi' = 1972, 
  'xinjiang' = 1975, 
  'hainan' = 1969
)

# Add policy_imp_years to data4
data4_1990$imp_y <- sapply(data4_1990$provin, function(provin) policy_imp_years[[tolower(provin)]])
unique(data4_1990$imp_y)

##construct the relevant year for the event studies

data4_1990$imp_y <- as.numeric(as.character(data4_1990$imp_y))
data4_1990$j <- data4_1990$birthyr - data4_1990$imp_y


data4_1990 <- data4_1990%>%
  mutate(sex_dummy = ifelse(cn1990a_sex == "female", 1, ifelse(cn1990a_sex == "male", 0, NA)))


#######################################regression model############################################# 
data7_1990 <-data4_1990

# assign 1 to 'early' where 'imp_y' is 1972 or less, 0 where 'imp_y' is 1977 or more, and NA for years between 1973 and 1976.
data7_1990$early <- ifelse(data7_1990$imp_y <= 1971, 1, ifelse(data7_1990$imp_y >= 1974, 0, NA))

data7_1990$age <- as.numeric(as.character(data7_1990$age))

# assign 1 to 'after' where people are still at the educational age (5-25)
data7_1990$after <- ifelse(data7_1990$age >= 5 & data7_1990$age <= 25, 1, 0)

##select appropriate data set
above_18 <- subset(data7_1990, age > 18)


##regression
model_1 <- lm(edattain_n ~ early + after + early:after, data = above_18)
summary(model_1)

# Subset data for males and females
data_male <- subset(above_18, sex_dummy == 0)
data_female <- subset(above_18, sex_dummy == 1)


################################vseparate regression for male and female############################
# Fit the model for males
model_male <- lm(edattain_n ~ early + after + early:after, data = data_male)

# Fit the model for females
model_female <- lm(edattain_n ~ early + after + early:after, data = data_female)

# Output the summary of the model for males
summary(model_male)

# Output the summary of the model for females
summary(model_female)

#plot 
tidy_male <- tidy(model_male) %>% 
  mutate(gender = "Male")

tidy_female <- tidy(model_female) %>% 
  mutate(gender = "Female")

# Combine the results
results <- bind_rows(tidy_male, tidy_female)

# Filter to exclude the Intercept and focus only on coefficients of interest
results_regression <- results %>%
  filter(term != "(Intercept)" & grepl("early|after", term))

# Plotting the coefficients
coeff_plot <- ggplot(results_regression, aes(x = term, y = estimate, fill = gender)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  geom_errorbar(aes(ymin = estimate - std.error, ymax = estimate + std.error), 
                position = position_dodge(width = 0.8), width = 0.25) +
  labs(title = "Comparison of Regression Coefficients by Gender",
       x = "Variables",
       y = "Coefficient Estimate",
       fill = "Gender") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(coeff_plot)

# Fit the model for males
model_male <- lm(edattain_n ~ early + after + early:after, data = data_male)

# Fit the model for females
model_female <- lm(edattain_n ~ early + after + early:after, data = data_female)

# Output the summary of the model for males
summary(model_male)

# Output the summary of the model for females
summary(model_female)

#plot 
tidy_male <- tidy(model_male) %>% 
  mutate(gender = "Male")

tidy_female <- tidy(model_female) %>% 
  mutate(gender = "Female")

# Combine the results
results <- bind_rows(tidy_male, tidy_female)

# Filter to exclude the Intercept and focus only on coefficients of interest
results_regression <- results %>%
  filter(term != "(Intercept)" & grepl("early|after", term))

# Plotting the coefficients
coeff_plot <- ggplot(results_regression, aes(x = term, y = estimate, fill = gender)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  geom_errorbar(aes(ymin = estimate - std.error, ymax = estimate + std.error), 
                position = position_dodge(width = 0.8), width = 0.25) +
  labs(title = "Comparison of Regression Coefficients by Gender",
       x = "Variables",
       y = "Coefficient Estimate",
       fill = "Gender") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(coeff_plot)


