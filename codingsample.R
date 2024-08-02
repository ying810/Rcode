##-------------------------
## load libraries
##-------------------------

library(readstata13)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(broom)
library(fixest)
## -----------------------------------------------------------------------------
## directory paths
## -----------------------------------------------------------------------------

getwd()


###############Data cleaning##########

data3_1990 <- read.dta13("1990_v3.dta")
colnames(data3_1990)
data3_1990 <- data3_1990 %>% rename(birthyr = cn1990a_birthy)
data3_1990 <- data3_1990 %>% rename(age = cn1990a_age)


# Find unique non-numeric values in edattain
unique(data3_1990$edattain)
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

#map to numeric values
data3_1990$edattain_n <- sapply(data3_1990$edattain_numeric, function(x) levels_mapping[x])
unique(data3_1990$edattain_n)

#Convert birthyear to Numeric
data3_1990$birthyr <- as.numeric(as.character(data3_1990$birthyr))

#############
#assume that people are from the province they residue in 1985. show unique value of province 
data3_1990 <- data3_1990 %>% rename(provin = geo1_cn1990)
unique(data3_1990$provin)

##exclude the birthplace equals 'zhejiang', 'tibet', 'chongqing', 'hong kong, macau, taiwan, or foreign country', which do not have implement year info
excluded_locations <- c('zhejiang', 'tibet', 'chongqing', 'hong kong, macau, taiwan, or foreign country')

data4_1990 <- data3_1990 %>%
  filter(!provin %in% excluded_locations)

unique(data4_1990$provin)


##map different policy implementation year to different province
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
#check unique value
unique(data4_1990$imp_y)

##construct the relevant year for the event studies

data4_1990$imp_y <- as.numeric(as.character(data4_1990$imp_y))
data4_1990$j <- data4_1990$birthyr - data4_1990$imp_y

#add sex dummy
data4_1990 <- data4_1990%>%
  mutate(sex_dummy = ifelse(cn1990a_sex == "female", 1, ifelse(cn1990a_sex == "male", 0, NA)))


#######################################underlying parallel assumption ############################################# 
#create a new data set for regression purpose
data7_1990 <-data4_1990

# assign 1 to 'early' where 'imp_y' is 1971 or less, 0 where 'imp_y' is 1974 or more, and NA for years between 1973 and 1976.
data7_1990$early <- ifelse(data7_1990$imp_y <= 1971, 1, ifelse(data7_1990$imp_y >= 1974, 0, NA))


data7_1990$age <- as.numeric(as.character(data7_1990$age))

# assign 1 to 'after' where people are still at the educational age (5-25)
data7_1990$after <- ifelse(data7_1990$age >= 5 & data7_1990$age <= 25, 1, 0)

##select appropriate data set
above_18 <- subset(data7_1990, age > 18)

data8 <- above_18 %>%
  group_by(j) %>%
  summarize(average_edattain_n = mean(edattain_n, na.rm = TRUE), .groups = 'drop') #%>%

# Generate label for plotting

data9 <- data7_1990 %>%
  group_by(early, j) %>%
  summarize(average_edattain_n = mean(edattain_n, na.rm = TRUE), .groups = 'drop') %>%
  mutate(grp = ifelse(early == 1, "Early", "Late")) %>%
  bind_rows(data8) %>%
  filter(j > -5 & j < 10)


#plot to see whether underlying parallel assumption satisfied
ggplot(data9, aes(x = j, y = average_edattain_n, group = grp)) +
  # Line plot with shapes
  geom_line(aes(linetype = grp)) +
  geom_point(aes(shape = grp)) +
  # Vertical line at year before implementation
  geom_vline(aes(xintercept = 0)) +
  # Axis labels
  labs(
    title = "Years Since LLF Implementation",
    y = "mean education outcome") +
  # Formatting lines
  scale_linetype_manual(values = c("solid", "dashed", "dashed")) +
  scale_shape_manual(values = c(20, 15, 0)) +
  scale_x_continuous(breaks = seq(min(data6$j), max(data6$j), by = 2)) +
  theme_classic() +
  theme(
    legend.title = element_blank(),
    legend.position = c(0.8,0.2),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(size=.1, color="black"))

#######################################regression model####################
model_1 <- lm(edattain_n ~ early + after + early*after, data = above_18)
summary(model_1)

# Subset data for males and females
data_male <- subset(above_18, sex_dummy == 0)
data_female <- subset(above_18, sex_dummy == 1)


################################separate regression for male and female############################
# Fit the model for males
model_male <- lm(edattain_n ~ early*after, data = data_male)

# Fit the model for females
model_female <- lm(edattain_n ~ early*after, data = data_female)

# model summary 
summary(model_male)
summary(model_female)

##########Plot the coefficients 
tidy_male <- tidy(model_male) %>% 
  mutate(gender = "Male")

tidy_female <- tidy(model_female) %>% 
  mutate(gender = "Female")

# Combine the results
results <- bind_rows(tidy_male, tidy_female)

# Filter to exclude the Intercept and focus only on coefficients
results_regression <- results %>%
  filter(term != "(Intercept)" & grepl("early|after", term))

# Graph
ggplot(results_regression, aes(x = term, y = estimate, fill = gender)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  geom_errorbar(aes(ymin = estimate - std.error, ymax = estimate + std.error), 
                position = position_dodge(width = 0.8), width = 0.25) +
  labs(title = "Comparison of Regression Coefficients by Gender",
       x = "Variables",
       y = "Coefficient Estimate",
       fill = "Gender") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

##################################regression with fixed effect#########################

# Fit the model with age and province fixed effect 
model_fix <- feols(edattain_n ~ early*after | age + provin, data = above_18)
summary(model_fix)
# show the coefficients of interests, 'early' and 'after' and interaction term 
tidy_model_fix <- tidy(model_fix)
tidy_model_fix %>% 
  filter(term == "early" | term == "after" | term == "early*after")
# Print the coefficients of interest
coefficients(model)[c("(Intercept)", "early", "after", "early*after")]


################################separate regression for male and female############################
# Subset data for males and females
data_male <- subset(above_18, sex_dummy == 0)
data_female <- subset(above_18, sex_dummy == 1)

# Fit the model for males
model_male_fix <- feols(edattain_n ~ early*after | age + provin,  data = data_male)

# Fit the model for females
model_female_fix <- feols(edattain_n ~ early*after | age + provin,  data = data_female)

#######province fixed effects perfectly collinear with early dummy

# Output
summary(model_male_fix)
summary(model_female_fix)

##########Plot the coefficients
tidy_male_fix <- tidy(model_male_fix) %>% 
  mutate(gender = "Male")

tidy_female_fix <- tidy(model_female_fix) %>% 
  mutate(gender = "Female")

# Combine the results
results_fix <- bind_rows(tidy_male_fix, tidy_female_fix)

# Filter to exclude the Intercept and focus only on coefficients of interest
results_regression_fix <- results_fix %>%
  filter(term == "early" | term == "after" | term == "early:after")

# graph
ggplot(results_regression_fix, aes(x = term, y = estimate, fill = gender)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  geom_errorbar(aes(ymin = estimate - std.error, ymax = estimate + std.error), 
                position = position_dodge(width = 0.8), width = 0.25) +
  labs(title = "Comparison of Regression Coefficients (with fixed effect) by Gender",
       x = "Variables",
       y = "Coefficient Estimate",
       fill = "Gender") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))





