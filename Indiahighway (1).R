##-------------------------
## load libraries
##-------------------------

library(readstata13)  
library(dplyr)
library(tidyr)
library(ggplot2)
library(fixest)


## -----------------------------------------------------------------------------
## directory paths
## -----------------------------------------------------------------------------

getwd()

district_distance <- read.dta13("district_distances.dta")
main_dataset <- read.dta13("main_dataset.dta")

#check unique state_name and district  
unique(district_distance$state_name)
unique(main_dataset$state_name)

unique(district_distance$district)
unique(main_dataset$district)

#combine data set, keep all observation in main_data set
merged_data <- merge(main_dataset, district_distance, by=c("state_name", "district"), all.x=TRUE)

#the combined data set has 2307 observation and 16 variables 
str(merged_data)


#5. transform data for regression purpose
# Create log distance variable and treatment variable
merged_data <- merged_data %>%
  mutate(log_dist_GQ = log(dist_GQ),
         close_to_GQ = ifelse(dist_GQ < 75, 1, 0))

#add urbanization rate 
urbanization_rate_data <- merged_data %>%
  group_by(state_name, district, year) %>%
  summarise(
    urban_pop = sum(pop[urban == 1]),
    rural_pop = sum(pop[urban == 0]),
    urban_emp = sum(emp[urban == 1]),
    rural_emp = sum(emp[urban == 0])
  ) %>%
  mutate(
    urbanization_rate_1 = urban_pop / (urban_pop + rural_pop),
    urbanization_rate_2 = urban_emp / (urban_emp + rural_emp)
  )

merged_data_2 <- merged_data %>%
  left_join(urbanization_rate_data, by = c("state_name", "district", "year"))


#create baseline level controls 
baseline_data <- merged_data_2 %>%
  filter(year == 2001) %>%
  select(state_name, district, urban, emp_agric, emp_ind, emp_serv) %>%
  rename(emp_agric_2001 = emp_agric, emp_ind_2001 = emp_ind, emp_serv_2001 = emp_serv)

# Merge baseline data with the original dataset
merged_data_3 <- merged_data_2 %>%
  left_join(baseline_data, by = c("state_name", "district", "urban"))

head(merged_data_3)


urban_data <- merged_data_3  %>%
  filter(urban == 1) 

urban_data_2011 <- urban_data %>%
  filter(year == 2011) 

urban_data_2001 <- urban_data %>%
  filter(year == 2001) 


# regression using population ratio urbanization_rate_1 initial level to examine pre-trend
model_0 <- feols(urbanization_rate_1 ~ close_to_GQ + pop + emp + emp_agric + emp_ind + emp_serv + dist_straightline| state_name, data = urban_data_2001)
model_1 <- feols(urbanization_rate_2 ~ close_to_GQ + pop + emp + emp_agric + emp_ind + emp_serv + dist_straightline| state_name, data = urban_data_2001)
model_2 <- feols(urbanization_rate_1 ~ log_dist_GQ + pop + emp + emp_agric + emp_ind + emp_serv + dist_straightline | state_name, data = urban_data_2001)
model_3 <- feols(urbanization_rate_2 ~ log_dist_GQ + pop + emp + emp_agric + emp_ind + emp_serv + dist_straightline | state_name, data = urban_data_2001)

##regression close_to_GQ
model_4 <- feols(urbanization_rate_1 ~ close_to_GQ + pop + emp + emp_agric_2001 + emp_ind_2001 + emp_serv_2001 + dist_straightline| state_name, data = urban_data_2011)
model_6 <- feols(urbanization_rate_2 ~ close_to_GQ + pop + emp + emp_agric_2001 + emp_ind_2001 + emp_serv_2001 + dist_straightline| state_name, data = urban_data_2011)

##regression log_dist_GQ
model_5 <- feols(urbanization_rate_1 ~ log_dist_GQ + pop + emp + emp_agric_2001 + emp_ind_2001 + emp_serv_2001 + dist_straightline| state_name, data = urban_data_2011)
model_7 <- feols(urbanization_rate_2 ~ log_dist_GQ + pop + emp + emp_agric_2001 + emp_ind_2001 + emp_serv_2001 + dist_straightline| state_name, data = urban_data_2011)

etable(model_0, model_1)
etable(model_5, model_7)
etable(model_2, model_3)
etable(model_4,model_6)

##wage regression
model_8 <- feols(wage_overall ~ close_to_GQ + pop + emp + emp_agric_2001 + emp_ind_2001 + emp_serv_2001 + dist_straightline| state_name, data = urban_data_2011)
model_9 <- feols(wage_man_form ~ close_to_GQ + pop + emp + emp_agric_2001 + emp_ind_2001 + emp_serv_2001 + dist_straightline| state_name, data = urban_data_2011)
model_10 <- feols(wage_man_inf ~ close_to_GQ + pop + emp + emp_agric_2001 + emp_ind_2001 + emp_serv_2001 + dist_straightline| state_name, data = urban_data_2011)

etable(model_8, model_9, model_10)


##6. graph 

# Extract the fitted values from the model

plot_data <- urban_data_2011 %>%
  filter(
    !is.na(close_to_GQ),
    !is.na(pop),
    !is.na(emp),
    !is.na(emp_agric_2001),
    !is.na(emp_ind_2001),
    !is.na(emp_serv_2001),
    !is.na(dist_straightline),
    !is.na(urbanization_rate_1)
  )


plot_data$fitted_values <- fitted(model_5)


# Plot the original data and the fitted values
ggplot(plot_data, aes(x = log_dist_GQ)) +
  geom_point(aes(y = urbanization_rate_1), color = "black", alpha = 0.5) +
  geom_line(aes(y = fitted_values), linetype = "dashed", color = "blue") +
  labs(title = "Urbanization Rate vs. Log Distance to GQ",
       x = "Log Distance to GQ",
       y = "Urbanization Rate") +
  xlim(0, NA)+ 
  theme_minimal()


ggplot(plot_data, aes(x = log_dist_GQ, y = fitted_values)) +
  geom_point(color = "orange", alpha = 0.5) +  
  geom_smooth(aes(y = fitted_values), color = "blue", method = "lm") + 
  labs(
    title = "Urbanization Rate vs. Log Distance to GQ (Fitted values as a smoothed line)",
    x = "Log Distance to GQ",
    y = "Urbanization Rate (Fitted values)"
  ) +
  theme_minimal()

ggplot(plot_data, aes(x = log_dist_GQ, y = urbanization_rate_1)) +
  geom_point(color = "blue", alpha = 0.5) +  
  geom_smooth(aes(y = fitted_values), color = "green", method = "lm") + 
  labs(
    title = "Urbanization Rate vs. Log Distance to GQ (Fitted values as a smoothed line)",
    x = "Log Distance to GQ",
    y = "Urbanization Rate (actual values)"
  ) +
  theme_minimal()




