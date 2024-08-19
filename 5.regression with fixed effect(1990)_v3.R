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
##################################regression with fixed effect#########################

# Fit the model with age and province fixed effect 
model_fix <- lm(edattain_n ~ early + after + early:after + factor(age) + factor(provin), data = above_18)
summary(model_fix)
# show the coefficients of interests, 'early' and 'after' and interaction term 
tidy_model_fix <- tidy(model_fix)
tidy_model_fix %>% 
  filter(term == "early" | term == "after" | term == "early:after")
# Print the coefficients of interest
coefficients(model)[c("(Intercept)", "early", "after", "early:after")]


################################vseparate regression for male and female############################
# Subset data for males and females
data_male <- subset(above_18, sex_dummy == 0)
data_female <- subset(above_18, sex_dummy == 1)

# Fit the model for males
model_male_fix <- lm(edattain_n ~ early + after + early:after + factor(age) + factor(provin), data = data_male)

# Fit the model for females
model_female_fix <- lm(edattain_n ~ early + after + early:after + factor(age) + factor(provin), data = data_female)

# Output the summary of the model for males
summary(model_male_fix)

# Output the summary of the model for females
summary(model_female_fix)

#plot 
tidy_male_fix <- tidy(model_male_fix) %>% 
  mutate(gender = "Male")

tidy_female_fix <- tidy(model_female_fix) %>% 
  mutate(gender = "Female")

# Combine the results
results_fix <- bind_rows(tidy_male_fix, tidy_female_fix)

# Filter to exclude the Intercept and focus only on coefficients of interest
results_regression_fix <- results_fix %>%
  filter(term == "early" | term == "after" | term == "early:after")

# Plotting the coefficients
coeff_plot_fix <- ggplot(results_regression_fix, aes(x = term, y = estimate, fill = gender)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  geom_errorbar(aes(ymin = estimate - std.error, ymax = estimate + std.error), 
                position = position_dodge(width = 0.8), width = 0.25) +
  labs(title = "Comparison of Regression Coefficients (with fixed effect) by Gender",
       x = "Variables",
       y = "Coefficient Estimate",
       fill = "Gender") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(coeff_plot_fix)


