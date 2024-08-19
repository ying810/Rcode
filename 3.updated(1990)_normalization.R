##-------------------------
## load libraries
##-------------------------

library(readstata13)  #imports .dta files from Stata 13 thru 15
library(tidyverse)
library(dplyr)
library(ggplot2)

## -----------------------------------------------------------------------------
## directory paths
## -----------------------------------------------------------------------------

getwd()

data3_1990 <- read.dta13("1990_v3.dta")
colnames(data3_1990)
data3_1990 <- data3_1990 %>% rename(birthyr = cn1990a_birthy)
##data3 <- data3 %>% rename(sex = cn1990a_sex)


# Find unique non-numeric values in edattain
unique(data3_1990$edattain)
unique(data3_1990$edattain_numeric)
data3_1990 <- subset(data3_1990, edattain != "niu (not in universe)")

data3_1990$edattain_numeric <- as.character(data3_1990$edattain)

unique(data3_1990$edattain_numeric)

#Create a mapping from categories to numeric values
levels_mapping <- c(
  "less than primary completed" = 1,
  "primary completed" = 2,
  "secondary completed" = 3,
  "university completed" = 4
)

# Ensure 'edattain' is a factor and use its levels to map to numeric values
data3_1990$edattain_n <- sapply(data3_1990$edattain_numeric, function(x) levels_mapping[x])


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




##################### need to be update: averages education attainment before and after LLF 
# Let's also plot averages education attainment across all provinces, male & female before and after policy implemented 
data5 <- data4_1990 %>%
  # Collapse by year relative to Medicaid across all states
  group_by(j) %>%
  summarize(average_edattain_n = mean(edattain_n, na.rm = TRUE), .groups = 'drop') %>%
  # Generate label for plotting
  mutate(grp = "all sex")


#now by male and female 

data6 <- data4_1990 %>%
  group_by(sex_dummy, j) %>%
  summarize(average_edattain_n = mean(edattain_n, na.rm = TRUE), .groups = 'drop') %>%
  mutate(grp = ifelse(sex_dummy == 0, "Male", "Female")) %>%
  bind_rows(data5) %>%
  filter(j > -10 & j < 10)


# Plot command
ggplot(data6, aes(x = j, y = average_edattain_n, group = grp)) +
  # Line plot with shapes
  geom_line(aes(linetype = grp)) +
  geom_point(aes(shape = grp)) +
  # Vertical line at year before implementation
  geom_vline(aes(xintercept = -1)) +
  # Axis labels
  labs(
    x = "Years Since LLF Implementation",
    y = "mean education outcome") +
  # Formatting lines
  scale_linetype_manual(values = c("solid", "dashed", "dashed")) +
  scale_shape_manual(values = c(20, 15, 0)) +
  scale_x_continuous(breaks = seq(min(data6$j), max(data6$j), by = 2)) +
  # Plot/legend formatting
  theme_classic() +
  theme(
    legend.title = element_blank(),
    legend.position = c(0.8,0.2),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(size=.1, color="black"))
