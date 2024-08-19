
##-------------------------
## load libraries
##-------------------------
library(readstata13) 
library(tidyverse)
library(dplyr)
library(ggplot2)

## -----------------------------------------------------------------------------
## directory paths
## -----------------------------------------------------------------------------

getwd()

data8 <- above_18 %>%
  # Collapse by year relative to Medicaid across all states
  group_by(j) %>%
  summarize(average_edattain_n = mean(edattain_n, na.rm = TRUE), .groups = 'drop') #%>%
# Generate label for plotting
#mutate(grp = "Early and late provinces")

data9 <- data7_1990 %>%
  group_by(early, j) %>%
  summarize(average_edattain_n = mean(edattain_n, na.rm = TRUE), .groups = 'drop') %>%
  mutate(grp = ifelse(early == 1, "Early", "Late")) %>%
  bind_rows(data8) %>%
  filter(j > -5 & j < 10)


# Plot as we could see from the graph 
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
  # Plot/legend formatting
  theme_classic() +
  theme(
    legend.title = element_blank(),
    legend.position = c(0.8,0.2),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(size=.1, color="black"))
