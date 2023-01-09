---
  title: "Sports Analysis"
output: html_document
---
  
  ```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)

# Load necessary libraries
library(tidyverse)
library(caret)

# Read in data
data <- read_csv("data.csv", col_names = TRUE)


# Summarize data by conference
conference_summary <- data %>%
  group_by(CONF) %>%
  summarize(mean_power_rating = mean(BARTHAG),
            mean_off_reb_rate = mean(ORB),
            mean_win_pct = mean(win_pct),
            mean_off_eff = mean(ADJOE),
            mean_ft_rate = mean(FTR),
            mean_fg2_pct = mean(X2P_O),
            mean_fg3_pct = mean(X3P_O))

# Print conference summary table

conference_summary
# Plot win percentage by conference
ggplot(conference_summary, aes(x = CONF, y = W)) +
  geom_col() +
  xlab("Conference") +
  ylab("Win Percentage")

# Plot offensive efficiency rating by conference
ggplot(conference_summary, aes(x = CONF, y = mean_off_eff_rating)) +  geom_

