
# Load necessary libraries
library(tidyverse)
library(caret)
library(dplyr)
library(ggplot2)

# Read in data
data <- read_csv("data.csv")
colnames(data)[16]='X2P_O'
colnames(data)[18]='X3P_O'



# Calculate the winning percentage for each team (W=WON, G=Played)
data <- mutate(data, win_pct = W / G)

# Create a bar plot showing the number of games won by each team
#W=WON, G=Played
ggplot(data, aes(x = TEAM, y = G)) +
  geom_bar(stat = "identity") +
  xlab("Team") +
  ylab("Games Won")


# Create a scatter plot showing the relationship between offensive efficiency and power rating
# ADJOE=offensive_efficiency, BARTHAG=power_rating
ggplot(data, aes(x = ADJOE, y = BARTHAG)) +
  geom_point() +
  xlab("Offensive Efficiency") +
  ylab("Power Rating")

# Calculate the correlation between offensive efficiency and power rating
cor(data$ORB, data$BARTHAG)

# By Efficiency 



# Summarize data by conference
conference_summary <- data %>%
  group_by(CONF) %>%
  summarize(mean_power_rating = mean(BARTHAG),
            mean_off_reb_rate = mean(ORB),
            mean_win_percentage = mean(win_pct),
            mean_off_eff = mean(ADJOE),
            mean_turnover_rate = mean(TOR),
            mean_ft_rate = mean(FTR),
            mean_fg2_pct = mean(X2P_O),
            mean_fg3_pct = mean(X3P_O))

# View the summary data
print(conference_summary)


# Plot win percentage by conference
ggplot(conference_summary, aes(x = CONF, y = mean_win_percentage)) +
  geom_col() +
  xlab("Conference") +
  ylab("Win Percentage")

# Plot offensive efficiency rating by conference
ggplot(conference_summary, aes(x = CONF, y = mean_off_reb_rate)) +
  geom_col() +
  xlab("Conference") +
  ylab("Offencive Efficiency")


# Find top 3 most powerful teams
most_powerful_teams <- data %>% 
  top_n(3, BARTHAG) %>% 
  select(TEAM) %>% 
  pull()

# Print top 3 most powerful teams
most_powerful_teams

# W=Game Won, X2P_O=Twopoint 

# Create training and test sets

set.seed(123)
train_index <- createDataPartition(data$W, p = 0.8, list = FALSE)
train_data <- data[train_index, ]
test_data <- data[-train_index, ]
# Fit linear regression model
model <- lm(W ~ X2P_O, data = train_data)

# Predict on test data
predictions <- predict(model, test_data)

# Calculate mean squared error
mse <- mean((predictions - test_data$W)^2)

# Print mean squared error
mse
 

# Plot predictions vs. actual values
ggplot(test_data, aes(x =W, y = predictions)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1) +
  xlab("Actual values") +
  ylab("Predictions")
