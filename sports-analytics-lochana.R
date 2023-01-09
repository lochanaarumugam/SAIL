# Load the necessary libraries
library(dplyr)
library(ggplot2)

# Read in the data from a csv file
df <- read.csv("data.csv")

# Calculate the winning percentage for each team (W=WON, G=Played)
data <- df %>% mutate(win_pct = W / G)

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
            mean_win_pct = mean(win_pct),
            mean_off_eff = mean(ADJOE),
            mean_ft_rate = mean(FTR),
            mean_fg2_pct = mean(X2P_O),
            mean_fg3_pct = mean(X3P_O))

# View the summary data
print(conference_summary)



# By Conference level Metrics same thing as above but broken down (remvoe them, just fo reading)
# Calculate the mean turnover percentage for each conference
# CONF=conference
data %>%
  group_by(CONF) %>%
  summarize(mean_turnover_pct = mean(TORD))


# Calculate the mean offensive rebound rate for each conference
#ORB=Offensive Rebound Rate
data %>%
  group_by(CONF) %>%
  summarize(mean_off_reb_rate = mean(ORB))

# Calculate the mean free throw rate for each conference
#FTR : Free Throw Rate
data %>%
  group_by(CONF) %>%
  summarize(mean_free_throw_rate = mean(FTR))

# Calculate the mean two point shooting percentage for each conference
#X2P_O: Two-Point Shooting Percentage
data %>%
  group_by(CONF) %>%
  summarize(mean_2pt_shooting_pct = mean(X2P_O))

# Calculate the mean three point shooting percentage for each conference
#X3P_O: Three-Point Shooting Percentage
data %>%
  group_by(CONF) %>%
  summarize(mean_3pt_shooting_pct = mean(X3P_O))


