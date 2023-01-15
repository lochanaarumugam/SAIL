
#loading libraries
library(tidyverse)
library(caret)

#retrieving data
rawdata<- read.csv("data.csv")

#renaming 
colnames(rawdata)<- c("Team","Conference","GamesPlayed","GamesWon","AOE","ADE",
           "PowerR","EFGPS","EFGPA","TOPA","TOPC","ORB","ORBA","FTR","FTRA",
           "TwoPShootP","TwoPAllow","ThreePShootP","ThreePAllow","ADJTempo","WAB","PostSeason",
           "Seed","Year")

#calculate win percentage games won/games played
WinPerc<- 100*(rawdata$GamesWon/rawdata$GamesPlayed)
rawdata<-cbind(rawdata,data.frame(WinPerc=WinPerc))

#Summarize data by conference
conference_summary <- rawdata %>%
  group_by(Conference) %>%
  summarize(meanPowerR = mean(PowerR),
            meanORB = mean(ORB),
            meanWinPerc = mean(WinPerc),
            meanAOE = mean(AOE),
            meanADE = mean(ADE),
            meanFTR = mean(FTR),
            meanTwoPShootP = mean(TwoPShootP),
            meanThreePShootP = mean(ThreePShootP))

#plot conference summary
csp<-ggplot(data=conference_summary,aes(x=meanThreePShootP, y=meanTwoPShootP))
csp+geom_point(aes(size=meanWinPerc,colour=Conference, alpha=0.6)) +
  xlab("Three Point Shooting Percentage") +
  ylab("Two Point Shooting Percentage") +
  ggtitle("Relationship between Win % and Shooting %","categorized by conferences") +
  theme( plot.title = element_text(colour="DarkBlue",size=15), 
         plot.subtitle = element_text(colour="DarkBlue",size=10))

#take conference top 5 WR 
conferencetop<- conference_summary %>% 
  top_n(5, meanWinPerc) %>% 
  select(Conference) %>% 
  pull()

conferencetop

#group by team
team_summary <- rawdata %>%
  group_by(Team) %>%
  summarize(meanPowerR = mean(PowerR),
            meanORB = mean(ORB),
            meanWinPerc = mean(WinPerc),
            meanAOE = mean(AOE),
            meanADE = mean(ADE),
            meanFTR = mean(FTR),
            meanTwoPShootP = mean(TwoPShootP),
            meanThreePShootP = mean(ThreePShootP))
team_summary


#top 10 winperc avg team:
teamtop <- team_summary %>%
  arrange(desc(meanWinPerc)) %>%
  top_n(10, meanWinPerc)
  
teamtop

#graph plot of the top ten teams and winrate
ggplot(data=teamtop,aes(x=Team,y=meanWinPerc,color=Team))+geom_point()+
  xlab("Team") +
  ylab("Mean Win Percentage") +
  ggtitle("Relationship between Team and Win Percentage")


#free throw rate and winrate
ggplot(data=teamtop,aes(x=meanFTR,y=meanWinPerc,color=Team,
))+geom_point()+
  xlab("Mean Free Throw Rate") +
  ylab("Mean Win Percentage") +
  ggtitle("Relationship between Free Throws and Win Percentage") 

#correlation for FTR and Win perc
ftrcor <- cor(teamtop$meanFTR,teamtop$meanWinPerc)
ftrcor

oftrcor<-cor(rawdata$FTR,rawdata$WinPerc)
oftrcor
#pull out and compare their power ratings and winrate by plot (2 point)
ggplot(data=teamtop,aes(x=meanPowerR,y=meanWinPerc,color=Team,
                       ))+geom_point()+
  xlab("Mean Power Rating") +
  ylab("Mean Win Percentage") +
  ggtitle("Relationship between Power Rating and Win Percentage")

#find correlation for power and win perc
powercor <- cor(teamtop$meanPowerR,teamtop$meanWinPerc)
powercor

opowercor<-cor(rawdata$PowerR,rawdata$WinPerc)
opowercor

#two point and win rate plot
ggplot(data=teamtop,aes(x=meanTwoPShootP,y=meanWinPerc,color=Team,
                        size=meanPowerR))+geom_point()+
  xlab("Mean Two Point Shooting Percentage") +
  ylab("Mean Win Percentage") +
  ggtitle("Relationship between Two Point Shooting Percentage and Win Percentage",
          "+ Power Ratings")

#find correlation for 2 and win perc
twocor <- cor(teamtop$meanTwoPShootP,teamtop$meanWinPerc)
twocor

otwocor<-cor(rawdata$TwoPShootP,rawdata$WinPerc)
otwocor

#three point and win rate plot
ggplot(data=teamtop,aes(x=meanThreePShootP,y=meanWinPerc,color=Team,
                        size=meanPowerR))+geom_point()+
  xlab("Mean Three Point Shooting Percentage") +
  ylab("Mean Win Percentage") +
  ggtitle("Relationship between Three Point Shooting Percentage and Win Percentage",
          "+ Power Ratings")

#find the correlation for 3 and win perc
threecor <- cor(teamtop$meanThreePShootP,teamtop$meanWinPerc)
threecor

othreecor<-cor(rawdata$ThreePShootP,rawdata$WinPerc)
othreecor

#AOE and Win rate plotted
ggplot(data=teamtop,aes(x=meanAOE,y=meanADE,color=Team,
                        size=meanWinPerc))+geom_point()+
  xlab("Mean Adjusted Offensive Effeciency") +
  ylab("Mean Adjusted Defensive Effeciency") +
  ggtitle("Relationship between Offensive and Defensive Effecincy with Win Rate")

#AOE and Win rate cor
AOEcor <- cor(teamtop$meanAOE,teamtop$meanWinPerc)
AOEcor

oAOE<-cor(rawdata$AOE,rawdata$WinPerc)
oAOE

#ADE and win rate cor
ADEcor <- cor(teamtop$meanADE,teamtop$meanWinPerc)
ADEcor

oADE<-cor(rawdata$ADE,rawdata$WinPerc)
oADE

#PREDICTION SECTION
# Create training and test sets
set.seed(123)
train_index <- createDataPartition(rawdata$WinPerc, p = 0.8, list = FALSE)
train_data <- rawdata[train_index, ]
test_data <- rawdata[-train_index, ]

# Fit linear regression model
model <- lm(WinPerc ~ AOE, data = train_data)

# Predict on test data
predictions <- predict(model, test_data)

# Calculate mean squared error
mse <- mean((predictions - test_data$WinPerc)^2)

# Print mean squared error
mse

#R Squared Value is in summary
summary(model) 

# prediction plot
ggplot(test_data, aes(x=AOE, y=predictions)) +
  geom_point() +
  xlab("Adjusted Offensive Effeciency") +
  ylab("Predicted Win Percentage")

#true plot
ggplot(data=rawdata, aes(x=AOE,y=WinPerc))+geom_point()+geom_smooth()+
  xlab("Adjusted Offensive Efficiency")+
  ylab("Win Percentage")

#Predicting win percent from increased AOE/
input<-120
yourdata<-data.frame(AOE=c(input))
yourchances<-predict(model,yourdata)
yourchances




