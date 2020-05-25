---
author: 'Aadarsh Gupta'
date: "9/28/2019"
---


```{r, include = 'FALSE'}
knitr::opts_chunk$set(echo = TRUE)
library(dplyr)
library(magrittr)
library(ggplot2)
library(tidyr)
library(lubridate)
library(reshape2)
library(arules)
library(arulesViz)
library(RColorBrewer)
```


```{r, include = 'FALSE'}
#setwd('C://Users//nitin//Documents//EDA//HW1//HW 1')
con <- src_sqlite("euro_soccer.sqlite")
country_tbl <- tbl(con, "country")
league_tbl <- tbl(con, "league")
match_tbl <- tbl(con, "match")
player_tbl <- tbl(con, "player")
player_atts_tbl <- tbl(con, "player_attributes")
team_tbl <- tbl(con, "team")
team_atts_tbl <- tbl(con, "team_attributes")

country = country_tbl %>%
  collect()
league = league_tbl %>%
  collect()
match = match_tbl %>%
  collect()
player = player_tbl %>%
  collect()
player_atts = player_atts_tbl %>% 
  collect()
teams = team_tbl %>%
  collect()
team_atts = team_atts_tbl %>%
  collect()

roma <- 'Roma'
roma_record <- team_tbl %>% 
                collect() %>%
                filter(grepl(roma, team_long_name))
roma_home_matches <- match %>%
  filter(home_team_api_id == roma_record$team_api_id) %>%
  mutate(goal_diff = home_team_goal - away_team_goal) %>%
  mutate(type = 'home', opponent = away_team_api_id)
roma_away_matches <- match %>%
  filter(away_team_api_id == roma_record$team_api_id) %>%
  mutate(goal_diff =away_team_goal - home_team_goal) %>%
  mutate(type = 'away', opponent = home_team_api_id)

roma_all_match = rbind(roma_home_matches, roma_away_matches)
roma_all_match$result = as.numeric(roma_all_match$goal_diff)
roma_all_match[roma_all_match$goal_diff > 0, ]$result = 'win'
roma_all_match[roma_all_match$goal_diff < 0, ]$result = 'loss'
roma_all_match[roma_all_match$goal_diff == 0, ]$result = 'draw'
```


### Exploratory Analysis 
#### How have we been doing previously?  How are we doing currently?  

```{r, include=FALSE}
#### Data Preparation
# Mapping team_api_id to team names
teams_serieA <- data.frame(unique(roma_all_match$away_team_api_id))
team_names_serieA <- merge(teams_serieA, teams, by.x = 'unique.roma_all_match.away_team_api_id.', by.y = 'team_api_id')
team_names_serieA <- team_names_serieA %>% 
  rename(team_api_id = unique.roma_all_match.away_team_api_id.) 

team_names_serieA <- team_names_serieA %>%
  select (team_api_id, team_long_name)


### Using attributes
roma_home_matches_ext <- match %>% 
  filter(home_team_api_id == 8686) %>%
  mutate(game_type = 'Home') %>%
  mutate(goal_difference = abs(home_team_goal - away_team_goal)) %>%
  mutate(match_result = ifelse(home_team_goal - away_team_goal > 0, 'Win', 
                               ifelse(home_team_goal - away_team_goal <0 , 'Loss','Draw'))) %>%
  rename(team_api_id = home_team_api_id) %>%
  rename(team_against_api_id = away_team_api_id)

roma_home_matches_ext <-  roma_home_matches_ext %>% 
  inner_join(team_atts, by = ('team_api_id')) %>%
  select(-team_api_id, -id.y) %>%
  rename(team_api_id = team_against_api_id)

roma_home_matches_with_attr <- roma_home_matches_ext %>%
  left_join(team_names_serieA, by = ('team_api_id')) %>%
  rename(team_against = team_long_name)

## --- ##

roma_away_matches_with_attr <- match %>% 
  filter(away_team_api_id == 8686) %>%
  mutate(game_type = 'Away') %>%
  mutate(goal_difference = abs(home_team_goal - away_team_goal)) %>%
  mutate(match_result = ifelse(home_team_goal - away_team_goal < 0, 'Win', 
                               ifelse(home_team_goal - away_team_goal > 0 , 'Loss','Draw'))) %>%
  rename(team_api_id = away_team_api_id)

roma_away_matches_with_attr <- roma_away_matches_with_attr %>% 
  inner_join(team_atts, by = 'team_api_id') %>%
  select(-team_api_id, -id.y) %>%
  rename(team_api_id = home_team_api_id)

roma_away_matches_with_attr <- roma_away_matches_with_attr %>%
  left_join(team_names_serieA, by = ('team_api_id')) %>%
  rename(team_against = team_long_name)

roma_all_matches_with_attr <- rbind(roma_home_matches_with_attr, roma_away_matches_with_attr)

colnames(roma_all_matches_with_attr)
roma_all_matches_attr <- roma_all_matches_with_attr %>%
  select(season, stage, match_date = date.x,team_against, home_team_goal, away_team_goal, game_type,
         goal_difference,match_result, attribute_date = date.y, buildUpPlaySpeedClass, buildUpPlayDribblingClass, 
         buildUpPlayPassingClass,buildUpPlayPositioningClass, chanceCreationPassingClass,
         chanceCreationCrossingClass, chanceCreationShootingClass, chanceCreationPositioningClass,
         defencePressureClass,defenceAggressionClass,defenceTeamWidthClass,defenceDefenderLineClass) %>%
  mutate(date_diff = abs(as.Date(attribute_date) - as.Date(match_date)))

roma_all_matches_attr_match <- roma_all_matches_attr %>%
  group_by(match_date) %>%
  top_n(-1,wt=date_diff)

roma_all_matches_attr_match_clean <- roma_all_matches_attr_match %>%
  select(-date_diff, -attribute_date,) %>%
  mutate(  buildUpPlaySpeedClass = paste(buildUpPlaySpeedClass, "buildUpPlaySpeedClass", sep = "."), 
           buildUpPlayDribblingClass = paste(buildUpPlayDribblingClass, "buildUpPlayDribblingClass", sep = "."), 
           buildUpPlayPassingClass = paste(buildUpPlayPassingClass, "buildUpPlayPassingClass", sep = "."),
           buildUpPlayPositioningClass = paste(buildUpPlayPositioningClass, "buildUpPlayPositioningClass", sep = "."), 
           chanceCreationPassingClass = paste(chanceCreationPassingClass, "chanceCreationPassingClass", sep = "."),
           chanceCreationCrossingClass = paste(chanceCreationCrossingClass, "chanceCreationCrossingClass", sep = "."), 
           chanceCreationShootingClass = paste(chanceCreationShootingClass, "chanceCreationShootingClass", sep = "."), 
           chanceCreationPositioningClass = paste(chanceCreationPositioningClass, "chanceCreationPositioningClass", sep = "."),
           defencePressureClass = paste(defencePressureClass, "defencePressureClass", sep = "."),
           defenceAggressionClass = paste(defenceAggressionClass, "defenceAggressionClass", sep = "."),
           defenceTeamWidthClass = paste(defenceTeamWidthClass, "defenceTeamWidthClass", sep = "."),
           defenceDefenderLineClass = paste(defenceDefenderLineClass, "defenceDefenderLineClass", sep = "."))


roma_all_matches_clean <- roma_all_matches_attr_match %>%
  select(-date_diff, -attribute_date, -team_against, -stage, -season) 


```
    
##### Roma League Performance from 2008/09 - 2015/16  

```{r, echo = FALSE}
 # Roma wins and losses over the seasons
  # ggplot(roma_matches_lost_by_seasons, aes(x = season, y = losses)) + 
  #   geom_bar(stat = 'identity', position = 'dodge')
  
  library(scales)


#Roma performance every season
ggplot(roma_all_matches_attr_match, aes(x = season, fill = match_result)) + 
  geom_bar( position = 'fill') + 
  scale_y_continuous(labels = percent_format(), limits=c(0,1))+
  ylab('% of games')+ xlab('League season')+ theme_classic()+
  scale_fill_manual("legend", values = c('Win' = 'Beige', 'Draw' = 'gold', 'Loss' ='dark red'))+
  labs(title = "AS Roma League Performance ")

```
  
  

##### Roma League performance by Stages in a season  

```{r}
# Matches played before May 2012
ggplot(roma_all_matches_attr_match %>% 
         filter(match_date < '2012-05-30'), aes(x = stage, fill = match_result)) +
  geom_bar( position = 'fill')+ theme_classic() +
  ylab('% Games') + 
  scale_y_continuous(labels = percent_format(), limits=c(0,1)) + 
  labs(title = "Roma performance across stages in a season (2008 - 2012)") +
  scale_fill_manual("legend", values = c('Win' = 'beige', 'Draw' = 'gold', 
                                         'Loss' ='dark red'))

```

  Looking across the length of the season gives us interesting readings. In the season from 2008 to 2012, Roma used to have a slow start to the campaign but a strong performance during the middle stages.

```{r}

# Matches played after May 2012
ggplot(roma_all_matches_attr_match %>% 
         filter(match_date > '2012-05-30'), aes(x = stage, fill = match_result)) +
  geom_bar( position = 'fill')+ theme_classic() +
  ylab('% Games') + 
  scale_y_continuous(labels = percent_format(), limits=c(0,1)) +
  labs(title = "Roma performance across stages in a season (2012-2016)") +
  scale_fill_manual("legend", values = c('Win' = 'beige', 'Draw' = 'gold', 
                                         'Loss' ='dark red'))
```
  



```{r}
#All matches played
ggplot(roma_all_matches_attr_match, aes(x = stage, fill = match_result)) +
  geom_bar( position = 'fill')+ theme_classic() +
  ylab('% Games') + 
  scale_y_continuous(labels = percent_format(), limits=c(0,1)) +
  labs(title = "Roma performance across stages in a season (All Seasons") +
  scale_fill_manual("legend", values = c('Win' = 'beige', 'Draw' = 'gold', 
                                         'Loss' ='dark red'))

```
 

#### What are our biggest challenges?  Who are our key competitors? 

```{r, echo = FALSE}
#  Matches lost and drawn against opponents 
melt_in <- table(roma_all_matches_attr_match$team_against, roma_all_matches_attr_match$match_result)
melted_in <- melt_in %>% melt(id.vars = 'team_against', values = 'match_result')
par(mfrow = c(1,1))
#sorted 
ggplot(melted_in %>% filter(value >2 & Var2 == 'Loss'), aes(reorder(Var1,-value),y = value, fill = Var2))+
  geom_bar(stat = 'identity', alpha = 0.95)+ theme_classic() + 
  xlab('Teams against') +
  ylab('No. of matches')  + 
  scale_fill_manual("legend", values = c( "Loss" = "Dark Red")) +
  labs(title = "Roma Losses against the Serie A teams ") + theme(legend.position = "none")+
  theme(text = element_text(size=10))
```
  

#### Is there a way to adjust Home vs Away Game Performance?


```{r}
roma_win_match = roma_all_match[roma_all_match$result == 'win',]
roma_win_match = roma_win_match %>%
  select(season, type, result) %>%
  group_by(season, type) %>%
  count(result)

ggplot(roma_win_match, aes(season, n, col = type, group = type)) +
  geom_line() +
  geom_point(size = 1) +
  theme_classic() +
  scale_y_continuous(breaks=seq(0, 15, 1)) +
  scale_color_discrete(name = 'Match Type', breaks = c('home','away'), 
                       labels = c('Home Match', 'Away Match')) +
  labs(x = 'Match Season', y = 'Number of Wins', 
       title = 'Number of Wins per Season for AS ROMA: Home VS Away') +
  theme(plot.title = element_text(size = 12))
```

  
#### Finding associations from the Team attributes that lead to Roma Win
```{r, include=FALSE}
options(warn=-1)

roma_all_matches_clean_rules <- roma_all_matches_clean %>%
  select(-match_date, -home_team_goal,-away_team_goal, -goal_difference)

roma_all_matches_trans <- as(roma_all_matches_clean_rules,'transactions')

# summary(roma_all_matches_trans)

# Get the frequency of most used item
#itemFrequency(roma_all_matches_trans)


# Creating and running apriori rules on undefined LHS, RHS
association.rules <- apriori(roma_all_matches_trans, parameter = list(supp = 0.01, conf = 0.8 ))
# summary(association.rules)

#Printing the top rules
# inspect(association.rules[1:10])

# Creating and running apriori rules on RHS = Win
association.rules.2 <- apriori(roma_all_matches_trans,parameter = list(supp = 0.09, conf = 0.7,maxlen = 5 ), appearance = list(rhs = 'match_result=Win'))
# inspect(association.rules.2[1:40])
```

```{r}
# Plotting parallel coords fro apriori rules to visualize
plot(association.rules.2, method = 'paracoord')
```
  

  
#### Finding associations from the Team attributes that lead to Roma Loss
```{r, include=FALSE}

#Apriori rules on RHS = Loss
association.rules.3 <- apriori(roma_all_matches_trans,
                       parameter = list(supp = 0.15,
                       conf = 0.25,maxlen = 5 ), 
                       appearance = list(rhs = 'match_result=Loss'))
                       
# inspect(association.rules.3[1:3])
```

```{r}
#Plotting parallel coords for apriori rules to visualize 
plot(association.rules.3, method = 'paracoord') 
 
```
    
#### Data Transformation

We will evaluate a transaction as each match in our data set that resulted in an opponent losing that match.
For example, as Juventus is the team we lose most matches to, we look into what combination of player attributes leads to Juventus loss.

  LHS: 38 Player Attributes of Opposing Team
  RHS: Juventus Loss

In order to achieve the association rules above, we need to transform the original data into transaction data format first.


##### Filtering out all the matches that Juventus participates


```{r}
juventus = 'Juventus'
juventus_record <- team_tbl %>% 
                    collect() %>%
                    filter(grepl(juventus, team_long_name))

juventus_home_matches <- match %>%
  filter(home_team_api_id == juventus_record$team_api_id) %>%
  mutate(goal_diff = home_team_goal - away_team_goal) %>%
  mutate(type = 'home', opponent = away_team_api_id)

juventus_away_matches <- match %>%
  filter(away_team_api_id == juventus_record$team_api_id) %>%
  mutate(goal_diff =away_team_goal - home_team_goal) %>%
  mutate(type = 'away', opponent = home_team_api_id)

all_match = rbind(juventus_home_matches, juventus_away_matches)
all_match$result = as.numeric(all_match$goal_diff)
all_match[all_match$goal_diff > 0, ]$result = 'win'
all_match[all_match$goal_diff < 0, ]$result = 'loss'
all_match[all_match$goal_diff == 0, ]$result = 'draw'

```
  
##### Map the position of player based on his X, Y coordinates on the field
Then, we use the X, Y coordinate provided to categorize the player's position. If (x, y) = (1, 1), the player is a goal keeper; if 2 ≤ y ≤ 5, the player is a defender; if 6 ≤ y ≤ 8, he is a mid fielder, and if 9 ≤ y ≤ 11, he is a forwarder(striker).
```{r}
for (i in 1:11){
  x = paste("home_player_X",i,sep='')
  y = paste("home_player_Y",i,sep='')
  inp = paste("hm_ply_pos",i,sep="_")
  all_match[inp] = ifelse(all_match[x]==1 & all_match[y]==1,"Goaly",
                      ifelse(all_match[y]>=2 & all_match[y]<=5,"Defender",
                             ifelse(all_match[y]>=6 & all_match[y]<=8,"Mid Fielders",
                                    ifelse(all_match[y]>=9 & all_match[y]<=11,
                                           "Forwards","NA"))))
}

for (i in 1:11){
  x = paste("away_player_X",i,sep='')
  y = paste("away_player_Y",i,sep='')
  inp = paste("away_ply_pos",i,sep="_")
  all_match[inp] = ifelse(all_match[x]==1 & all_match[y]==1,"Goaly",
                      ifelse(all_match[y]>=2 & all_match[y]<=5,"Defender",
                             ifelse(all_match[y]>=6 & all_match[y]<=8,"Mid Fielders",
                                    ifelse(all_match[y]>=9 & all_match[y]<=11,
                                    "Forwards","NA"))))
}

```

##### Categorizing Player attributes:
Attributes are currently ranked on a 0 to 100 scale.  We will take min / max for each attribute, then divide the range into 5 categories. This will create a low, low-med, med, med-high, high value for each attribute.

```{r}
column_names = c("overall_rating","potential","crossing", "finishing","heading_accuracy",    
                 "short_passing", "volleys","dribbling",    
                 "curve", "free_kick_accuracy" ,"long_passing",    
                 "ball_control","acceleration","sprint_speed",    
                 "agility","reactions","balance",    
                 "shot_power","jumping","stamina",    
                 "strength","long_shots","aggression",    
                 "interceptions","positioning","vision",    
                 "penalties", "marking","standing_tackle",
                 "sliding_tackle","gk_diving","gk_handling",    
                 "gk_kicking","gk_positioning","gk_reflexes")
for (i in column_names){
  k = paste(i, "Bucket",sep="_")
  player_atts[k] = 
    cut(player_atts[[i]], breaks = 5, labels=c('low','low-med','med','med-high','high'))
}

player_atts <- select(player_atts, c(1:9, overall_rating_Bucket:gk_reflexes_Bucket))
player_atts = player_atts[, -c(5,6)]
all_match <- select(all_match, c(id:away_team_goal, home_player_1:away_player_11, 
                                 opponent:away_ply_pos_11))

```


##### Merge player attributes to matches by taking the player attribute records closest to the particular match time
Since we already have a categorical player attributes table and a Juventus match table, next we merge these two table into the final table where one row represents one match along with attributes of both 11 home players and 11 away players. For each player in each match, we select the record of his attributes closest to that particular match date, and comebine his position and 38 attributes together into one cell. We show the data manipulation for home player 1, and repeat the similar process to other 21 players (which we hide the repeated codes).

```{r}
#HP1
match_player <- merge(x = all_match, y=player_atts,by.x='home_player_1',
                      by.y='player_api_id',all.x=TRUE)
match_player$date.x<-ymd_hms(match_player$date.x)
match_player$date.y<-ymd_hms(match_player$date.y)
match_player$time_diff <- difftime(match_player$date.x, match_player$date.y,units='days')
match_player = match_player[match_player$time_diff >= 0,]
match_player_1 = match_player %>%
  group_by(match_api_id, home_player_1) %>%
  filter(time_diff == min(time_diff))
for (i in 61:98){
  colnames(match_player_1)[i] = paste(colnames(match_player_1)[i], "hp_1", sep = "_")
}
for (i in 61:98){
  match_player_1[[i]] = paste(match_player_1[[i]],match_player_1[[36]],sep="_")
}
match_player_1$time_diff=NULL

```

```{r, include= FALSE}
# HP2

match_player2 <- merge(x = match_player_1, y=player_atts,by.x='home_player_2',by.y='player_api_id',all.x=TRUE)
match_player2$date.x<-ymd(match_player2$date.x)
match_player2$date<-ymd_hms(match_player2$date)
match_player2$time_diff <- difftime(match_player2$date.x, match_player2$date,units='days')
match_player2 = match_player2[match_player2$time_diff >= 0,]
match_player2 = match_player2 %>%
  group_by(match_api_id, home_player_2) %>%
  filter(time_diff == min(time_diff))
for (i in 102:139){
  colnames(match_player2)[i] = paste(colnames(match_player2)[i], "hp_2", sep = "_")
}
for (i in 102:139){
  match_player2[[i]] = paste(match_player2[[i]],match_player2[[37]],sep="_")
}
match_player2$time_diff=NULL
match_player2$id.y = NULL
match_player2$date.y=NULL
match_player2$date=NULL
match_player2$id.x=NULL
##
#HP3

match_player3 <- merge(x = match_player2, y=player_atts,by.x='home_player_3',by.y='player_api_id',all.x=TRUE)
match_player3$date.x<-ymd(match_player3$date.x)
match_player3$date<-ymd_hms(match_player3$date)
match_player3$time_diff <- difftime(match_player3$date.x, match_player3$date,units='days')
match_player3 = match_player3[match_player3$time_diff >= 0,]
match_player3 = match_player3 %>%
  group_by(match_api_id, home_player_3) %>%
  filter(time_diff == min(time_diff))
for (i in 139:176){
  colnames(match_player3)[i] = paste(colnames(match_player3)[i], "hp_3", sep = "_")
}
for (i in 139:176){
  match_player3[[i]] = paste(match_player3[[i]],match_player3[[37]],sep="_")
}
match_player3$time_diff=NULL
match_player3$id.y = NULL
match_player3$date.y=NULL
match_player3$date=NULL
match_player3$id.x=NULL
match_player3$player_fifa_api_id=NULL

#HP4

match_player4 <- merge(x = match_player3, y=player_atts,by.x='home_player_4',by.y='player_api_id',all.x=TRUE)
match_player4$date.x<-ymd(match_player4$date.x)
match_player4$date<-ymd_hms(match_player4$date)
match_player4$time_diff <- difftime(match_player4$date.x, match_player4$date,units='days')
match_player4 = match_player4[match_player4$time_diff >= 0,]
match_player4 = match_player4 %>%
  group_by(match_api_id, home_player_4) %>%
  filter(time_diff == min(time_diff))
for (i in 176:213){
  colnames(match_player4)[i] = paste(colnames(match_player4)[i], "hp_4", sep = "_")
}
for (i in 176:213){
  match_player4[[i]] = paste(match_player4[[i]],match_player4[[38]],sep="_")
}
match_player4$time_diff=NULL
match_player4$id.y = NULL
match_player4$date.y=NULL
match_player4$date=NULL
match_player4$id.x=NULL
match_player4$id=NULL
match_player4$player_fifa_api_id.x=NULL
match_player4$player_fifa_api_id.y=NULL
#HP5

match_player5 <- merge(x = match_player4, y=player_atts,by.x='home_player_5',by.y='player_api_id',all.x=TRUE)
match_player5$date.x<-ymd(match_player5$date.x)
match_player5$date<-ymd_hms(match_player5$date)
match_player5$time_diff <- difftime(match_player5$date.x, match_player5$date,units='days')
match_player5 = match_player5[match_player5$time_diff >= 0,]
match_player5 = match_player5 %>%
  group_by(match_api_id, home_player_5) %>%
  filter(time_diff == min(time_diff))
for (i in 213:250){
  colnames(match_player5)[i] = paste(colnames(match_player5)[i], "hp_5", sep = "_")
}
for (i in 213:250){
  match_player5[[i]] = paste(match_player5[[i]],match_player5[[39]],sep="_")
}
match_player5$time_diff=NULL
match_player5$id.y = NULL
match_player5$date.y=NULL
match_player5$date=NULL
match_player5$id.x=NULL
match_player5$id=NULL
###
#HP6
match_player6 <- merge(x = match_player5, y=player_atts,by.x='home_player_6',by.y='player_api_id',all.x=TRUE)
match_player6$date.x<-ymd(match_player6$date.x)
match_player6$date<-ymd_hms(match_player6$date)
match_player6$time_diff <- difftime(match_player6$date.x, match_player6$date,units='days')
match_player6 = match_player6[match_player6$time_diff >= 0,]
match_player6 = match_player6 %>%
  group_by(match_api_id, home_player_6) %>%
  filter(time_diff == min(time_diff))
for (i in 252:289){
  colnames(match_player6)[i] = paste(colnames(match_player6)[i], "hp_6", sep = "_")
}
for (i in 252:289){
  match_player6[[i]] = paste(match_player6[[i]],match_player6[[40]],sep="_")
}
match_player6$time_diff=NULL
match_player6$id.y = NULL
match_player6$date.y=NULL
match_player6$date=NULL
match_player6$id.x=NULL
match_player6$id=NULL
match_player6$player_fifa_api_id.x=NULL
match_player6$player_fifa_api_id.y=NULL

#HP7
match_player7 <- merge(x = match_player6, y=player_atts,by.x='home_player_7',by.y='player_api_id',all.x=TRUE)
match_player7$date.x<-ymd(match_player7$date.x)
match_player7$date<-ymd_hms(match_player7$date)
match_player7$time_diff <- difftime(match_player7$date.x, match_player7$date,units='days')
match_player7 = match_player7[match_player7$time_diff >= 0,]
match_player7 = match_player7 %>%
  group_by(match_api_id, home_player_7) %>%
  filter(time_diff == min(time_diff))
for (i in 289:326){
  colnames(match_player7)[i] = paste(colnames(match_player7)[i], "hp_7", sep = "_")
}
for (i in 289:326){
  match_player7[[i]] = paste(match_player7[[i]],match_player7[[41]],sep="_")
}
match_player7$time_diff=NULL
match_player7$id.y = NULL
match_player7$date.y=NULL
match_player7$date=NULL
match_player7$id.x=NULL
match_player7$id=NULL
match_player7$player_fifa_api_id.x=NULL
match_player7$player_fifa_api_id.y=NULL

#HP8
match_player8 <- merge(x = match_player7, y=player_atts,by.x='home_player_8',by.y='player_api_id',all.x=TRUE)
match_player8$date.x<-ymd(match_player8$date.x)
match_player8$date<-ymd_hms(match_player8$date)
match_player8$time_diff <- difftime(match_player8$date.x, match_player8$date,units='days')
match_player8 = match_player8[match_player8$time_diff >= 0,]
match_player8 = match_player8 %>%
  group_by(match_api_id, home_player_8) %>%
  filter(time_diff == min(time_diff))
for (i in 326:363){
  colnames(match_player8)[i] = paste(colnames(match_player8)[i], "hp_8", sep = "_")
}
for (i in 326:363){
  match_player8[[i]] = paste(match_player8[[i]],match_player8[[42]],sep="_")
}
match_player8$time_diff=NULL
match_player8$id.y = NULL
match_player8$date.y=NULL
match_player8$date=NULL
match_player8$id.x=NULL
match_player8$id=NULL
match_player8$player_fifa_api_id.x=NULL
match_player8$player_fifa_api_id.y=NULL

#HP9
match_player9 <- merge(x = match_player8, y=player_atts,by.x='home_player_9',by.y='player_api_id',all.x=TRUE)
match_player9$date.x<-ymd(match_player9$date.x)
match_player9$date<-ymd_hms(match_player9$date)
match_player9$time_diff <- difftime(match_player9$date.x, match_player9$date,units='days')
match_player9 = match_player9[match_player9$time_diff >= 0,]
match_player9 = match_player9 %>%
  group_by(match_api_id, home_player_9) %>%
  filter(time_diff == min(time_diff))
for (i in 365:402){
  colnames(match_player9)[i] = paste(colnames(match_player9)[i], "hp_9", sep = "_")
}
for (i in 365:402){
  match_player9[[i]] = paste(match_player9[[i]],match_player9[[43]],sep="_")
}
match_player9$time_diff=NULL
match_player9$id.y = NULL
match_player9$date.y=NULL
match_player9$date=NULL
match_player9$id.x=NULL
match_player9$id=NULL
match_player9$player_fifa_api_id.x=NULL
match_player9$player_fifa_api_id.y=NULL

#HP10
match_player10 <- merge(x = match_player9, y=player_atts,by.x='home_player_10',by.y='player_api_id',all.x=TRUE)
match_player10$date.x<-ymd(match_player10$date.x)
match_player10$date<-ymd_hms(match_player10$date)
match_player10$time_diff <- difftime(match_player10$date.x, match_player10$date,units='days')
match_player10 = match_player10[match_player10$time_diff >= 0,]
match_player10 = match_player10 %>%
  group_by(match_api_id, home_player_10) %>%
  filter(time_diff == min(time_diff))
for (i in 402:439){
  colnames(match_player10)[i] = paste(colnames(match_player10)[i], "hp_10", sep = "_")
}
for (i in 402:439){
  match_player10[[i]] = paste(match_player10[[i]],match_player10[[44]],sep="_")
}
match_player10$time_diff=NULL
match_player10$id.y = NULL
match_player10$date.y=NULL
match_player10$date=NULL
match_player10$id.x=NULL
match_player10$id=NULL
match_player10$player_fifa_api_id.x=NULL
match_player10$player_fifa_api_id.y=NULL

#HP11
match_player11 <- merge(x = match_player10, y=player_atts,by.x='home_player_11',by.y='player_api_id',all.x=TRUE)
match_player11$date.x<-ymd(match_player11$date.x)
match_player11$date<-ymd_hms(match_player11$date)
match_player11$time_diff <- difftime(match_player11$date.x, match_player11$date,units='days')
match_player11 = match_player11[match_player11$time_diff >= 0,]
match_player11 = match_player11 %>%
  group_by(match_api_id, home_player_11) %>%
  filter(time_diff == min(time_diff))
for (i in 441:478){
  colnames(match_player11)[i] = paste(colnames(match_player11)[i], "hp_11", sep = "_")
}
for (i in 441:478){
  match_player11[[i]] = paste(match_player11[[i]],match_player11[[45]],sep="_")
}
match_player11$time_diff=NULL
match_player11$id.y = NULL
match_player11$date.y=NULL
match_player11$date=NULL
match_player11$id.x=NULL
match_player11$id=NULL
match_player11$player_fifa_api_id.x=NULL
match_player11$player_fifa_api_id.y=NULL

#AP1

match_player12 <- merge(x = match_player11, y=player_atts,by.x='away_player_1',by.y='player_api_id',all.x=TRUE)
match_player12$date.x<-ymd(match_player12$date.x)
match_player12$date<-ymd_hms(match_player12$date)
match_player12$time_diff <- difftime(match_player12$date.x, match_player12$date,units='days')
match_player12 = match_player12[match_player12$time_diff >= 0,]
match_player12 = match_player12 %>%
  group_by(match_api_id, away_player_1) %>%
  filter(time_diff == min(time_diff))
for (i in 478:515){
  colnames(match_player12)[i] = paste(colnames(match_player12)[i], "aw_p_1", sep = "_")
}
for (i in 478:515){
  match_player12[[i]] = paste(match_player12[[i]],match_player12[[46]],sep="_")
}
match_player12$time_diff=NULL
match_player12$id.y = NULL
match_player12$date.y=NULL
match_player12$date=NULL
match_player12$id.x=NULL
match_player12$id=NULL
match_player12$player_fifa_api_id.x=NULL
match_player12$player_fifa_api_id.y=NULL
#AP2
match_player13 <- merge(x = match_player12, y=player_atts,by.x='away_player_2',by.y='player_api_id',all.x=TRUE)
match_player13$date.x<-ymd(match_player13$date.x)
match_player13$date<-ymd_hms(match_player13$date)
match_player13$time_diff <- difftime(match_player13$date.x, match_player13$date,units='days')
match_player13 = match_player13[match_player13$time_diff >= 0,]
match_player13 = match_player13 %>%
  group_by(match_api_id, away_player_2) %>%
  filter(time_diff == min(time_diff))
for (i in 517:554){
  colnames(match_player13)[i] = paste(colnames(match_player13)[i], "aw_p_2", sep = "_")
}
for (i in 517:554){
  match_player13[[i]] = paste(match_player13[[i]],match_player13[[47]],sep="_")
}
match_player13$time_diff=NULL
match_player13$id.y = NULL
match_player13$date.y=NULL
match_player13$date=NULL
match_player13$id.x=NULL
match_player13$id=NULL
match_player13$player_fifa_api_id.x=NULL
match_player13$player_fifa_api_id.y=NULL

#AP3
match_player14 <- merge(x = match_player13, y=player_atts,by.x='away_player_3',by.y='player_api_id',all.x=TRUE)
match_player14$date.x<-ymd(match_player14$date.x)
match_player14$date<-ymd_hms(match_player14$date)
match_player14$time_diff <- difftime(match_player14$date.x, match_player14$date,units='days')
match_player14 = match_player14[match_player14$time_diff >= 0,]
match_player14 = match_player14 %>%
  group_by(match_api_id, away_player_3) %>%
  filter(time_diff == min(time_diff))
for (i in 554:591){
  colnames(match_player14)[i] = paste(colnames(match_player14)[i], "aw_p_3", sep = "_")
}
for (i in 554:591){
  match_player14[[i]] = paste(match_player14[[i]],match_player14[[48]],sep="_")
}
match_player14$time_diff=NULL
match_player14$id.y = NULL
match_player14$date.y=NULL
match_player14$date=NULL
match_player14$id.x=NULL
match_player14$id=NULL
match_player14$player_fifa_api_id.x=NULL
match_player14$player_fifa_api_id.y=NULL

#AP4
match_player15 <- merge(x = match_player14, y=player_atts,by.x='away_player_4',by.y='player_api_id',all.x=TRUE)
match_player15$date.x<-ymd(match_player15$date.x)
match_player15$date<-ymd_hms(match_player15$date)
match_player15$time_diff <- difftime(match_player15$date.x, match_player15$date,units='days')
match_player15 = match_player15[match_player15$time_diff >= 0,]
match_player15 = match_player15 %>%
  group_by(match_api_id, away_player_4) %>%
  filter(time_diff == min(time_diff))
for (i in 593:630){
  colnames(match_player15)[i] = paste(colnames(match_player15)[i], "aw_p_4", sep = "_")
}
for (i in 593:630){
  match_player15[[i]] = paste(match_player15[[i]],match_player15[[49]],sep="_")
}
match_player15$time_diff=NULL
match_player15$id.y = NULL
match_player15$date.y=NULL
match_player15$date=NULL
match_player15$id.x=NULL
match_player15$id=NULL
match_player15$player_fifa_api_id.x=NULL
match_player15$player_fifa_api_id.y=NULL

#AP5
match_player16 <- merge(x = match_player15, y=player_atts,by.x='away_player_5',by.y='player_api_id',all.x=TRUE)
match_player16$date.x<-ymd(match_player16$date.x)
match_player16$date<-ymd_hms(match_player16$date)
match_player16$time_diff <- difftime(match_player16$date.x, match_player16$date,units='days')
match_player16 = match_player16[match_player16$time_diff >= 0,]
match_player16 = match_player16 %>%
  group_by(match_api_id, away_player_5) %>%
  filter(time_diff == min(time_diff))
for (i in 630:667){
  colnames(match_player16)[i] = paste(colnames(match_player16)[i], "aw_p_5", sep = "_")
}
for (i in 630:667){
  match_player16[[i]] = paste(match_player16[[i]],match_player16[[50]],sep="_")
}
match_player16$time_diff=NULL
match_player16$id.y = NULL
match_player16$date.y=NULL
match_player16$date=NULL
match_player16$id.x=NULL
match_player16$id=NULL
match_player16$player_fifa_api_id.x=NULL
match_player16$player_fifa_api_id.y=NULL
#AP6
match_player17 <- merge(x = match_player16, y=player_atts,by.x='away_player_6',by.y='player_api_id',all.x=TRUE)
match_player17$date.x<-ymd(match_player17$date.x)
match_player17$date<-ymd_hms(match_player17$date)
match_player17$time_diff <- difftime(match_player17$date.x, match_player17$date,units='days')
match_player17 = match_player17[match_player17$time_diff >= 0,]
match_player17 = match_player17 %>%
  group_by(match_api_id, away_player_6) %>%
  filter(time_diff == min(time_diff))
for (i in 669:706){
  colnames(match_player17)[i] = paste(colnames(match_player17)[i], "aw_p_6", sep = "_")
}
for (i in 669:706){
  match_player17[[i]] = paste(match_player17[[i]],match_player17[[51]],sep="_")
}
match_player17$time_diff=NULL
match_player17$id.y = NULL
match_player17$date.y=NULL
match_player17$date=NULL
match_player17$id.x=NULL
match_player17$id=NULL
match_player17$player_fifa_api_id.x=NULL
match_player17$player_fifa_api_id.y=NULL
#AP7
match_player18 <- merge(x = match_player17, y=player_atts,by.x='away_player_7',by.y='player_api_id',all.x=TRUE)
match_player18$date.x<-ymd(match_player18$date.x)
match_player18$date<-ymd_hms(match_player18$date)
match_player18$time_diff <- difftime(match_player18$date.x, match_player18$date,units='days')
match_player18 = match_player18[match_player18$time_diff >= 0,]
match_player18 = match_player18 %>%
  group_by(match_api_id, away_player_7) %>%
  filter(time_diff == min(time_diff))
for (i in 706:743){
  colnames(match_player18)[i] = paste(colnames(match_player18)[i], "aw_p_7", sep = "_")
}
for (i in 706:743){
  match_player18[[i]] = paste(match_player18[[i]],match_player18[[52]],sep="_")
}
match_player18$time_diff=NULL
match_player18$id.y = NULL
match_player18$date.y=NULL
match_player18$date=NULL
match_player18$id.x=NULL
match_player18$id=NULL
match_player18$player_fifa_api_id.x=NULL
match_player18$player_fifa_api_id.y=NULL
#AP8
match_player19 <- merge(x = match_player18, y=player_atts,by.x='away_player_8',by.y='player_api_id',all.x=TRUE)
match_player19$date.x<-ymd(match_player19$date.x)
match_player19$date<-ymd_hms(match_player19$date)
match_player19$time_diff <- difftime(match_player19$date.x, match_player19$date,units='days')
match_player19 = match_player19[match_player19$time_diff >= 0,]
match_player19 = match_player19 %>%
  group_by(match_api_id, away_player_8) %>%
  filter(time_diff == min(time_diff))
for (i in 745:782){
  colnames(match_player19)[i] = paste(colnames(match_player19)[i], "aw_p_8", sep = "_")
}
for (i in 745:782){
  match_player19[[i]] = paste(match_player19[[i]],match_player19[[53]],sep="_")
}
match_player19$time_diff=NULL
match_player19$id.y = NULL
match_player19$date.y=NULL
match_player19$date=NULL
match_player19$id.x=NULL
match_player19$id=NULL
match_player19$player_fifa_api_id.x=NULL
match_player19$player_fifa_api_id.y=NULL
#AP9
match_player20 <- merge(x = match_player19, y=player_atts,by.x='away_player_9',by.y='player_api_id',all.x=TRUE)
match_player20$date.x<-ymd(match_player20$date.x)
match_player20$date<-ymd_hms(match_player20$date)
match_player20$time_diff <- difftime(match_player20$date.x, match_player20$date,units='days')
match_player20 = match_player20[match_player20$time_diff >= 0,]
match_player20 = match_player20 %>%
  group_by(match_api_id, away_player_9) %>%
  filter(time_diff == min(time_diff))
for (i in 782:819){
  colnames(match_player20)[i] = paste(colnames(match_player20)[i], "aw_p_9", sep = "_")
}
for (i in 782:819){
  match_player20[[i]] = paste(match_player20[[i]],match_player20[[54]],sep="_")
}
match_player20$time_diff=NULL
match_player20$id.y = NULL
match_player20$date.y=NULL
match_player20$date=NULL
match_player20$id.x=NULL
match_player20$id=NULL
match_player20$player_fifa_api_id.x=NULL
match_player20$player_fifa_api_id.y=NULL
#AP10
match_player21 <- merge(x = match_player20, y=player_atts,by.x='away_player_10',by.y='player_api_id',all.x=TRUE)
match_player21$date.x<-ymd(match_player21$date.x)
match_player21$date<-ymd_hms(match_player21$date)
match_player21$time_diff <- difftime(match_player21$date.x, match_player21$date,units='days')
match_player21 = match_player21[match_player21$time_diff >= 0,]
match_player21 = match_player21 %>%
  group_by(match_api_id, away_player_10) %>%
  filter(time_diff == min(time_diff))
for (i in 821:858){
  colnames(match_player21)[i] = paste(colnames(match_player21)[i], "aw_p_10", sep = "_")
}
for (i in 821:858){
  match_player21[[i]] = paste(match_player21[[i]],match_player21[[55]],sep="_")
}
match_player21$time_diff=NULL
match_player21$id.y = NULL
match_player21$date.y=NULL
match_player21$date=NULL
match_player21$id.x=NULL
match_player21$id=NULL
match_player21$player_fifa_api_id.x=NULL
match_player21$player_fifa_api_id.y=NULL
#AP11
match_player22 <- merge(x = match_player21, y=player_atts,by.x='away_player_11',by.y='player_api_id',all.x=TRUE)
match_player22$date.x<-ymd(match_player22$date.x)
match_player22$date<-ymd_hms(match_player22$date)
match_player22$time_diff <- difftime(match_player22$date.x, match_player22$date,units='days')
match_player22 = match_player22[match_player22$time_diff >= 0,]
match_player22 = match_player22 %>%
  group_by(match_api_id, away_player_11) %>%
  filter(time_diff == min(time_diff))
for (i in 858:895){
  colnames(match_player22)[i] = paste(colnames(match_player22)[i], "aw_p_11", sep = "_")
}
for (i in 858:895){
  match_player22[[i]] = paste(match_player22[[i]],match_player22[[56]],sep="_")
}
match_player22$time_diff=NULL
match_player22$id.y = NULL
match_player22$date.y=NULL
match_player22$date=NULL
match_player22$id.x=NULL
match_player22$id=NULL
match_player22$player_fifa_api_id.x=NULL
match_player22$player_fifa_api_id.y=NULL
```
  
  
  In the final dataset, each cell contains position, attribute and its value
Finally, we create unique item composed of position, player attribute and attribute value so that when we run association rule, the we can identify the attributes needed for different posisitions. We added the position and attribute name into its value as prefix, and then deleted the player number since we only care about the position layout we need to win against Juventus.

```{r, message=FALSE}
options(warn=-1)
match_trans = match_player22

### Juventus plays as home team, need to use away player attributes in the next steps
home_match_trans = match_trans[match_trans$opponent == match_trans$away_team_api_id,]
home_trans_ar = home_match_trans %>%
  select(preferred_foot_aw_p_1:gk_reflexes_Bucket_aw_p_11, result)
home_trans_ar = home_trans_ar[,-c(1,2)]
home_trans_ar = subset(home_trans_ar, select = -player_fifa_api_id)

### Juventus plays as away team, need to use home player attributes in the next steps
away_match_trans = match_trans[match_trans$opponent == match_trans$home_team_api_id,]
away_trans_ar = away_match_trans %>%
  select(preferred_foot_hp_1:gk_reflexes_Bucket_hp_11, result)
away_trans_ar = away_trans_ar[,-c(1,2)]

colnames(away_trans_ar) = colnames(home_trans_ar)
all_trans_ar = rbind(home_trans_ar, away_trans_ar)

##### add column name to values
column_names = names(all_trans_ar)
for (column in column_names) {
  all_trans_ar[[column]] = paste(column, all_trans_ar[[column]], sep = '_')
}

###### delete player numbers
for (i in 1:418) {
  for (j in 1:287) {
    all_trans_ar[j,i] = gsub('_aw_p_[0-9]', '', all_trans_ar[j,i])
  }
}
for (i in 1:418) {
  for (j in 1:287) {
    all_trans_ar[j,i] = gsub('[0-9]', '', all_trans_ar[j,i])
  }
}

#### write as final transaction data
write.csv(all_trans_ar,'juventus.csv')
#head(all_trans_ar)
```

## Findings and Recommendations


##### Juventus

```{r, results = "hide", message = F, warning = F}

juventus_all = read.transactions("juventus.csv", format = "basket",
                           sep = ",", rm.duplicates = TRUE, skip = 1, header = F)
# inspect(juventus)
# itemInfo(juventus)
rules <- apriori(juventus_all, parameter = list(supp = 0.1, conf = 0.34), 
                 appearance = list(rhs = 'result_loss'))

rules_lift = sort(rules,by='lift',decreasing = TRUE)
```
  
  When we run the associations of player attributes that correspond to Juventus's loss, we found the following rules:

```{r, fig.width=16}
inspect(rules_lift[1:10])
```
  
  Plotting on a parallel coordinaltes plot helps to visualize the most important player attributes.

```{r}
# plot(rules_lift[1:5], method = 'graph', engine = 'htmlwidget')
plot(rules_lift[1:10], method = 'paracoord')
```


#### Sampdoria 

```{r, include = FALSE}
player_atts = player_atts_tbl %>% 
  collect()
long_team_name <- 'Sampdoria'
roma_record <- team_tbl %>% 
  collect() %>%
  filter(grepl(long_team_name, team_long_name))
home_matches <- match %>%
  filter(home_team_api_id == roma_record$team_api_id) %>%
  mutate(goal_diff = home_team_goal - away_team_goal) %>%
  mutate(type = 'home', opponent = away_team_api_id)
# hist(home_matches$goal_diff)
# summary(home_matches$goal_diff)

away_matches <- match %>%
  filter(away_team_api_id == roma_record$team_api_id) %>%
  mutate(goal_diff =away_team_goal - home_team_goal) %>%
  mutate(type = 'away', opponent = home_team_api_id)

# hist(away_matches$goal_diff)
# summary(away_matches$goal_diff)

all_match = rbind(home_matches, away_matches)
# ggplot(all_match, aes(type, goal_diff, col = type)) +
#   geom_boxplot()

all_match$result = as.numeric(all_match$goal_diff)
all_match[all_match$goal_diff > 0, ]$result = 'win'
all_match[all_match$goal_diff < 0, ]$result = 'loss'
all_match[all_match$goal_diff == 0, ]$result = 'draw'
# ggplot(all_match, aes(result)) +
#   geom_bar()

# ggplot(all_match[all_match$result == 'lose',], aes(as.factor(opponent))) +
#   geom_bar()

##################### position ###################
for (i in 1:11){
  x = paste("home_player_X",i,sep='')
  y = paste("home_player_Y",i,sep='')
  inp = paste("hm_ply_pos",i,sep="_")
  all_match[inp] = ifelse(all_match[x]==1 & all_match[y]==1,"Goaly",
                      ifelse(all_match[y]>=2 & all_match[y]<=5,"Defender",
                             ifelse(all_match[y]>=6 & all_match[y]<=8,"Mid Fielders",
                                    ifelse(all_match[y]>=9 & all_match[y]<=11,"Forwards","NA"))))
}

for (i in 1:11){
  x = paste("away_player_X",i,sep='')
  y = paste("away_player_Y",i,sep='')
  inp = paste("away_ply_pos",i,sep="_")
  all_match[inp] = ifelse(all_match[x]==1 & all_match[y]==1,"Goaly",
                      ifelse(all_match[y]>=2 & all_match[y]<=5,"Defender",
                             ifelse(all_match[y]>=6 & all_match[y]<=8,"Mid Fielders",
                                    ifelse(all_match[y]>=9 & all_match[y]<=11,"Forwards","NA"))))
}
```

```{r, include  = FALSE, message=FALSE, results="hide"}
column_names = c("overall_rating","potential","crossing", "finishing","heading_accuracy",    
                 "short_passing", "volleys","dribbling",    
                 "curve", "free_kick_accuracy" ,"long_passing",    
                 "ball_control","acceleration","sprint_speed",    
                 "agility","reactions","balance",    
                 "shot_power","jumping","stamina",    
                 "strength","long_shots","aggression",    
                 "interceptions","positioning","vision",    
                 "penalties", "marking","standing_tackle",
                 "sliding_tackle","gk_diving","gk_handling",    
                 "gk_kicking","gk_positioning","gk_reflexes")

for (i in column_names){
  k = paste(i, "Bucket",sep="_")
  player_atts[k] = 
    cut(player_atts[[i]], breaks = 5, labels=c('low','low-med','med','med-high','high'))
}

player_atts <- select(player_atts, c(1:9, overall_rating_Bucket:gk_reflexes_Bucket))
player_atts = player_atts[, -c(5,6)]
all_match <- select(all_match, c(id:away_team_goal, home_player_1:away_player_11, opponent:away_ply_pos_11))

#######
# match_g <- gather(all_match, player, player_id, home_player_1:away_player_11, na.rm = TRUE)
# View(match_g)
# match_s <- arrange(match_g, match_api_id, player)
# View(match_s)
# #match_s <- select(match_s, c(1:10, player, player_id))

# colnames(match_s)[37] <- "player_api_id"

#HP1
match_player <- merge(x = all_match, y=player_atts,by.x='home_player_1',by.y='player_api_id',all.x=TRUE)
match_player$date.x<-ymd_hms(match_player$date.x)
match_player$date.y<-ymd_hms(match_player$date.y)
match_player$time_diff <- difftime(match_player$date.x, match_player$date.y,units='days')
match_player = match_player[match_player$time_diff >= 0,]
match_player_1 = match_player %>%
  group_by(match_api_id, home_player_1) %>%
  filter(time_diff == min(time_diff))
for (i in 61:98){
  colnames(match_player_1)[i] = paste(colnames(match_player_1)[i], "hp_1", sep = "_")
}
for (i in 61:98){
  match_player_1[[i]] = paste(match_player_1[[i]],match_player_1[[36]],sep="_")
}
match_player_1$time_diff=NULL
# HP2

match_player2 <- merge(x = match_player_1, y=player_atts,by.x='home_player_2',by.y='player_api_id',all.x=TRUE)
match_player2$date.x<-ymd(match_player2$date.x)
match_player2$date<-ymd_hms(match_player2$date)
match_player2$time_diff <- difftime(match_player2$date.x, match_player2$date,units='days')
match_player2 = match_player2[match_player2$time_diff >= 0,]
match_player2 = match_player2 %>%
  group_by(match_api_id, home_player_2) %>%
  filter(time_diff == min(time_diff))
for (i in 102:139){
  colnames(match_player2)[i] = paste(colnames(match_player2)[i], "hp_2", sep = "_")
}
for (i in 102:139){
  match_player2[[i]] = paste(match_player2[[i]],match_player2[[37]],sep="_")
}
match_player2$time_diff=NULL
match_player2$id.y = NULL
match_player2$date.y=NULL
match_player2$date=NULL
match_player2$id.x=NULL
##
#HP3

match_player3 <- merge(x = match_player2, y=player_atts,by.x='home_player_3',by.y='player_api_id',all.x=TRUE)
match_player3$date.x<-ymd(match_player3$date.x)
match_player3$date<-ymd_hms(match_player3$date)
match_player3$time_diff <- difftime(match_player3$date.x, match_player3$date,units='days')
match_player3 = match_player3[match_player3$time_diff >= 0,]
match_player3 = match_player3 %>%
  group_by(match_api_id, home_player_3) %>%
  filter(time_diff == min(time_diff))
for (i in 139:176){
  colnames(match_player3)[i] = paste(colnames(match_player3)[i], "hp_3", sep = "_")
}
for (i in 139:176){
  match_player3[[i]] = paste(match_player3[[i]],match_player3[[37]],sep="_")
}
match_player3$time_diff=NULL
match_player3$id.y = NULL
match_player3$date.y=NULL
match_player3$date=NULL
match_player3$id.x=NULL
match_player3$player_fifa_api_id=NULL

#HP4

match_player4 <- merge(x = match_player3, y=player_atts,by.x='home_player_4',by.y='player_api_id',all.x=TRUE)
match_player4$date.x<-ymd(match_player4$date.x)
match_player4$date<-ymd_hms(match_player4$date)
match_player4$time_diff <- difftime(match_player4$date.x, match_player4$date,units='days')
match_player4 = match_player4[match_player4$time_diff >= 0,]
match_player4 = match_player4 %>%
  group_by(match_api_id, home_player_4) %>%
  filter(time_diff == min(time_diff))
for (i in 176:213){
  colnames(match_player4)[i] = paste(colnames(match_player4)[i], "hp_4", sep = "_")
}
for (i in 176:213){
  match_player4[[i]] = paste(match_player4[[i]],match_player4[[38]],sep="_")
}
match_player4$time_diff=NULL
match_player4$id.y = NULL
match_player4$date.y=NULL
match_player4$date=NULL
match_player4$id.x=NULL
match_player4$id=NULL
match_player4$player_fifa_api_id.x=NULL
match_player4$player_fifa_api_id.y=NULL
#HP5

match_player5 <- merge(x = match_player4, y=player_atts,by.x='home_player_5',by.y='player_api_id',all.x=TRUE)
match_player5$date.x<-ymd(match_player5$date.x)
match_player5$date<-ymd_hms(match_player5$date)
match_player5$time_diff <- difftime(match_player5$date.x, match_player5$date,units='days')
match_player5 = match_player5[match_player5$time_diff >= 0,]
match_player5 = match_player5 %>%
  group_by(match_api_id, home_player_5) %>%
  filter(time_diff == min(time_diff))
for (i in 213:250){
  colnames(match_player5)[i] = paste(colnames(match_player5)[i], "hp_5", sep = "_")
}
for (i in 213:250){
  match_player5[[i]] = paste(match_player5[[i]],match_player5[[39]],sep="_")
}
match_player5$time_diff=NULL
match_player5$id.y = NULL
match_player5$date.y=NULL
match_player5$date=NULL
match_player5$id.x=NULL
match_player5$id=NULL
###
#HP6
match_player6 <- merge(x = match_player5, y=player_atts,by.x='home_player_6',by.y='player_api_id',all.x=TRUE)
match_player6$date.x<-ymd(match_player6$date.x)
match_player6$date<-ymd_hms(match_player6$date)
match_player6$time_diff <- difftime(match_player6$date.x, match_player6$date,units='days')
match_player6 = match_player6[match_player6$time_diff >= 0,]
match_player6 = match_player6 %>%
  group_by(match_api_id, home_player_6) %>%
  filter(time_diff == min(time_diff))
for (i in 252:289){
  colnames(match_player6)[i] = paste(colnames(match_player6)[i], "hp_6", sep = "_")
}
for (i in 252:289){
  match_player6[[i]] = paste(match_player6[[i]],match_player6[[40]],sep="_")
}
match_player6$time_diff=NULL
match_player6$id.y = NULL
match_player6$date.y=NULL
match_player6$date=NULL
match_player6$id.x=NULL
match_player6$id=NULL
match_player6$player_fifa_api_id.x=NULL
match_player6$player_fifa_api_id.y=NULL

#HP7
match_player7 <- merge(x = match_player6, y=player_atts,by.x='home_player_7',by.y='player_api_id',all.x=TRUE)
match_player7$date.x<-ymd(match_player7$date.x)
match_player7$date<-ymd_hms(match_player7$date)
match_player7$time_diff <- difftime(match_player7$date.x, match_player7$date,units='days')
match_player7 = match_player7[match_player7$time_diff >= 0,]
match_player7 = match_player7 %>%
  group_by(match_api_id, home_player_7) %>%
  filter(time_diff == min(time_diff))
for (i in 289:326){
  colnames(match_player7)[i] = paste(colnames(match_player7)[i], "hp_7", sep = "_")
}
for (i in 289:326){
  match_player7[[i]] = paste(match_player7[[i]],match_player7[[41]],sep="_")
}
match_player7$time_diff=NULL
match_player7$id.y = NULL
match_player7$date.y=NULL
match_player7$date=NULL
match_player7$id.x=NULL
match_player7$id=NULL
match_player7$player_fifa_api_id.x=NULL
match_player7$player_fifa_api_id.y=NULL

#HP8
match_player8 <- merge(x = match_player7, y=player_atts,by.x='home_player_8',by.y='player_api_id',all.x=TRUE)
match_player8$date.x<-ymd(match_player8$date.x)
match_player8$date<-ymd_hms(match_player8$date)
match_player8$time_diff <- difftime(match_player8$date.x, match_player8$date,units='days')
match_player8 = match_player8[match_player8$time_diff >= 0,]
match_player8 = match_player8 %>%
  group_by(match_api_id, home_player_8) %>%
  filter(time_diff == min(time_diff))
for (i in 326:363){
  colnames(match_player8)[i] = paste(colnames(match_player8)[i], "hp_8", sep = "_")
}
for (i in 326:363){
  match_player8[[i]] = paste(match_player8[[i]],match_player8[[42]],sep="_")
}
match_player8$time_diff=NULL
match_player8$id.y = NULL
match_player8$date.y=NULL
match_player8$date=NULL
match_player8$id.x=NULL
match_player8$id=NULL
match_player8$player_fifa_api_id.x=NULL
match_player8$player_fifa_api_id.y=NULL

#HP9
match_player9 <- merge(x = match_player8, y=player_atts,by.x='home_player_9',by.y='player_api_id',all.x=TRUE)
match_player9$date.x<-ymd(match_player9$date.x)
match_player9$date<-ymd_hms(match_player9$date)
match_player9$time_diff <- difftime(match_player9$date.x, match_player9$date,units='days')
match_player9 = match_player9[match_player9$time_diff >= 0,]
match_player9 = match_player9 %>%
  group_by(match_api_id, home_player_9) %>%
  filter(time_diff == min(time_diff))
for (i in 365:402){
  colnames(match_player9)[i] = paste(colnames(match_player9)[i], "hp_9", sep = "_")
}
for (i in 365:402){
  match_player9[[i]] = paste(match_player9[[i]],match_player9[[43]],sep="_")
}
match_player9$time_diff=NULL
match_player9$id.y = NULL
match_player9$date.y=NULL
match_player9$date=NULL
match_player9$id.x=NULL
match_player9$id=NULL
match_player9$player_fifa_api_id.x=NULL
match_player9$player_fifa_api_id.y=NULL

#HP10
match_player10 <- merge(x = match_player9, y=player_atts,by.x='home_player_10',by.y='player_api_id',all.x=TRUE)
match_player10$date.x<-ymd(match_player10$date.x)
match_player10$date<-ymd_hms(match_player10$date)
match_player10$time_diff <- difftime(match_player10$date.x, match_player10$date,units='days')
match_player10 = match_player10[match_player10$time_diff >= 0,]
match_player10 = match_player10 %>%
  group_by(match_api_id, home_player_10) %>%
  filter(time_diff == min(time_diff))
for (i in 402:439){
  colnames(match_player10)[i] = paste(colnames(match_player10)[i], "hp_10", sep = "_")
}
for (i in 402:439){
  match_player10[[i]] = paste(match_player10[[i]],match_player10[[44]],sep="_")
}
match_player10$time_diff=NULL
match_player10$id.y = NULL
match_player10$date.y=NULL
match_player10$date=NULL
match_player10$id.x=NULL
match_player10$id=NULL
match_player10$player_fifa_api_id.x=NULL
match_player10$player_fifa_api_id.y=NULL

#HP11
match_player11 <- merge(x = match_player10, y=player_atts,by.x='home_player_11',by.y='player_api_id',all.x=TRUE)
match_player11$date.x<-ymd(match_player11$date.x)
match_player11$date<-ymd_hms(match_player11$date)
match_player11$time_diff <- difftime(match_player11$date.x, match_player11$date,units='days')
match_player11 = match_player11[match_player11$time_diff >= 0,]
match_player11 = match_player11 %>%
  group_by(match_api_id, home_player_11) %>%
  filter(time_diff == min(time_diff))
for (i in 441:478){
  colnames(match_player11)[i] = paste(colnames(match_player11)[i], "hp_11", sep = "_")
}
for (i in 441:478){
  match_player11[[i]] = paste(match_player11[[i]],match_player11[[45]],sep="_")
}
match_player11$time_diff=NULL
match_player11$id.y = NULL
match_player11$date.y=NULL
match_player11$date=NULL
match_player11$id.x=NULL
match_player11$id=NULL
match_player11$player_fifa_api_id.x=NULL
match_player11$player_fifa_api_id.y=NULL

#AP1

match_player12 <- merge(x = match_player11, y=player_atts,by.x='away_player_1',by.y='player_api_id',all.x=TRUE)
match_player12$date.x<-ymd(match_player12$date.x)
match_player12$date<-ymd_hms(match_player12$date)
match_player12$time_diff <- difftime(match_player12$date.x, match_player12$date,units='days')
match_player12 = match_player12[match_player12$time_diff >= 0,]
match_player12 = match_player12 %>%
  group_by(match_api_id, away_player_1) %>%
  filter(time_diff == min(time_diff))
for (i in 478:515){
  colnames(match_player12)[i] = paste(colnames(match_player12)[i], "aw_p_1", sep = "_")
}
for (i in 478:515){
  match_player12[[i]] = paste(match_player12[[i]],match_player12[[46]],sep="_")
}
match_player12$time_diff=NULL
match_player12$id.y = NULL
match_player12$date.y=NULL
match_player12$date=NULL
match_player12$id.x=NULL
match_player12$id=NULL
match_player12$player_fifa_api_id.x=NULL
match_player12$player_fifa_api_id.y=NULL
#AP2
match_player13 <- merge(x = match_player12, y=player_atts,by.x='away_player_2',by.y='player_api_id',all.x=TRUE)
match_player13$date.x<-ymd(match_player13$date.x)
match_player13$date<-ymd_hms(match_player13$date)
match_player13$time_diff <- difftime(match_player13$date.x, match_player13$date,units='days')
match_player13 = match_player13[match_player13$time_diff >= 0,]
match_player13 = match_player13 %>%
  group_by(match_api_id, away_player_2) %>%
  filter(time_diff == min(time_diff))
for (i in 517:554){
  colnames(match_player13)[i] = paste(colnames(match_player13)[i], "aw_p_2", sep = "_")
}
for (i in 517:554){
  match_player13[[i]] = paste(match_player13[[i]],match_player13[[47]],sep="_")
}
match_player13$time_diff=NULL
match_player13$id.y = NULL
match_player13$date.y=NULL
match_player13$date=NULL
match_player13$id.x=NULL
match_player13$id=NULL
match_player13$player_fifa_api_id.x=NULL
match_player13$player_fifa_api_id.y=NULL

#AP3
match_player14 <- merge(x = match_player13, y=player_atts,by.x='away_player_3',by.y='player_api_id',all.x=TRUE)
match_player14$date.x<-ymd(match_player14$date.x)
match_player14$date<-ymd_hms(match_player14$date)
match_player14$time_diff <- difftime(match_player14$date.x, match_player14$date,units='days')
match_player14 = match_player14[match_player14$time_diff >= 0,]
match_player14 = match_player14 %>%
  group_by(match_api_id, away_player_3) %>%
  filter(time_diff == min(time_diff))
for (i in 554:591){
  colnames(match_player14)[i] = paste(colnames(match_player14)[i], "aw_p_3", sep = "_")
}
for (i in 554:591){
  match_player14[[i]] = paste(match_player14[[i]],match_player14[[48]],sep="_")
}
match_player14$time_diff=NULL
match_player14$id.y = NULL
match_player14$date.y=NULL
match_player14$date=NULL
match_player14$id.x=NULL
match_player14$id=NULL
match_player14$player_fifa_api_id.x=NULL
match_player14$player_fifa_api_id.y=NULL

#AP4
match_player15 <- merge(x = match_player14, y=player_atts,by.x='away_player_4',by.y='player_api_id',all.x=TRUE)
match_player15$date.x<-ymd(match_player15$date.x)
match_player15$date<-ymd_hms(match_player15$date)
match_player15$time_diff <- difftime(match_player15$date.x, match_player15$date,units='days')
match_player15 = match_player15[match_player15$time_diff >= 0,]
match_player15 = match_player15 %>%
  group_by(match_api_id, away_player_4) %>%
  filter(time_diff == min(time_diff))
for (i in 593:630){
  colnames(match_player15)[i] = paste(colnames(match_player15)[i], "aw_p_4", sep = "_")
}
for (i in 593:630){
  match_player15[[i]] = paste(match_player15[[i]],match_player15[[49]],sep="_")
}
match_player15$time_diff=NULL
match_player15$id.y = NULL
match_player15$date.y=NULL
match_player15$date=NULL
match_player15$id.x=NULL
match_player15$id=NULL
match_player15$player_fifa_api_id.x=NULL
match_player15$player_fifa_api_id.y=NULL

#AP5
match_player16 <- merge(x = match_player15, y=player_atts,by.x='away_player_5',by.y='player_api_id',all.x=TRUE)
match_player16$date.x<-ymd(match_player16$date.x)
match_player16$date<-ymd_hms(match_player16$date)
match_player16$time_diff <- difftime(match_player16$date.x, match_player16$date,units='days')
match_player16 = match_player16[match_player16$time_diff >= 0,]
match_player16 = match_player16 %>%
  group_by(match_api_id, away_player_5) %>%
  filter(time_diff == min(time_diff))
for (i in 630:667){
  colnames(match_player16)[i] = paste(colnames(match_player16)[i], "aw_p_5", sep = "_")
}
for (i in 630:667){
  match_player16[[i]] = paste(match_player16[[i]],match_player16[[50]],sep="_")
}
match_player16$time_diff=NULL
match_player16$id.y = NULL
match_player16$date.y=NULL
match_player16$date=NULL
match_player16$id.x=NULL
match_player16$id=NULL
match_player16$player_fifa_api_id.x=NULL
match_player16$player_fifa_api_id.y=NULL
#AP6
match_player17 <- merge(x = match_player16, y=player_atts,by.x='away_player_6',by.y='player_api_id',all.x=TRUE)
match_player17$date.x<-ymd(match_player17$date.x)
match_player17$date<-ymd_hms(match_player17$date)
match_player17$time_diff <- difftime(match_player17$date.x, match_player17$date,units='days')
match_player17 = match_player17[match_player17$time_diff >= 0,]
match_player17 = match_player17 %>%
  group_by(match_api_id, away_player_6) %>%
  filter(time_diff == min(time_diff))
for (i in 669:706){
  colnames(match_player17)[i] = paste(colnames(match_player17)[i], "aw_p_6", sep = "_")
}
for (i in 669:706){
  match_player17[[i]] = paste(match_player17[[i]],match_player17[[51]],sep="_")
}
match_player17$time_diff=NULL
match_player17$id.y = NULL
match_player17$date.y=NULL
match_player17$date=NULL
match_player17$id.x=NULL
match_player17$id=NULL
match_player17$player_fifa_api_id.x=NULL
match_player17$player_fifa_api_id.y=NULL
#AP7
match_player18 <- merge(x = match_player17, y=player_atts,by.x='away_player_7',by.y='player_api_id',all.x=TRUE)
match_player18$date.x<-ymd(match_player18$date.x)
match_player18$date<-ymd_hms(match_player18$date)
match_player18$time_diff <- difftime(match_player18$date.x, match_player18$date,units='days')
match_player18 = match_player18[match_player18$time_diff >= 0,]
match_player18 = match_player18 %>%
  group_by(match_api_id, away_player_7) %>%
  filter(time_diff == min(time_diff))
for (i in 706:743){
  colnames(match_player18)[i] = paste(colnames(match_player18)[i], "aw_p_7", sep = "_")
}
for (i in 706:743){
  match_player18[[i]] = paste(match_player18[[i]],match_player18[[52]],sep="_")
}
match_player18$time_diff=NULL
match_player18$id.y = NULL
match_player18$date.y=NULL
match_player18$date=NULL
match_player18$id.x=NULL
match_player18$id=NULL
match_player18$player_fifa_api_id.x=NULL
match_player18$player_fifa_api_id.y=NULL
#AP8
match_player19 <- merge(x = match_player18, y=player_atts,by.x='away_player_8',by.y='player_api_id',all.x=TRUE)
match_player19$date.x<-ymd(match_player19$date.x)
match_player19$date<-ymd_hms(match_player19$date)
match_player19$time_diff <- difftime(match_player19$date.x, match_player19$date,units='days')
match_player19 = match_player19[match_player19$time_diff >= 0,]
match_player19 = match_player19 %>%
  group_by(match_api_id, away_player_8) %>%
  filter(time_diff == min(time_diff))
for (i in 745:782){
  colnames(match_player19)[i] = paste(colnames(match_player19)[i], "aw_p_8", sep = "_")
}
for (i in 745:782){
  match_player19[[i]] = paste(match_player19[[i]],match_player19[[53]],sep="_")
}
match_player19$time_diff=NULL
match_player19$id.y = NULL
match_player19$date.y=NULL
match_player19$date=NULL
match_player19$id.x=NULL
match_player19$id=NULL
match_player19$player_fifa_api_id.x=NULL
match_player19$player_fifa_api_id.y=NULL
#AP9
match_player20 <- merge(x = match_player19, y=player_atts,by.x='away_player_9',by.y='player_api_id',all.x=TRUE)
match_player20$date.x<-ymd(match_player20$date.x)
match_player20$date<-ymd_hms(match_player20$date)
match_player20$time_diff <- difftime(match_player20$date.x, match_player20$date,units='days')
match_player20 = match_player20[match_player20$time_diff >= 0,]
match_player20 = match_player20 %>%
  group_by(match_api_id, away_player_9) %>%
  filter(time_diff == min(time_diff))
for (i in 782:819){
  colnames(match_player20)[i] = paste(colnames(match_player20)[i], "aw_p_9", sep = "_")
}
for (i in 782:819){
  match_player20[[i]] = paste(match_player20[[i]],match_player20[[54]],sep="_")
}
match_player20$time_diff=NULL
match_player20$id.y = NULL
match_player20$date.y=NULL
match_player20$date=NULL
match_player20$id.x=NULL
match_player20$id=NULL
match_player20$player_fifa_api_id.x=NULL
match_player20$player_fifa_api_id.y=NULL
#AP10
match_player21 <- merge(x = match_player20, y=player_atts,by.x='away_player_10',by.y='player_api_id',all.x=TRUE)
match_player21$date.x<-ymd(match_player21$date.x)
match_player21$date<-ymd_hms(match_player21$date)
match_player21$time_diff <- difftime(match_player21$date.x, match_player21$date,units='days')
match_player21 = match_player21[match_player21$time_diff >= 0,]
match_player21 = match_player21 %>%
  group_by(match_api_id, away_player_10) %>%
  filter(time_diff == min(time_diff))
for (i in 821:858){
  colnames(match_player21)[i] = paste(colnames(match_player21)[i], "aw_p_10", sep = "_")
}
for (i in 821:858){
  match_player21[[i]] = paste(match_player21[[i]],match_player21[[55]],sep="_")
}
match_player21$time_diff=NULL
match_player21$id.y = NULL
match_player21$date.y=NULL
match_player21$date=NULL
match_player21$id.x=NULL
match_player21$id=NULL
match_player21$player_fifa_api_id.x=NULL
match_player21$player_fifa_api_id.y=NULL
#AP11
match_player22 <- merge(x = match_player21, y=player_atts,by.x='away_player_11',by.y='player_api_id',all.x=TRUE)
match_player22$date.x<-ymd(match_player22$date.x)
match_player22$date<-ymd_hms(match_player22$date)
match_player22$time_diff <- difftime(match_player22$date.x, match_player22$date,units='days')
match_player22 = match_player22[match_player22$time_diff >= 0,]
match_player22 = match_player22 %>%
  group_by(match_api_id, away_player_11) %>%
  filter(time_diff == min(time_diff))
for (i in 858:895){
  colnames(match_player22)[i] = paste(colnames(match_player22)[i], "aw_p_11", sep = "_")
}
for (i in 858:895){
  match_player22[[i]] = paste(match_player22[[i]],match_player22[[56]],sep="_")
}
match_player22$time_diff=NULL
match_player22$id.y = NULL
match_player22$date.y=NULL
match_player22$date=NULL
match_player22$id.x=NULL
match_player22$id=NULL
match_player22$player_fifa_api_id.x=NULL
match_player22$player_fifa_api_id.y=NULL

################# ARules ##################
# all the matches that lose
loss_match = match_player22
### lose as home play, need to use away player attributes in the next steps
home_loss_match = loss_match[loss_match$opponent == loss_match$away_team_api_id,]
home_loss_ar = home_loss_match %>%
  select(preferred_foot_aw_p_1:gk_reflexes_Bucket_aw_p_11, result)
home_loss_ar = home_loss_ar[,-c(1,2)]
home_loss_ar = subset(home_loss_ar, select = -player_fifa_api_id)
### lose as away play, need to use home player attributes in the next steps
away_loss_match = loss_match[loss_match$opponent == loss_match$home_team_api_id,]
away_loss_ar = away_loss_match %>%
  select(preferred_foot_hp_1:gk_reflexes_Bucket_hp_11, result)
away_loss_ar = away_loss_ar[,-c(1,2)]
colnames(away_loss_ar) = colnames(home_loss_ar)
all_loss_ar = rbind(home_loss_ar, away_loss_ar)
##### add column name to values
column_names = names(all_loss_ar)

for (column in column_names) {
  all_loss_ar[[column]] = paste(column, all_loss_ar[[column]], sep = '_')
}

###### delete player numbers
for (i in 1:418) {
  for (j in 1:287) {
    all_loss_ar[j,i] = gsub('_aw_p_[0-9]', '', all_loss_ar[j,i])
  }
}
for (i in 1:418) {
  for (j in 1:287) {
    all_loss_ar[j,i] = gsub('[0-9]', '', all_loss_ar[j,i])
  }
}


write.csv(all_loss_ar,'sampdoria.csv')
```

```{r include = FALSE}
sampdoria_all3 = read.transactions("sampdoria.csv", format = "basket",
                           sep = ",", rm.duplicates = TRUE, skip = 1, header = FALSE)
# inspect(sampdoria_all3)
# itemInfo(sampdoria_all3)
rules <- apriori(sampdoria_all3, parameter = list(supp = 0.2, conf = 0.3), appearance = list(rhs = 'result_loss'))

rules_lift = sort(rules,by='lift',decreasing = TRUE)
```

 When we run the associations of player attributes that correspond to Sampdoria's loss, we found the following rules:

```{r, fig.width=16}
inspect(rules_lift[1:10])
```

```{r}
# plot(rules_lift[1:5], method = 'graph', engine = 'htmlwidget')
plot(rules_lift[1:10], method = 'paracoord')
```

#### Palermo

```{r, include = FALSE}
player_atts = player_atts_tbl %>% 
  collect()
long_team_name <- 'Palermo'
roma_record <- team_tbl %>% 
  collect() %>%
  filter(grepl(long_team_name, team_long_name))
home_matches <- match %>%
  filter(home_team_api_id == roma_record$team_api_id) %>%
  mutate(goal_diff = home_team_goal - away_team_goal) %>%
  mutate(type = 'home', opponent = away_team_api_id)
# hist(home_matches$goal_diff)
# summary(home_matches$goal_diff)

away_matches <- match %>%
  filter(away_team_api_id == roma_record$team_api_id) %>%
  mutate(goal_diff =away_team_goal - home_team_goal) %>%
  mutate(type = 'away', opponent = home_team_api_id)

# hist(away_matches$goal_diff)
# summary(away_matches$goal_diff)

all_match = rbind(home_matches, away_matches)
# ggplot(all_match, aes(type, goal_diff, col = type)) +
#   geom_boxplot()

all_match$result = as.numeric(all_match$goal_diff)
all_match[all_match$goal_diff > 0, ]$result = 'win'
all_match[all_match$goal_diff < 0, ]$result = 'loss'
all_match[all_match$goal_diff == 0, ]$result = 'draw'
# ggplot(all_match, aes(result)) +
#   geom_bar()

# ggplot(all_match[all_match$result == 'lose',], aes(as.factor(opponent))) +
#   geom_bar()

##################### position ###################
for (i in 1:11){
  x = paste("home_player_X",i,sep='')
  y = paste("home_player_Y",i,sep='')
  inp = paste("hm_ply_pos",i,sep="_")
  all_match[inp] = ifelse(all_match[x]==1 & all_match[y]==1,"Goaly",
                      ifelse(all_match[y]>=2 & all_match[y]<=5,"Defender",
                             ifelse(all_match[y]>=6 & all_match[y]<=8,"Mid Fielders",
                                    ifelse(all_match[y]>=9 & all_match[y]<=11,"Forwards","NA"))))
}

for (i in 1:11){
  x = paste("away_player_X",i,sep='')
  y = paste("away_player_Y",i,sep='')
  inp = paste("away_ply_pos",i,sep="_")
  all_match[inp] = ifelse(all_match[x]==1 & all_match[y]==1,"Goaly",
                      ifelse(all_match[y]>=2 & all_match[y]<=5,"Defender",
                             ifelse(all_match[y]>=6 & all_match[y]<=8,"Mid Fielders",
                                    ifelse(all_match[y]>=9 & all_match[y]<=11,"Forwards","NA"))))
}
```

```{r, include = FALSE, message= FALSE}
column_names = c("overall_rating","potential","crossing", "finishing","heading_accuracy",    
                 "short_passing", "volleys","dribbling",    
                 "curve", "free_kick_accuracy" ,"long_passing",    
                 "ball_control","acceleration","sprint_speed",    
                 "agility","reactions","balance",    
                 "shot_power","jumping","stamina",    
                 "strength","long_shots","aggression",    
                 "interceptions","positioning","vision",    
                 "penalties", "marking","standing_tackle",
                 "sliding_tackle","gk_diving","gk_handling",    
                 "gk_kicking","gk_positioning","gk_reflexes")

for (i in column_names){
  k = paste(i, "Bucket",sep="_")
  player_atts[k] = 
    cut(player_atts[[i]], breaks = 5, labels=c('low','low-med','med','med-high','high'))
}

player_atts <- select(player_atts, c(1:9, overall_rating_Bucket:gk_reflexes_Bucket))
player_atts = player_atts[, -c(5,6)]
all_match <- select(all_match, c(id:away_team_goal, home_player_1:away_player_11, opponent:away_ply_pos_11))

#######
# match_g <- gather(all_match, player, player_id, home_player_1:away_player_11, na.rm = TRUE)
# View(match_g)
# match_s <- arrange(match_g, match_api_id, player)
# View(match_s)
# #match_s <- select(match_s, c(1:10, player, player_id))

# colnames(match_s)[37] <- "player_api_id"

#HP1
match_player <- merge(x = all_match, y=player_atts,by.x='home_player_1',by.y='player_api_id',all.x=TRUE)
match_player$date.x<-ymd_hms(match_player$date.x)
match_player$date.y<-ymd_hms(match_player$date.y)
match_player$time_diff <- difftime(match_player$date.x, match_player$date.y,units='days')
match_player = match_player[match_player$time_diff >= 0,]
match_player_1 = match_player %>%
  group_by(match_api_id, home_player_1) %>%
  filter(time_diff == min(time_diff))
for (i in 61:98){
  colnames(match_player_1)[i] = paste(colnames(match_player_1)[i], "hp_1", sep = "_")
}
for (i in 61:98){
  match_player_1[[i]] = paste(match_player_1[[i]],match_player_1[[36]],sep="_")
}
match_player_1$time_diff=NULL
# HP2

match_player2 <- merge(x = match_player_1, y=player_atts,by.x='home_player_2',by.y='player_api_id',all.x=TRUE)
match_player2$date.x<-ymd(match_player2$date.x)
match_player2$date<-ymd_hms(match_player2$date)
match_player2$time_diff <- difftime(match_player2$date.x, match_player2$date,units='days')
match_player2 = match_player2[match_player2$time_diff >= 0,]
match_player2 = match_player2 %>%
  group_by(match_api_id, home_player_2) %>%
  filter(time_diff == min(time_diff))
for (i in 102:139){
  colnames(match_player2)[i] = paste(colnames(match_player2)[i], "hp_2", sep = "_")
}
for (i in 102:139){
  match_player2[[i]] = paste(match_player2[[i]],match_player2[[37]],sep="_")
}
match_player2$time_diff=NULL
match_player2$id.y = NULL
match_player2$date.y=NULL
match_player2$date=NULL
match_player2$id.x=NULL
##
#HP3

match_player3 <- merge(x = match_player2, y=player_atts,by.x='home_player_3',by.y='player_api_id',all.x=TRUE)
match_player3$date.x<-ymd(match_player3$date.x)
match_player3$date<-ymd_hms(match_player3$date)
match_player3$time_diff <- difftime(match_player3$date.x, match_player3$date,units='days')
match_player3 = match_player3[match_player3$time_diff >= 0,]
match_player3 = match_player3 %>%
  group_by(match_api_id, home_player_3) %>%
  filter(time_diff == min(time_diff))
for (i in 139:176){
  colnames(match_player3)[i] = paste(colnames(match_player3)[i], "hp_3", sep = "_")
}
for (i in 139:176){
  match_player3[[i]] = paste(match_player3[[i]],match_player3[[37]],sep="_")
}
match_player3$time_diff=NULL
match_player3$id.y = NULL
match_player3$date.y=NULL
match_player3$date=NULL
match_player3$id.x=NULL
match_player3$player_fifa_api_id=NULL

#HP4

match_player4 <- merge(x = match_player3, y=player_atts,by.x='home_player_4',by.y='player_api_id',all.x=TRUE)
match_player4$date.x<-ymd(match_player4$date.x)
match_player4$date<-ymd_hms(match_player4$date)
match_player4$time_diff <- difftime(match_player4$date.x, match_player4$date,units='days')
match_player4 = match_player4[match_player4$time_diff >= 0,]
match_player4 = match_player4 %>%
  group_by(match_api_id, home_player_4) %>%
  filter(time_diff == min(time_diff))
for (i in 176:213){
  colnames(match_player4)[i] = paste(colnames(match_player4)[i], "hp_4", sep = "_")
}
for (i in 176:213){
  match_player4[[i]] = paste(match_player4[[i]],match_player4[[38]],sep="_")
}
match_player4$time_diff=NULL
match_player4$id.y = NULL
match_player4$date.y=NULL
match_player4$date=NULL
match_player4$id.x=NULL
match_player4$id=NULL
match_player4$player_fifa_api_id.x=NULL
match_player4$player_fifa_api_id.y=NULL
#HP5

match_player5 <- merge(x = match_player4, y=player_atts,by.x='home_player_5',by.y='player_api_id',all.x=TRUE)
match_player5$date.x<-ymd(match_player5$date.x)
match_player5$date<-ymd_hms(match_player5$date)
match_player5$time_diff <- difftime(match_player5$date.x, match_player5$date,units='days')
match_player5 = match_player5[match_player5$time_diff >= 0,]
match_player5 = match_player5 %>%
  group_by(match_api_id, home_player_5) %>%
  filter(time_diff == min(time_diff))
for (i in 213:250){
  colnames(match_player5)[i] = paste(colnames(match_player5)[i], "hp_5", sep = "_")
}
for (i in 213:250){
  match_player5[[i]] = paste(match_player5[[i]],match_player5[[39]],sep="_")
}
match_player5$time_diff=NULL
match_player5$id.y = NULL
match_player5$date.y=NULL
match_player5$date=NULL
match_player5$id.x=NULL
match_player5$id=NULL
###
#HP6
match_player6 <- merge(x = match_player5, y=player_atts,by.x='home_player_6',by.y='player_api_id',all.x=TRUE)
match_player6$date.x<-ymd(match_player6$date.x)
match_player6$date<-ymd_hms(match_player6$date)
match_player6$time_diff <- difftime(match_player6$date.x, match_player6$date,units='days')
match_player6 = match_player6[match_player6$time_diff >= 0,]
match_player6 = match_player6 %>%
  group_by(match_api_id, home_player_6) %>%
  filter(time_diff == min(time_diff))
for (i in 252:289){
  colnames(match_player6)[i] = paste(colnames(match_player6)[i], "hp_6", sep = "_")
}
for (i in 252:289){
  match_player6[[i]] = paste(match_player6[[i]],match_player6[[40]],sep="_")
}
match_player6$time_diff=NULL
match_player6$id.y = NULL
match_player6$date.y=NULL
match_player6$date=NULL
match_player6$id.x=NULL
match_player6$id=NULL
match_player6$player_fifa_api_id.x=NULL
match_player6$player_fifa_api_id.y=NULL

#HP7
match_player7 <- merge(x = match_player6, y=player_atts,by.x='home_player_7',by.y='player_api_id',all.x=TRUE)
match_player7$date.x<-ymd(match_player7$date.x)
match_player7$date<-ymd_hms(match_player7$date)
match_player7$time_diff <- difftime(match_player7$date.x, match_player7$date,units='days')
match_player7 = match_player7[match_player7$time_diff >= 0,]
match_player7 = match_player7 %>%
  group_by(match_api_id, home_player_7) %>%
  filter(time_diff == min(time_diff))
for (i in 289:326){
  colnames(match_player7)[i] = paste(colnames(match_player7)[i], "hp_7", sep = "_")
}
for (i in 289:326){
  match_player7[[i]] = paste(match_player7[[i]],match_player7[[41]],sep="_")
}
match_player7$time_diff=NULL
match_player7$id.y = NULL
match_player7$date.y=NULL
match_player7$date=NULL
match_player7$id.x=NULL
match_player7$id=NULL
match_player7$player_fifa_api_id.x=NULL
match_player7$player_fifa_api_id.y=NULL

#HP8
match_player8 <- merge(x = match_player7, y=player_atts,by.x='home_player_8',by.y='player_api_id',all.x=TRUE)
match_player8$date.x<-ymd(match_player8$date.x)
match_player8$date<-ymd_hms(match_player8$date)
match_player8$time_diff <- difftime(match_player8$date.x, match_player8$date,units='days')
match_player8 = match_player8[match_player8$time_diff >= 0,]
match_player8 = match_player8 %>%
  group_by(match_api_id, home_player_8) %>%
  filter(time_diff == min(time_diff))
for (i in 326:363){
  colnames(match_player8)[i] = paste(colnames(match_player8)[i], "hp_8", sep = "_")
}
for (i in 326:363){
  match_player8[[i]] = paste(match_player8[[i]],match_player8[[42]],sep="_")
}
match_player8$time_diff=NULL
match_player8$id.y = NULL
match_player8$date.y=NULL
match_player8$date=NULL
match_player8$id.x=NULL
match_player8$id=NULL
match_player8$player_fifa_api_id.x=NULL
match_player8$player_fifa_api_id.y=NULL

#HP9
match_player9 <- merge(x = match_player8, y=player_atts,by.x='home_player_9',by.y='player_api_id',all.x=TRUE)
match_player9$date.x<-ymd(match_player9$date.x)
match_player9$date<-ymd_hms(match_player9$date)
match_player9$time_diff <- difftime(match_player9$date.x, match_player9$date,units='days')
match_player9 = match_player9[match_player9$time_diff >= 0,]
match_player9 = match_player9 %>%
  group_by(match_api_id, home_player_9) %>%
  filter(time_diff == min(time_diff))
for (i in 365:402){
  colnames(match_player9)[i] = paste(colnames(match_player9)[i], "hp_9", sep = "_")
}
for (i in 365:402){
  match_player9[[i]] = paste(match_player9[[i]],match_player9[[43]],sep="_")
}
match_player9$time_diff=NULL
match_player9$id.y = NULL
match_player9$date.y=NULL
match_player9$date=NULL
match_player9$id.x=NULL
match_player9$id=NULL
match_player9$player_fifa_api_id.x=NULL
match_player9$player_fifa_api_id.y=NULL

#HP10
match_player10 <- merge(x = match_player9, y=player_atts,by.x='home_player_10',by.y='player_api_id',all.x=TRUE)
match_player10$date.x<-ymd(match_player10$date.x)
match_player10$date<-ymd_hms(match_player10$date)
match_player10$time_diff <- difftime(match_player10$date.x, match_player10$date,units='days')
match_player10 = match_player10[match_player10$time_diff >= 0,]
match_player10 = match_player10 %>%
  group_by(match_api_id, home_player_10) %>%
  filter(time_diff == min(time_diff))
for (i in 402:439){
  colnames(match_player10)[i] = paste(colnames(match_player10)[i], "hp_10", sep = "_")
}
for (i in 402:439){
  match_player10[[i]] = paste(match_player10[[i]],match_player10[[44]],sep="_")
}
match_player10$time_diff=NULL
match_player10$id.y = NULL
match_player10$date.y=NULL
match_player10$date=NULL
match_player10$id.x=NULL
match_player10$id=NULL
match_player10$player_fifa_api_id.x=NULL
match_player10$player_fifa_api_id.y=NULL

#HP11
match_player11 <- merge(x = match_player10, y=player_atts,by.x='home_player_11',by.y='player_api_id',all.x=TRUE)
match_player11$date.x<-ymd(match_player11$date.x)
match_player11$date<-ymd_hms(match_player11$date)
match_player11$time_diff <- difftime(match_player11$date.x, match_player11$date,units='days')
match_player11 = match_player11[match_player11$time_diff >= 0,]
match_player11 = match_player11 %>%
  group_by(match_api_id, home_player_11) %>%
  filter(time_diff == min(time_diff))
for (i in 441:478){
  colnames(match_player11)[i] = paste(colnames(match_player11)[i], "hp_11", sep = "_")
}
for (i in 441:478){
  match_player11[[i]] = paste(match_player11[[i]],match_player11[[45]],sep="_")
}
match_player11$time_diff=NULL
match_player11$id.y = NULL
match_player11$date.y=NULL
match_player11$date=NULL
match_player11$id.x=NULL
match_player11$id=NULL
match_player11$player_fifa_api_id.x=NULL
match_player11$player_fifa_api_id.y=NULL

#AP1

match_player12 <- merge(x = match_player11, y=player_atts,by.x='away_player_1',by.y='player_api_id',all.x=TRUE)
match_player12$date.x<-ymd(match_player12$date.x)
match_player12$date<-ymd_hms(match_player12$date)
match_player12$time_diff <- difftime(match_player12$date.x, match_player12$date,units='days')
match_player12 = match_player12[match_player12$time_diff >= 0,]
match_player12 = match_player12 %>%
  group_by(match_api_id, away_player_1) %>%
  filter(time_diff == min(time_diff))
for (i in 478:515){
  colnames(match_player12)[i] = paste(colnames(match_player12)[i], "aw_p_1", sep = "_")
}
for (i in 478:515){
  match_player12[[i]] = paste(match_player12[[i]],match_player12[[46]],sep="_")
}
match_player12$time_diff=NULL
match_player12$id.y = NULL
match_player12$date.y=NULL
match_player12$date=NULL
match_player12$id.x=NULL
match_player12$id=NULL
match_player12$player_fifa_api_id.x=NULL
match_player12$player_fifa_api_id.y=NULL
#AP2
match_player13 <- merge(x = match_player12, y=player_atts,by.x='away_player_2',by.y='player_api_id',all.x=TRUE)
match_player13$date.x<-ymd(match_player13$date.x)
match_player13$date<-ymd_hms(match_player13$date)
match_player13$time_diff <- difftime(match_player13$date.x, match_player13$date,units='days')
match_player13 = match_player13[match_player13$time_diff >= 0,]
match_player13 = match_player13 %>%
  group_by(match_api_id, away_player_2) %>%
  filter(time_diff == min(time_diff))
for (i in 517:554){
  colnames(match_player13)[i] = paste(colnames(match_player13)[i], "aw_p_2", sep = "_")
}
for (i in 517:554){
  match_player13[[i]] = paste(match_player13[[i]],match_player13[[47]],sep="_")
}
match_player13$time_diff=NULL
match_player13$id.y = NULL
match_player13$date.y=NULL
match_player13$date=NULL
match_player13$id.x=NULL
match_player13$id=NULL
match_player13$player_fifa_api_id.x=NULL
match_player13$player_fifa_api_id.y=NULL

#AP3
match_player14 <- merge(x = match_player13, y=player_atts,by.x='away_player_3',by.y='player_api_id',all.x=TRUE)
match_player14$date.x<-ymd(match_player14$date.x)
match_player14$date<-ymd_hms(match_player14$date)
match_player14$time_diff <- difftime(match_player14$date.x, match_player14$date,units='days')
match_player14 = match_player14[match_player14$time_diff >= 0,]
match_player14 = match_player14 %>%
  group_by(match_api_id, away_player_3) %>%
  filter(time_diff == min(time_diff))
for (i in 554:591){
  colnames(match_player14)[i] = paste(colnames(match_player14)[i], "aw_p_3", sep = "_")
}
for (i in 554:591){
  match_player14[[i]] = paste(match_player14[[i]],match_player14[[48]],sep="_")
}
match_player14$time_diff=NULL
match_player14$id.y = NULL
match_player14$date.y=NULL
match_player14$date=NULL
match_player14$id.x=NULL
match_player14$id=NULL
match_player14$player_fifa_api_id.x=NULL
match_player14$player_fifa_api_id.y=NULL

#AP4
match_player15 <- merge(x = match_player14, y=player_atts,by.x='away_player_4',by.y='player_api_id',all.x=TRUE)
match_player15$date.x<-ymd(match_player15$date.x)
match_player15$date<-ymd_hms(match_player15$date)
match_player15$time_diff <- difftime(match_player15$date.x, match_player15$date,units='days')
match_player15 = match_player15[match_player15$time_diff >= 0,]
match_player15 = match_player15 %>%
  group_by(match_api_id, away_player_4) %>%
  filter(time_diff == min(time_diff))
for (i in 593:630){
  colnames(match_player15)[i] = paste(colnames(match_player15)[i], "aw_p_4", sep = "_")
}
for (i in 593:630){
  match_player15[[i]] = paste(match_player15[[i]],match_player15[[49]],sep="_")
}
match_player15$time_diff=NULL
match_player15$id.y = NULL
match_player15$date.y=NULL
match_player15$date=NULL
match_player15$id.x=NULL
match_player15$id=NULL
match_player15$player_fifa_api_id.x=NULL
match_player15$player_fifa_api_id.y=NULL

#AP5
match_player16 <- merge(x = match_player15, y=player_atts,by.x='away_player_5',by.y='player_api_id',all.x=TRUE)
match_player16$date.x<-ymd(match_player16$date.x)
match_player16$date<-ymd_hms(match_player16$date)
match_player16$time_diff <- difftime(match_player16$date.x, match_player16$date,units='days')
match_player16 = match_player16[match_player16$time_diff >= 0,]
match_player16 = match_player16 %>%
  group_by(match_api_id, away_player_5) %>%
  filter(time_diff == min(time_diff))
for (i in 630:667){
  colnames(match_player16)[i] = paste(colnames(match_player16)[i], "aw_p_5", sep = "_")
}
for (i in 630:667){
  match_player16[[i]] = paste(match_player16[[i]],match_player16[[50]],sep="_")
}
match_player16$time_diff=NULL
match_player16$id.y = NULL
match_player16$date.y=NULL
match_player16$date=NULL
match_player16$id.x=NULL
match_player16$id=NULL
match_player16$player_fifa_api_id.x=NULL
match_player16$player_fifa_api_id.y=NULL
#AP6
match_player17 <- merge(x = match_player16, y=player_atts,by.x='away_player_6',by.y='player_api_id',all.x=TRUE)
match_player17$date.x<-ymd(match_player17$date.x)
match_player17$date<-ymd_hms(match_player17$date)
match_player17$time_diff <- difftime(match_player17$date.x, match_player17$date,units='days')
match_player17 = match_player17[match_player17$time_diff >= 0,]
match_player17 = match_player17 %>%
  group_by(match_api_id, away_player_6) %>%
  filter(time_diff == min(time_diff))
for (i in 669:706){
  colnames(match_player17)[i] = paste(colnames(match_player17)[i], "aw_p_6", sep = "_")
}
for (i in 669:706){
  match_player17[[i]] = paste(match_player17[[i]],match_player17[[51]],sep="_")
}
match_player17$time_diff=NULL
match_player17$id.y = NULL
match_player17$date.y=NULL
match_player17$date=NULL
match_player17$id.x=NULL
match_player17$id=NULL
match_player17$player_fifa_api_id.x=NULL
match_player17$player_fifa_api_id.y=NULL
#AP7
match_player18 <- merge(x = match_player17, y=player_atts,by.x='away_player_7',by.y='player_api_id',all.x=TRUE)
match_player18$date.x<-ymd(match_player18$date.x)
match_player18$date<-ymd_hms(match_player18$date)
match_player18$time_diff <- difftime(match_player18$date.x, match_player18$date,units='days')
match_player18 = match_player18[match_player18$time_diff >= 0,]
match_player18 = match_player18 %>%
  group_by(match_api_id, away_player_7) %>%
  filter(time_diff == min(time_diff))
for (i in 706:743){
  colnames(match_player18)[i] = paste(colnames(match_player18)[i], "aw_p_7", sep = "_")
}
for (i in 706:743){
  match_player18[[i]] = paste(match_player18[[i]],match_player18[[52]],sep="_")
}
match_player18$time_diff=NULL
match_player18$id.y = NULL
match_player18$date.y=NULL
match_player18$date=NULL
match_player18$id.x=NULL
match_player18$id=NULL
match_player18$player_fifa_api_id.x=NULL
match_player18$player_fifa_api_id.y=NULL
#AP8
match_player19 <- merge(x = match_player18, y=player_atts,by.x='away_player_8',by.y='player_api_id',all.x=TRUE)
match_player19$date.x<-ymd(match_player19$date.x)
match_player19$date<-ymd_hms(match_player19$date)
match_player19$time_diff <- difftime(match_player19$date.x, match_player19$date,units='days')
match_player19 = match_player19[match_player19$time_diff >= 0,]
match_player19 = match_player19 %>%
  group_by(match_api_id, away_player_8) %>%
  filter(time_diff == min(time_diff))
for (i in 745:782){
  colnames(match_player19)[i] = paste(colnames(match_player19)[i], "aw_p_8", sep = "_")
}
for (i in 745:782){
  match_player19[[i]] = paste(match_player19[[i]],match_player19[[53]],sep="_")
}
match_player19$time_diff=NULL
match_player19$id.y = NULL
match_player19$date.y=NULL
match_player19$date=NULL
match_player19$id.x=NULL
match_player19$id=NULL
match_player19$player_fifa_api_id.x=NULL
match_player19$player_fifa_api_id.y=NULL
#AP9
match_player20 <- merge(x = match_player19, y=player_atts,by.x='away_player_9',by.y='player_api_id',all.x=TRUE)
match_player20$date.x<-ymd(match_player20$date.x)
match_player20$date<-ymd_hms(match_player20$date)
match_player20$time_diff <- difftime(match_player20$date.x, match_player20$date,units='days')
match_player20 = match_player20[match_player20$time_diff >= 0,]
match_player20 = match_player20 %>%
  group_by(match_api_id, away_player_9) %>%
  filter(time_diff == min(time_diff))
for (i in 782:819){
  colnames(match_player20)[i] = paste(colnames(match_player20)[i], "aw_p_9", sep = "_")
}
for (i in 782:819){
  match_player20[[i]] = paste(match_player20[[i]],match_player20[[54]],sep="_")
}
match_player20$time_diff=NULL
match_player20$id.y = NULL
match_player20$date.y=NULL
match_player20$date=NULL
match_player20$id.x=NULL
match_player20$id=NULL
match_player20$player_fifa_api_id.x=NULL
match_player20$player_fifa_api_id.y=NULL
#AP10
match_player21 <- merge(x = match_player20, y=player_atts,by.x='away_player_10',by.y='player_api_id',all.x=TRUE)
match_player21$date.x<-ymd(match_player21$date.x)
match_player21$date<-ymd_hms(match_player21$date)
match_player21$time_diff <- difftime(match_player21$date.x, match_player21$date,units='days')
match_player21 = match_player21[match_player21$time_diff >= 0,]
match_player21 = match_player21 %>%
  group_by(match_api_id, away_player_10) %>%
  filter(time_diff == min(time_diff))
for (i in 821:858){
  colnames(match_player21)[i] = paste(colnames(match_player21)[i], "aw_p_10", sep = "_")
}
for (i in 821:858){
  match_player21[[i]] = paste(match_player21[[i]],match_player21[[55]],sep="_")
}
match_player21$time_diff=NULL
match_player21$id.y = NULL
match_player21$date.y=NULL
match_player21$date=NULL
match_player21$id.x=NULL
match_player21$id=NULL
match_player21$player_fifa_api_id.x=NULL
match_player21$player_fifa_api_id.y=NULL
#AP11
match_player22 <- merge(x = match_player21, y=player_atts,by.x='away_player_11',by.y='player_api_id',all.x=TRUE)
match_player22$date.x<-ymd(match_player22$date.x)
match_player22$date<-ymd_hms(match_player22$date)
match_player22$time_diff <- difftime(match_player22$date.x, match_player22$date,units='days')
match_player22 = match_player22[match_player22$time_diff >= 0,]
match_player22 = match_player22 %>%
  group_by(match_api_id, away_player_11) %>%
  filter(time_diff == min(time_diff))
for (i in 858:895){
  colnames(match_player22)[i] = paste(colnames(match_player22)[i], "aw_p_11", sep = "_")
}
for (i in 858:895){
  match_player22[[i]] = paste(match_player22[[i]],match_player22[[56]],sep="_")
}
match_player22$time_diff=NULL
match_player22$id.y = NULL
match_player22$date.y=NULL
match_player22$date=NULL
match_player22$id.x=NULL
match_player22$id=NULL
match_player22$player_fifa_api_id.x=NULL
match_player22$player_fifa_api_id.y=NULL

################# ARules ##################
# all the matches that lose
loss_match = match_player22
### lose as home play, need to use away player attributes in the next steps
home_loss_match = loss_match[loss_match$opponent == loss_match$away_team_api_id,]
home_loss_ar = home_loss_match %>%
  select(preferred_foot_aw_p_1:gk_reflexes_Bucket_aw_p_11, result)
home_loss_ar = home_loss_ar[,-c(1,2)]
home_loss_ar = subset(home_loss_ar, select = -player_fifa_api_id)
### lose as away play, need to use home player attributes in the next steps
away_loss_match = loss_match[loss_match$opponent == loss_match$home_team_api_id,]
away_loss_ar = away_loss_match %>%
  select(preferred_foot_hp_1:gk_reflexes_Bucket_hp_11, result)
away_loss_ar = away_loss_ar[,-c(1,2)]
colnames(away_loss_ar) = colnames(home_loss_ar)
all_loss_ar = rbind(home_loss_ar, away_loss_ar)
##### add column name to values
column_names = names(all_loss_ar)

for (column in column_names) {
  all_loss_ar[[column]] = paste(column, all_loss_ar[[column]], sep = '_')
}

###### delete player numbers
for (i in 1:418) {
  for (j in 1:287) {
    all_loss_ar[j,i] = gsub('_aw_p_[0-9]', '', all_loss_ar[j,i])
  }
}
for (i in 1:418) {
  for (j in 1:287) {
    all_loss_ar[j,i] = gsub('[0-9]', '', all_loss_ar[j,i])
  }
}


write.csv(all_loss_ar,'palermo.csv')
```

  When we run the associations of player attributes that correspond to Palermo's loss, we found the following rules:
  
```{r, include = FALSE, message=FALSE, results="hide"}
palermo_all3 = read.transactions("palermo.csv", format = "basket",
                           sep = ",", rm.duplicates = TRUE, skip =1 , header = FALSE)

# inspect(palermo_all3)
# itemInfo(palermo_all3)
rules <- apriori(palermo_all3, parameter = list(supp = 0.2, conf = 0.3), appearance = list(rhs = 'result_loss'))

rules_lift = sort(rules,by='lift',decreasing = TRUE)

```
```{r, fig.width=16}
inspect(rules_lift[1:10])
```

```{r}
# plot(rules_lift[1:5], method = 'graph', engine = 'htmlwidget')
plot(rules_lift[1:10], method = 'paracoord')
```


#### Genoa 

```{r, include = FALSE}
player_atts = player_atts_tbl %>% 
  collect()
long_team_name <- 'Genoa'
roma_record <- team_tbl %>% 
  collect() %>%
  filter(grepl(long_team_name, team_long_name))
home_matches <- match %>%
  filter(home_team_api_id == roma_record$team_api_id) %>%
  mutate(goal_diff = home_team_goal - away_team_goal) %>%
  mutate(type = 'home', opponent = away_team_api_id)
# hist(home_matches$goal_diff)
# summary(home_matches$goal_diff)

away_matches <- match %>%
  filter(away_team_api_id == roma_record$team_api_id) %>%
  mutate(goal_diff =away_team_goal - home_team_goal) %>%
  mutate(type = 'away', opponent = home_team_api_id)

# hist(away_matches$goal_diff)
# summary(away_matches$goal_diff)

all_match = rbind(home_matches, away_matches)
# ggplot(all_match, aes(type, goal_diff, col = type)) +
#   geom_boxplot()

all_match$result = as.numeric(all_match$goal_diff)
all_match[all_match$goal_diff > 0, ]$result = 'win'
all_match[all_match$goal_diff < 0, ]$result = 'loss'
all_match[all_match$goal_diff == 0, ]$result = 'draw'
# ggplot(all_match, aes(result)) +
#   geom_bar()

# ggplot(all_match[all_match$result == 'lose',], aes(as.factor(opponent))) +
#   geom_bar()

##################### position ###################
for (i in 1:11){
  x = paste("home_player_X",i,sep='')
  y = paste("home_player_Y",i,sep='')
  inp = paste("hm_ply_pos",i,sep="_")
  all_match[inp] = ifelse(all_match[x]==1 & all_match[y]==1,"Goaly",
                      ifelse(all_match[y]>=2 & all_match[y]<=5,"Defender",
                             ifelse(all_match[y]>=6 & all_match[y]<=8,"Mid Fielders",
                                    ifelse(all_match[y]>=9 & all_match[y]<=11,"Forwards","NA"))))
}

for (i in 1:11){
  x = paste("away_player_X",i,sep='')
  y = paste("away_player_Y",i,sep='')
  inp = paste("away_ply_pos",i,sep="_")
  all_match[inp] = ifelse(all_match[x]==1 & all_match[y]==1,"Goaly",
                      ifelse(all_match[y]>=2 & all_match[y]<=5,"Defender",
                             ifelse(all_match[y]>=6 & all_match[y]<=8,"Mid Fielders",
                                    ifelse(all_match[y]>=9 & all_match[y]<=11,"Forwards","NA"))))
}
```

```{r, include = FALSE}
column_names = c("overall_rating","potential","crossing", "finishing","heading_accuracy",    
                 "short_passing", "volleys","dribbling",    
                 "curve", "free_kick_accuracy" ,"long_passing",    
                 "ball_control","acceleration","sprint_speed",    
                 "agility","reactions","balance",    
                 "shot_power","jumping","stamina",    
                 "strength","long_shots","aggression",    
                 "interceptions","positioning","vision",    
                 "penalties", "marking","standing_tackle",
                 "sliding_tackle","gk_diving","gk_handling",    
                 "gk_kicking","gk_positioning","gk_reflexes")

for (i in column_names){
  k = paste(i, "Bucket",sep="_")
  player_atts[k] = 
    cut(player_atts[[i]], breaks = 5, labels=c('low','low-med','med','med-high','high'))
}

player_atts <- select(player_atts, c(1:9, overall_rating_Bucket:gk_reflexes_Bucket))
player_atts = player_atts[, -c(5,6)]
all_match <- select(all_match, c(id:away_team_goal, home_player_1:away_player_11, opponent:away_ply_pos_11))

#######
# match_g <- gather(all_match, player, player_id, home_player_1:away_player_11, na.rm = TRUE)
# View(match_g)
# match_s <- arrange(match_g, match_api_id, player)
# View(match_s)
# #match_s <- select(match_s, c(1:10, player, player_id))
# colnames(match_s)[37] <- "player_api_id"

#HP1
match_player <- merge(x = all_match, y=player_atts,by.x='home_player_1',by.y='player_api_id',all.x=TRUE)
match_player$date.x<-ymd_hms(match_player$date.x)
match_player$date.y<-ymd_hms(match_player$date.y)
match_player$time_diff <- difftime(match_player$date.x, match_player$date.y,units='days')
match_player = match_player[match_player$time_diff >= 0,]
match_player_1 = match_player %>%
  group_by(match_api_id, home_player_1) %>%
  filter(time_diff == min(time_diff))
for (i in 61:98){
  colnames(match_player_1)[i] = paste(colnames(match_player_1)[i], "hp_1", sep = "_")
  }
for (i in 61:98){
  match_player_1[[i]] = paste(match_player_1[[i]],match_player_1[[36]],sep="_")
}
match_player_1$time_diff=NULL
# HP2

match_player2 <- merge(x = match_player_1, y=player_atts,by.x='home_player_2',by.y='player_api_id',all.x=TRUE)
match_player2$date.x<-ymd(match_player2$date.x)
match_player2$date<-ymd_hms(match_player2$date)
match_player2$time_diff <- difftime(match_player2$date.x, match_player2$date,units='days')
match_player2 = match_player2[match_player2$time_diff >= 0,]
match_player2 = match_player2 %>%
  group_by(match_api_id, home_player_2) %>%
  filter(time_diff == min(time_diff))
for (i in 102:139){
  colnames(match_player2)[i] = paste(colnames(match_player2)[i], "hp_2", sep = "_")
}
for (i in 102:139){
  match_player2[[i]] = paste(match_player2[[i]],match_player2[[37]],sep="_")
}
match_player2$time_diff=NULL
match_player2$id.y = NULL
match_player2$date.y=NULL
match_player2$date=NULL
match_player2$id.x=NULL
##
#HP3

match_player3 <- merge(x = match_player2, y=player_atts,by.x='home_player_3',by.y='player_api_id',all.x=TRUE)
match_player3$date.x<-ymd(match_player3$date.x)
match_player3$date<-ymd_hms(match_player3$date)
match_player3$time_diff <- difftime(match_player3$date.x, match_player3$date,units='days')
match_player3 = match_player3[match_player3$time_diff >= 0,]
match_player3 = match_player3 %>%
  group_by(match_api_id, home_player_3) %>%
  filter(time_diff == min(time_diff))
for (i in 139:176){
  colnames(match_player3)[i] = paste(colnames(match_player3)[i], "hp_3", sep = "_")
}
for (i in 139:176){
  match_player3[[i]] = paste(match_player3[[i]],match_player3[[37]],sep="_")
}
match_player3$time_diff=NULL
match_player3$id.y = NULL
match_player3$date.y=NULL
match_player3$date=NULL
match_player3$id.x=NULL
match_player3$player_fifa_api_id=NULL

#HP4

match_player4 <- merge(x = match_player3, y=player_atts,by.x='home_player_4',by.y='player_api_id',all.x=TRUE)
match_player4$date.x<-ymd(match_player4$date.x)
match_player4$date<-ymd_hms(match_player4$date)
match_player4$time_diff <- difftime(match_player4$date.x, match_player4$date,units='days')
match_player4 = match_player4[match_player4$time_diff >= 0,]
match_player4 = match_player4 %>%
  group_by(match_api_id, home_player_4) %>%
  filter(time_diff == min(time_diff))
for (i in 176:213){
  colnames(match_player4)[i] = paste(colnames(match_player4)[i], "hp_4", sep = "_")
}
for (i in 176:213){
  match_player4[[i]] = paste(match_player4[[i]],match_player4[[38]],sep="_")
}
match_player4$time_diff=NULL
match_player4$id.y = NULL
match_player4$date.y=NULL
match_player4$date=NULL
match_player4$id.x=NULL
match_player4$id=NULL
match_player4$player_fifa_api_id.x=NULL
match_player4$player_fifa_api_id.y=NULL
#HP5

match_player5 <- merge(x = match_player4, y=player_atts,by.x='home_player_5',by.y='player_api_id',all.x=TRUE)
match_player5$date.x<-ymd(match_player5$date.x)
match_player5$date<-ymd_hms(match_player5$date)
match_player5$time_diff <- difftime(match_player5$date.x, match_player5$date,units='days')
match_player5 = match_player5[match_player5$time_diff >= 0,]
match_player5 = match_player5 %>%
  group_by(match_api_id, home_player_5) %>%
  filter(time_diff == min(time_diff))
for (i in 213:250){
  colnames(match_player5)[i] = paste(colnames(match_player5)[i], "hp_5", sep = "_")
}
for (i in 213:250){
  match_player5[[i]] = paste(match_player5[[i]],match_player5[[39]],sep="_")
}
match_player5$time_diff=NULL
match_player5$id.y = NULL
match_player5$date.y=NULL
match_player5$date=NULL
match_player5$id.x=NULL
match_player5$id=NULL
###
#HP6
match_player6 <- merge(x = match_player5, y=player_atts,by.x='home_player_6',by.y='player_api_id',all.x=TRUE)
match_player6$date.x<-ymd(match_player6$date.x)
match_player6$date<-ymd_hms(match_player6$date)
match_player6$time_diff <- difftime(match_player6$date.x, match_player6$date,units='days')
match_player6 = match_player6[match_player6$time_diff >= 0,]
match_player6 = match_player6 %>%
  group_by(match_api_id, home_player_6) %>%
  filter(time_diff == min(time_diff))
for (i in 252:289){
  colnames(match_player6)[i] = paste(colnames(match_player6)[i], "hp_6", sep = "_")
}
for (i in 252:289){
  match_player6[[i]] = paste(match_player6[[i]],match_player6[[40]],sep="_")
}
match_player6$time_diff=NULL
match_player6$id.y = NULL
match_player6$date.y=NULL
match_player6$date=NULL
match_player6$id.x=NULL
match_player6$id=NULL
match_player6$player_fifa_api_id.x=NULL
match_player6$player_fifa_api_id.y=NULL

#HP7
match_player7 <- merge(x = match_player6, y=player_atts,by.x='home_player_7',by.y='player_api_id',all.x=TRUE)
match_player7$date.x<-ymd(match_player7$date.x)
match_player7$date<-ymd_hms(match_player7$date)
match_player7$time_diff <- difftime(match_player7$date.x, match_player7$date,units='days')
match_player7 = match_player7[match_player7$time_diff >= 0,]
match_player7 = match_player7 %>%
  group_by(match_api_id, home_player_7) %>%
  filter(time_diff == min(time_diff))
for (i in 289:326){
  colnames(match_player7)[i] = paste(colnames(match_player7)[i], "hp_7", sep = "_")
}
for (i in 289:326){
  match_player7[[i]] = paste(match_player7[[i]],match_player7[[41]],sep="_")
}
match_player7$time_diff=NULL
match_player7$id.y = NULL
match_player7$date.y=NULL
match_player7$date=NULL
match_player7$id.x=NULL
match_player7$id=NULL
match_player7$player_fifa_api_id.x=NULL
match_player7$player_fifa_api_id.y=NULL

#HP8
match_player8 <- merge(x = match_player7, y=player_atts,by.x='home_player_8',by.y='player_api_id',all.x=TRUE)
match_player8$date.x<-ymd(match_player8$date.x)
match_player8$date<-ymd_hms(match_player8$date)
match_player8$time_diff <- difftime(match_player8$date.x, match_player8$date,units='days')
match_player8 = match_player8[match_player8$time_diff >= 0,]
match_player8 = match_player8 %>%
  group_by(match_api_id, home_player_8) %>%
  filter(time_diff == min(time_diff))
for (i in 326:363){
  colnames(match_player8)[i] = paste(colnames(match_player8)[i], "hp_8", sep = "_")
}
for (i in 326:363){
  match_player8[[i]] = paste(match_player8[[i]],match_player8[[42]],sep="_")
}
match_player8$time_diff=NULL
match_player8$id.y = NULL
match_player8$date.y=NULL
match_player8$date=NULL
match_player8$id.x=NULL
match_player8$id=NULL
match_player8$player_fifa_api_id.x=NULL
match_player8$player_fifa_api_id.y=NULL

#HP9
match_player9 <- merge(x = match_player8, y=player_atts,by.x='home_player_9',by.y='player_api_id',all.x=TRUE)
match_player9$date.x<-ymd(match_player9$date.x)
match_player9$date<-ymd_hms(match_player9$date)
match_player9$time_diff <- difftime(match_player9$date.x, match_player9$date,units='days')
match_player9 = match_player9[match_player9$time_diff >= 0,]
match_player9 = match_player9 %>%
  group_by(match_api_id, home_player_9) %>%
  filter(time_diff == min(time_diff))
for (i in 365:402){
  colnames(match_player9)[i] = paste(colnames(match_player9)[i], "hp_9", sep = "_")
}
for (i in 365:402){
  match_player9[[i]] = paste(match_player9[[i]],match_player9[[43]],sep="_")
}
match_player9$time_diff=NULL
match_player9$id.y = NULL
match_player9$date.y=NULL
match_player9$date=NULL
match_player9$id.x=NULL
match_player9$id=NULL
match_player9$player_fifa_api_id.x=NULL
match_player9$player_fifa_api_id.y=NULL

#HP10
match_player10 <- merge(x = match_player9, y=player_atts,by.x='home_player_10',by.y='player_api_id',all.x=TRUE)
match_player10$date.x<-ymd(match_player10$date.x)
match_player10$date<-ymd_hms(match_player10$date)
match_player10$time_diff <- difftime(match_player10$date.x, match_player10$date,units='days')
match_player10 = match_player10[match_player10$time_diff >= 0,]
match_player10 = match_player10 %>%
  group_by(match_api_id, home_player_10) %>%
  filter(time_diff == min(time_diff))
for (i in 402:439){
  colnames(match_player10)[i] = paste(colnames(match_player10)[i], "hp_10", sep = "_")
}
for (i in 402:439){
  match_player10[[i]] = paste(match_player10[[i]],match_player10[[44]],sep="_")
}
match_player10$time_diff=NULL
match_player10$id.y = NULL
match_player10$date.y=NULL
match_player10$date=NULL
match_player10$id.x=NULL
match_player10$id=NULL
match_player10$player_fifa_api_id.x=NULL
match_player10$player_fifa_api_id.y=NULL

#HP11
match_player11 <- merge(x = match_player10, y=player_atts,by.x='home_player_11',by.y='player_api_id',all.x=TRUE)
match_player11$date.x<-ymd(match_player11$date.x)
match_player11$date<-ymd_hms(match_player11$date)
match_player11$time_diff <- difftime(match_player11$date.x, match_player11$date,units='days')
match_player11 = match_player11[match_player11$time_diff >= 0,]
match_player11 = match_player11 %>%
  group_by(match_api_id, home_player_11) %>%
  filter(time_diff == min(time_diff))
for (i in 441:478){
  colnames(match_player11)[i] = paste(colnames(match_player11)[i], "hp_11", sep = "_")
}
for (i in 441:478){
  match_player11[[i]] = paste(match_player11[[i]],match_player11[[45]],sep="_")
}
match_player11$time_diff=NULL
match_player11$id.y = NULL
match_player11$date.y=NULL
match_player11$date=NULL
match_player11$id.x=NULL
match_player11$id=NULL
match_player11$player_fifa_api_id.x=NULL
match_player11$player_fifa_api_id.y=NULL

#AP1

match_player12 <- merge(x = match_player11, y=player_atts,by.x='away_player_1',by.y='player_api_id',all.x=TRUE)
match_player12$date.x<-ymd(match_player12$date.x)
match_player12$date<-ymd_hms(match_player12$date)
match_player12$time_diff <- difftime(match_player12$date.x, match_player12$date,units='days')
match_player12 = match_player12[match_player12$time_diff >= 0,]
match_player12 = match_player12 %>%
  group_by(match_api_id, away_player_1) %>%
  filter(time_diff == min(time_diff))
for (i in 478:515){
  colnames(match_player12)[i] = paste(colnames(match_player12)[i], "aw_p_1", sep = "_")
}
for (i in 478:515){
  match_player12[[i]] = paste(match_player12[[i]],match_player12[[46]],sep="_")
}
match_player12$time_diff=NULL
match_player12$id.y = NULL
match_player12$date.y=NULL
match_player12$date=NULL
match_player12$id.x=NULL
match_player12$id=NULL
match_player12$player_fifa_api_id.x=NULL
match_player12$player_fifa_api_id.y=NULL
#AP2
match_player13 <- merge(x = match_player12, y=player_atts,by.x='away_player_2',by.y='player_api_id',all.x=TRUE)
match_player13$date.x<-ymd(match_player13$date.x)
match_player13$date<-ymd_hms(match_player13$date)
match_player13$time_diff <- difftime(match_player13$date.x, match_player13$date,units='days')
match_player13 = match_player13[match_player13$time_diff >= 0,]
match_player13 = match_player13 %>%
  group_by(match_api_id, away_player_2) %>%
  filter(time_diff == min(time_diff))
for (i in 517:554){
  colnames(match_player13)[i] = paste(colnames(match_player13)[i], "aw_p_2", sep = "_")
}
for (i in 517:554){
  match_player13[[i]] = paste(match_player13[[i]],match_player13[[47]],sep="_")
}
match_player13$time_diff=NULL
match_player13$id.y = NULL
match_player13$date.y=NULL
match_player13$date=NULL
match_player13$id.x=NULL
match_player13$id=NULL
match_player13$player_fifa_api_id.x=NULL
match_player13$player_fifa_api_id.y=NULL

#AP3
match_player14 <- merge(x = match_player13, y=player_atts,by.x='away_player_3',by.y='player_api_id',all.x=TRUE)
match_player14$date.x<-ymd(match_player14$date.x)
match_player14$date<-ymd_hms(match_player14$date)
match_player14$time_diff <- difftime(match_player14$date.x, match_player14$date,units='days')
match_player14 = match_player14[match_player14$time_diff >= 0,]
match_player14 = match_player14 %>%
  group_by(match_api_id, away_player_3) %>%
  filter(time_diff == min(time_diff))
for (i in 554:591){
  colnames(match_player14)[i] = paste(colnames(match_player14)[i], "aw_p_3", sep = "_")
}
for (i in 554:591){
  match_player14[[i]] = paste(match_player14[[i]],match_player14[[48]],sep="_")
}
match_player14$time_diff=NULL
match_player14$id.y = NULL
match_player14$date.y=NULL
match_player14$date=NULL
match_player14$id.x=NULL
match_player14$id=NULL
match_player14$player_fifa_api_id.x=NULL
match_player14$player_fifa_api_id.y=NULL

#AP4
match_player15 <- merge(x = match_player14, y=player_atts,by.x='away_player_4',by.y='player_api_id',all.x=TRUE)
match_player15$date.x<-ymd(match_player15$date.x)
match_player15$date<-ymd_hms(match_player15$date)
match_player15$time_diff <- difftime(match_player15$date.x, match_player15$date,units='days')
match_player15 = match_player15[match_player15$time_diff >= 0,]
match_player15 = match_player15 %>%
  group_by(match_api_id, away_player_4) %>%
  filter(time_diff == min(time_diff))
for (i in 593:630){
  colnames(match_player15)[i] = paste(colnames(match_player15)[i], "aw_p_4", sep = "_")
}
for (i in 593:630){
  match_player15[[i]] = paste(match_player15[[i]],match_player15[[49]],sep="_")
}
match_player15$time_diff=NULL
match_player15$id.y = NULL
match_player15$date.y=NULL
match_player15$date=NULL
match_player15$id.x=NULL
match_player15$id=NULL
match_player15$player_fifa_api_id.x=NULL
match_player15$player_fifa_api_id.y=NULL

#AP5
match_player16 <- merge(x = match_player15, y=player_atts,by.x='away_player_5',by.y='player_api_id',all.x=TRUE)
match_player16$date.x<-ymd(match_player16$date.x)
match_player16$date<-ymd_hms(match_player16$date)
match_player16$time_diff <- difftime(match_player16$date.x, match_player16$date,units='days')
match_player16 = match_player16[match_player16$time_diff >= 0,]
match_player16 = match_player16 %>%
  group_by(match_api_id, away_player_5) %>%
  filter(time_diff == min(time_diff))
for (i in 630:667){
  colnames(match_player16)[i] = paste(colnames(match_player16)[i], "aw_p_5", sep = "_")
}
for (i in 630:667){
  match_player16[[i]] = paste(match_player16[[i]],match_player16[[50]],sep="_")
}
match_player16$time_diff=NULL
match_player16$id.y = NULL
match_player16$date.y=NULL
match_player16$date=NULL
match_player16$id.x=NULL
match_player16$id=NULL
match_player16$player_fifa_api_id.x=NULL
match_player16$player_fifa_api_id.y=NULL
#AP6
match_player17 <- merge(x = match_player16, y=player_atts,by.x='away_player_6',by.y='player_api_id',all.x=TRUE)
match_player17$date.x<-ymd(match_player17$date.x)
match_player17$date<-ymd_hms(match_player17$date)
match_player17$time_diff <- difftime(match_player17$date.x, match_player17$date,units='days')
match_player17 = match_player17[match_player17$time_diff >= 0,]
match_player17 = match_player17 %>%
  group_by(match_api_id, away_player_6) %>%
  filter(time_diff == min(time_diff))
for (i in 669:706){
  colnames(match_player17)[i] = paste(colnames(match_player17)[i], "aw_p_6", sep = "_")
}
for (i in 669:706){
  match_player17[[i]] = paste(match_player17[[i]],match_player17[[51]],sep="_")
}
match_player17$time_diff=NULL
match_player17$id.y = NULL
match_player17$date.y=NULL
match_player17$date=NULL
match_player17$id.x=NULL
match_player17$id=NULL
match_player17$player_fifa_api_id.x=NULL
match_player17$player_fifa_api_id.y=NULL
#AP7
match_player18 <- merge(x = match_player17, y=player_atts,by.x='away_player_7',by.y='player_api_id',all.x=TRUE)
match_player18$date.x<-ymd(match_player18$date.x)
match_player18$date<-ymd_hms(match_player18$date)
match_player18$time_diff <- difftime(match_player18$date.x, match_player18$date,units='days')
match_player18 = match_player18[match_player18$time_diff >= 0,]
match_player18 = match_player18 %>%
  group_by(match_api_id, away_player_7) %>%
  filter(time_diff == min(time_diff))
for (i in 706:743){
  colnames(match_player18)[i] = paste(colnames(match_player18)[i], "aw_p_7", sep = "_")
}
for (i in 706:743){
  match_player18[[i]] = paste(match_player18[[i]],match_player18[[52]],sep="_")
}
match_player18$time_diff=NULL
match_player18$id.y = NULL
match_player18$date.y=NULL
match_player18$date=NULL
match_player18$id.x=NULL
match_player18$id=NULL
match_player18$player_fifa_api_id.x=NULL
match_player18$player_fifa_api_id.y=NULL
#AP8
match_player19 <- merge(x = match_player18, y=player_atts,by.x='away_player_8',by.y='player_api_id',all.x=TRUE)
match_player19$date.x<-ymd(match_player19$date.x)
match_player19$date<-ymd_hms(match_player19$date)
match_player19$time_diff <- difftime(match_player19$date.x, match_player19$date,units='days')
match_player19 = match_player19[match_player19$time_diff >= 0,]
match_player19 = match_player19 %>%
  group_by(match_api_id, away_player_8) %>%
  filter(time_diff == min(time_diff))
for (i in 745:782){
  colnames(match_player19)[i] = paste(colnames(match_player19)[i], "aw_p_8", sep = "_")
}
for (i in 745:782){
  match_player19[[i]] = paste(match_player19[[i]],match_player19[[53]],sep="_")
}
match_player19$time_diff=NULL
match_player19$id.y = NULL
match_player19$date.y=NULL
match_player19$date=NULL
match_player19$id.x=NULL
match_player19$id=NULL
match_player19$player_fifa_api_id.x=NULL
match_player19$player_fifa_api_id.y=NULL
#AP9
match_player20 <- merge(x = match_player19, y=player_atts,by.x='away_player_9',by.y='player_api_id',all.x=TRUE)
match_player20$date.x<-ymd(match_player20$date.x)
match_player20$date<-ymd_hms(match_player20$date)
match_player20$time_diff <- difftime(match_player20$date.x, match_player20$date,units='days')
match_player20 = match_player20[match_player20$time_diff >= 0,]
match_player20 = match_player20 %>%
  group_by(match_api_id, away_player_9) %>%
  filter(time_diff == min(time_diff))
for (i in 782:819){
  colnames(match_player20)[i] = paste(colnames(match_player20)[i], "aw_p_9", sep = "_")
}
for (i in 782:819){
  match_player20[[i]] = paste(match_player20[[i]],match_player20[[54]],sep="_")
}
match_player20$time_diff=NULL
match_player20$id.y = NULL
match_player20$date.y=NULL
match_player20$date=NULL
match_player20$id.x=NULL
match_player20$id=NULL
match_player20$player_fifa_api_id.x=NULL
match_player20$player_fifa_api_id.y=NULL
#AP10
match_player21 <- merge(x = match_player20, y=player_atts,by.x='away_player_10',by.y='player_api_id',all.x=TRUE)
match_player21$date.x<-ymd(match_player21$date.x)
match_player21$date<-ymd_hms(match_player21$date)
match_player21$time_diff <- difftime(match_player21$date.x, match_player21$date,units='days')
match_player21 = match_player21[match_player21$time_diff >= 0,]
match_player21 = match_player21 %>%
  group_by(match_api_id, away_player_10) %>%
  filter(time_diff == min(time_diff))
for (i in 821:858){
  colnames(match_player21)[i] = paste(colnames(match_player21)[i], "aw_p_10", sep = "_")
}
for (i in 821:858){
  match_player21[[i]] = paste(match_player21[[i]],match_player21[[55]],sep="_")
}
match_player21$time_diff=NULL
match_player21$id.y = NULL
match_player21$date.y=NULL
match_player21$date=NULL
match_player21$id.x=NULL
match_player21$id=NULL
match_player21$player_fifa_api_id.x=NULL
match_player21$player_fifa_api_id.y=NULL
#AP11
match_player22 <- merge(x = match_player21, y=player_atts,by.x='away_player_11',by.y='player_api_id',all.x=TRUE)
match_player22$date.x<-ymd(match_player22$date.x)
match_player22$date<-ymd_hms(match_player22$date)
match_player22$time_diff <- difftime(match_player22$date.x, match_player22$date,units='days')
match_player22 = match_player22[match_player22$time_diff >= 0,]
match_player22 = match_player22 %>%
  group_by(match_api_id, away_player_11) %>%
  filter(time_diff == min(time_diff))
for (i in 858:895){
  colnames(match_player22)[i] = paste(colnames(match_player22)[i], "aw_p_11", sep = "_")
}
for (i in 858:895){
  match_player22[[i]] = paste(match_player22[[i]],match_player22[[56]],sep="_")
}
match_player22$time_diff=NULL
match_player22$id.y = NULL
match_player22$date.y=NULL
match_player22$date=NULL
match_player22$id.x=NULL
match_player22$id=NULL
match_player22$player_fifa_api_id.x=NULL
match_player22$player_fifa_api_id.y=NULL

################# ARules ##################
# all the matches that lose
loss_match = match_player22
### lose as home play, need to use away player attributes in the next steps
home_loss_match = loss_match[loss_match$opponent == loss_match$away_team_api_id,]
home_loss_ar = home_loss_match %>%
  select(preferred_foot_aw_p_1:gk_reflexes_Bucket_aw_p_11, result)
home_loss_ar = home_loss_ar[,-c(1,2)]
home_loss_ar = subset(home_loss_ar, select = -player_fifa_api_id)
### lose as away play, need to use home player attributes in the next steps
away_loss_match = loss_match[loss_match$opponent == loss_match$home_team_api_id,]
away_loss_ar = away_loss_match %>%
  select(preferred_foot_hp_1:gk_reflexes_Bucket_hp_11, result)
away_loss_ar = away_loss_ar[,-c(1,2)]
colnames(away_loss_ar) = colnames(home_loss_ar)
all_loss_ar = rbind(home_loss_ar, away_loss_ar)
##### add column name to values
column_names = names(all_loss_ar)

for (column in column_names) {
  all_loss_ar[[column]] = paste(column, all_loss_ar[[column]], sep = '_')
}

###### delete player numbers
for (i in 1:418) {
  for (j in 1:287) {
    all_loss_ar[j,i] = gsub('_aw_p_[0-9]', '', all_loss_ar[j,i])
  }
}
for (i in 1:418) {
  for (j in 1:287) {
    all_loss_ar[j,i] = gsub('[0-9]', '', all_loss_ar[j,i])
  }
}


write.csv(all_loss_ar,'genoa.csv')
```

```{r include=FALSE, warning=FALSE, results="hide"}
genoa_all3 = read.transactions("genoa.csv", format = "basket",
                           sep = ",", rm.duplicates = TRUE, skip =1 , header = FALSE)
# inspect(genoa_all3)
# itemInfo(genoa_all3)
rules <- apriori(genoa_all3, parameter = list(supp = 0.2, conf = 0.3), appearance = list(rhs = 'result_loss'))

rules_lift = sort(rules,by='lift',decreasing = TRUE)

```
  
  When we run the associations of player attributes that correspond to Genoa's loss, we found the following rules:  
  
```{r, fig.width=16}
inspect(rules_lift[1:10])
```

```{r}
# plot(rules_lift[1:5], method = 'graph', engine = 'htmlwidget')
plot(rules_lift[1:10], method = 'paracoord')
```


#### Napoli 

```{r, include = FALSE}
player_atts = player_atts_tbl %>% 
  collect()
long_team_name <- 'Napoli'
roma_record <- team_tbl %>% 
  collect() %>%
  filter(grepl(long_team_name, team_long_name))
home_matches <- match %>%
  filter(home_team_api_id == roma_record$team_api_id) %>%
  mutate(goal_diff = home_team_goal - away_team_goal) %>%
  mutate(type = 'home', opponent = away_team_api_id)
# hist(home_matches$goal_diff)
# summary(home_matches$goal_diff)

away_matches <- match %>%
  filter(away_team_api_id == roma_record$team_api_id) %>%
  mutate(goal_diff =away_team_goal - home_team_goal) %>%
  mutate(type = 'away', opponent = home_team_api_id)

# hist(away_matches$goal_diff)
# summary(away_matches$goal_diff)

all_match = rbind(home_matches, away_matches)
# ggplot(all_match, aes(type, goal_diff, col = type)) +
#   geom_boxplot()

all_match$result = as.numeric(all_match$goal_diff)
all_match[all_match$goal_diff > 0, ]$result = 'win'
all_match[all_match$goal_diff < 0, ]$result = 'loss'
all_match[all_match$goal_diff == 0, ]$result = 'draw'
# ggplot(all_match, aes(result)) +
#   geom_bar()

# ggplot(all_match[all_match$result == 'lose',], aes(as.factor(opponent))) +
#   geom_bar()

##################### position ###################
for (i in 1:11){
  x = paste("home_player_X",i,sep='')
  y = paste("home_player_Y",i,sep='')
  inp = paste("hm_ply_pos",i,sep="_")
  all_match[inp] = ifelse(all_match[x]==1 & all_match[y]==1,"Goaly",
                      ifelse(all_match[y]>=2 & all_match[y]<=5,"Defender",
                             ifelse(all_match[y]>=6 & all_match[y]<=8,"Mid Fielders",
                                    ifelse(all_match[y]>=9 & all_match[y]<=11,"Forwards","NA"))))
}

for (i in 1:11){
  x = paste("away_player_X",i,sep='')
  y = paste("away_player_Y",i,sep='')
  inp = paste("away_ply_pos",i,sep="_")
  all_match[inp] = ifelse(all_match[x]==1 & all_match[y]==1,"Goaly",
                      ifelse(all_match[y]>=2 & all_match[y]<=5,"Defender",
                             ifelse(all_match[y]>=6 & all_match[y]<=8,"Mid Fielders",
                                    ifelse(all_match[y]>=9 & all_match[y]<=11,"Forwards","NA"))))
}
```

```{r,include=FALSE}
column_names = c("overall_rating","potential","crossing", "finishing","heading_accuracy",    
                 "short_passing", "volleys","dribbling",    
                 "curve", "free_kick_accuracy" ,"long_passing",    
                 "ball_control","acceleration","sprint_speed",    
                 "agility","reactions","balance",    
                 "shot_power","jumping","stamina",    
                 "strength","long_shots","aggression",    
                 "interceptions","positioning","vision",    
                 "penalties", "marking","standing_tackle",
                 "sliding_tackle","gk_diving","gk_handling",    
                 "gk_kicking","gk_positioning","gk_reflexes")

for (i in column_names){
  k = paste(i, "Bucket",sep="_")
  player_atts[k] = 
    cut(player_atts[[i]], breaks = 5, labels=c('low','low-med','med','med-high','high'))
}

player_atts <- select(player_atts, c(1:9, overall_rating_Bucket:gk_reflexes_Bucket))
player_atts = player_atts[, -c(5,6)]
all_match <- select(all_match, c(id:away_team_goal, home_player_1:away_player_11, opponent:away_ply_pos_11))

#######
# match_g <- gather(all_match, player, player_id, home_player_1:away_player_11, na.rm = TRUE)
# View(match_g)
# match_s <- arrange(match_g, match_api_id, player)
# View(match_s)
# #match_s <- select(match_s, c(1:10, player, player_id))

# colnames(match_s)[37] <- "player_api_id"

#HP1
match_player <- merge(x = all_match, y=player_atts,by.x='home_player_1',by.y='player_api_id',all.x=TRUE)
match_player$date.x<-ymd_hms(match_player$date.x)
match_player$date.y<-ymd_hms(match_player$date.y)
match_player$time_diff <- difftime(match_player$date.x, match_player$date.y,units='days')
match_player = match_player[match_player$time_diff >= 0,]
match_player_1 = match_player %>%
  group_by(match_api_id, home_player_1) %>%
  filter(time_diff == min(time_diff))
for (i in 61:98){
  colnames(match_player_1)[i] = paste(colnames(match_player_1)[i], "hp_1", sep = "_")
}
for (i in 61:98){
  match_player_1[[i]] = paste(match_player_1[[i]],match_player_1[[36]],sep="_")
}
match_player_1$time_diff=NULL
# HP2

match_player2 <- merge(x = match_player_1, y=player_atts,by.x='home_player_2',by.y='player_api_id',all.x=TRUE)
match_player2$date.x<-ymd(match_player2$date.x)
match_player2$date<-ymd_hms(match_player2$date)
match_player2$time_diff <- difftime(match_player2$date.x, match_player2$date,units='days')
match_player2 = match_player2[match_player2$time_diff >= 0,]
match_player2 = match_player2 %>%
  group_by(match_api_id, home_player_2) %>%
  filter(time_diff == min(time_diff))
for (i in 102:139){
  colnames(match_player2)[i] = paste(colnames(match_player2)[i], "hp_2", sep = "_")
}
for (i in 102:139){
  match_player2[[i]] = paste(match_player2[[i]],match_player2[[37]],sep="_")
}
match_player2$time_diff=NULL
match_player2$id.y = NULL
match_player2$date.y=NULL
match_player2$date=NULL
match_player2$id.x=NULL
##
#HP3

match_player3 <- merge(x = match_player2, y=player_atts,by.x='home_player_3',by.y='player_api_id',all.x=TRUE)
match_player3$date.x<-ymd(match_player3$date.x)
match_player3$date<-ymd_hms(match_player3$date)
match_player3$time_diff <- difftime(match_player3$date.x, match_player3$date,units='days')
match_player3 = match_player3[match_player3$time_diff >= 0,]
match_player3 = match_player3 %>%
  group_by(match_api_id, home_player_3) %>%
  filter(time_diff == min(time_diff))
for (i in 139:176){
  colnames(match_player3)[i] = paste(colnames(match_player3)[i], "hp_3", sep = "_")
}
for (i in 139:176){
  match_player3[[i]] = paste(match_player3[[i]],match_player3[[37]],sep="_")
}
match_player3$time_diff=NULL
match_player3$id.y = NULL
match_player3$date.y=NULL
match_player3$date=NULL
match_player3$id.x=NULL
match_player3$player_fifa_api_id=NULL

#HP4

match_player4 <- merge(x = match_player3, y=player_atts,by.x='home_player_4',by.y='player_api_id',all.x=TRUE)
match_player4$date.x<-ymd(match_player4$date.x)
match_player4$date<-ymd_hms(match_player4$date)
match_player4$time_diff <- difftime(match_player4$date.x, match_player4$date,units='days')
match_player4 = match_player4[match_player4$time_diff >= 0,]
match_player4 = match_player4 %>%
  group_by(match_api_id, home_player_4) %>%
  filter(time_diff == min(time_diff))
for (i in 176:213){
  colnames(match_player4)[i] = paste(colnames(match_player4)[i], "hp_4", sep = "_")
}
for (i in 176:213){
  match_player4[[i]] = paste(match_player4[[i]],match_player4[[38]],sep="_")
}
match_player4$time_diff=NULL
match_player4$id.y = NULL
match_player4$date.y=NULL
match_player4$date=NULL
match_player4$id.x=NULL
match_player4$id=NULL
match_player4$player_fifa_api_id.x=NULL
match_player4$player_fifa_api_id.y=NULL
#HP5

match_player5 <- merge(x = match_player4, y=player_atts,by.x='home_player_5',by.y='player_api_id',all.x=TRUE)
match_player5$date.x<-ymd(match_player5$date.x)
match_player5$date<-ymd_hms(match_player5$date)
match_player5$time_diff <- difftime(match_player5$date.x, match_player5$date,units='days')
match_player5 = match_player5[match_player5$time_diff >= 0,]
match_player5 = match_player5 %>%
  group_by(match_api_id, home_player_5) %>%
  filter(time_diff == min(time_diff))
for (i in 213:250){
  colnames(match_player5)[i] = paste(colnames(match_player5)[i], "hp_5", sep = "_")
}
for (i in 213:250){
  match_player5[[i]] = paste(match_player5[[i]],match_player5[[39]],sep="_")
}
match_player5$time_diff=NULL
match_player5$id.y = NULL
match_player5$date.y=NULL
match_player5$date=NULL
match_player5$id.x=NULL
match_player5$id=NULL
###
#HP6
match_player6 <- merge(x = match_player5, y=player_atts,by.x='home_player_6',by.y='player_api_id',all.x=TRUE)
match_player6$date.x<-ymd(match_player6$date.x)
match_player6$date<-ymd_hms(match_player6$date)
match_player6$time_diff <- difftime(match_player6$date.x, match_player6$date,units='days')
match_player6 = match_player6[match_player6$time_diff >= 0,]
match_player6 = match_player6 %>%
  group_by(match_api_id, home_player_6) %>%
  filter(time_diff == min(time_diff))
for (i in 252:289){
  colnames(match_player6)[i] = paste(colnames(match_player6)[i], "hp_6", sep = "_")
}
for (i in 252:289){
  match_player6[[i]] = paste(match_player6[[i]],match_player6[[40]],sep="_")
}
match_player6$time_diff=NULL
match_player6$id.y = NULL
match_player6$date.y=NULL
match_player6$date=NULL
match_player6$id.x=NULL
match_player6$id=NULL
match_player6$player_fifa_api_id.x=NULL
match_player6$player_fifa_api_id.y=NULL

#HP7
match_player7 <- merge(x = match_player6, y=player_atts,by.x='home_player_7',by.y='player_api_id',all.x=TRUE)
match_player7$date.x<-ymd(match_player7$date.x)
match_player7$date<-ymd_hms(match_player7$date)
match_player7$time_diff <- difftime(match_player7$date.x, match_player7$date,units='days')
match_player7 = match_player7[match_player7$time_diff >= 0,]
match_player7 = match_player7 %>%
  group_by(match_api_id, home_player_7) %>%
  filter(time_diff == min(time_diff))
for (i in 289:326){
  colnames(match_player7)[i] = paste(colnames(match_player7)[i], "hp_7", sep = "_")
}
for (i in 289:326){
  match_player7[[i]] = paste(match_player7[[i]],match_player7[[41]],sep="_")
}
match_player7$time_diff=NULL
match_player7$id.y = NULL
match_player7$date.y=NULL
match_player7$date=NULL
match_player7$id.x=NULL
match_player7$id=NULL
match_player7$player_fifa_api_id.x=NULL
match_player7$player_fifa_api_id.y=NULL

#HP8
match_player8 <- merge(x = match_player7, y=player_atts,by.x='home_player_8',by.y='player_api_id',all.x=TRUE)
match_player8$date.x<-ymd(match_player8$date.x)
match_player8$date<-ymd_hms(match_player8$date)
match_player8$time_diff <- difftime(match_player8$date.x, match_player8$date,units='days')
match_player8 = match_player8[match_player8$time_diff >= 0,]
match_player8 = match_player8 %>%
  group_by(match_api_id, home_player_8) %>%
  filter(time_diff == min(time_diff))
for (i in 326:363){
  colnames(match_player8)[i] = paste(colnames(match_player8)[i], "hp_8", sep = "_")
}
for (i in 326:363){
  match_player8[[i]] = paste(match_player8[[i]],match_player8[[42]],sep="_")
}
match_player8$time_diff=NULL
match_player8$id.y = NULL
match_player8$date.y=NULL
match_player8$date=NULL
match_player8$id.x=NULL
match_player8$id=NULL
match_player8$player_fifa_api_id.x=NULL
match_player8$player_fifa_api_id.y=NULL

#HP9
match_player9 <- merge(x = match_player8, y=player_atts,by.x='home_player_9',by.y='player_api_id',all.x=TRUE)
match_player9$date.x<-ymd(match_player9$date.x)
match_player9$date<-ymd_hms(match_player9$date)
match_player9$time_diff <- difftime(match_player9$date.x, match_player9$date,units='days')
match_player9 = match_player9[match_player9$time_diff >= 0,]
match_player9 = match_player9 %>%
  group_by(match_api_id, home_player_9) %>%
  filter(time_diff == min(time_diff))
for (i in 365:402){
  colnames(match_player9)[i] = paste(colnames(match_player9)[i], "hp_9", sep = "_")
}
for (i in 365:402){
  match_player9[[i]] = paste(match_player9[[i]],match_player9[[43]],sep="_")
}
match_player9$time_diff=NULL
match_player9$id.y = NULL
match_player9$date.y=NULL
match_player9$date=NULL
match_player9$id.x=NULL
match_player9$id=NULL
match_player9$player_fifa_api_id.x=NULL
match_player9$player_fifa_api_id.y=NULL

#HP10
match_player10 <- merge(x = match_player9, y=player_atts,by.x='home_player_10',by.y='player_api_id',all.x=TRUE)
match_player10$date.x<-ymd(match_player10$date.x)
match_player10$date<-ymd_hms(match_player10$date)
match_player10$time_diff <- difftime(match_player10$date.x, match_player10$date,units='days')
match_player10 = match_player10[match_player10$time_diff >= 0,]
match_player10 = match_player10 %>%
  group_by(match_api_id, home_player_10) %>%
  filter(time_diff == min(time_diff))
for (i in 402:439){
  colnames(match_player10)[i] = paste(colnames(match_player10)[i], "hp_10", sep = "_")
}
for (i in 402:439){
  match_player10[[i]] = paste(match_player10[[i]],match_player10[[44]],sep="_")
}
match_player10$time_diff=NULL
match_player10$id.y = NULL
match_player10$date.y=NULL
match_player10$date=NULL
match_player10$id.x=NULL
match_player10$id=NULL
match_player10$player_fifa_api_id.x=NULL
match_player10$player_fifa_api_id.y=NULL

#HP11
match_player11 <- merge(x = match_player10, y=player_atts,by.x='home_player_11',by.y='player_api_id',all.x=TRUE)
match_player11$date.x<-ymd(match_player11$date.x)
match_player11$date<-ymd_hms(match_player11$date)
match_player11$time_diff <- difftime(match_player11$date.x, match_player11$date,units='days')
match_player11 = match_player11[match_player11$time_diff >= 0,]
match_player11 = match_player11 %>%
  group_by(match_api_id, home_player_11) %>%
  filter(time_diff == min(time_diff))
for (i in 441:478){
  colnames(match_player11)[i] = paste(colnames(match_player11)[i], "hp_11", sep = "_")
}
for (i in 441:478){
  match_player11[[i]] = paste(match_player11[[i]],match_player11[[45]],sep="_")
}
match_player11$time_diff=NULL
match_player11$id.y = NULL
match_player11$date.y=NULL
match_player11$date=NULL
match_player11$id.x=NULL
match_player11$id=NULL
match_player11$player_fifa_api_id.x=NULL
match_player11$player_fifa_api_id.y=NULL

#AP1

match_player12 <- merge(x = match_player11, y=player_atts,by.x='away_player_1',by.y='player_api_id',all.x=TRUE)
match_player12$date.x<-ymd(match_player12$date.x)
match_player12$date<-ymd_hms(match_player12$date)
match_player12$time_diff <- difftime(match_player12$date.x, match_player12$date,units='days')
match_player12 = match_player12[match_player12$time_diff >= 0,]
match_player12 = match_player12 %>%
  group_by(match_api_id, away_player_1) %>%
  filter(time_diff == min(time_diff))
for (i in 478:515){
  colnames(match_player12)[i] = paste(colnames(match_player12)[i], "aw_p_1", sep = "_")
}
for (i in 478:515){
  match_player12[[i]] = paste(match_player12[[i]],match_player12[[46]],sep="_")
}
match_player12$time_diff=NULL
match_player12$id.y = NULL
match_player12$date.y=NULL
match_player12$date=NULL
match_player12$id.x=NULL
match_player12$id=NULL
match_player12$player_fifa_api_id.x=NULL
match_player12$player_fifa_api_id.y=NULL
#AP2
match_player13 <- merge(x = match_player12, y=player_atts,by.x='away_player_2',by.y='player_api_id',all.x=TRUE)
match_player13$date.x<-ymd(match_player13$date.x)
match_player13$date<-ymd_hms(match_player13$date)
match_player13$time_diff <- difftime(match_player13$date.x, match_player13$date,units='days')
match_player13 = match_player13[match_player13$time_diff >= 0,]
match_player13 = match_player13 %>%
  group_by(match_api_id, away_player_2) %>%
  filter(time_diff == min(time_diff))
for (i in 517:554){
  colnames(match_player13)[i] = paste(colnames(match_player13)[i], "aw_p_2", sep = "_")
}
for (i in 517:554){
  match_player13[[i]] = paste(match_player13[[i]],match_player13[[47]],sep="_")
}
match_player13$time_diff=NULL
match_player13$id.y = NULL
match_player13$date.y=NULL
match_player13$date=NULL
match_player13$id.x=NULL
match_player13$id=NULL
match_player13$player_fifa_api_id.x=NULL
match_player13$player_fifa_api_id.y=NULL

#AP3
match_player14 <- merge(x = match_player13, y=player_atts,by.x='away_player_3',by.y='player_api_id',all.x=TRUE)
match_player14$date.x<-ymd(match_player14$date.x)
match_player14$date<-ymd_hms(match_player14$date)
match_player14$time_diff <- difftime(match_player14$date.x, match_player14$date,units='days')
match_player14 = match_player14[match_player14$time_diff >= 0,]
match_player14 = match_player14 %>%
  group_by(match_api_id, away_player_3) %>%
  filter(time_diff == min(time_diff))
for (i in 554:591){
  colnames(match_player14)[i] = paste(colnames(match_player14)[i], "aw_p_3", sep = "_")
}
for (i in 554:591){
  match_player14[[i]] = paste(match_player14[[i]],match_player14[[48]],sep="_")
}
match_player14$time_diff=NULL
match_player14$id.y = NULL
match_player14$date.y=NULL
match_player14$date=NULL
match_player14$id.x=NULL
match_player14$id=NULL
match_player14$player_fifa_api_id.x=NULL
match_player14$player_fifa_api_id.y=NULL

#AP4
match_player15 <- merge(x = match_player14, y=player_atts,by.x='away_player_4',by.y='player_api_id',all.x=TRUE)
match_player15$date.x<-ymd(match_player15$date.x)
match_player15$date<-ymd_hms(match_player15$date)
match_player15$time_diff <- difftime(match_player15$date.x, match_player15$date,units='days')
match_player15 = match_player15[match_player15$time_diff >= 0,]
match_player15 = match_player15 %>%
  group_by(match_api_id, away_player_4) %>%
  filter(time_diff == min(time_diff))
for (i in 593:630){
  colnames(match_player15)[i] = paste(colnames(match_player15)[i], "aw_p_4", sep = "_")
}
for (i in 593:630){
  match_player15[[i]] = paste(match_player15[[i]],match_player15[[49]],sep="_")
}
match_player15$time_diff=NULL
match_player15$id.y = NULL
match_player15$date.y=NULL
match_player15$date=NULL
match_player15$id.x=NULL
match_player15$id=NULL
match_player15$player_fifa_api_id.x=NULL
match_player15$player_fifa_api_id.y=NULL

#AP5
match_player16 <- merge(x = match_player15, y=player_atts,by.x='away_player_5',by.y='player_api_id',all.x=TRUE)
match_player16$date.x<-ymd(match_player16$date.x)
match_player16$date<-ymd_hms(match_player16$date)
match_player16$time_diff <- difftime(match_player16$date.x, match_player16$date,units='days')
match_player16 = match_player16[match_player16$time_diff >= 0,]
match_player16 = match_player16 %>%
  group_by(match_api_id, away_player_5) %>%
  filter(time_diff == min(time_diff))
for (i in 630:667){
  colnames(match_player16)[i] = paste(colnames(match_player16)[i], "aw_p_5", sep = "_")
}
for (i in 630:667){
  match_player16[[i]] = paste(match_player16[[i]],match_player16[[50]],sep="_")
}
match_player16$time_diff=NULL
match_player16$id.y = NULL
match_player16$date.y=NULL
match_player16$date=NULL
match_player16$id.x=NULL
match_player16$id=NULL
match_player16$player_fifa_api_id.x=NULL
match_player16$player_fifa_api_id.y=NULL
#AP6
match_player17 <- merge(x = match_player16, y=player_atts,by.x='away_player_6',by.y='player_api_id',all.x=TRUE)
match_player17$date.x<-ymd(match_player17$date.x)
match_player17$date<-ymd_hms(match_player17$date)
match_player17$time_diff <- difftime(match_player17$date.x, match_player17$date,units='days')
match_player17 = match_player17[match_player17$time_diff >= 0,]
match_player17 = match_player17 %>%
  group_by(match_api_id, away_player_6) %>%
  filter(time_diff == min(time_diff))
for (i in 669:706){
  colnames(match_player17)[i] = paste(colnames(match_player17)[i], "aw_p_6", sep = "_")
}
for (i in 669:706){
  match_player17[[i]] = paste(match_player17[[i]],match_player17[[51]],sep="_")
}
match_player17$time_diff=NULL
match_player17$id.y = NULL
match_player17$date.y=NULL
match_player17$date=NULL
match_player17$id.x=NULL
match_player17$id=NULL
match_player17$player_fifa_api_id.x=NULL
match_player17$player_fifa_api_id.y=NULL
#AP7
match_player18 <- merge(x = match_player17, y=player_atts,by.x='away_player_7',by.y='player_api_id',all.x=TRUE)
match_player18$date.x<-ymd(match_player18$date.x)
match_player18$date<-ymd_hms(match_player18$date)
match_player18$time_diff <- difftime(match_player18$date.x, match_player18$date,units='days')
match_player18 = match_player18[match_player18$time_diff >= 0,]
match_player18 = match_player18 %>%
  group_by(match_api_id, away_player_7) %>%
  filter(time_diff == min(time_diff))
for (i in 706:743){
  colnames(match_player18)[i] = paste(colnames(match_player18)[i], "aw_p_7", sep = "_")
}
for (i in 706:743){
  match_player18[[i]] = paste(match_player18[[i]],match_player18[[52]],sep="_")
}
match_player18$time_diff=NULL
match_player18$id.y = NULL
match_player18$date.y=NULL
match_player18$date=NULL
match_player18$id.x=NULL
match_player18$id=NULL
match_player18$player_fifa_api_id.x=NULL
match_player18$player_fifa_api_id.y=NULL
#AP8
match_player19 <- merge(x = match_player18, y=player_atts,by.x='away_player_8',by.y='player_api_id',all.x=TRUE)
match_player19$date.x<-ymd(match_player19$date.x)
match_player19$date<-ymd_hms(match_player19$date)
match_player19$time_diff <- difftime(match_player19$date.x, match_player19$date,units='days')
match_player19 = match_player19[match_player19$time_diff >= 0,]
match_player19 = match_player19 %>%
  group_by(match_api_id, away_player_8) %>%
  filter(time_diff == min(time_diff))
for (i in 745:782){
  colnames(match_player19)[i] = paste(colnames(match_player19)[i], "aw_p_8", sep = "_")
}
for (i in 745:782){
  match_player19[[i]] = paste(match_player19[[i]],match_player19[[53]],sep="_")
}
match_player19$time_diff=NULL
match_player19$id.y = NULL
match_player19$date.y=NULL
match_player19$date=NULL
match_player19$id.x=NULL
match_player19$id=NULL
match_player19$player_fifa_api_id.x=NULL
match_player19$player_fifa_api_id.y=NULL
#AP9
match_player20 <- merge(x = match_player19, y=player_atts,by.x='away_player_9',by.y='player_api_id',all.x=TRUE)
match_player20$date.x<-ymd(match_player20$date.x)
match_player20$date<-ymd_hms(match_player20$date)
match_player20$time_diff <- difftime(match_player20$date.x, match_player20$date,units='days')
match_player20 = match_player20[match_player20$time_diff >= 0,]
match_player20 = match_player20 %>%
  group_by(match_api_id, away_player_9) %>%
  filter(time_diff == min(time_diff))
for (i in 782:819){
  colnames(match_player20)[i] = paste(colnames(match_player20)[i], "aw_p_9", sep = "_")
}
for (i in 782:819){
  match_player20[[i]] = paste(match_player20[[i]],match_player20[[54]],sep="_")
}
match_player20$time_diff=NULL
match_player20$id.y = NULL
match_player20$date.y=NULL
match_player20$date=NULL
match_player20$id.x=NULL
match_player20$id=NULL
match_player20$player_fifa_api_id.x=NULL
match_player20$player_fifa_api_id.y=NULL
#AP10
match_player21 <- merge(x = match_player20, y=player_atts,by.x='away_player_10',by.y='player_api_id',all.x=TRUE)
match_player21$date.x<-ymd(match_player21$date.x)
match_player21$date<-ymd_hms(match_player21$date)
match_player21$time_diff <- difftime(match_player21$date.x, match_player21$date,units='days')
match_player21 = match_player21[match_player21$time_diff >= 0,]
match_player21 = match_player21 %>%
  group_by(match_api_id, away_player_10) %>%
  filter(time_diff == min(time_diff))
for (i in 821:858){
  colnames(match_player21)[i] = paste(colnames(match_player21)[i], "aw_p_10", sep = "_")
}
for (i in 821:858){
  match_player21[[i]] = paste(match_player21[[i]],match_player21[[55]],sep="_")
}
match_player21$time_diff=NULL
match_player21$id.y = NULL
match_player21$date.y=NULL
match_player21$date=NULL
match_player21$id.x=NULL
match_player21$id=NULL
match_player21$player_fifa_api_id.x=NULL
match_player21$player_fifa_api_id.y=NULL
#AP11
match_player22 <- merge(x = match_player21, y=player_atts,by.x='away_player_11',by.y='player_api_id',all.x=TRUE)
match_player22$date.x<-ymd(match_player22$date.x)
match_player22$date<-ymd_hms(match_player22$date)
match_player22$time_diff <- difftime(match_player22$date.x, match_player22$date,units='days')
match_player22 = match_player22[match_player22$time_diff >= 0,]
match_player22 = match_player22 %>%
  group_by(match_api_id, away_player_11) %>%
  filter(time_diff == min(time_diff))
for (i in 858:895){
  colnames(match_player22)[i] = paste(colnames(match_player22)[i], "aw_p_11", sep = "_")
}
for (i in 858:895){
  match_player22[[i]] = paste(match_player22[[i]],match_player22[[56]],sep="_")
}
match_player22$time_diff=NULL
match_player22$id.y = NULL
match_player22$date.y=NULL
match_player22$date=NULL
match_player22$id.x=NULL
match_player22$id=NULL
match_player22$player_fifa_api_id.x=NULL
match_player22$player_fifa_api_id.y=NULL

################# ARules ##################
# all the matches that lose
loss_match = match_player22
### lose as home play, need to use away player attributes in the next steps
home_loss_match = loss_match[loss_match$opponent == loss_match$away_team_api_id,]
home_loss_ar = home_loss_match %>%
  select(preferred_foot_aw_p_1:gk_reflexes_Bucket_aw_p_11, result)
home_loss_ar = home_loss_ar[,-c(1,2)]
home_loss_ar = subset(home_loss_ar, select = -player_fifa_api_id)
### lose as away play, need to use home player attributes in the next steps
away_loss_match = loss_match[loss_match$opponent == loss_match$home_team_api_id,]
away_loss_ar = away_loss_match %>%
  select(preferred_foot_hp_1:gk_reflexes_Bucket_hp_11, result)
away_loss_ar = away_loss_ar[,-c(1,2)]
colnames(away_loss_ar) = colnames(home_loss_ar)
all_loss_ar = rbind(home_loss_ar, away_loss_ar)
##### add column name to values
column_names = names(all_loss_ar)

for (column in column_names) {
  all_loss_ar[[column]] = paste(column, all_loss_ar[[column]], sep = '_')
}

###### delete player numbers
for (i in 1:418) {
  for (j in 1:287) {
    all_loss_ar[j,i] = gsub('_aw_p_[0-9]', '', all_loss_ar[j,i])
  }
}
for (i in 1:418) {
  for (j in 1:287) {
    all_loss_ar[j,i] = gsub('[0-9]', '', all_loss_ar[j,i])
  }
}


write.csv(all_loss_ar,'napoli.csv')
```

```{r, include=FALSE, message=FALSE, results="hide"}
napoli_all3 = read.transactions("napoli.csv", format = "basket",
                           sep = ",", rm.duplicates = TRUE, skip =1, header = FALSE)
# inspect(napoli_all3)
# itemInfo(napoli_all3)
rules <- apriori(napoli_all3, parameter = list(supp = 0.2, conf = 0.3), appearance = list(rhs = 'result_loss'))

rules_lift = sort(rules,by='lift',decreasing = TRUE)

```
  
  When we run the associations of player attributes that correspond to Napoli's loss, we found the following rules:  
  
```{r, fig.width=16}
inspect(rules_lift[1:10])
```

```{r}
# plot(rules_lift[1:5], method = 'graph', engine = 'htmlwidget')
plot(rules_lift[1:10], method = 'paracoord', title = "Parallel Coordinates plot for Napoli's Loss")
```

