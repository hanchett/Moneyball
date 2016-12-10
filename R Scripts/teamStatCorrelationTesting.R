# Christopher Li, INFO 370
# This following is R code that explore data on missed calls for Baseball
# The data represent 2013, 2014, 2015, 2016

# Todo: Create functions / methods for plot generation
#       Modify Abbreviations to match across all data frames 

# Download/set up sqldf lib
# sqldf provides easy way to preform SQL selects in R dataframes
install.packages("sqldf")
library(sqldf)

library(ggplot2)

# For renaming
library(plyr)

# Wide to long format
library(tidyr)

# Navigate to folder with data
getwd()
# NOT WORKING!!
setwd("/Desktop/370_Project")

baseball_2013 = read.csv("/Users/ChrisLi/Desktop/370_Project/Data/data_2013.csv")
baseball_2014 = read.csv("/Users/ChrisLi/Desktop/370_Project/Data/data_2014.csv")
baseball_2015 = read.csv("/Users/ChrisLi/Desktop/370_Project/Data/data_2015.csv")
baseball_2016 = read.csv("/Users/ChrisLi/Desktop/370_Project/Data/data_2016.csv")

# 2013 DATA =======================================================
# Make dataframe
teamTable2013 <- data.frame(table(baseball_2013$home_team))

# Reorder team by occurences of Missed Calls
teamTable2013$Var1 <- factor(teamTable2013$Var1, levels=teamTable2013[order(teamTable2013$Freq), "Var1"])

# Set line variables
lowBound2013 <- 850
upBound2013 <- 1150

# Create barplot
ggplot(teamTable2013, aes(y=teamTable2013$Freq, x=teamTable2013$Var1, fill=teamTable2013$Freq)) + 
       geom_bar(stat="identity") + 
       coord_flip() +
       geom_hline(yintercept = 850, color="#ffffff") +
       geom_text(aes(0,lowBound2013,label = lowBound2013, hjust = 1, vjust = -.8), color="#ffffff") +
       geom_hline(yintercept = 1150, color="#515151") +
       geom_text(aes(0,upBound2013,label = upBound2013, hjust = 1, vjust = -.8), color="#515151") +
       labs(title = "2013 Missed Calls Freq at Home Team's Field", x = "Team Name", y = "Number of Missed Calls", fill='Missed Call Freq') +
       scale_fill_gradient2(mid="#400093", high = "#d3002d")

#====
# # Umpire dataframe
# umpireTable <- data.frame(table(baseball_2013$umpire))
# 
# # Create barplot
# ggplot(umpireTable, aes(y=umpireTable$Freq, x=umpireTable$Var1)) + 
#   geom_bar(stat="identity") + 
#   coord_flip()
#====

# 2014 DATA =======================================================
# Make dataframe
teamTable2014 <- data.frame(table(baseball_2014$home_team))

# Reorder team by occurences of Missed Calls
teamTable2014$Var1 <- factor(teamTable2014$Var1, levels=teamTable2014[order(teamTable2014$Freq), "Var1"])

# Create barplot
ggplot(teamTable2014, aes(y=teamTable2014$Freq, x=teamTable2014$Var1, fill=teamTable2014$Freq)) + 
  geom_bar(stat="identity") + 
  coord_flip() +
  geom_hline(yintercept = 850, color="#ffffff") +
  geom_text(aes(0,lowBound2013,label = lowBound2013, hjust = 1, vjust = -.8), color="#ffffff") +
  geom_hline(yintercept = 1150, color="#515151") +
  geom_text(aes(0,upBound2013,label = upBound2013, hjust = 1, vjust = -.8), color="#515151") +
  labs(title = "2014 Missed Calls Freq at Home Team's Field", x = "Team Name", y = "Number of Missed Calls", fill='Missed Call Freq') +
  scale_fill_gradient2(mid="#400093", high = "#d3002d")


# 2015 DATA =======================================================
# Make dataframe
teamTable2015 <- data.frame(table(baseball_2015$home_team))

# Reorder team by occurences of Missed Calls
teamTable2015$Var1 <- factor(teamTable2015$Var1, levels=teamTable2015[order(teamTable2015$Freq), "Var1"])

# Create barplot
ggplot(teamTable2015, aes(y=teamTable2015$Freq, x=teamTable2015$Var1, fill=teamTable2015$Freq)) + 
  geom_bar(stat="identity") + 
  coord_flip() +
  geom_hline(yintercept = 850, color="#ffffff") +
  geom_text(aes(0,lowBound2013,label = lowBound2013, hjust = 1, vjust = -.8), color="#ffffff") +
  geom_hline(yintercept = 1150, color="#515151") +
  geom_text(aes(0,upBound2013,label = upBound2013, hjust = 1, vjust = -.8), color="#515151") +
  labs(title = "2015 Missed Calls Freq at Home Team's Field", x = "Team Name", y = "Number of Missed Calls", fill='Missed Call Freq') +
  scale_fill_gradient2(mid="#400093", high = "#d3002d")

# 2016 DATA =======================================================
# Make dataframe
teamTable2016 <- data.frame(table(baseball_2016$home_team))

# Reorder team by occurences of Missed Calls
teamTable2016$Var1 <- factor(teamTable2016$Var1, levels=teamTable2016[order(teamTable2016$Freq), "Var1"])

# Create barplot
ggplot(teamTable2016, aes(y=teamTable2016$Freq, x=teamTable2016$Var1, fill=teamTable2016$Freq)) + 
  geom_bar(stat="identity") + 
  coord_flip() +
  geom_hline(yintercept = 850, color="#ffffff") +
  geom_text(aes(0,lowBound2013,label = lowBound2013, hjust = 1, vjust = -.8), color="#ffffff") +
  geom_hline(yintercept = 1150, color="#515151") +
  geom_text(aes(0,upBound2013,label = upBound2013, hjust = 1, vjust = -.8), color="#515151") +
  labs(title = "2016 Missed Calls Freq at Home Team's Field", x = "Team Name", y = "Number of Missed Calls", fill='Missed Call Freq') +
  scale_fill_gradient2(mid="#400093", high = "#d3002d")

# RANGES FOR ALL TEAMS ============================================
range(teamTable2013$Freq) #850-1140  290
range(teamTable2014$Freq) #829-1207  378
range(teamTable2015$Freq) #842-1148  306
range(teamTable2016$Freq) #906-1195  286
missedCallsRange <- c(290, 378, 306, 286)
sd(missedCallsRange)
# [1] 42.87968
summary(missedCallsRange)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 286     289     298     315     324     378 


# ========================== Team's Away Experience ============================

# 2013 Data ========================================================
# Make data frame
awayTable2013 <- data.frame(table(baseball_2013$away_team))

# Reorder team by occurences of Missed Calls
awayTable2013$Var1 <- factor(awayTable2013$Var1, levels=awayTable2013[order(awayTable2013$Freq), "Var1"])

lowBoundAway <- 850
upBoundAway <- 1200

# Create barplot
ggplot(awayTable2013, aes(y=awayTable2013$Freq, x=awayTable2013$Var1, fill=awayTable2013$Freq)) + 
  geom_bar(stat="identity") + 
  coord_flip() +
  geom_hline(yintercept = lowBoundAway, color="green") +
  geom_text(aes(0,lowBoundAway,label = lowBoundAway, hjust = 1, vjust = -.8), color="green") +
  geom_hline(yintercept = upBoundAway, color="red") +
  geom_text(aes(0,upBoundAway,label = upBoundAway, hjust = 1, vjust = -.8), color="red") +
  labs(title = "2013 Missed Calls Freq durring Team's Away Games", x = "Team Name", y = "Number of Missed Calls", fill='Missed Call Freq')
  scale_fill_gradient2(low="#b7006b", mid="#fffbd8", high = "#d3002d")

# 2014 Data ========================================================
# Make data frame
awayTable2014 <- data.frame(table(baseball_2014$away_team))

# Reorder team by occurences of Missed Calls  
awayTable2014$Var1 <- factor(awayTable2014$Var1, levels=awayTable2014[order(awayTable2014$Freq), "Var1"])

# Create barplot
ggplot(awayTable2014, aes(y=awayTable2014$Freq, x=awayTable2014$Var1, fill=awayTable2014$Freq)) + 
  geom_bar(stat="identity") + 
  coord_flip() +
  geom_hline(yintercept = lowBoundAway, color="green") +
  geom_text(aes(0,lowBoundAway,label = lowBoundAway, hjust = 1, vjust = -.8), color="green") +
  geom_hline(yintercept = upBoundAway, color="red") +
  geom_text(aes(0,upBoundAway,label = upBoundAway, hjust = 1, vjust = -.8), color="red") +
  labs(title = "2014 Missed Calls Freq durring Team's Away Games", x = "Team Name", y = "Number of Missed Calls", fill='Missed Call Freq')
  scale_fill_gradient2(low="#b7006b", mid="#fffbd8", high = "#d3002d")
  
# 2015 Data ========================================================
# Make data frame
awayTable2015 <- data.frame(table(baseball_2015$away_team))
  
# Reorder team by occurences of Missed Calls  
awayTable2015$Var1 <- factor(awayTable2015$Var1, levels=awayTable2015[order(awayTable2015$Freq), "Var1"])
  
# Create barplot
ggplot(awayTable2015, aes(y=awayTable2015$Freq, x=awayTable2015$Var1, fill=awayTable2015$Freq)) + 
  geom_bar(stat="identity") + 
  coord_flip() +
  geom_hline(yintercept = lowBoundAway, color="green") +
  geom_text(aes(0,lowBoundAway,label = lowBoundAway, hjust = 1, vjust = -.8), color="green") +
  geom_hline(yintercept = upBoundAway, color="red") +
  geom_text(aes(0,upBoundAway,label = upBoundAway, hjust = 1, vjust = -.8), color="red") +
  labs(title = "2015 Missed Calls Freq durring Team's Away Games", x = "Team Name", y = "Number of Missed Calls", fill='Missed Call Freq')
  scale_fill_gradient2(low="#b7006b", mid="#fffbd8", high = "#d3002d")
  
# 2016 Data ========================================================
# Make data frame
awayTable2016 <- data.frame(table(baseball_2016$away_team))
  
# Reorder team by occurences of Missed Calls  
awayTable2016$Var1 <- factor(awayTable2016$Var1, levels=awayTable2016[order(awayTable2016$Freq), "Var1"])
  
# Create barplot
ggplot(awayTable2016, aes(y=awayTable2016$Freq, x=awayTable2016$Var1, fill=awayTable2016$Freq)) + 
  geom_bar(stat="identity") + 
  coord_flip() +
  geom_hline(yintercept = lowBoundAway, color="green") +
  geom_text(aes(0,lowBoundAway,label = lowBoundAway, hjust = 1, vjust = -.8), color="green") +
  geom_hline(yintercept = upBoundAway, color="red") +
  geom_text(aes(0,upBoundAway,label = upBoundAway, hjust = 1, vjust = -.8), color="red") +
  labs(title = "2016 Missed Calls Freq durring Team's Away Games", x = "Team Name", y = "Number of Missed Calls", fill='Missed Call Freq')
  scale_fill_gradient2(low="#b7006b", high = "#d3002d")

# RANGES FOR ALL TEAMS ============================================  
range(awayTable2013$Freq)
#  809 1146  337  
range(awayTable2014$Freq)
#  862 1217  355
range(awayTable2015$Freq)
#  874 1145  271
range(awayTable2016$Freq)
#  800 1142  342 


# ========================== Line Chats ==========================
homeTeamMergered1 <- merge(teamTable2013, teamTable2014, by = "Var1", all = TRUE)
homeTeamMergered2 <- merge(teamTable2015, teamTable2016, by = "Var1", all = TRUE)
homeTeamMergeredAll <- merge(homeTeamMergered1, homeTeamMergered2, by = "Var1", all = TRUE)

# Renaming Columns
homeTeamMergeredAll <- rename(homeTeamMergeredAll, 
                              c("Var1" = "Team", "Freq.x.x" = "2013","Freq.y.x" = "2014",
                                "Freq.x.y" = "2015", "Freq.y.y" = "2016"))

# Wide to long data
home_data_long <- gather(homeTeamMergeredAll, Year, Freq, 2:5, factor_key=TRUE)

# Teams Home Field's Missed Calles Line Chart
ggplot(data=home_data_long, aes(x=home_data_long$Year, y=home_data_long$Freq, group=home_data_long$Team, colour=Team)) +
  geom_line() +
  geom_point() + 
  labs(title = "Missed Calls Freq at Team's Field by Year", x = "Year", y = "Number of Missed Calls")


# New York Yankies
NYY_Years <- homeTeamMergeredAll[19,]
NYY_Years <- gather(NYY_Years, Year, Freq, 2:5, factor_key=TRUE)

ggplot(data=NYY_Years, aes(x=NYY_Years$Year, y=NYY_Years$Freq, group=NYY_Years$Team, colour=Team)) +
  geom_line() +
  geom_point() + 
  labs(title = "New York Yankees Home Missed Calls Freq", x = "Year", y = "Number of Missed Calls")


# New York Mets
NYM_Years <- homeTeamMergeredAll[18,]
NYM_Years <- gather(NYM_Years, Year, Freq, 2:5, factor_key=TRUE)

ggplot(data=NYM_Years, aes(x=NYM_Years$Year, y=NYM_Years$Freq, group=NYM_Years$Team, colour=Team)) +
  geom_line() +
  geom_point() + 
  labs(title = "NYM Home Missed Calls Freq", x = "Year", y = "Number of Missed Calls") +
  theme(axis.text=element_text(size=20), axis.title=element_text(size=20, face='bold'), legend.text=element_text(size=20), plot.title=element_text(size=28, face='bold'))


# Kansas City Royals
KS_Years <- homeTeamMergeredAll[12,]
KS_Years <- gather(KS_Years, Year, Freq, 2:5, factor_key=TRUE)

ggplot(data=KS_Years, aes(x=KS_Years$Year, y=KS_Years$Freq, group=KS_Years$Team, colour=Team)) +
  geom_line() +
  geom_point() + 
  labs(title = "Kansas City Royals Home Missed Calls Freq", x = "Year", y = "Number of Missed Calls")

# Boston Red Sox
BOS_Years <- homeTeamMergeredAll[4,]
BOS_Years <- gather(BOS_Years, Year, Freq, 2:5, factor_key=TRUE)

ggplot(data=BOS_Years, aes(x=BOS_Years$Year, y=BOS_Years$Freq, group=BOS_Years$Team, colour=Team)) +
  geom_line() +
  geom_point() + 
  labs(title = "Boston Red Sox Home Missed Calls Freq", x = "Year", y = "Number of Missed Calls")



# Away Teams===========================
awayTeamMergered1 <- merge(awayTable2013, awayTable2014, by = "Var1", all = TRUE)
awayTeamMergered2 <- merge(awayTable2015, awayTable2016, by = "Var1", all = TRUE)
awayTeamMergeredAll <- merge(awayTeamMergered1, awayTeamMergered2, by = "Var1", all = TRUE)

# Renaming Columns
awayTeamMergeredAll <- rename(awayTeamMergeredAll, 
                              c("Var1" = "Team", "Freq.x.x" = "2013","Freq.y.x" = "2014",
                                "Freq.x.y" = "2015", "Freq.y.y" = "2016"))

# Wide to long data
away_data_long <- gather(awayTeamMergeredAll, Year, Freq, 2:5, factor_key=TRUE)

# Map sex to different point shape, and use larger points
ggplot(data=away_data_long, aes(x=away_data_long$Year, y=away_data_long$Freq, group=away_data_long$Team, colour=Team)) +
  geom_line() +
  geom_point() + 
  labs(title = "Teams Away Game Missed Calls Freq", x = "Year", y = "Number of Missed Calls")


# New York Mets
Away_NYM_Years <- awayTeamMergeredAll[18,]
Away_NYM_Years <- gather(Away_NYM_Years, Year, Freq, 2:5, factor_key=TRUE)

ggplot(data=Away_NYM_Years, aes(x=Away_NYM_Years$Year, y=Away_NYM_Years$Freq, group=Away_NYM_Years$Team, colour=Team)) +
  geom_line() +
  geom_point() + 
  labs(title = "NYM Away Missed Calls Freq", x = "Year", y = "Number of Missed Calls") +
  theme(axis.text=element_text(size=20), axis.title=element_text(size=20, face='bold'), legend.text=element_text(size=20), plot.title=element_text(size=28, face='bold'))

# Kansas City Royals
Away_KC_Years <- awayTeamMergeredAll[12,]
Away_KC_Years <- gather(Away_KC_Years, Year, Freq, 2:5, factor_key=TRUE)

ggplot(data=Away_KC_Years, aes(x=Away_KC_Years$Year, y=Away_KC_Years$Freq, group=Away_KC_Years$Team, colour=Team)) +
  geom_line() +
  geom_point() + 
  labs(title = "Kansas City Royals Away Missed Calls Freq", x = "Year", y = "Number of Missed Calls")



# Win Percents ====================================
baseball_WinNumbers = read.csv("/Users/ChrisLi/Desktop/370_Project/Data/MLB_Wins_2013-2016.csv")

# NYM =============
NYM_Wins <- data.frame(baseball_WinNumbers$Year, baseball_WinNumbers$NYM)
NYM_Wins <- rename(NYM_Wins, c("baseball_WinNumbers.Year" = "Year", "baseball_WinNumbers.NYM" = "Wins"))

# line plot
ggplot(data=NYM_Wins, aes(x=NYM_Wins$Year, y=NYM_Wins$Wins)) +
  geom_line() +
  geom_point() + 
  labs(title = "New York Mets Wins", x = "Year", y = "Number of Wins") +
  theme(axis.text=element_text(size=20), axis.title=element_text(size=20, face='bold'), legend.text=element_text(size=20), plot.title=element_text(size=28, face='bold'))

NYM_WinsPerc <- data.frame(baseball_WinNumbers$Year, baseball_WinNumbers$NYM / baseball_WinNumbers$G)


# KC =============
KC_Wins <- data.frame(baseball_WinNumbers$Year, baseball_WinNumbers$KCR)
KC_Wins <- rename(KC_Wins, c("baseball_WinNumbers.Year" = "Year", "baseball_WinNumbers.KCR" = "Wins"))
# line plot
ggplot(data=KC_Wins, aes(x=KC_Wins$Year, y=KC_Wins$Wins)) +
  geom_line() +
  geom_point() + 
  labs(title = "Kansas City Royals Wins", x = "Year", y = "Number of Wins")


# ===================== Correlations =============================

# New York Mets
# Getting the data
NYM_Test <- c(homeTeamMergeredAll[18, 2:5])
NYM_Test2 <- c(as.data.frame(t(NYM_Wins$Wins)))

# Win Rate
NYM_Test2 <- c(NYM_Test2$V1,NYM_Test2$V2,NYM_Test2$V3,NYM_Test2$V4)
# Home Missed calls
NYM_Test1 <- c(NYM_Test$`2013`, NYM_Test$`2014`, NYM_Test$`2015`, NYM_Test$`2016`)

# Correlation of Win Rate to Home Missed Calls
cor(NYM_Test1, NYM_Test2)
# [1] -0.8530922

# Away Missed calls
NYM_Test3 <- c(awayTeamMergeredAll[18, 2:5])
NYM_Test3 <- c(NYM_Test3$`2013`, NYM_Test3$`2014`, NYM_Test3$`2015`, NYM_Test3$`2016`)
cor(NYM_Test1, NYM_Test3)
# [1] 0.9842503


# ===================== Missed By Top and Bot Innings at Home ==========================
top_bot_calls_2013 <- data.frame(table(baseball_2013$home_team, baseball_2013$inning_topbot))
top_bot_calls_2014 <- data.frame(table(baseball_2014$home_team, baseball_2014$inning_topbot))
top_bot_calls_2015 <- data.frame(table(baseball_2015$home_team, baseball_2015$inning_topbot))
top_bot_calls_2016 <- data.frame(table(baseball_2016$home_team, baseball_2016$inning_topbot))

# First half merge
top_bot_calls_merged1 <- merge(top_bot_calls_2013, top_bot_calls_2014, by = c("Var1", "Var2"), all = TRUE)
# Second half merge
top_bot_calls_merged2 <- merge(top_bot_calls_2015, top_bot_calls_2016, by = c("Var1", "Var2"), all = TRUE)
# Final merge
top_bot_calls_mergedAll <- merge(top_bot_calls_merged1, top_bot_calls_merged2, by = c("Var1", "Var2"), all = TRUE)

# Renaming Columns
top_bot_calls_mergedAll <- rename(top_bot_calls_mergedAll, 
                              c("Var1" = "Team", "Var2" = "Inning_topbot", "Freq.x.x" = "2013","Freq.y.x" = "2014",
                                "Freq.x.y" = "2015", "Freq.y.y" = "2016"))

# Wide to long data
top_bot_home_data_long <- gather(top_bot_calls_mergedAll, Year, Freq, 3:6, factor_key=TRUE)

# Top exclusive missed calls
top_merged <- sqldf("SELECT * FROM top_bot_home_data_long WHERE Inning_topbot = 'top'")
# Bot exclusive missed calls
bot_merged <- sqldf("SELECT * FROM top_bot_home_data_long WHERE Inning_topbot = 'bot'")


# Teams Home Field's Missed Calles for Away Team
ggplot(top_merged, aes(x=top_merged$Year, y=top_merged$Freq, group=top_merged$Team, colour=Team)) +
  geom_line() +
  geom_point() + 
  labs(title = "Missed Calls Against Away at Home's Field", x = "Year", y = "Number of Missed Calls")

# Teams Home Field's Missed Calles for Home Team
ggplot(bot_merged, aes(x=bot_merged$Year, y=bot_merged$Freq, group=bot_merged$Team, colour=Team)) +
  geom_line() +
  geom_point() + 
  labs(title = "Missed Calls Against Home at Home's Field", x = "Year", y = "Number of Missed Calls")

# Tests for World Series team 2013-2016

# Boston Red Sox ============================
BOS_top_years  <- top_bot_calls_mergedAll[8,]
BOS_top_years <- gather(BOS_top_years, Year, Freq, 3:6, factor_key=TRUE)

BOS_bot_years <- top_bot_calls_mergedAll[7,]
BOS_bot_years <- gather(BOS_bot_years, Year, Freq, 3:6, factor_key=TRUE)

# Boston Top
ggplot(data=BOS_top_years, aes(x=BOS_top_years$Year, y=BOS_top_years$Freq, group=BOS_top_years$Team, colour=Team)) +
  geom_line() +
  geom_point() + 
  labs(title = "BOS Home Missed calls against Away", x = "Year", y = "Number of Missed Calls") +
  theme(axis.text=element_text(size=20), axis.title=element_text(size=20, face='bold'), legend.text=element_text(size=20), plot.title=element_text(size=24, face='bold'))

# Boston Bot
ggplot(data=BOS_bot_years, aes(x=BOS_bot_years$Year, y=BOS_bot_years$Freq, group=BOS_bot_years$Team, colour=Team)) +
  geom_line() +
  geom_point() + 
  labs(title = "BOS Home Missed calls against Home", x = "Year", y = "Number of Missed Calls") +
  theme(axis.text=element_text(size=20), axis.title=element_text(size=20, face='bold'), legend.text=element_text(size=20), plot.title=element_text(size=24, face='bold'))

BOS_Wins <- data.frame(baseball_WinNumbers$Year, baseball_WinNumbers$BOS)
BOS_Wins <- rename(BOS_Wins, c("baseball_WinNumbers.Year" = "Year", "baseball_WinNumbers.BOS" = "Wins"))
BOS_WinsPerc <- data.frame(baseball_WinNumbers$Year, baseball_WinNumbers$BOS / baseball_WinNumbers$G)
BOS_WinsPerc <- rename(BOS_WinsPerc, c("baseball_WinNumbers.Year" = "Year", "baseball_WinNumbers.BOS.baseball_WinNumbers.G" = "Win_Percent"))

# Boston Win rate by Percent
ggplot(data=BOS_WinsPerc, aes(x=BOS_WinsPerc$Year, y=BOS_WinsPerc$Win_Percent)) +
  geom_line() +
  geom_point() + 
  labs(title = "Boston Red Sox Win Percent", x = "Year", y = "Wins Percent") +
  theme(axis.text=element_text(size=20), axis.title=element_text(size=20, face='bold'), legend.text=element_text(size=20), plot.title=element_text(size=24, face='bold'))

# Correlations:
BOS_cor_wins <- c(BOS_Wins$Wins)
BOS_cor_top <- c(BOS_top_years$Freq)
BOS_cor_bot <- c(BOS_bot_years$Freq)

cor(BOS_cor_top, BOS_cor_wins)
# [1] -0.7602084
cor(BOS_cor_bot, BOS_cor_wins)
# [1] -0.8145406


# St. Louis Cardinals ============================
STL_top_years  <- top_bot_calls_mergedAll[52,]
STL_top_years <- gather(STL_top_years, Year, Freq, 3:6, factor_key=TRUE)

STL_bot_years <- top_bot_calls_mergedAll[51,]
STL_bot_years <- gather(STL_bot_years, Year, Freq, 3:6, factor_key=TRUE)

# St. Louis Top
ggplot(data=STL_top_years, aes(x=STL_top_years$Year, y=STL_top_years$Freq, group=STL_top_years$Team, colour=Team)) +
  geom_line() +
  geom_point() + 
  labs(title = "STL Home Missed calls against Away", x = "Year", y = "Number of Missed Calls") +
  theme(axis.text=element_text(size=20), axis.title=element_text(size=20, face='bold'), legend.text=element_text(size=20), plot.title=element_text(size=24, face='bold'))

# St. Louis Bot
ggplot(data=STL_bot_years, aes(x=STL_bot_years$Year, y=STL_bot_years$Freq, group=STL_bot_years$Team, colour=Team)) +
  geom_line() +
  geom_point() + 
  labs(title = "STL Home Missed calls against Home", x = "Year", y = "Number of Missed Calls") +
  theme(axis.text=element_text(size=20), axis.title=element_text(size=20, face='bold'), legend.text=element_text(size=20), plot.title=element_text(size=24, face='bold'))

STL_Wins <- data.frame(baseball_WinNumbers$Year, baseball_WinNumbers$STL)
STL_Wins <- rename(STL_Wins, c("baseball_WinNumbers.Year" = "Year", "baseball_WinNumbers.STL" = "Wins"))
STL_WinsPerc <- data.frame(baseball_WinNumbers$Year, baseball_WinNumbers$STL / baseball_WinNumbers$G)
STL_WinsPerc <- rename(STL_WinsPerc, c("baseball_WinNumbers.Year" = "Year", "baseball_WinNumbers.STL.baseball_WinNumbers.G" = "Win_Percent"))

# St. Louis Win rate by Percent
ggplot(data=STL_WinsPerc, aes(x=STL_WinsPerc$Year, y=STL_WinsPerc$Win_Percent)) +
  geom_line() +
  geom_point() + 
  labs(title = "St. Louis Cardinals Win Percent", x = "Year", y = "Wins Percent") +
  theme(axis.text=element_text(size=20), axis.title=element_text(size=20, face='bold'), legend.text=element_text(size=20), plot.title=element_text(size=24, face='bold'))

# Correlations:
STL_cor_wins <- c(STL_Wins$Wins)
STL_cor_top <- c(STL_top_years$Freq)
STL_cor_bot <- c(STL_bot_years$Freq)

cor(STL_cor_top, STL_cor_wins)
# [1] 0.6622238
cor(STL_cor_bot, STL_cor_wins)
# [1] 0.2613116


# San Francisco Giants ============================
SF_top_years  <- top_bot_calls_mergedAll[50,]
SF_top_years <- gather(SF_top_years, Year, Freq, 3:6, factor_key=TRUE)

SF_bot_years <- top_bot_calls_mergedAll[49,]
SF_bot_years <- gather(SF_bot_years, Year, Freq, 3:6, factor_key=TRUE)

# San Francisco Top
ggplot(data=SF_top_years, aes(x=SF_top_years$Year, y=SF_top_years$Freq, group=SF_top_years$Team, colour=Team)) +
  geom_line() +
  geom_point() + 
  labs(title = "SF Home Missed calls against Away", x = "Year", y = "Number of Missed Calls") +
  theme(axis.text=element_text(size=20), axis.title=element_text(size=20, face='bold'), legend.text=element_text(size=20), plot.title=element_text(size=24, face='bold'))

# San Francisco Bot
ggplot(data=SF_bot_years, aes(x=SF_bot_years$Year, y=SF_bot_years$Freq, group=SF_bot_years$Team, colour=Team)) +
  geom_line() +
  geom_point() + 
  labs(title = "SF Home Missed calls against Home", x = "Year", y = "Number of Missed Calls") +
  theme(axis.text=element_text(size=20), axis.title=element_text(size=20, face='bold'), legend.text=element_text(size=20), plot.title=element_text(size=24, face='bold'))

SF_Wins <- data.frame(baseball_WinNumbers$Year, baseball_WinNumbers$SF)
SF_Wins <- rename(SF_Wins, c("baseball_WinNumbers.Year" = "Year", "baseball_WinNumbers.SF" = "Wins"))
SF_WinsPerc <- data.frame(baseball_WinNumbers$Year, baseball_WinNumbers$SF / baseball_WinNumbers$G)
SF_WinsPerc <- rename(SF_WinsPerc, c("baseball_WinNumbers.Year" = "Year", "baseball_WinNumbers.SF.baseball_WinNumbers.G" = "Win_Percent"))

# San Francisco Win rate by Percent
ggplot(data=SF_WinsPerc, aes(x=SF_WinsPerc$Year, y=SF_WinsPerc$Win_Percent)) +
  geom_line() +
  geom_point() + 
  labs(title = "San Francisco Win Percent", x = "Year", y = "Wins Percent") +
  theme(axis.text=element_text(size=20), axis.title=element_text(size=20, face='bold'), legend.text=element_text(size=20), plot.title=element_text(size=24, face='bold'))

# Correlations:
SF_cor_wins <- c(SF_Wins$Wins)
SF_cor_top <- c(SF_top_years$Freq)
SF_cor_bot <- c(SF_bot_years$Freq)

cor(SF_cor_top, SF_cor_wins)
# [1] -0.2208167
cor(SF_cor_bot, SF_cor_wins)
# [1] 0.4354521