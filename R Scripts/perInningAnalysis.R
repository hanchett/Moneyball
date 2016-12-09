setwd("Desktop/INFO 370/Preliminary Data Analysis")
install.packages("sqldf")
install.packages("ggplot2")
install.packages("dplyr")
library("ggplot2")
library("sqldf")
library("dplyr")
rm(list=ls(all=TRUE))
data2016 <- read.csv("data_2016.csv")
data2015 <- read.csv("data_2015.csv")
data2014 <- read.csv("data_2014.csv")
data2013 <- read.csv("data_2013.csv")
fullData <- rbind(data2016, data2015, data2014, data2013)

#By inning analysis
#test if data is normal
hist(filteredData$inning, main = "Histogram of Innings where Missed Calls Occur")
hist(log10(filteredData$inning, main = "Histogram of Log10(Innings where Missed Calls Occur)")) #answer: data is not normal, use Wilcox test

#Group data by inning segment and inning number, do it for top only groups  
countByInningAll <- sqldf("SELECT inning, inning_topbot, COUNT(*) as numPerInningSegment 
                       FROM fullData
                       WHERE inning < 10
                       GROUP by inning, inning_topbot")
countByGamesAll <- sqldf("SELECT inning, inning_topbot, game_pk, COUNT(game_pk) as numGames
                       FROM fullData
                       WHERE inning < 10
                       GROUP by inning, inning_topbot, game_pk")
countByGamesAll <- sqldf("SELECT inning, inning_topbot, COUNT(*) as numGames
                       FROM countByGamesAll
                       GROUP by inning, inning_topbot")
countByInningTop <- sqldf("SELECT inning, inning_topbot, game_date, COUNT(*) as numPerInningSegment
                        FROM fullData
                        WHERE inning_topbot = 'top'
                        AND inning < 10
                        GROUP by inning, inning_topbot")
countByGamesTop <- sqldf("SELECT inning, inning_topbot, game_date, game_pk, COUNT(*) as numGames
                      FROM fullData
                      WHERE inning_topbot = 'top'
                      AND inning < 10
                      GROUP by inning, game_date, inning_topbot, game_pk")
countByGamesTop <- sqldf("SELECT inning, COUNT(*) as numGames
                      FROM countByGamesTop
                      GROUP by inning")
countByInningBot <- sqldf("SELECT inning, inning_topbot, game_date, COUNT(*) as numPerInningSegment
                        FROM fullData
                        WHERE inning_topbot = 'bot'
                        AND inning < 10
                        GROUP by inning, inning_topbot")
countByGamesBot <- sqldf("SELECT inning, inning_topbot, game_date, game_pk, COUNT(*) as numGames
                         FROM fullData
                         WHERE inning_topbot = 'bot'
                         AND inning < 10
                         GROUP by inning, game_date, inning_topbot, game_pk")
countByGamesBot <- sqldf("SELECT inning, COUNT(*) as numGames
                         FROM countByGamesBot
                         GROUP by inning")
seInning <- c()
for (i in c(1:9)) {
  inning <- i
  calculatingSE <- fn$sqldf("SELECT inning, inning_topbot, game_pk, COUNT(game_pk) as numGames
                         FROM fullData
                         WHERE inning_topbot = 'bot'
                         AND inning = $inning
                         GROUP by inning, inning_topbot, game_pk")
  seInning <- c(seInning, sd(calculatingSE$numGames)/sqrt(nrow(calculatingSE)))
  calculatingSE <- fn$sqldf("SELECT inning, inning_topbot, game_pk, COUNT(game_pk) as numGames
                         FROM fullData
                         WHERE inning_topbot = 'top'
                         AND inning = $inning
                         GROUP by inning, inning_topbot, game_pk")
  seInning <- c(seInning, sd(calculatingSE$numGames)/sqrt(nrow(calculatingSE)))
}
seInning
pInning <- qplot(countByInningAll$inning, (countByInningAll$numPerInningSegment / countByGamesAll$numGames),
      aes(fill = value),     
      xlab="Inning", ylab="Number of Missed Calls per Game", geom=c("point"), 
      method="lm", formula=y~x, color=countByInningAll$inning_topbot, 
      main = "Number of Missed Calls in Top and Bottom Inning Segments") + labs(color='Inning Segment') + geom_smooth(se = FALSE, method = "lm")
pInning
pInning + scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9)) + geom_errorbar(aes(ymin = (countByInningAll$numPerInningSegment / countByGamesAll$numGames)-seInning, ymax=(countByInningAll$numPerInningSegment / countByGamesAll$numGames)+seInning), width =.1)
wilInning <- wilcox.test((countByInningTop$numPerInningSegment/countByGamesTop$numGames), countByInningBot$numPerInningSegment/countByGamesBot$numGames)
wilInning

#Focusing in on the 9th Inning, by zone
ninthInningTop <- sqldf("SELECT *
                     FROM fullData
                     WHERE inning = 9
                     AND inning_topbot = 'top'")
ninthInningBot <- sqldf("SELECT *
                     FROM fullData
                     WHERE inning = 9
                     AND inning_topbot = 'bot'")
botSample <- sample_n(ninthInningBot, 2000)
topSample <- sample_n(ninthInningTop, 2000)
topHist <- hist(topSample$zone, freq=FALSE, main = "Density Plot of Top 9", xlab = "Zone")
botHist <- hist(botSample$zone, freq=FALSE, main = "Density Plot of Bot 9", xlab = "Zone")
plot(topSample$px, topSample$pz, ylab = "Pitch Z Axis", xlab = "Pitch X Axis", main = "Top 9 Heat Map", col=alpha("red", 0.09), type ="p", pch = 16, cex=1, xlim = c(-2,2), ylim = c(0, 4))
abline(2, 0)
abline(v=0)
plot(botSample$px, botSample$pz, ylab = "Pitch Z Axis", xlab = "Pitch X Axis", main = "Bot 9 Heat Map", col=alpha("blue", 0.09), type ="p", pch = 16, cex=1, xlim = c(-2,2), ylim = c(0, 4))
abline(2, 0)
abline(v=0)
wilcox.test(topSample$px, botSample$px)
wilcox.test(topSample$pz, botSample$pz)
pxRangeTop <- range(ninthInningTop$px)
pzRangeTop <- range(ninthInningTop$pz)
pxRangeBot <- range(ninthInningBot$px)
pzRangeBot <- range(ninthInningBot$pz)
#Calculate Area of Each Zone
topArea <- (pzRangeTop[2] - pzRangeTop[1]) * (pxRangeTop[2] - pxRangeTop[1])
botArea <- (pzRangeBot[2] - pzRangeBot[1]) * (pxRangeBot[2] - pxRangeBot[1])
ninthInningTopPitchType <- sqldf("SELECT pitch_type, inning_topbot, count(*) FROM fullData WHERE inning = 9 GROUP BY pitch_type, inning_topbot")
ninthInningBot <- sqldf("SELECT inning, inning_topbot, game_date, COUNT(*) as numBot
                        FROM fullData
                        WHERE inning_topbot = 'bot'
                        AND inning = 9
                         GROUP by inning, inning_topbot, game_date")
ninthInningTop <- sqldf("SELECT inning, inning_topbot, game_date, COUNT(*) as numTop
                        FROM fullData
                        WHERE inning_topbot = 'top'
                        AND inning = 9
                        GROUP by inning, game_date, inning_topbot")
nineTopGames <- sqldf("SELECT inning, inning_topbot, game_date, game_pk, COUNT(*) as numTopGames
                      FROM fullData
                      WHERE inning_topbot = 'top'
                      AND inning = 9
                      GROUP by inning, game_date, inning_topbot, game_pk")
nineTopGames <- sqldf("SELECT game_date, COUNT(*) as numTopGames
                      FROM nineTopGames
                      GROUP by game_date")
nineBotGames <- sqldf("SELECT inning, inning_topbot, game_date, game_pk, COUNT(*) as numTopGames
                      FROM fullData
                      WHERE inning_topbot = 'bot'
                      AND inning = 9
                      GROUP by inning, game_date, inning_topbot, game_pk")
nineBotGames <- sqldf("SELECT game_date, COUNT(*) as numTopGames
                      FROM nineBotGames
                      GROUP by game_date")
normTop <- ninthInningTop$numTop/nineTopGames$numTopGames
normBot <- ninthInningBot$numBot/nineBotGames$numTopGames
ninthInningWil <- wilcox.test(ninthInningCombined$numTop, ninthInningCombined$numBot, paired = TRUE)
ninthInningWil
wilcox.test(ninthInningTop$numTop, ninthInningBot$numBot)
wilcox.test(normTop, normBot)
topPT <- table(ninthInningTop$pitch_type)/sum(table(ninthInningTop$pitch_type))
botPT <- table(ninthInningBot$pitch_type)/sum(table(ninthInningBot$pitch_type))
topPT
botPT
wilcox.test(topPT, botPT, exact = FALSE)
