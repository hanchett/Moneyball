#Set up work setting directory, adding packages and importing data 
setwd("C:/Users/Ryan/Desktop/CanoAnalysis")
install.packages("sqldf")
install.packages("ggplot2")
install.packages('reshape2')
library("ggplot2")
library("sqldf")
library('reshape2') 

Cano2016 <- read.csv('CanoData2016.csv')
Cano2015 <- read.csv('CanoData2015.csv')
Cano2014 <- read.csv('CanoData2014.csv')

#Compiling all of Cano's data over the past three seasons
TotalCanoData <- rbind(Cano2016, Cano2015, Cano2014)

#Grabbing just his pitches where a called strike was outside the strike zone
CanoBadCalls <- sqldf("SELECT * FROM TotalCanoData WHERE (zone=11 OR zone=12 OR zone=13 OR zone=14) AND description like '%Called Strike%'")

TotalCanoResults <- sqldf("SELECT events, count(*) as Frequency FROM TotalCanoData GROUP BY events ORDER BY events")
BadCallCanoResults <- sqldf("SELECT events as badEvents, count(*) as BadCallFrequency FROM CanoBadCalls GROUP BY events ORDER BY events")
CombinedTotals <- sqldf('SELECT * FROM TotalCanoResults tcr JOIN BadCallCanoResults bcrn ON tcr.events = bcrn.badEvents')

#Frequencies of both total and just bad calls' results 
TotalResultsNormd <- sqldf('SELECT events, Frequency*1.0 / (SELECT SUM(Frequency) FROM TotalCanoResults) as Freq FROM TotalCanoResults GROUP BY events')
BadResultsNormd <- sqldf('SELECT badEvents, BadCallFrequency*1.0 / (SELECT SUM(BadCallFrequency) FROM BadCallCanoResults) as BadCallFreq FROM BadCallCanoResults GROUP BY badEvents')
CombinedFreq <- sqldf("SELECT * FROM TotalResultsNormd trn JOIN BadResultsNormd brn ON brn.badEvents = trn.events")

#Grabbing the total number of pitches by result to normalize data 
TotalEventsByGame <- sqldf('SELECT pitch_id, count(pitch_id) as numPitches, tcd.events, bccr.BadCallFrequency FROM TotalCanoData tcd JOIN CombinedTotals ct ON tcd.events = ct.events JOIN BadCallCanoResults bccr ON bccr.badEvents = ct.events GROUP BY tcd.events ORDER BY tcd.events')
#TotalEventsByGameBad <- sqldf('SELECT pitch_id, count(pitch_id) as numPitches, tcd.events as numberOfPitches FROM TotalCanoData tcd JOIN CombinedTotals ct ON tcd.events = ct.badEvents WHERE (zone=11 OR zone=12 OR zone=13 OR zone=14) AND description like "%Called Strike%"  GROUP BY tcd.events ORDER BY tcd.events')


#Goes through all calls and calculates standard error for good calls
SE <- c()
for (f in CombinedFreq$Freq) {
  SE <- c(SE, f)
}
stdev <- sd(SE)
standardErrorAll <- stdev / sqrt(length(CombinedFreq$Freq))

xlabels <- CombinedFreq$events

#Plots the normalized event values for all pitches and pitches with missed calls
plot(CombinedFreq$Freq,type="l", col="red", xaxt="n")
lines(CombinedFreq$BadCallFreq,col="green")
axis(1,at=1:14,labels=xlabels)


#Creating a binary array of Cano's strikeouts(1) vs non-strikeouts(0)
totalStrikeout <- c()
for(pitch in TotalCanoData$events) {
  if(pitch == "Strikeout")
    totalStrikeout <- c(totalStrikeout, 1)  
  else 
    totalStrikeout <- c(totalStrikeout, 0)
}

#Creating vectors of the events & zones from Cano's data
events <- c()
zones <- c()
count <- 1
for(pitch in TotalCanoData) {
  if(count == 10)
    events <-(pitch)
  else if(count == 16)
    zones <- (pitch)
  count = count + 1
}

#Looping through the events and zones to create another binary vector with 1s being 
#missed call strike outs and 0 being everything else
missedStrikeout <- c()
for(i in 1:7311){
  event <- events[i]
  zone <- zones[i]
  if((zone == '14' | zone == '13' | zone == '12' | zone == '11') & event == 'Strikeout'){
    missedStrikeout <- c(missedStrikeout, 1)
  }  else {
    missedStrikeout <- c(missedStrikeout, 0)
  }
}

# Performing the Wilcox test on the number of total strikes vs the number of strikes 
# in the same at bat as a missed call since the data does not have a normal distribution 
wilcox.test(missedStrikeout, totalStrikeout)

#Converting the frequencies of each event to dataframes to allow easier plotting 
df1 <- data.frame(CombinedFreq$Freq)
df2 <- data.frame(CombinedFreq$BadCallFreq)
id <- c(rep('Frequency For All Pitches', times=15), rep('Frequency For Missed Pitches', times=15))

#Combining two sets of data with different IDs for all calls vs missed calls
test_data <- data.frame(c(CombinedFreq$Freq, CombinedFreq$BadCallFreq), id)

#Adding column names
colnames(test_data) <- c("Freq", "Type")

#Reshaping data and removing variable column
test_data <- melt(test_data, id.vars="Type")
test_data <- subset(test_data, select = -c(variable) )

#Adding labeling to data
combined <- data.frame(xlabels, test_data)

#GGPlot required parameters
Outcomes <- combined$xlabels
Frequency <- test_data$value
Legend <- factor(combined$Type)

#Creating barplots with labels
theme_set(theme_grey(base_size= 12, face='bold'))
CanoPlot <-ggplot(combined, aes(x=Outcomes, y=Frequency, fill=Legend)) + geom_bar(stat="identity", position = position_dodge())
LabeledPlot <- CanoPlot + labs(title="Results of Pitches to Cano", x="Result of pitch", y="Frequency of Result", color="Legend")  
LabeledPlot <- CanoPlot + labs(title="Results of Pitches to Cano", x="Result of pitch", y="Frequency of Result", color="Legend")  
LabeledPlot <- LabeledPlot + theme(axis.text=element_text(size=16, face='bold', angle=90), axis.title=element_text(size=16, face='bold'), legend.text=element_text(size=16))
LabeledPlot
#Adding error bars to plot 
errDF1 <- aes(ymax = combined$value + standardError, ymin= combined$value - standardError)
#errDF2 <- aes(ymax = df2 + standardError, ymin=df2 - standardError)
PlotWithError <- LabeledPlot + geom_errorbar(errDF1, width=0.3, color='Black', position=position_dodge(0.85)) 
PlotWithError


