# Create Dataframe
game <- data.frame(steam_charts)

# Fix date format 
game <- subset(game, Month!="Last 30 Days")
game1 <- sub(" ", " 01 ", game$Month)
game$Month <- game1

game$Month <- as.Date(game$Month, "%B %d %Y"); game$Month

# Show date type
class(game$Month)

# Clean up column names
cnames <- c("Month","AvgPlayers","Gain","PercentGain","PeakPlayers","AppID","Game")
colnames(game) <- cnames

# What is the total number of peak players for one game?

max(game$PeakPlayers)
mean(game$PeakPlayers)

gameSorted <- game[order(-game$PeakPlayers),]
gameSorted
game[which.max(game$PeakPlayers),]
head(game$PeakPlayers, 15)

# PUBG: BATTLEGROUNDS has the most PeakPlayers in a month January 2018 was the most at 3,236,027

# Total number of PeakPlayers
peak <- tapply(game$PeakPlayers, game$Game, sum)
gameName <- rownames(peak)
totalPeakPlayers <- data.frame(gameName, peak)
summarise(totalPeakPlayers)

# or use group_by
totalPeakPlayersGroupBy =
  game %>% group_by(Game) %>%
  summarise(totalPeakPlayers =sum(peak),)
  
# Dota 2 has the most players of all with 87,132,203
totalPeakPlayers[which.max(totalPeakPlayers$peak),]

#For each game average # of peak players?
#group the data by APPID and then PeakPlayers
PPGroupBy =
  game %>% group_by(AppID,PeakPlayers)
#summarise the data into summary
summary <- summarise(PPGroupBy)
#find the average and store in variable
average_peak_pergame <- with(summary, by(PeakPlayers, AppID, mean))

## Text Mining and Word Cloud

library(quanteda)
library(quanteda.textplots)


## Top 500 and Bottom 500 performing games of November (based on percent gain)

gameLast30.Top500 <- gameLast30[with(gameLast30, order(-PercentGain)),]
gameLast30.Top500 <- gameLast30.Top500[1:500,]

gameLast30.Bottom500 <- gameLast30[with(gameLast30, order(PercentGain)),]
gameLast30.Bottom500 <- gameLast30.Bottom500[1:500,]


## Corpus, Tokens, and Word Cloud for Top 500 (November)

gameCorpus.Top <- corpus(gameLast30.Top500$Game)

toks.Top <- tokens(gameCorpus.Top, remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE)
toksNoStops.Top <- tokens_select(toks.Top, pattern = stopwords("english"), selection = "remove")

gameDFM.Top <- dfm(toksNoStops.Top, remove = c("Ã", "Â", "¯", "¿", "½", "¢", "®", "é", "»", "'", "ç", ">", "S", "Y", "å", "T", "¨"))

gameWC.Top <- textplot_wordcloud(gameDFM.Top, min_count = 2)


## Corpus, Tokens, and Word Cloud for Bottom 500 (November)

gameCorpus.Bottom <- corpus(gameLast30.Bottom500$Game)

toks.Bottom <- tokens(gameCorpus.Bottom, remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE)
toksNoStops.Bottom <- tokens_select(toks.Bottom, pattern = stopwords("english"), selection = "remove")

gameDFM.Bottom <- dfm(toksNoStops.Bottom, remove = c("Ã", "Â", "¯", "¿", "½", "¢", "®", "é", "»", "'", "ç", ">", "S", "Y", "å", "T", "¨"))

gameWC.Bottom <- textplot_wordcloud(gameDFM.Bottom, min_count = 2)


#For each game average # of peak players?
#group the data by APPID and then PeakPlayers
PPGroupBy =
game %>% group_by(Game,PeakPlayers)
#summarise the data into summary
summary <- summarise(PPGroupBy)
#find the average and store in variable
average_peak_pergame <- with(summary, by(PeakPlayers, Game, mean))
#put back into a matrix
average_peak_pergame_2 <- t(sapply(average_peak_pergame, I))
#create data frame for avg peak per game
new_avg_peak.df <- as.data.frame(average_peak_pergame_2)
#stack two create two columns
df <- stack(new_avg_peak.df)
df <- na.omit(df)
#update the column names
colnames(df) <- c('AvgPeakPlayers','Game')
#store into new dataframe and order in ascending order
newdf <- df[order(df$AvgPeakPlayers),]
#set top 10 and lowest 10 to veriables
low <- head(newdf, n = 10)
high <- tail(newdf, n = 10)

#visuals
#create bar graph for top 10 lowest 10 avg Peak Players
p1<-ggplot(data=low, aes(x=Game, y=AvgPeakPlayers)) +
geom_bar(stat="identity", show.legend = FALSE,  fill = "#0072B2")+
theme_classic()
p1+coord_flip()
p2<-ggplot(data=high, aes(x=Game, y=AvgPeakPlayers)) +
geom_bar(stat="identity", show.legend = FALSE,  fill = "#0072B2")+
theme_classic()
p2+coord_flip()

#write a file with all the average Peak Players per Game, not sure if this is useful for the project
write.csv(newdf, file = 'avg_peakplayers_pergame.csv')


#Is there a specific season or month where the number of players increases
#the most on average?

#create a column with just month store in a new column name
game$MonthAbrev <- format(game$Month, format="%b")
game$Year <- format(game$Month, format="%Y")
#group by month, year,  and average players
SeasonsData =
game %>% group_by(MonthAbrev,Year,AvgPlayer)
#summarise the data into summary
summary2 <- summarise(SeasonsData)
#total amount of players per year
totalperyear <- with(summary2, by(AvgPlayer, Year, sum))
#put back into a matrix
totalperyear2 <- t(sapply(totalperyear, I))
#create data frame for avg peak per game
totalperyear.df <- as.data.frame(totalperyear2)
#stack two create two columns
df2 <- stack(totalperyear.df)
#update the column names
colnames(df2) <- c('Total Avg Players','Year')
#create column with month and year together
game$MonYear <- format(game$Month, format="%b %Y")
#group by month, year,  and average players
SeasonsData2 =
game %>% group_by(MonYear,AvgPlayer)
#summarise the data into summary
summary3 <- summarise(SeasonsData2)
#total amount of players per month per year
totalpermonth <- with(summary3, by(AvgPlayer, MonYear, sum))
#put back into a matrix
totalpermonth2 <- t(sapply(totalpermonth, I))
#create data frame for avg peak per game
totalpermonth.df <- as.data.frame(totalpermonth2)
#stack two create two columns
df3 <- stack(totalpermonth.df)
#update the column names
colnames(df3) <- c('TotalAvgPlayers','MonthYear')
#visulizations
p3<-ggplot(data=df3, aes(x=MonthYear, y=TotalAvgPlayers)) +
geom_bar(stat="identity",show.legend = FALSE, fill = '#009E73')+
theme_classic()+
theme(axis.text.x = element_text(angle = 90))
p3
#display settings
require(scales)
p3 + scale_y_continuous(labels = comma)
View(p3)

