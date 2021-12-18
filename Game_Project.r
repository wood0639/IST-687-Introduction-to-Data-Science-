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
