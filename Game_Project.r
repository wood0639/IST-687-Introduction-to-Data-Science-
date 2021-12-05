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
cnames <- c("Month","AvgPlayer","Gain","PercentGain","PeakPlayers","AppID","Game")
colnames(game) <- cnames

