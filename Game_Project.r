game <- data.frame(steam_charts)

game <- subset(game, Month!="Last 30 Days")
game1 <- sub(" ", " 01 ", game$Month)
game$Month <- game1



game$Month <- as.Date(game$Month, "%B %d %Y"); game$Month
class(game$Month)

