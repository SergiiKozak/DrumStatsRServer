getGames <- function(){
  library(jsonlite)
  games <- fromJSON("http://foosball-results.herokuapp.com/api/games", flatten = TRUE)

  games <- games[games$blue.score != games$red.score,]
  games <- games[is.na(games$source) | games$source != "og-source/staging",]

  dupes <- merge(games, games, by.x = c("blue.offense._id", "blue.defense._id", "red.offense._id", "red.defense._id", "blue.score", "red.score"), by.y = c("blue.offense._id", "blue.defense._id", "red.offense._id", "red.defense._id", "blue.score", "red.score"))
  dupes <- dupes[dupes$`_id.x` != dupes$`_id.y`,]
  dupes <- dupes[abs(difftime(strptime(dupes$endDate.x, format = '%Y-%m-%dT%H:%M:%OSZ'), strptime(dupes$endDate.y, format = '%Y-%m-%dT%H:%M:%OSZ'), units = c("mins"))) < 10,]
  dupes <- dupes[grep("^DrumStats", dupes$source.x),]

  games <- subset(games, !(games$`_id` %in% dupes$`_id.y`))

  return(games)
}
