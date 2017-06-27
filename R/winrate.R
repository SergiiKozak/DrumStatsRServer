winrate <- function(d){
  library(jsonlite)

  games <- fromJSON("http://foosball-results.herokuapp.com/api/games", flatten = TRUE)

  players <- fromJSON("http://foosball-results.herokuapp.com/api/players", flatten = TRUE)
  players <- data.frame(Id = players$`_id`)

  games <- games[games$blue.score != games$red.score,]

  blueWiners <- games[games$blue.score > games$red.score,]
  redWinners <- games[games$red.score > games$blue.score,]

  blueWiners <- data.frame(offense = blueWiners$blue.offense._id, defense = blueWiners$blue.defense._id)
  redWinners <- data.frame(offense = redWinners$red.offense._id, defense = redWinners$red.defense._id)

  winners <- rbind(blueWiners, redWinners)

  allPlays <- rbind(data.frame(offense = games$blue.offense._id, defense = games$blue.defense._id), data.frame(offense = games$red.offense._id, defense = games$red.defense._id))

  offense.wincount = data.frame(table(winners$offense))
  colnames(offense.wincount) <- c("Id", "offense.wincount")
  defense.wincount = data.frame(table(winners$defense))
  colnames(defense.wincount) <- c("Id", "defense.wincount")

  offense.playcount = data.frame(table(allPlays$offense))
  colnames(offense.playcount) <- c("Id", "offense.playcount")
  defense.playcount = data.frame(table(allPlays$defense))
  colnames(defense.playcount) <- c("Id", "defense.playcount")

  offense.scoretable <- merge(offense.wincount, offense.playcount, by.x = "Id", by.y = "Id", all.y = TRUE)
  offense.scoretable[is.na(offense.scoretable)] <- 0

  defense.scoretable <- merge(defense.wincount, defense.playcount, by.x = "Id", by.y = "Id", all.y = TRUE)
  defense.scoretable[is.na(defense.scoretable)] <- 0

  scoretable <- merge(offense.scoretable, defense.scoretable, by.x = "Id", by.y = "Id", all = TRUE)
  scoretable[is.na(scoretable)] <- 0

  winRateRelative <- data.frame(Id = scoretable$Id, offenseWinRate = scoretable$offense.wincount/scoretable$offense.playcount, defenseWinRate = scoretable$defense.wincount/scoretable$defense.playcount)
  winRateAbsolute <- data.frame(Id = scoretable$Id, offenseWinRate = scoretable$offense.wincount/nrow(games), defenseWinRate = scoretable$defense.wincount/nrow(games))
  playCount <- data.frame(Id = scoretable$Id, offensePlayCount = scoretable$offense.playcount, defensePlayCount = scoretable$defense.playcount)

  statsBundle <- list( winRateRelative = winRateRelative, winRateAbsolute = winRateAbsolute, playCount = playCount)

  return(statsBundle)

}
