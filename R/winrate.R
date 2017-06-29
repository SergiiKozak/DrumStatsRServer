winrate <- function(d){
  library(jsonlite)
  library(data.table)

  games <- fromJSON("http://foosball-results.herokuapp.com/api/games", flatten = TRUE)

  games <- games[games$blue.score != games$red.score,]

  dupes <- merge(games, games, by.x = c("blue.offense._id", "blue.defense._id", "red.offense._id", "red.defense._id", "blue.score", "red.score"), by.y = c("blue.offense._id", "blue.defense._id", "red.offense._id", "red.defense._id", "blue.score", "red.score"))
  dupes <- dupes[dupes$`_id.x` != dupes$`_id.y`,]
  dupes <- dupes[abs(difftime(strptime(dupes$endDate.x, format = '%Y-%m-%dT%H:%M:%OSZ'), strptime(dupes$endDate.y, format = '%Y-%m-%dT%H:%M:%OSZ'), units = c("mins"))) < 10,]
  dupes <- dupes[grep("^DrumStats", dupes$source.x),]

  games <- subset(games, !(games$`_id` %in% dupes$`_id.y`))


  blueWiners <- games[games$blue.score > games$red.score,]
  redWinners <- games[games$red.score > games$blue.score,]

  blueWiners <- data.frame(offense = blueWiners$blue.offense._id, defense = blueWiners$blue.defense._id)
  redWinners <- data.frame(offense = redWinners$red.offense._id, defense = redWinners$red.defense._id)

  winners <- rbind(blueWiners, redWinners)

  allPlays <- rbind(data.frame(offense = games$blue.offense._id, defense = games$blue.defense._id, ownscore = games$blue.score, totalscore = games$blue.score + games$red.score),
                    data.frame(offense = games$red.offense._id, defense = games$red.defense._id, ownscore = games$red.score, totalscore = games$blue.score + games$red.score))

  offense.wincount = data.frame(table(winners$offense))
  colnames(offense.wincount) <- c("Id", "offense.wincount")
  defense.wincount = data.frame(table(winners$defense))
  colnames(defense.wincount) <- c("Id", "defense.wincount")
  total.wincount = data.frame(Id = offense.wincount$Id, total.wincount = defense.wincount$defense.wincount + offense.wincount$offense.wincount)

  offense.playcount = data.frame(table(allPlays$offense))
  colnames(offense.playcount) <- c("Id", "offense.playcount")
  defense.playcount = data.frame(table(allPlays$defense))
  colnames(defense.playcount) <- c("Id", "defense.playcount")
  total.playcount = data.frame(Id = offense.playcount$Id, total.playcount = defense.playcount$defense.playcount + offense.playcount$offense.playcount)

  offense.goalcount = aggregate(data.frame(allPlays$ownscore, allPlays$totalscore), list(allPlays$offense), sum)
  colnames(offense.goalcount) <- c("Id", "offense.ownscore", "offense.totalscore")
  defense.goalcount = aggregate(data.frame(allPlays$ownscore, allPlays$totalscore), list(allPlays$defense), sum)
  colnames(defense.goalcount) <- c("Id", "defense.ownscore", "defense.totalscore")
  total.goalcount = data.frame(Id = offense.goalcount$Id,
                               total.ownscore = defense.goalcount$defense.ownscore + offense.goalcount$offense.ownscore,
                               total.totalscore = defense.goalcount$defense.totalscore + offense.goalcount$offense.totalscore)


  offense.scoretable <- merge(offense.wincount, offense.playcount, by.x = "Id", by.y = "Id", all.y = TRUE)
  offense.scoretable <- merge(offense.goalcount, offense.scoretable, by.x = "Id", by.y = "Id", all.y = TRUE)
  offense.scoretable[is.na(offense.scoretable)] <- 0

  defense.scoretable <- merge(defense.wincount, defense.playcount, by.x = "Id", by.y = "Id", all.y = TRUE)
  defense.scoretable <- merge(defense.goalcount, defense.scoretable, by.x = "Id", by.y = "Id", all.y = TRUE)
  defense.scoretable[is.na(defense.scoretable)] <- 0

  total.scoretable <- merge(total.wincount, total.playcount, by.x = "Id", by.y = "Id", all.y = TRUE)
  total.scoretable <- merge(total.goalcount, total.scoretable, by.x = "Id", by.y = "Id", all.y = TRUE)
  total.scoretable[is.na(total.scoretable)] <- 0

  scoretable <- merge(offense.scoretable, defense.scoretable, by.x = "Id", by.y = "Id", all = TRUE)
  scoretable <- merge(scoretable, total.scoretable, by.x = "Id", by.y = "Id", all = TRUE)
  scoretable[is.na(scoretable)] <- 0

  winRateRelative <- data.frame(Id = scoretable$Id,
                                offenseWinRate = scoretable$offense.wincount/scoretable$offense.playcount,
                                defenseWinRate = scoretable$defense.wincount/scoretable$defense.playcount,
                                totalWinRate = scoretable$total.wincount/scoretable$total.playcount)
  winRateAbsolute <- data.frame(Id = scoretable$Id,
                                offenseWinRate = scoretable$offense.wincount/nrow(games),
                                defenseWinRate = scoretable$defense.wincount/nrow(games),
                                totalWinRate = scoretable$total.wincount/nrow(games))
  playCount <- data.frame(Id = scoretable$Id,
                          offensePlayCount = scoretable$offense.playcount,
                          defensePlayCount = scoretable$defense.playcount,
                          totalPlayCount = scoretable$total.playcount)

  goalRate <- data.frame(Id = scoretable$Id,
                         offenseGoalRate = scoretable$offense.ownscore/scoretable$offense.totalscore,
                         defenseGoalRate = scoretable$defense.ownscore/scoretable$defense.totalscore,
                         totalGoalRate = scoretable$total.ownscore/scoretable$total.totalscore)

  statsBundle <- list( winRateRelative = winRateRelative, winRateAbsolute = winRateAbsolute, goalRate = goalRate, playCount = playCount)

  return(statsBundle)

}
