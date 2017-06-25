winrate <- function(){
  library(jsonlite)

  games <- fromJSON("http://foosball-results.herokuapp.com/api/games", flatten = TRUE)

  games <- games[games$blue.score != games$red.score,]

  blueWiners <- games[games$blue.score > games$red.score,]
  redWinners <- games[games$red.score > games$blue.score,]

  blueWiners <- data.frame(offense = blueWiners$blue.offense._id, defense = blueWiners$blue.defense._id)
  redWinners <- data.frame(offense = redWinners$red.offense._id, defense = redWinners$red.defense._id)

  winners <- rbind(blueWiners, redWinners)

  offense <- winners$offense
  defense <- winners$defense

  offense.relfreq = table(offense)/nrow(winners)
  defense.relfreq = table(defense)/nrow(winners)

  result <- cbind(offense.relfreq, defense.relfreq)

  result <- data.frame(result)

  result <- data.frame(Id = rownames(result), offenseWinRate = result$offense.relfreq, defenseWinRate = result$defense.relfreq)

  return(toJSON(result))

}

print(winrate())
