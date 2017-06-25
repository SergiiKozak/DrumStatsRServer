winrate <- function(){
  library(jsonlite)

  games <- fromJSON("http://foosball-results.herokuapp.com/api/games", flatten = TRUE)

  games <- games[games$blue.score != games$red.score]

  return(games)

}
