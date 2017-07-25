getPlayers <- function(){
  players <- fromJSON("http://foosball-results.herokuapp.com/api/players", flatten = TRUE)
  players <- data.frame(Id = players$`_id`)

  return(players)
}
