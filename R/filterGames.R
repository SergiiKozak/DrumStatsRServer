filterGames <- function(games, cutoff.days, cutoff.games){
  library(lubridate)

  if(cutoff.days > 0){
    Sys.setenv(tz="Europe/Prague")
    start.date <- Sys.time() - days(cutoff.days)
    games <- games[games$endDate >= start.date,]
  }

  if(cutoff.games > 0){
    games <- tail(games[with(games, order(endDate)), ], cutoff.games)
  }

  return(games)
}
