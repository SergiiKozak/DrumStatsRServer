filterGames <- function(games, cutoff.days, cutoff.games){
  library(lubridate)

  if(cutoff.days > 0){
    time.current <- Sys.time()
    time.required <- as.POSIXct(format(time.current, tz="Europe/Prague",usetz=TRUE), tz="Europe/Prague")
    start.date <- time.required - days(cutoff.days)
    games <- games[games$endDate >= start.date,]
  }

  if(cutoff.games > 0){
    games <- tail(games[with(games, order(endDate)), ], cutoff.games)
  }

  return(games)
}
