statsbundle <- function(ticker, cutoff.days, cutoff.games){
  library(jsonlite)
  library(plyr)
  library(dplyr)

  Sys.setenv(TZ='Europe/Prague')

  games <- getGames()
  games <- filterGames(games, cutoff.days, cutoff.games)

  if(nrow(games) == 0){
    return(NULL)
  }

  players <- getPlayers()

  matchOnPlayers <- function(data, matchColumn = "Id"){

    replaceNa <- function(x){
      if(is.character(x)||is.factor(x)){
        x[is.na(x)] <- ""
      }
      else if (is.numeric(x)){
        x[is.na(x)] <- 0
      }
      return(x)
    }

    data %>% mutate_if(is.factor, as.character) -> data

    result <- merge(data, players, by.x = matchColumn, by.y = "Id", all.y = TRUE)

    result <- colwise(replaceNa)(result)

    return(result)
  }


  blueWiners <- games[games$blue.score > games$red.score,]
  redWinners <- games[games$red.score > games$blue.score,]

  blueWiners <- data.frame(offense = blueWiners$blue.offense._id, defense = blueWiners$blue.defense._id, enemy.offense = blueWiners$red.offense._id, enemy.defense = blueWiners$red.defense._id)
  redWinners <- data.frame(offense = redWinners$red.offense._id, defense = redWinners$red.defense._id, enemy.offense = redWinners$blue.offense._id, enemy.defense = redWinners$blue.defense._id)

  winners <- rbind(blueWiners, redWinners)

  allPlays <- rbind(data.frame(offense = games$blue.offense._id, defense = games$blue.defense._id, ownscore = games$blue.score, totalscore = games$blue.score + games$red.score),
                    data.frame(offense = games$red.offense._id, defense = games$red.defense._id, ownscore = games$red.score, totalscore = games$blue.score + games$red.score))


  #Win Count

  offense.wincount = data.frame(table(winners$offense))
  colnames(offense.wincount) <- c("Id", "offense.wincount")
  offense.wincount = matchOnPlayers(offense.wincount)

  defense.wincount = data.frame(table(winners$defense))
  colnames(defense.wincount) <- c("Id", "defense.wincount")
  defense.wincount = matchOnPlayers(defense.wincount)

  total.wincount = data.frame(Id = offense.wincount$Id, total.wincount = defense.wincount$defense.wincount + offense.wincount$offense.wincount)

  #Play Count

  offense.playcount = data.frame(table(allPlays$offense))
  colnames(offense.playcount) <- c("Id", "offense.playcount")
  offense.playcount = matchOnPlayers(offense.playcount)

  defense.playcount = data.frame(table(allPlays$defense))
  colnames(defense.playcount) <- c("Id", "defense.playcount")
  defense.playcount = matchOnPlayers(defense.playcount)

  total.playcount = data.frame(Id = offense.playcount$Id, total.playcount = defense.playcount$defense.playcount + offense.playcount$offense.playcount)

  #Goal Count

  offense.goalcount = aggregate(data.frame(allPlays$ownscore, allPlays$totalscore), list(allPlays$offense), sum)
  colnames(offense.goalcount) <- c("Id", "offense.ownscore", "offense.totalscore")
  offense.goalcount = matchOnPlayers(offense.goalcount)

  defense.goalcount = aggregate(data.frame(allPlays$ownscore, allPlays$totalscore), list(allPlays$defense), sum)
  colnames(defense.goalcount) <- c("Id", "defense.ownscore", "defense.totalscore")
  defense.goalcount = matchOnPlayers(defense.goalcount)

  total.goalcount = data.frame(Id = offense.goalcount$Id,
                               total.ownscore = defense.goalcount$defense.ownscore + offense.goalcount$offense.ownscore,
                               total.totalscore = defense.goalcount$defense.totalscore + offense.goalcount$offense.totalscore)


  #Score Table

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
                                offenseRate = scoretable$offense.wincount/scoretable$offense.playcount,
                                defenseRate = scoretable$defense.wincount/scoretable$defense.playcount,
                                totalRate = scoretable$total.wincount/scoretable$total.playcount)
  winRateRelative[is.na(winRateRelative)] <- 0

  winRateAbsolute <- data.frame(Id = scoretable$Id,
                                offenseRate = scoretable$offense.wincount/nrow(games),
                                defenseRate = scoretable$defense.wincount/nrow(games),
                                totalRate = scoretable$total.wincount/nrow(games))
  winRateAbsolute[is.na(winRateAbsolute)] <- 0

  playCount <- data.frame(Id = scoretable$Id,
                          offensePlayCount = scoretable$offense.playcount,
                          defensePlayCount = scoretable$defense.playcount,
                          totalPlayCount = scoretable$total.playcount)

  goalRate <- data.frame(Id = scoretable$Id,
                         offenseRate = scoretable$offense.ownscore/scoretable$offense.totalscore,
                         defenseRate = scoretable$defense.ownscore/scoretable$defense.totalscore,
                         totalRate = scoretable$total.ownscore/scoretable$total.totalscore)
  goalRate[is.na(goalRate)] <- 0

  #Personalities


  winners.merged <- rbind(data.frame(offense = winners$offense, defense = winners$defense, enemy = winners$enemy.offense),
                          data.frame(offense = winners$offense, defense = winners$defense, enemy = winners$enemy.defense))

  losers.merged <- rbind(data.frame(offense = winners$enemy.offense, defense = winners$enemy.defense, enemy = winners$offense),
                         data.frame(offense = winners$enemy.offense, defense = winners$enemy.defense, enemy = winners$defense))

  winnerpairs <- ddply(winners.merged, .(offense, defense), summarize, x = length(enemy)/2)

  loserpairs <- ddply(losers.merged, .(offense, defense), summarize, x = length(enemy)/2)

  partners <- merge(winnerpairs, loserpairs, by.x = c("offense", "defense"), by.y = c("offense", "defense"), all = TRUE)
  partners[is.na(partners)] <- 0
  partners <- data.frame(offense = partners$offense, defense = partners$defense, winrate = partners$x.x/(partners$x.x + partners$x.y))

  bestpartner.offense <- ddply(partners, .(player = offense), summarize, rate = max(winrate), partner = defense[which.max(winrate)])
  bestpartner.offense <- matchOnPlayers(bestpartner.offense, "player")
  bestpartner.defense <- ddply(partners, .(player = defense), summarize, rate = max(winrate), partner = offense[which.max(winrate)])
  bestpartner.defense <- matchOnPlayers(bestpartner.defense, "player")
  bestpartner.total <- rbind(bestpartner.offense, bestpartner.defense)
  bestpartner.total <- ddply(bestpartner.total, .(player), summarize, rate.tot = max(rate), partner = partner[which.max(rate)])

  bestpartner.offense[bestpartner.offense$rate == 0, "partner"] <- ""
  bestpartner.defense[bestpartner.defense$rate == 0, "partner"] <- ""
  bestpartner.total[bestpartner.total$rate == 0, "partner"] <- ""


  worstpartner.offense <- ddply(partners, .(player = offense), summarize, rate = min(winrate), partner = defense[which.min(winrate)])
  worstpartner.offense <- matchOnPlayers(worstpartner.offense, "player")
  worstpartner.defense <- ddply(partners, .(player = defense), summarize, rate = min(winrate), partner = offense[which.min(winrate)])
  worstpartner.defense <- matchOnPlayers(worstpartner.defense, "player")
  worstpartner.total <- rbind(worstpartner.offense, worstpartner.defense)
  worstpartner.total <- ddply(worstpartner.total, .(player), summarize, rate.tot = min(rate), partner = partner[which.min(rate)])

  worstpartner.offense[worstpartner.offense$rate == 1, "partner"] <- ""
  worstpartner.defense[worstpartner.defense$rate == 1, "partner"] <- ""
  worstpartner.total[worstpartner.total$rate == 1, "partner"] <- ""

  victimpairs.offense <- ddply(winners.merged, .(offense, enemy), summarize, x = length(defense))
  victimpairs.defense <- ddply(winners.merged, .(defense, enemy), summarize, x = length(offense))

  nemesispairs.offense <- ddply(losers.merged, .(offense, enemy), summarize, x = length(defense))
  nemesispairs.defense <- ddply(losers.merged, .(defense, enemy), summarize, x = length(offense))

  enemies.offense <- merge(victimpairs.offense, nemesispairs.offense, by.x = c("offense", "enemy"), by.y = c("offense", "enemy"), all = TRUE)
  enemies.offense[is.na(enemies.offense)] <- 0
  enemies.offense <- data.frame(offense = enemies.offense$offense, enemy = enemies.offense$enemy, winrate = enemies.offense$x.x/(enemies.offense$x.x + enemies.offense$x.y))

  enemies.defense <- merge(victimpairs.defense, nemesispairs.defense, by.x = c("defense", "enemy"), by.y = c("defense", "enemy"), all = TRUE)
  enemies.defense[is.na(enemies.defense)] <- 0
  enemies.defense <- data.frame(defense = enemies.defense$defense, enemy = enemies.defense$enemy, winrate = enemies.defense$x.x/(enemies.defense$x.x + enemies.defense$x.y))

  victim.offense <- ddply(enemies.offense, .(player = offense), summarize, rate = max(winrate), enemy = enemy[which.max(winrate)])
  victim.offense <- matchOnPlayers(victim.offense, "player")
  victim.defense <- ddply(enemies.defense, .(player = defense), summarize, rate = max(winrate), enemy = enemy[which.max(winrate)])
  victim.defense <- matchOnPlayers(victim.defense, "player")
  victim.total <- rbind(victim.offense, victim.defense)
  victim.total <- ddply(victim.total, .(player), summarize, rate.tot = max(rate), enemy = enemy[which.max(rate)])

  victim.offense[victim.offense$rate == 0, "enemy"] <- ""
  victim.defense[victim.defense$rate == 0, "enemy"] <- ""
  victim.total[victim.total$rate == 0, "enemy"] <- ""

  nemesis.offense <- ddply(enemies.offense, .(player = offense), summarize, rate = min(winrate), enemy = enemy[which.min(winrate)])
  nemesis.offense <- matchOnPlayers(nemesis.offense, "player")
  nemesis.defense <- ddply(enemies.defense, .(player = defense), summarize, rate = min(winrate), enemy = enemy[which.min(winrate)])
  nemesis.defense <- matchOnPlayers(nemesis.defense, "player")
  nemesis.total <- rbind(nemesis.offense, nemesis.defense)
  nemesis.total <- ddply(nemesis.total, .(player), summarize, rate.tot = min(rate), enemy = enemy[which.min(rate)])

  nemesis.offense[nemesis.offense$rate == 1, "enemy"] <- ""
  nemesis.defense[nemesis.defense$rate == 1, "enemy"] <- ""
  nemesis.total[nemesis.total$rate == 1, "enemy"] <- ""


  bestPartner <- data.frame(Id = bestpartner.offense$player,
                            offenseId = bestpartner.offense$partner,
                            defenseId = bestpartner.defense$partner,
                            totalId = bestpartner.total$partner)

  worstPartner <- data.frame(Id = worstpartner.offense$player,
                             offenseId = worstpartner.offense$partner,
                             defenseId = worstpartner.defense$partner,
                             totalId = worstpartner.total$partner)

  victim <- data.frame(Id = victim.offense$player,
                       offenseId = victim.offense$enemy,
                       defenseId = victim.defense$enemy,
                       totalId = victim.total$enemy)

  nemesis <- data.frame(Id = nemesis.offense$player,
                        offenseId = nemesis.offense$enemy,
                        defenseId = nemesis.defense$enemy,
                        totalId = nemesis.total$enemy)



  statsBundle <- list( winRateRelative = winRateRelative,
                       winRateAbsolute = winRateAbsolute,
                       goalRate = goalRate,
                       playCount = playCount,
                       bestPartner = bestPartner,
                       worstPartner = worstPartner,
                       victim = victim,
                       nemesis = nemesis)

  return(statsBundle)

}
