winrate <- function(d){
  library(jsonlite)
  library(plyr)

  games <- getGames()

  players <- getPlayers()


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
  offense.wincount = merge(players, offense.wincount, by.x = "Id", by.y = "Id", all.x = TRUE)
  offense.wincount[is.na(offense.wincount)] <- 0

  defense.wincount = data.frame(table(winners$defense))
  colnames(defense.wincount) <- c("Id", "defense.wincount")
  defense.wincount = merge(players, defense.wincount, by.x = "Id", by.y = "Id", all.x = TRUE)
  defense.wincount[is.na(defense.wincount)] <- 0

  total.wincount = data.frame(Id = offense.wincount$Id, total.wincount = defense.wincount$defense.wincount + offense.wincount$offense.wincount)

  #Play Count

  offense.playcount = data.frame(table(allPlays$offense))
  colnames(offense.playcount) <- c("Id", "offense.playcount")
  offense.playcount = merge(players, offense.playcount, by.x = "Id", by.y = "Id", all.x = TRUE)
  offense.playcount[is.na(offense.playcount)] <- 0

  defense.playcount = data.frame(table(allPlays$defense))
  colnames(defense.playcount) <- c("Id", "defense.playcount")
  defense.playcount = merge(players, defense.playcount, by.x = "Id", by.y = "Id", all.x = TRUE)
  defense.playcount[is.na(defense.playcount)] <- 0

  total.playcount = data.frame(Id = offense.playcount$Id, total.playcount = defense.playcount$defense.playcount + offense.playcount$offense.playcount)

  #Goal Count

  offense.goalcount = aggregate(data.frame(allPlays$ownscore, allPlays$totalscore), list(allPlays$offense), sum)
  colnames(offense.goalcount) <- c("Id", "offense.ownscore", "offense.totalscore")
  offense.goalcount = merge(players, offense.goalcount, by.x = "Id", by.y = "Id", all.x = TRUE)
  offense.goalcount[is.na(offense.goalcount)] <- 0

  defense.goalcount = aggregate(data.frame(allPlays$ownscore, allPlays$totalscore), list(allPlays$defense), sum)
  colnames(defense.goalcount) <- c("Id", "defense.ownscore", "defense.totalscore")
  defense.goalcount = merge(players, defense.goalcount, by.x = "Id", by.y = "Id", all.x = TRUE)
  defense.goalcount[is.na(defense.goalcount)] <- 0

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

  winnerpairs <- ddply(winners.merged, .(offense, defense), summarize, x = length(enemy))
  bestpartner.offense <- ddply(winnerpairs, .(player = offense), summarize, wins = max(x)/2, partner = defense[which.max(x)])
  bestpartner.defense <- ddply(winnerpairs, .(player = defense), summarize, wins = max(x)/2, partner = offense[which.max(x)])
  bestpartner.total <- rbind(bestpartner.offense, bestpartner.defense)
  bestpartner.total <- ddply(bestpartner.total, .(player), summarize, wins.tot = max(wins), partner = partner[which.max(wins)])

  victimpairs.offense <- ddply(winners.merged, .(offense, enemy), summarize, x = length(defense))
  victimpairs.defense <- ddply(winners.merged, .(defense, enemy), summarize, x = length(offense))
  victim.offense <- ddply(victimpairs.offense, .(player = offense), summarize, wins = max(x), victim = enemy[which.max(x)])
  victim.defense <- ddply(victimpairs.defense, .(player = defense), summarize, wins = max(x), victim = enemy[which.max(x)])
  victim.total <- rbind(victim.offense, victim.defense)
  victim.total <- ddply(victim.total, .(player), summarize, wins.tot = max(wins), victim = victim[which.max(wins)])

  losers.merged <- rbind(data.frame(offense = winners$enemy.offense, defense = winners$enemy.defense, enemy = winners$offense),
                          data.frame(offense = winners$enemy.offense, defense = winners$enemy.defense, enemy = winners$defense))

  loserpairs <- ddply(losers.merged, .(offense, defense), summarize, x = length(enemy))
  worstpartner.offense <- ddply(loserpairs, .(player = offense), summarize, fails = max(x)/2, partner = defense[which.max(x)])
  worstpartner.defense <- ddply(loserpairs, .(player = defense), summarize, fails = max(x)/2, partner = offense[which.max(x)])
  worstpartner.total <- rbind(worstpartner.offense, worstpartner.defense)
  worstpartner.total <- ddply(worstpartner.total, .(player), summarize, fails.tot = max(fails), partner = partner[which.max(fails)])

  nemesispairs.offense <- ddply(losers.merged, .(offense, enemy), summarize, x = length(defense))
  nemesispairs.defense <- ddply(losers.merged, .(defense, enemy), summarize, x = length(offense))
  nemesis.offense <- ddply(nemesispairs.offense, .(player = offense), summarize, fails = max(x), nemesis = enemy[which.max(x)])
  nemesis.defense <- ddply(nemesispairs.defense, .(player = defense), summarize, fails = max(x), nemesis = enemy[which.max(x)])
  nemesis.total <- rbind(nemesis.offense, nemesis.defense)
  nemesis.total <- ddply(nemesis.total, .(player), summarize, fails.tot = max(fails), nemesis = nemesis[which.max(fails)])

  bestPartner <- data.frame(Id = bestpartner.offense$player,
                            offenseId = bestpartner.offense$partner,
                            defenseId = bestpartner.defense$partner,
                            totalId = bestpartner.total$partner)

  worstPartner <- data.frame(Id = worstpartner.offense$player,
                            offenseId = worstpartner.offense$partner,
                            defenseId = worstpartner.defense$partner,
                            totalId = worstpartner.total$partner)

  victim <- data.frame(Id = victim.offense$player,
                             offenseId = victim.offense$victim,
                             defenseId = victim.defense$victim,
                             totalId = victim.total$victim)

  nemesis <- data.frame(Id = nemesis.offense$player,
                       offenseId = nemesis.offense$nemesis,
                       defenseId = nemesis.defense$nemesis,
                       totalId = nemesis.total$nemesis)



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
