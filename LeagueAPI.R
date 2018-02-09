rm(list=ls())

library(jsonlite)
library(scales)
library(dplyr)
library(reshape2)


jsonurl<-"https://na1.api.riotgames.com/lol/summoner/v3/summoners/by-account/207238211?api_key=RGAPI-d6b153dd-af23-4c78-bac6-c1e516ab6aee"
locData <- fromJSON(jsonurl)

#lines 11 through 27 are challenger tier list

jsonurl<-"https://na1.api.riotgames.com/lol/league/v3/challengerleagues/by-queue/RANKED_SOLO_5x5?api_key=RGAPI-d6b153dd-af23-4c78-bac6-c1e516ab6aee"
locData <- fromJSON(jsonurl)
df<-locData[5]
df<-as.data.frame(df)
colnames(df)<-c("Id","PlayerName","leaguePoint","Rank","Wins","Loses","Veteran","inactive","freshBlood","hotStreak")
df<-df[order(df$leaguePoint,decreasing = TRUE),]
df$GamesPlayed<-df$Wins+df$Loses
df$WinPercent<-df$Wins/df$GamesPlayed
df$Rank<-1:nrow(df)
cor.test(df$WinPercent,df$Rank, method='pearson')
cor.test(df$GamesPlayed,df$Rank, method='pearson')
cor.test(df$Wins,df$Rank, method='pearson')=
AvgGame<-mean(df$GamesPlayed)
AvgPr<-mean(df$WinPercent)



jsonurl="https://na1.api.riotgames.com/lol/summoner/v3/summoners/by-name/Seraphs_Embrace?api_key=RGAPI-d6b153dd-af23-4c78-bac6-c1e516ab6aee"
locdata=fromJSON(jsonurl)
#lines 31 through 53 are creating a champion lookup
jsonurl<-"https://na1.api.riotgames.com/lol/static-data/v3/champions?locale=en_US&dataById=false&api_key=RGAPI-d6b153dd-af23-4c78-bac6-c1e516ab6aee"
locData<-fromJSON(jsonurl)
locData<-locData[1]
df<-as.data.frame(locData)
create_empty_table <- function(num_rows, num_cols) {
  frame <- data.frame(matrix(NA, nrow = num_rows, ncol = num_cols))
  return(frame)
}
df1<-create_empty_table(139,3)
colnames(df1)<-c("id","Name","Title")
df1$Name<-as.character(df1$Name)
df1$Title<-as.character(df1$Title)

oldright<-0
for (i in c(1:139)){
  right<-oldright+4
  left<-right-3
  df1$id[i]<-df[,c(left:right)][[1]]
  df1$Name[i]<-levels(df[,c(left:right)][[3]])
  df1$Title[i]<-levels(df[,c(left:right)][[4]])
  oldright<-right
}
df1<-df1[order(df1$Name),]


zachs=207238211
luke=49739319
#match numbers 
jsonurl<-"https://na1.api.riotgames.com/lol/match/v3/matchlists/by-account/207238211?api_key=RGAPI-d6b153dd-af23-4c78-bac6-c1e516ab6aee"
locdata<-fromJSON(jsonurl)
locdata<-locdata[1]
locdata<-as.data.frame(locdata)
colnames(locdata)<-c("Platform","gameId","championId","queue","season","timestamp","role","lane")
locdata<-locdata[locdata$queue==440,]
locdata<-locdata[,c("gameId","championId","timestamp")]
locdata$timestamp<-(as.POSIXct(as.numeric(as.character(locdata$timestamp))/1000,origin="1970-01-01",tz="America/Chicago"))
locdata$Date<-as.Date(locdata$timestamp)
mNums<-locdata

#finding the acutal game info from match numbers 
rn<-nrow(locdata)
df2<-create_empty_table(rn,20)
colnames(df2)<-c("gameId","team","win","firstBlood","firstTower","firstInhib","firstBaron","firstDragon",
                 "firstRift","towerKills","inhibKills","barons","dragons","DamageDealt","visionScore",
                 "wardsBought","kills","deaths","assists","KP")
df2$gameId<-as.integer(df2$gameId)
df2$team<-as.character(df2$team)
df2$win<-as.character(df2$win)
df2$firstBlood<-as.logical(df2$firstBlood)
df2$firstTower<-as.logical(df2$firstTower)
df2$firstInhib<-as.logical(df2$firstInhib)
df2$firstBaron<-as.logical(df2$firstBaron)
df2$firstDragon<-as.logical(df2$firstDragon)
df2$firstRift<-as.logical(df2$firstRift)
df2$towerKills<-as.integer(df2$towerKills)
df2$inhibKills<-as.integer(df2$inhibKills)
df2$barons<-as.integer(df2$barons)
df2$dragons<-as.integer(df2$dragons)
df2$DamageDealt<-as.integer(df2$DamageDealt)
df2$visionScore<-as.integer(df2$visionScore)
df2$wardsBought<-as.integer(df2$wardsBought)
df2$kills<-as.integer(df2$kills)
df2$deaths<-as.integer(df2$deaths)
df2$assists<-as.integer(df2$assists)
df2$KP<-as.character(df2$KP)
games<-as.list(locdata$gameId)
for (i in c(1:rn)){
  df2$gameId[i]<-games[i]
}
for (id in games){
  idurl<-as.character(id)
  bodyUrl<-"https://na1.api.riotgames.com/lol/match/v3/matches/"
  key<-"?api_key=RGAPI-d6b153dd-af23-4c78-bac6-c1e516ab6aee"
  jsonurl<-paste(bodyUrl,idurl,key,sep="")
  locdata<-fromJSON(jsonurl)
  teams<-locdata[11]
  teams<-teams[[1]]
  playerInfo<-locdata[13]
  playerInfo<-playerInfo[[1]]
  play<-playerInfo[[2]]
  
  players<-locdata[12]
  players<-as.data.frame(players)
  stats<-players[7]
  stats<-as.data.frame(stats)
  stats<-stats$participants.stats
  stats<-stats[,c("participantId","kills","deaths","assists","totalDamageDealtToChampions","visionScore","visionWardsBoughtInGame")]
  play$participantId<-c(1:nrow(play))
  rows<-play$summonerName=="zjsbears"
  ids<-as.list(play$participantId)
  pid<-ids[rows][1]
  pid<-as.numeric(pid)
  if (pid <6){
    teamid<-100
    team<-"blue"
    stats<-stats[stats$participantId==1 | stats$participantId==2 | stats$participantId==3 | stats$participantId==4 | stats$participantId==5,]
  } else {
    teamid<-200
    team<-"red"
    stats<-stats[stats$participantId==6 | stats$participantId==7 | stats$participantId==8 | stats$participantId==9 | stats$participantId==10,]
  }
  
  teamKills<-sum(stats$kills)
  stats$KP<-percent((stats$kills+stats$assists)/teamKills)
  stats<-stats[stats$participantId==pid,]
  
  
  gameinfo<-teams[teams$teamId==teamid,c(1:12)]
  rindex<-which(df2$gameId==id)
  df2$team[rindex]<-team
  df2$win[rindex]<-gameinfo$win[1]
  df2$firstBlood[rindex]<-gameinfo$firstBlood[1]
  df2$firstTower[rindex]<-gameinfo$firstTower[1]
  df2$firstInhib[rindex]<-gameinfo$firstInhibitor[1]
  df2$firstBaron[rindex]<-gameinfo$firstBaron[1]
  df2$firstDragon[rindex]<-gameinfo$firstDragon[1]
  df2$firstRift[rindex]<-gameinfo$firstRiftHerald[1]
  df2$towerKills[rindex]<-gameinfo$towerKills[1]
  df2$inhibKills[rindex]<-gameinfo$inhibitorKills[1]
  df2$barons[rindex]<-gameinfo$baronKills[1]
  df2$dragons[rindex]<-gameinfo$dragonKills[1]
  df2$DamageDealt[rindex]<-stats$totalDamageDealtToChampions[1]
  df2$visionScore[rindex]<-stats$visionScore[1]
  df2$wardsBought[rindex]<-stats$visionWardsBoughtInGame[1]
  df2$kills[rindex]<-stats$kills[1]
  df2$deaths[rindex]<-stats$deaths[1]
  df2$assists[rindex]<-stats$assists[1]
  df2$KP[rindex]<-stats$KP[1]
}  

dfAll<-merge(mNums,df2, by = "gameId")
dfAll<-merge(dfAll,df1,by.x = "championId",by.y = "id")
dfAll<-dfAll[,c("Date","team","win","firstBlood","firstTower","firstInhib","firstBaron","firstDragon","firstRift","towerKills","inhibKills","barons"
           ,"dragons","DamageDealt","visionScore","wardsBought","kills","deaths","assists","KP","Name")]
dfAll<-dfAll[dfAll$DamageDealt!=0,]
dfAll$firstBlood<-dfAll$firstBlood*1
dfAll$firstTower<-dfAll$firstTower*1
dfAll$firstInhib<-dfAll$firstInhib*1
dfAll$firstBaron<-dfAll$firstBaron*1
dfAll$firstDragon<-dfAll$firstDragon*1
dfAll$firstRift<-dfAll$firstRift*1

dfAll$WinN<-0
for (i in 1:nrow(dfAll)){
  if (dfAll$win[i] == "Win"){
    dfAll$WinN[i]<-1
  }
}

topct <- function(x) { as.numeric( sub("\\D*([0-9.]+)\\D*","\\1",x) )/100 }
dfAll$KPN<-topct(dfAll$KP)

dfAll$kda<-0
dfAll$kda<-as.numeric(dfAll$kda)
for (i in 1:nrow(dfAll)){
  if (dfAll$deaths[i]==0){
    dfAll$kda[i]<-dfAll$kills[i]+dfAll$assists[i]
  } else {
    dfAll$kda[i]<-as.numeric((dfAll$kills[i]+dfAll$assists[i])/dfAll$deaths[i])
  }
}
dfAll$Name<-as.factor(dfAll$Name)
averages<-summarize(group_by(dfAll,Name),GamesPlayed=n(),Win=mean(WinN),KDA=(sum(kills)+sum(assists))/sum(deaths),KP=mean(KPN),
                    DMG=mean(DamageDealt),Vision=mean(visionScore),Wards=mean(wardsBought),FirstBlood=mean(firstBlood)
                    ,FirstTower=mean(firstTower))
averages<-averages[order(averages$GamesPlayed,decreasing = TRUE),]
dfAll$highMMR<-0
for (i in 1:nrow(dfAll)){
  if (dfAll$team[i]=="red"){
    dfAll$highMMR[i]<-1
  }
}
View(averages)

cor.test(averages$Win,averages$KDA,method="pearson")[4]
cor.test(averages$Win,averages$GamesPlayed,method="pearson")[4]
cor.test(averages$Win,averages$KP,method="pearson")[4]
cor.test(averages$Win,averages$DMG,method="pearson")[4]
cor.test(averages$Win,averages$Vision,method="pearson")[4]
cor.test(averages$Win,averages$Wards,method="pearson")[4]
cor.test(averages$Win,averages$FirstBlood,method="pearson")[4]
cor.test(averages$Win,averages$FirstTower,method="pearson")[4]
#Verify all match data works, then merge the two data set and do a look up against champs to see what champ I played 







