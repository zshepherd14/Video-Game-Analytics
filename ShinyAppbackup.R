# Application title
#titlePanel("LoL Api Challenge 2017"),

# Sidebar with a slider input for number of bins 
#sidebarLayout(
#  sidebarPanel(
#    textInput("sum","Summoner Name"),
#    actionButton("pull","Pull Summoners Data (Give me a second)")
#  ),
  
  # Show a table 
#  mainPanel(
#    tabsetPanel(
#     tabPanel("Match History", tableOutput("table1")),
#      tabPanel("Rune Averages in your Games", tableOutput("table2")),
#      tabPanel("Champions you play Averages", tableOutput("table3"))
#   )
# )
#)
#)

#ShinAppBackup

library(shiny)
library(shinydashboard)

# Define UI for application that draws a histogram
ui <- dashboardPage(
  dashboardHeader(
    title="NA.2GG",
    titleWidth = 200
  ),
  dashboardSidebar(
    width=300,
    sidebarMenu(
      textInput("sum","Summoner Name"),
      actionButton("pull","Pull Summoners Data (Give it a minute)")
    )
  ),
  dashboardBody(tabsetPanel(
    tabPanel("Match History", tableOutput("table1")),
    tabPanel("Rune Averages in your Games", tableOutput("table2")),
    tabPanel("Champions you play Averages", tableOutput("table3")))
  )
)

# Define server logic required to make tables
server <- function(input, output) {
  observeEvent(input$pull,{
    summoner<- unlist(input$sum)
    key<-"RGAPI-b2100f06-6d6b-4411-834f-a8dbafb30482"
    findSummonerData<- function(summoner,key){
      #Clean library and load packages 
      library(jsonlite)
      library(scales)
      library(plyr)
      library(dplyr)
      library(reshape2)
      
      #find the given summoners ID
      sUrl<-tolower(summoner)
      sUrl<-gsub(" ","_",sUrl)
      sBody<-"https://na1.api.riotgames.com/lol/summoner/v3/summoners/by-name/"
      sMid<-"?api_key="
      sJson<-paste(sBody,sUrl,sMid,key,sep="")
      sData<-fromJSON(sJson)
      sId<-unlist(sData[2])
      
      #Create Champion Data frame
      cBody<-"https://na1.api.riotgames.com/lol/static-data/v3/champions?locale=en_US&dataById=false&api_key="
      jsonurl<-paste(cBody,key,sep="")
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
      
      #Find Summoner Matches 
      mBody<-"https://na1.api.riotgames.com/lol/match/v3/matchlists/by-account/"
      mJson<-paste(mBody,sId,sMid,key,sep="")
      mdata<-fromJSON(mJson)
      mdata<-mdata[1]
      mdata<-as.data.frame(mdata)
      colnames(mdata)<-c("Platform","gameId","championId","queue","season","timestamp","role","lane")
      mdata<-mdata[mdata$queue==440,]
      mdata$timestamp<-(as.POSIXct(as.numeric(as.character(mdata$timestamp))/1000,origin="1970-01-01",tz="America/Chicago"))
      mdata$Date<-as.Date(mdata$timestamp)
      
      #Loop through each match and query match data from API
      runes<-create_empty_table(0,4)
      colnames(runes)<-c("perk","val1","val2","val3")
      runes$perk<-as.integer(runes$perk)
      runes$val1<-as.integer(runes$val1)
      runes$val2<-as.integer(runes$val2)
      runes$val3<-as.integer(runes$val3)
      
      games<-as.list(mdata$gameId)
      rm(statsDf,teamsDf)
      i<-0
      for (id in games){
        idurl<-as.character(id)
        bodyUrl<-"https://na1.api.riotgames.com/lol/match/v3/matches/"
        jsonurl<-paste(bodyUrl,idurl,sMid,key,sep="")
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
        play$participantId<-c(1:nrow(play))
        rows<-play$summonerName==summoner
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
        stats$gameId<-id
        teams<-teams[teams$teamId==teamid,]
        teams$gameId<-id
        if (i ==0){
          statsDf<-stats
          teamsDf<-teams
          i<-i +1 
        } else { 
          statsDf<-plyr::rbind.fill(statsDf,stats)
          teamsDf<-plyr::rbind.fill(teamsDf,teams)
        }
        thisgame<-players$participants.stats
        thisgame<-thisgame[,c("perk0","perk0Var1","perk0Var2","perk0Var3","perk1","perk1Var1","perk1Var2","perk1Var3","perk2","perk2Var1","perk2Var2","perk2Var3"
                              ,"perk3","perk3Var1","perk3Var2","perk3Var3","perk4","perk4Var1","perk4Var2","perk4Var3","perk5","perk5Var1","perk5Var2","perk5Var3",
                              "participantId")]
        oldright<-0
        for (i in c(0:5)){
          right<-oldright+4
          left<-right-3
          tempdf<-thisgame[,c(left:right)]
          colnames(tempdf)<-c("perk","val1","val2","val3")
          runes<-rbind(runes,tempdf)
          oldright<-right
        }
      } 
      
      #merge data and create statistics 
      dfAll<-merge(statsDf,mdata, by = "gameId")
      dfAll<-merge(dfAll,teamsDf, by = "gameId")
      dfAll<-merge(dfAll,df1, by.x="championId",by.y="id")
      
      #kda and win as binary 
      dfAll$kda<-0
      dfAll$kda<-as.numeric(dfAll$kda)
      for (i in 1:nrow(dfAll)){
        if (dfAll$deaths[i]==0){
          dfAll$kda[i]<-dfAll$kills[i]+dfAll$assists[i]
        } else {
          dfAll$kda[i]<-as.numeric((dfAll$kills[i]+dfAll$assists[i])/dfAll$deaths[i])
        }
      }
      dfAll$win.x<-dfAll$win.x*1
      topct <- function(x) { as.numeric( sub("\\D*([0-9.]+)\\D*","\\1",x) )/100 }
      dfAll$KPN<-topct(dfAll$KP)
      
      
      #Summarize Rune Data and combine with lookup table 
      runes<-dplyr::summarize(dplyr::group_by(runes,runes$perk),val1Avg=mean(val1),va2Avg=mean(val2),val3Avg=mean(val3))
      colnames(runes)<-c("perk","val1Avg","val2Avg","val3Avg")
      url<-"http://www.json-generator.com/api/json/get/cfquvkyYte?indent=2"
      runelookup<-fromJSON(url)
      runes<-merge(runelookup,runes, by = "perk")
      colnames(runes)<-c("perk","PerkVal1","PerkName","PerkVal3","PerkVal2","val1Avg","val2Avg","val3Avg")
      runes<-runes[,c("PerkName","PerkVal1","val1Avg","PerkVal2","val2Avg","PerkVal3","val3Avg","perk")]
      
      #Remove unesscary columns from dfAll
      columns<-c("Name","gameId","win.x","kills","deaths","assists",
                 "KP","KPN","kda","totalDamageDealtToChampions","totalDamageTaken","goldEarned",
                 "perk0","perk0Var1","perk0Var2","perk0Var3","perk1","perk1Var1","perk1Var2","perk1Var3","perk2","perk2Var1","perk2Var2","perk2Var3"
                 ,"perk3","perk3Var1","perk3Var2","perk3Var3","perk4","perk4Var1","perk4Var2","perk4Var3","perk5","perk5Var1","perk5Var2","perk5Var3",
                 "perkPrimaryStyle","perkSubStyle")
      
      dfAll<-dfAll[,columns]
      
      #Sudo Mergo Runes with dfAll, have to do a lookup for all 6 runes 
      dfAll$perk0Name<-""
      dfAll$perk0var1Measure<-""
      dfAll$perk0var2Measure<-""
      dfAll$perk0var2Measure<-""
      dfAll$perk0Var1Avg<-0
      dfAll$perk0Var2Avg<-0
      dfAll$perk0Var3Avg<-0
      dfAll$perk1Name<-""
      dfAll$perk1var1Measure<-""
      dfAll$perk1var2Measure<-""
      dfAll$perk1var2Measure<-""
      dfAll$perk1Var1Avg<-0
      dfAll$perk1Var2Avg<-0
      dfAll$perk1Var3Avg<-0
      dfAll$perk2Name<-""
      dfAll$perk2var1Measure<-""
      dfAll$perk2var2Measure<-""
      dfAll$perk2var2Measure<-""
      dfAll$perk2Var1Avg<-0
      dfAll$perk2Var2Avg<-0
      dfAll$perk2Var3Avg<-0
      dfAll$perk3Name<-""
      dfAll$perk3var1Measure<-""
      dfAll$perk3var2Measure<-""
      dfAll$perk3var2Measure<-""
      dfAll$perk3Var1Avg<-0
      dfAll$perk3Var2Avg<-0
      dfAll$perk3Var3Avg<-0
      dfAll$perk4Name<-""
      dfAll$perk4var1Measure<-""
      dfAll$perk4var2Measure<-""
      dfAll$perk4var2Measure<-""
      dfAll$perk4Var1Avg<-0
      dfAll$perk4Var2Avg<-0
      dfAll$perk4Var3Avg<-0
      dfAll$perk5Name<-""
      dfAll$perk5var1Measure<-""
      dfAll$perk5var2Measure<-""
      dfAll$perk5var2Measure<-""
      dfAll$perk5Var1Avg<-0
      dfAll$perk5Var2Avg<-0
      dfAll$perk5Var3Avg<-0
      
      for (row in c(1:nrow(dfAll))){
        rune0<-dfAll[row,"perk0"]
        rune1<-dfAll[row,"perk1"]
        rune2<-dfAll[row,"perk2"]
        rune3<-dfAll[row,"perk3"]
        rune4<-dfAll[row,"perk4"]
        rune5<-dfAll[row,"perk5"]
        dfAll$perk0Name[row]<-as.character(runes[runes$perk==rune0,"PerkName"])
        dfAll$perk1Name[row]<-as.character(runes[runes$perk==rune1,"PerkName"])
        dfAll$perk2Name[row]<-as.character(runes[runes$perk==rune2,"PerkName"])
        dfAll$perk3Name[row]<-as.character(runes[runes$perk==rune3,"PerkName"])
        dfAll$perk4Name[row]<-as.character(runes[runes$perk==rune4,"PerkName"])
        dfAll$perk5Name[row]<-as.character(runes[runes$perk==rune5,"PerkName"])
        dfAll$perk0var1Measure[row]<-as.character(runes[runes$perk==rune0,"PerkVal1"])
        dfAll$perk0var2Measure[row]<-as.character(runes[runes$perk==rune0,"PerkVal2"])
        dfAll$perk0var3Measure[row]<-as.character(runes[runes$perk==rune0,"PerkVal3"])
        dfAll$perk1var1Measure[row]<-as.character(runes[runes$perk==rune1,"PerkVal1"])
        dfAll$perk1var2Measure[row]<-as.character(runes[runes$perk==rune1,"PerkVal2"])
        dfAll$perk1var3Measure[row]<-as.character(runes[runes$perk==rune1,"PerkVal3"])
        dfAll$perk2var1Measure[row]<-as.character(runes[runes$perk==rune2,"PerkVal1"])
        dfAll$perk2var2Measure[row]<-as.character(runes[runes$perk==rune2,"PerkVal2"])
        dfAll$perk2var3Measure[row]<-as.character(runes[runes$perk==rune2,"PerkVal3"])
        dfAll$perk3var1Measure[row]<-as.character(runes[runes$perk==rune3,"PerkVal1"])
        dfAll$perk3var2Measure[row]<-as.character(runes[runes$perk==rune3,"PerkVal2"])
        dfAll$perk3var3Measure[row]<-as.character(runes[runes$perk==rune3,"PerkVal3"])
        dfAll$perk4var1Measure[row]<-as.character(runes[runes$perk==rune4,"PerkVal1"])
        dfAll$perk4var2Measure[row]<-as.character(runes[runes$perk==rune4,"PerkVal2"])
        dfAll$perk4var3Measure[row]<-as.character(runes[runes$perk==rune4,"PerkVal3"])
        dfAll$perk5var1Measure[row]<-as.character(runes[runes$perk==rune5,"PerkVal1"])
        dfAll$perk5var2Measure[row]<-as.character(runes[runes$perk==rune5,"PerkVal2"])
        dfAll$perk5var3Measure[row]<-as.character(runes[runes$perk==rune5,"PerkVal3"])
        dfAll$perk0Var1Avg[row]<-as.integer(runes[runes$perk==rune0,"val1Avg"])
        dfAll$perk0Var2Avg[row]<-as.integer(runes[runes$perk==rune0,"val2Avg"])
        dfAll$perk0Var3Avg[row]<-as.integer(runes[runes$perk==rune0,"val3Avg"])
        dfAll$perk1Var1Avg[row]<-as.integer(runes[runes$perk==rune1,"val1Avg"])
        dfAll$perk1Var2Avg[row]<-as.integer(runes[runes$perk==rune1,"val2Avg"])
        dfAll$perk1Var3Avg[row]<-as.integer(runes[runes$perk==rune1,"val3Avg"])
        dfAll$perk2Var1Avg[row]<-as.integer(runes[runes$perk==rune2,"val1Avg"])
        dfAll$perk2Var2Avg[row]<-as.integer(runes[runes$perk==rune2,"val2Avg"])
        dfAll$perk2Var3Avg[row]<-as.integer(runes[runes$perk==rune2,"val3Avg"])
        dfAll$perk3Var1Avg[row]<-as.integer(runes[runes$perk==rune3,"val1Avg"])
        dfAll$perk3Var2Avg[row]<-as.integer(runes[runes$perk==rune3,"val2Avg"])
        dfAll$perk3Var3Avg[row]<-as.integer(runes[runes$perk==rune3,"val3Avg"])
        dfAll$perk4Var1Avg[row]<-as.integer(runes[runes$perk==rune4,"val1Avg"])
        dfAll$perk4Var2Avg[row]<-as.integer(runes[runes$perk==rune4,"val2Avg"])
        dfAll$perk4Var3Avg[row]<-as.integer(runes[runes$perk==rune4,"val3Avg"])
        dfAll$perk5Var1Avg[row]<-as.integer(runes[runes$perk==rune5,"val1Avg"])
        dfAll$perk5Var2Avg[row]<-as.integer(runes[runes$perk==rune5,"val2Avg"])
        dfAll$perk5Var3Avg[row]<-as.integer(runes[runes$perk==rune5,"val3Avg"])
      }
      
      #Find wheater rune values are below average per game 
      #If the perk vals are time based flip the expression 
      dfAll$perk0Below<-0
      dfAll$perk1Below<-0
      dfAll$perk2Below<-0
      dfAll$perk3Below<-0
      dfAll$perk4Below<-0
      dfAll$perk5Below<-0
      for (i in c(1:nrow(dfAll))){
        if(dfAll$perk0[i]==9103 || dfAll$perk0[i]==9103 || dfAll$perk0[i]==9103 || dfAll$perk0[i]==9103){
          if (dfAll$perk0Var1[i]>dfAll$perk0Var1Avg[i]){
            dfAll$perk0Below[i]<-1
          }
        }else {
          if (dfAll$perk0Var1[i]<dfAll$perk0Var1Avg[i] || dfAll$perk0Var2[i]<dfAll$perk0Var2Avg[i] || dfAll$perk0Var3[i]<dfAll$perk0Var3Avg[i]){
            dfAll$perk0Below[i]<-1
          }
        }
        if(dfAll$perk1[i]==9103 || dfAll$perk1[i]==9103 || dfAll$perk1[i]==9103 || dfAll$perk1[i]==9103){
          if (dfAll$perk1Var1[i]>dfAll$perk1Var1Avg[i]){
            dfAll$perk1Below[i]<-1
          }
        }else {
          if (dfAll$perk1Var1[i]<dfAll$perk1Var1Avg[i] || dfAll$perk1Var2[i]<dfAll$perk1Var2Avg[i] || dfAll$perk1Var3[i]<dfAll$perk1Var3Avg[i]){
            dfAll$perk1Below[i]<-1
          }
        }
        if(dfAll$perk2[i]==9103 || dfAll$perk2[i]==9103 || dfAll$perk2[i]==9103 || dfAll$perk2[i]==9103){
          if (dfAll$perk2Var1[i]>dfAll$perk2Var1Avg[i]){
            dfAll$perk2Below[i]<-1
          }
        }else {
          if (dfAll$perk2Var1[i]<dfAll$perk2Var1Avg[i] || dfAll$perk2Var2[i]<dfAll$perk2Var2Avg[i] || dfAll$perk2Var3[i]<dfAll$perk2Var3Avg[i]){
            dfAll$perk2Below[i]<-1
          }
        }
        if(dfAll$perk3[i]==9103 || dfAll$perk3[i]==9103 || dfAll$perk3[i]==9103 || dfAll$perk3[i]==9103){
          if (dfAll$perk3Var1[i]>dfAll$perk3Var1Avg[i]){
            dfAll$perk3Below[i]<-1
          }
        }else {
          if (dfAll$perk3Var1[i]<dfAll$perk3Var1Avg[i] || dfAll$perk3Var2[i]<dfAll$perk3Var2Avg[i] || dfAll$perk3Var3[i]<dfAll$perk3Var3Avg[i]){
            dfAll$perk3Below[i]<-1
          }
        }
        if(dfAll$perk4[i]==9103 || dfAll$perk4[i]==9103 || dfAll$perk4[i]==9103 || dfAll$perk4[i]==9103){
          if (dfAll$perk4Var1[i]>dfAll$perk4Var1Avg[i]){
            dfAll$perk4Below[i]<-1
          }
        }else {
          if (dfAll$perk4Var1[i]<dfAll$perk4Var1Avg[i] || dfAll$perk4Var2[i]<dfAll$perk4Var2Avg[i] || dfAll$perk4Var3[i]<dfAll$perk4Var3Avg[i]){
            dfAll$perk4Below[i]<-1
          }
        }
        if(dfAll$perk5[i]==9103 || dfAll$perk5[i]==9103 || dfAll$perk5[i]==9103 || dfAll$perk5[i]==9103){
          if (dfAll$perk5Var1[i]>dfAll$perk5Var1Avg[i]){
            dfAll$perk5Below[i]<-1
          }
        }else {
          if (dfAll$perk5Var1[i]<dfAll$perk5Var1Avg[i] || dfAll$perk5Var2[i]<dfAll$perk5Var2Avg[i] || dfAll$perk5Var3[i]<dfAll$perk5Var3Avg[i]){
            dfAll$perk5Below[i]<-1
          }
        }
      }
      
      #Set all of the Primary and Secondary Tree Names
      dfAll$PerkPrimary<-""
      dfAll$PerkSub<-""
      for (i in c(1:nrow(dfAll))){
        if (dfAll$perkPrimaryStyle[i] == 8000){
          dfAll$PerkPrimary[i]<-"Precision"
        }
        if (dfAll$perkPrimaryStyle[i] == 8200){
          dfAll$PerkPrimary[i]<-"Sorcery"
        }
        if (dfAll$perkPrimaryStyle[i] == 8100){
          dfAll$PerkPrimary[i]<-"Domination"
        }
        if (dfAll$perkPrimaryStyle[i] == 8400){
          dfAll$PerkPrimary[i]<-"Resolve"
        }
        if (dfAll$perkPrimaryStyle[i] == 8300){
          dfAll$PerkPrimary[i]<-"Inspiratoin"
        }
        if (dfAll$perkSubStyle[i] == 8000){
          dfAll$PerkSub[i]<-"Precision"
        }
        if (dfAll$perkSubStyle[i] == 8200){
          dfAll$PerkSub[i]<-"Sorcery"
        }
        if (dfAll$perkSubStyle[i] == 8100){
          dfAll$PerkSub[i]<-"Domination"
        }
        if (dfAll$perkSubStyle[i] == 8400){
          dfAll$PerkSub[i]<-"Resolve"
        }
        if (dfAll$perkSubStyle[i] == 8300){
          dfAll$PerkSub[i]<-"Inspiratoin"
        }
      }
      
      #ReOrder Dfall to a more useable format and remove games that are remakes, or too short to have any meaningful analysis 
      columns<-c("Name","gameId","win.x","kills","deaths","assists",
                 "KP","KPN","kda","totalDamageDealtToChampions","totalDamageTaken","goldEarned","PerkPrimary","PerkSub","perkPrimaryStyle","perkSubStyle",
                 "perk0Name","perk0Below","perk0var1Measure","perk0Var1","perk0Var1Avg","perk0var2Measure","perk0Var2","perk0Var2Avg","perk0var3Measure","perk0Var3","perk0Var3Avg",
                 "perk1Name","perk1Below","perk1var1Measure","perk1Var1","perk1Var1Avg","perk1var2Measure","perk1Var2","perk1Var2Avg","perk1var3Measure","perk1Var3","perk1Var3Avg",
                 "perk2Name","perk2Below","perk2var1Measure","perk2Var1","perk2Var1Avg","perk2var2Measure","perk2Var2","perk2Var2Avg","perk2var3Measure","perk2Var3","perk2Var3Avg",
                 "perk3Name","perk3Below","perk3var1Measure","perk3Var1","perk3Var1Avg","perk3var2Measure","perk3Var2","perk3Var2Avg","perk3var3Measure","perk3Var3","perk3Var3Avg",
                 "perk4Name","perk4Below","perk4var1Measure","perk4Var1","perk4Var1Avg","perk4var2Measure","perk4Var2","perk4Var2Avg","perk4var3Measure","perk4Var3","perk4Var3Avg",
                 "perk5Name","perk5Below","perk5var1Measure","perk5Var1","perk5Var1Avg","perk5var2Measure","perk5Var2","perk5Var2Avg","perk5var3Measure","perk5Var3","perk5Var3Avg",
                 "perk0","perk1","perk2","perk3","perk4","perk5")
      
      dfAll<-dfAll[,columns]
      dfAll<-dfAll[!is.na(dfAll$KPN),]
      dfAll<-dfAll[dfAll$KPN!=0,]
      colnames(dfAll)<-c("Champion","gameId","win.x","kills","deaths","assists",
                         "KP","KPN","kda","totalDamageDealtToChampions","totalDamageTaken","goldEarned","PerkPrimary","PerkSub","perkPrimaryStyle","perkSubStyle",
                         "perk0Name","perk0Below","perk0var1Measure","perk0Var1","perk0Var1Avg","perk0var2Measure","perk0Var2","perk0Var2Avg","perk0var3Measure","perk0Var3","perk0Var3Avg",
                         "perk1Name","perk1Below","perk1var1Measure","perk1Var1","perk1Var1Avg","perk1var2Measure","perk1Var2","perk1Var2Avg","perk1var3Measure","perk1Var3","perk1Var3Avg",
                         "perk2Name","perk2Below","perk2var1Measure","perk2Var1","perk2Var1Avg","perk2var2Measure","perk2Var2","perk2Var2Avg","perk2var3Measure","perk2Var3","perk2Var3Avg",
                         "perk3Name","perk3Below","perk3var1Measure","perk3Var1","perk3Var1Avg","perk3var2Measure","perk3Var2","perk3Var2Avg","perk3var3Measure","perk3Var3","perk3Var3Avg",
                         "perk4Name","perk4Below","perk4var1Measure","perk4Var1","perk4Var1Avg","perk4var2Measure","perk4Var2","perk4Var2Avg","perk4var3Measure","perk4Var3","perk4Var3Avg",
                         "perk5Name","perk5Below","perk5var1Measure","perk5Var1","perk5Var1Avg","perk5var2Measure","perk5Var2","perk5Var2Avg","perk5var3Measure","perk5Var3","perk5Var3Avg",
                         "perk0","perk1","perk2","perk3","perk4","perk5")
      
      #Summary Table by Champion and rune setup
      summaryTable<-dplyr::summarize(dplyr::group_by(dfAll,Champion,perk0Name,perk0var1Measure,perk0var2Measure,perk0var3Measure,perk0Var1Avg,perk0Var2Avg,perk0Var3Avg,
                                                     perk1Name,perk1var1Measure,perk1var2Measure,perk1var3Measure,perk1Var1Avg,perk1Var2Avg,perk1Var3Avg,
                                                     perk2Name,perk2var1Measure,perk2var2Measure,perk2var3Measure,perk2Var1Avg,perk2Var2Avg,perk2Var3Avg,
                                                     perk3Name,perk3var1Measure,perk3var2Measure,perk3var3Measure,perk3Var1Avg,perk3Var2Avg,perk3Var3Avg,
                                                     perk4Name,perk4var1Measure,perk4var2Measure,perk4var3Measure,perk4Var1Avg,perk4Var2Avg,perk4Var3Avg,
                                                     perk5Name,perk5var1Measure,perk5var2Measure,perk5var3Measure,perk5Var1Avg,perk5Var2Avg,perk5Var3Avg,
                                                     perk0,perk1,perk2,perk3,perk4,perk5),gamesplayed=n(),WinPercent=mean(win.x),
                                     KDA=(sum(kills)+sum(assists))/sum(deaths),KillParticipation=mean(KPN),AvgDmg=mean(totalDamageDealtToChampions),
                                     AvgDmgTaken=mean(totalDamageTaken),perk0Below=mean(perk0Below),perk1Below=mean(perk1Below),perk2Below=mean(perk2Below)
                                     ,perk3Below=mean(perk3Below),perk4Below=mean(perk4Below),perk5Below=mean(perk5Below),
                                     perk0var1Mean=mean(perk0Var1),perk0var2Mean=mean(perk0Var2),perk0var3Mean=mean(perk0Var3),
                                     perk1var1Mean=mean(perk1Var1),perk1var2Mean=mean(perk1Var2),perk1var3Mean=mean(perk1Var3),
                                     perk2var1Mean=mean(perk2Var1),perk2var2Mean=mean(perk2Var2),perk2var3Mean=mean(perk2Var3),
                                     perk3var1Mean=mean(perk3Var1),perk3var2Mean=mean(perk3Var2),perk3var3Mean=mean(perk3Var3),
                                     perk4var1Mean=mean(perk4Var1),perk4var2Mean=mean(perk4Var2),perk4var3Mean=mean(perk4Var3),
                                     perk5var1Mean=mean(perk5Var1),perk5var2Mean=mean(perk5Var2),perk5var3Mean=mean(perk5Var3))
      #ReOrdering and Renaming columns in summary table into a more meaningful way that should reduce the time moving this data in the javascript           
      columns<-c("Champion","gamesplayed","WinPercent","KDA","KillParticipation","AvgDmg","AvgDmgTaken",
                 "perk0Name","perk0Below","perk0var1Measure","perk0var1Mean","perk0Var1Avg","perk0var2Measure","perk0var2Mean","perk0Var2Avg","perk0var3Measure","perk0var3Mean","perk0Var3Avg",
                 "perk1Name","perk1Below","perk1var1Measure","perk1var1Mean","perk1Var1Avg","perk1var2Measure","perk1var2Mean","perk1Var2Avg","perk1var3Measure","perk1var3Mean","perk1Var3Avg",
                 "perk2Name","perk2Below","perk2var1Measure","perk2var1Mean","perk2Var1Avg","perk2var2Measure","perk2var2Mean","perk2Var2Avg","perk2var3Measure","perk2var3Mean","perk2Var3Avg",
                 "perk3Name","perk3Below","perk3var1Measure","perk3var1Mean","perk3Var1Avg","perk3var2Measure","perk3var2Mean","perk3Var2Avg","perk3var3Measure","perk3var3Mean","perk3Var3Avg",
                 "perk4Name","perk4Below","perk4var1Measure","perk4var1Mean","perk4Var1Avg","perk4var2Measure","perk4var2Mean","perk4Var2Avg","perk4var3Measure","perk4var3Mean","perk4Var3Avg",
                 "perk5Name","perk5Below","perk5var1Measure","perk5var1Mean","perk5Var1Avg","perk5var2Measure","perk5var2Mean","perk5Var2Avg","perk5var3Measure","perk5var3Mean","perk5Var3Avg",
                 "perk0","perk1","perk2","perk3","perk4","perk5")
      summaryTable<-summaryTable[,columns]
      colnames(summaryTable)<-c("Champion","gamesplayed","WinPercent","KDA","KillParticipation","AvgDmg","AvgDmgTaken",
                                "perk0Name","perk0Below","perk0var1Measure","perk0var1PlayerAvg","perk0Var1GameAvg","perk0var2Measure","perk0var2PlayerAvg","perk0Var2GameAvg","perk0var3Measure","perk0var3PlayerAvg","perk0Var3GameAvg",
                                "perk1Name","perk1Below","perk1var1Measure","perk1var1PlayerAvg","perk1Var1GameAvg","perk1var2Measure","perk1var2PlayerAvg","perk1Var2GameAvg","perk1var3Measure","perk1var3PlayerAvg","perk1Var3GameAvg",
                                "perk2Name","perk2Below","perk2var1Measure","perk2var1PlayerAvg","perk2Var1GameAvg","perk2var2Measure","perk2var2PlayerAvg","perk2Var2GameAvg","perk2var3Measure","perk2var3PlayerAvg","perk2Var3GameAvg",
                                "perk3Name","perk3Below","perk3var1Measure","perk3var1PlayerAvg","perk3Var1GameAvg","perk3var2Measure","perk3var2PlayerAvg","perk3Var2GameAvg","perk3var3Measure","perk3var3PlayerAvg","perk3Var3GameAvg",
                                "perk4Name","perk4Below","perk4var1Measure","perk4var1PlayerAvg","perk4Var1GameAvg","perk4var2Measure","perk4var2PlayerAvg","perk4Var2GameAvg","perk4var3Measure","perk4var3PlayerAvg","perk4Var3GameAvg",
                                "perk5Name","perk5Below","perk5var1Measure","perk5var1PlayerAvg","perk5Var1GameAvg","perk5var2Measure","perk5var2PlayerAvg","perk5Var2GameAvg","perk5var3Measure","perk5var3PlayerAvg","perk5Var3GameAvg",
                                "perk0","perk1","perk2","perk3","perk4","perk5")
      #Actual Edits to be made 
      dfAll$kda<-format(round(dfAll$kda,2),nsmall=2)
      dfAll$kd<-paste(dfAll$kills,"/",dfAll$deaths,"/",dfAll$assists,"   ",dfAll$kda,sep="")
      dfAll$Result<-ifelse(dfAll$win.x==1,"Win","Loss")
      dfAll$rune<-paste(
        paste(dfAll$PerkPrimary,"/",dfAll$PerkSub,sep=""),
        paste(dfAll$perk0Name,", ",dfAll$perk0var1Measure,": ",dfAll$perk0Var1,", ",dfAll$perk0var2Measure,": ",dfAll$perk0Var2,", ",dfAll$perk0var3Measure,": ",dfAll$perk0Var3,", ",sep=""),
        paste(dfAll$perk1Name,", ",dfAll$perk1var1Measure,": ",dfAll$perk1Var1,", ",dfAll$perk1var2Measure,": ",dfAll$perk1Var2,", ",dfAll$perk1var3Measure,": ",dfAll$perk1Var3,", ",sep=""),
        paste(dfAll$perk2Name,", ",dfAll$perk2var1Measure,": ",dfAll$perk2Var1,", ",dfAll$perk2var2Measure,": ",dfAll$perk2Var2,", ",dfAll$perk2var3Measure,": ",dfAll$perk2Var3,", ",sep=""),
        paste(dfAll$perk3Name,", ",dfAll$perk3var1Measure,": ",dfAll$perk3Var1,", ",dfAll$perk3var2Measure,": ",dfAll$perk3Var2,", ",dfAll$perk3var3Measure,": ",dfAll$perk3Var3,", ",sep=""),
        paste(dfAll$perk4Name,", ",dfAll$perk4var1Measure,": ",dfAll$perk4Var1,", ",dfAll$perk4var2Measure,": ",dfAll$perk4Var2,", ",dfAll$perk4var3Measure,": ",dfAll$perk4Var3,", ",sep=""),
        paste(dfAll$perk5Name,", ",dfAll$perk5var1Measure,": ",dfAll$perk5Var1,", ",dfAll$perk5var2Measure,": ",dfAll$perk5Var2,", ",dfAll$perk5var3Measure,": ",dfAll$perk5Var3,", ",sep=""),
        sep="\n"
      )
      dfAll<-dfAll[order(dfAll$gameId,decreasing = TRUE),]
      cols<-c("Result","Champion,","kd","KP","totalDamageDealtToChampions","totalDamageTaken","goldEarned","rune")
      cols<-c(90,1,89,7,10,11,12,91)
      dfAll<-dfAll[,cols]
      colnames(dfAll)<-c("Result","Champion","K/D/A","Kill Part.","Total Damage"," Total Damage Taken", "Gold", " Runes")
      
      summary<-summaryTable
      summary$WinPercent<-percent(summary$WinPercent)
      summary$KillPart<-percent(summary$KillParticipation)
      summary$KDA<-format(round(summary$KDA,2),nsmall=2)
      summary$AvgDmg<-format(round(summary$AvgDmg,0),nsmall=0)
      summary$AvgDmgTaken<-format(round(summary$AvgDmgTaken,0),nsmall=0)
      summary$perk0Below<-percent(summary$perk0Below)
      summary$perk1Below<-percent(summary$perk1Below)
      summary$perk2Below<-percent(summary$perk2Below)
      summary$perk3Below<-percent(summary$perk3Below)
      summary$perk4Below<-percent(summary$perk4Below)
      summary$perk5Below<-percent(summary$perk5Below)
      summary$rune<-paste(
        paste(summary$perk0Name," Below Average %: ",summary$perk0Below,", ",summary$perk0var1Measure," Your Avg:",summary$perk0var1PlayerAvg," Game Avg:",summary$perk0Var1GameAvg,
              ", ",summary$perk0var2Measure," Your Avg:",summary$perk0var2PlayerAvg," Game Avg:",summary$perk0Var2GameAvg,
              ", ",summary$perk0var3Measure," Your Avg:",summary$perk0var3PlayerAvg," Game Avg:",summary$perk0Var3GameAvg,sep=""
        ),
        paste(summary$perk1Name," Below Average %: ",summary$perk1Below,", ",summary$perk1var1Measure," Your Avg:",summary$perk1var1PlayerAvg," Game Avg:",summary$perk1Var1GameAvg,
              ", ",summary$perk1var2Measure," Your Avg:",summary$perk1var2PlayerAvg," Game Avg:",summary$perk1Var2GameAvg,
              ", ",summary$perk1var3Measure," Your Avg:",summary$perk1var3PlayerAvg," Game Avg:",summary$perk1Var3GameAvg,sep=""
        ),
        paste(summary$perk2Name," Below Average %: ",summary$perk2Below,", ",summary$perk2var1Measure," Your Avg:",summary$perk2var1PlayerAvg," Game Avg:",summary$perk2Var1GameAvg,
              ", ",summary$perk2var2Measure," Your Avg:",summary$perk2var2PlayerAvg," Game Avg:",summary$perk2Var2GameAvg,
              ", ",summary$perk2var3Measure," Your Avg:",summary$perk2var3PlayerAvg," Game Avg:",summary$perk2Var3GameAvg,sep=""
        ),
        paste(summary$perk3Name," Below Average %: ",summary$perk3Below,", ",summary$perk3var1Measure," Your Avg:",summary$perk3var1PlayerAvg," Game Avg:",summary$perk3Var1GameAvg,
              ", ",summary$perk3var2Measure," Your Avg:",summary$perk3var2PlayerAvg," Game Avg:",summary$perk3Var2GameAvg,
              ", ",summary$perk3var3Measure," Your Avg:",summary$perk3var3PlayerAvg," Game Avg:",summary$perk3Var3GameAvg,sep=""
        ),
        paste(summary$perk4Name," Below Average %: ",summary$perk4Below,", ",summary$perk4var1Measure," Your Avg:",summary$perk4var1PlayerAvg," Game Avg:",summary$perk4Var1GameAvg,
              ", ",summary$perk4var2Measure," Your Avg:",summary$perk4var2PlayerAvg," Game Avg:",summary$perk4Var2GameAvg,
              ", ",summary$perk4var3Measure," Your Avg:",summary$perk4var3PlayerAvg," Game Avg:",summary$perk4Var3GameAvg,sep=""
        ),
        paste(summary$perk5Name," Below Average %: ",summary$perk5Below,", ",summary$perk5var1Measure," Your Avg:",summary$perk5var1PlayerAvg," Game Avg:",summary$perk5Var1GameAvg,
              ", ",summary$perk5var2Measure," Your Avg:",summary$perk5var2PlayerAvg," Game Avg:",summary$perk5Var2GameAvg,
              ", ",summary$perk5var3Measure," Your Avg:",summary$perk5var3PlayerAvg," Game Avg:",summary$perk5Var3GameAvg,sep=""
        ),
        sep="\n"
      )
      
      cols<-c("Champion","gamesplayed","WinPercent","KDA","KillPart","AvgDmg","AvgDmgTaken","rune")
      summary<-summary[,cols]
      colnames(summary)<-c("Champion","Games Played","Win Percent","KDA","Kill Part", "Average Damage", "AVerage Damage Taken", "Runes")
      
      runes<-runes[order(runes$perk),]
      runes<-runes[,c(1:7)]
      colnames(runes)<-c("Rune","Measure_1","Average_of_Measure_1","Measure_2","Average_of_Measure_2","Measure_3","Average_of_Measure_3")
      runes<-runes[order(runes$Rune),]
      summonerData<-list(dfAll,runes,summary)
      print("Done")
      return(summonerData)
    }
    SummonerData<-findSummonerData(summoner,key)
    output$table1<-renderTable(SummonerData[1])
    output$table2<-renderTable(SummonerData[2])
    output$table3<-renderTable(SummonerData[3])
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

