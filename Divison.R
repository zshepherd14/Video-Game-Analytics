library(ggplot2)
library(dplyr)
library(reshape2)
shotDmg<-24000
create_empty_table <- function(num_rows, num_cols) {
  frame <- data.frame(matrix(NA, nrow = num_rows, ncol = num_cols))
  return(frame)
}
dfP<-create_empty_table(66,3)
dfS<-create_empty_table(66,3)
dfAll<-create_empty_table(0,3)
colnames(dfAll)<-c("Bullet","Dmg","Build")
colnames(dfP)<-c("Bullet","Dmg","Build")
colnames(dfS)<-c("Bullet","Dmg","Build")
dfP$Bullet<-as.numeric(dfP$Bullet)
dfP$Dmg<-as.numeric(dfP$Bullet)
dfP$Build<-"Predator"
dfS$Bullet<-as.numeric(dfS$Bullet)
dfS$Dmg<-as.numeric(dfS$Bullet)
dfS$Build<-"Stiker"
for (j in 1:1000){
  i<-0
  stacks<-0
  PtotalDmg<-0
  StotalDmg<-0
  accuarcy<-runif(1, 0.45, 0.65)
  while (i<=66){
    hit<-rbinom(1,1,accuarcy)
    if (hit==0){
      stacks<-stacks-1
    } 
    else{
      stacks<-stacks+2
      if(stacks<=100){
        StotalDmg<-StotalDmg+(shotDmg*(1+(stacks/100)))
      } else {
        StotalDmg<-StotalDmg+shotDmg*(2)
      }
      if ((i>=10 && i<=75)||i>85){
        PtotalDmg<-PtotalDmg+shotDmg+5076.92
      } else{
        PtotalDmg<-PtotalDmg+shotDmg*1.01
      }
    }
    dfP$Bullet[i]=i
    dfS$Bullet[i]=i
    dfP$Dmg[i]=PtotalDmg
    dfS$Dmg[i]=StotalDmg
    i=i+1
  }
  df<-rbind(dfP,dfS)
  dfAll<-rbind(dfAll,df)
}
df<-summarize(group_by(dfAll,Build,Bullet),avgDmg=mean(Dmg))
qplot(df$Bullet,df$avgDmg,geom="point",color=df$Build)

