#SET WORKING DIRECTORY
setwd("C:/WPI/DS502/FinalProject/")

#IMPORT DATA
library(readxl)
library(readr)

ANON_WPIstoplight201617 <- read_csv("ANON_WPIstoplight201617.csv")
View(ANON_WPIstoplight201617)

DF=ANON_WPIstoplight201617[-c(2,3,5:10,12,13)]
names(DF)
attach(DF)
date <- as.Date(fecha, "%m/%d/%Y")

DF=data.frame(DF, date)
DF[c("baseline")][is.na(DF[c("baseline")])] = 0
DF[1:5,]



DF$ind_tot <- apply(DF[,c(3:53)], 1, sum)

date_ch=rep("",nrow(DF)) 
date_ch[DF$baseline =="1"]=DF$fecha[DF$baseline =="1"] 
DF$base_date=as.Date(date_ch,"%m/%d/%Y")
DF$base_score[DF$baseline =="1"]=DF$ind_tot[DF$baseline =="1"] 

date_ch=rep("",nrow(DF)) 
date_ch[DF$baseline =="0"]=DF$fecha[DF$baseline =="0"] 
DF$follow_date=as.Date(date_ch,"%m/%d/%Y")
DF$follow_score[DF$baseline =="0"]=DF$ind_tot[DF$baseline =="0"] 




pre_DF=subset(DF, baseline==1, select=-c(follow_date,follow_score))
keep=duplicated(pre_DF$anon_id, fromLast=TRUE)
pre_DF=data.frame(pre_DF,keep)
pre_DF=subset(pre_DF,keep==FALSE)


post_DF=subset(DF,baseline==0)
keep=duplicated(post_DF$anon_id, fromLast=FALSE)
post_DF=data.frame(post_DF,keep)
post_DF=subset(post_DF,keep==FALSE)


merge_DF=subset(post_DF, select=c(anon_id,follow_date,follow_score))

DF_raw=DF

DF=merge.data.frame(merge_DF,pre_DF,by="anon_id")
DF=DF[,c(1,110,111,2,3,7:56)]
DF$change=DF$follow_score-DF$base_score
DF$improve=0
DF$improve[DF$change>0]=1
DF=DF[,c(1:5,56:57,6:55)]
write.csv(DF,"analysisDF.csv")

DF$remove=0
DF$remove[DF$change<(-50)]=1
DF$remove[DF$change>50]=1

DF=subset(DF, remove==0)
boxplot(DF$change)







