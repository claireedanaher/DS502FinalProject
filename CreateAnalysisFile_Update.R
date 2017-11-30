#SET WORKING DIRECTORY
setwd("C:/WPI/DS502/FinalProject/")

#IMPORT DATA
library(readxl)
library(readr)
library(ggplot2) 
library("ggthemes")
library("scales")


ANON_WPIstoplight201617 <- read_csv("ANON_WPIstoplight201617.csv")
View(ANON_WPIstoplight201617)

DF=ANON_WPIstoplight201617[-c(6,7,9,12,13,64:114)]
names(DF)
attach(DF)
date <- as.Date(fecha, "%m/%d/%Y")

DF=data.frame(DF, date)
DF[c("baseline")][is.na(DF[c("baseline")])] = 0

DF$ind_tot <- apply(DF[,c(9:58)], 1, sum)

date_ch=rep("",nrow(DF)) 
date_ch[DF$baseline =="1"]=DF$fecha[DF$baseline =="1"] 
DF$base_date=as.Date(date_ch,"%m/%d/%Y")


date_ch=rep("",nrow(DF)) 
date_ch[DF$baseline =="0"]=DF$fecha[DF$baseline =="0"] 
DF$follow_date=as.Date(date_ch,"%m/%d/%Y")
DF$follow_score[DF$baseline =="0"]=DF$ind_tot[DF$baseline =="0"] 

colnames(DF)[60] <- "base_score"
pre_DF=subset(DF, baseline==1, select=-c(follow_date,follow_score))


keep=duplicated(pre_DF$anon_id, fromLast=TRUE)
pre_DF=data.frame(pre_DF,keep)
pre_DF=subset(pre_DF,keep==FALSE)

post_DF=subset(DF,baseline==0)
keep=duplicated(post_DF$anon_id, fromLast=FALSE)
post_DF=data.frame(post_DF,keep)
post_DF=subset(post_DF,keep==FALSE)
colnames(post_DF)[5] <- "follow_income"

post_DF=subset(post_DF, select=c(anon_id,follow_date,follow_score,follow_income))


DF=merge.data.frame(post_DF,pre_DF,by="anon_id")


DF=DF[,c(1:6,8:10,12:61,63:64)]

colnames(DF)[5] <- "advisor_id"
colnames(DF)[6] <- "branch_id"
colnames(DF)[7] <- "baseline_income"
colnames(DF)[8] <- "client_type"

DF$change=DF$follow_score-DF$base_score
DF$improve=0
DF$improve[DF$change>0]=1

DF$baseline_income=DF$baseline_income/1000
DF$follow_income=DF$follow_income/1000





######################################################################
# Create Summary Scatter Plots
# Scatterplot
theme_pander()
scale_fill_pander()
gg <- ggplot(DF, aes(base_score, follow_score)) + geom_jitter(width = .5, size=1) +
  labs(subtitle="Comparison of Score Scatterplot-Including Outliers", 
       y="Follow-up", 
       x="Baseline")

plot(gg)


######################################################################




DF$remove=0
DF$remove[DF$change<(-50)]=1
DF$remove[DF$change>50]=1
DF=subset(DF, remove==0)

######################################################################
# Create Summary Scatter Plots
# Scatterplot
theme_pander()
scale_fill_pander()
gg <- ggplot(DF, aes(base_score, follow_score)) + geom_jitter(width = .5, size=1) +
  labs(subtitle="Comparison of Score Scatterplot-Excluding Outliers", 
       y="Follow-up", 
       x="Baseline")

plot(gg)

######################################################################


######################################################################
# Create Summary Scatter Plots
# Scatterplot
theme_pander()
scale_fill_pander()
options(scipen=999)
gg <- ggplot(DF, aes(baseline_income, follow_score )) + geom_jitter(width = .5, size=1) +
  labs(subtitle="Income to Follow-Up Score", 
       y="Follow-Up", 
       x="Income")

plot(gg)

######################################################################

write.csv(DF,"analysisDF.csv")
