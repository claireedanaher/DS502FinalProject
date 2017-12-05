#SET WORKING DIRECTORY
setwd("C:/WPI/DS502/FinalProject/")

#IMPORT DATA
library(readxl)
library(readr)
library(ISLR)
library(MASS)
library(class)
library(FactoMineR)
library(ggplot2)

#prints variable names

data <- read_csv("analysisDF.csv")
DF=data.frame(data)
DF$remove[DF$in_32==0 | DF$in_33==0 |DF$in_34==0 | DF$in_34==0] <- 1
for(i in 11:61) DF[,i]=as.factor(DF[,i])
DF$improve=factor(DF$improve)
DF_subset=DF[DF$remove==0,c(2,11:60,64)]



set.seed(10)
train=sample(1:nrow(DF),nrow(DF)/2)
DF_train=DF_subset[train,]
DF_test=DF_subset[-train,]


cats = apply(DF_test[,-1], 2, function(x) nlevels(as.factor(x)))
cats

mca1=MCA(DF_test[,-1], graph=FALSE)


DF_eig=data.frame(mca1$eig)
DF_eig
test=DF_eig[order(-DF_eig$percentage.of.variance),]


ggplot(DF_eig, aes(percentage.of.variance)) + 
  geom_line(aes(y = val.3, colour = "Improve")) + 
  geom_line(aes(y = val.2, colour = "Not Improve"))+
  labs( 
    y="True Positives", 
    x="Probability Cutoff")



barplot(test$eigenvalue, test$cumulative.percentage.of.variance)

plot(mca1,
     invisible = c("var", "quali.sup"),
     cex = 0.8,                                    
     autoLab = "yes")


fviz_mca_var(mca1, choice = "mca.cor", 
             repel = TRUE, # Avoid text overlapping (slow)
             ggtheme = theme_minimal())



# data frame with variable coordinates
mca1_vars_df = data.frame(mca1$var$coord, Variable = rep(names(cats), cats))

# data frame with observation coordinates
mca1_obs_df = data.frame(mca1$ind$coord)

# plot of variable categories
ggplot(data=mca1_vars_df, 
       aes(x = Dim.1, y = Dim.2, label = rownames(mca1_vars_df))) +
  geom_hline(yintercept = 0, colour = "gray70") +
  geom_vline(xintercept = 0, colour = "gray70") +
  geom_text(aes(colour=Variable)) +
  ggtitle("MCA plot of variables using R package FactoMineR")

ggplot(data = mca1_obs_df, aes(x = Dim.1, y = Dim.2)) +
  geom_hline(yintercept = 0, colour = "gray70") +
  geom_vline(xintercept = 0, colour = "gray70") +
  geom_point(colour = "gray50", alpha = 0.7) +
  geom_density2d(colour = "gray80") +
  geom_text(data = mca1_vars_df, 
            aes(x = Dim.1, y = Dim.2, 
                label = rownames(mca1_vars_df), colour = Variable)) +
  ggtitle("MCA plot of variables using R package FactoMineR") +
  scale_colour_discrete(name = "Variable")



plot(mca1,
     invisible = c("var", "quali.sup", "quanti.sup"),
     cex = 0.8,                                    
     autoLab = "yes")

#######################################################################





