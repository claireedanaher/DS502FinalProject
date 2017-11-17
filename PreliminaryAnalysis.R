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

for(i in 7:57) DF[,i]=as.factor(DF[,i])
summary(DF)


set.seed(10)
train=sample(1:nrow(DF),nrow(DF)/2)
DF_train=DF[train,]
DF_test=DF[-train,]
DF$improve=factor(DF$improve)

#################LDA ALL VARIABELS#####################################
subdata_train=subset(DF_train,select=c(7:57))
subdata_test=subset(DF_test,select=c(7:57))
improve.test=subdata_test$improve

lda.fit=lda(improve~.,data=subdata_train)
summary(lda.fit)
lda.predict=predict(lda.fit,subdata_test)
lda.class=lda.predict$class
table(lda.class,improve.test)

mean(lda.class==improve.test)

#################  #MCA ###############################################


subdata_train=subset(DF_train,select=c(8:57))
subdata_test=subset(DF_test,select=c(8:57))


cats = apply(subdata_train, 2, function(x) nlevels(as.factor(x)))
cats
mca1=MCA(subdata_train, graph=FALSE)


DF_eig=data.frame(mca1$eig)
test=DF_eig[order(-DF_eig$percentage.of.variance),]
barplot(test$eigenvalue, test$cumulative.percentage.of.variance)


plot(mca1, 
     invisible = c("ind", "quali.sup", "quanti.sup"),
     cex = 0.8,
     autoLab = "yes",
     keepvar = c("in_29_1", "in_32_0")
     )

plot(mca1, 
     keepvar = c("in_29_1", "in_32_0")
)



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
  #geom_text(data = mca1_vars_df, 
            #aes(x = Dim.1, y = Dim.2, 
                #label = rownames(mca1_vars_df), colour = Variable)) +
  ggtitle("MCA plot of variables using R package FactoMineR") +
  scale_colour_discrete(name = "Variable")



plot(mca1,
     invisible = c("var", "quali.sup", "quanti.sup"),
     cex = 0.8,                                    
     autoLab = "yes")

#######################################################################





