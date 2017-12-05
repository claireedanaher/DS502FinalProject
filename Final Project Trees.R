library(tree)
library(randomForest)
library(adabag)
library(caret)
library(class)
set.seed(29)

df <- read.csv("DS502/Final Project/analysisDF.csv", header = TRUE)
df$remove[df$in_32 == 0 | df$in_33 == 0 | df$in_34 == 0 | df$in_34 == 0] <- 1
df <- df[df$remove == 0,]
keep <- c(11:60, 64)
df <- df[,keep]

df$money <- apply(df[,1:6], 1, sum)
df$health <- apply(df[,7:15], 1, sum)
df$housing <- apply(df[,16:27], 1, sum)
df$edu <- apply(df[,28:38], 1, sum)
df$org <- apply(df[,39:42], 1, sum)
df$motive <- apply(df[,43:50], 1, sum)
df <- df[,51:57]
df$improve <- as.factor(df$improve)

quarter <- sample(dim(df)[1], dim(df)[1]/4)
train <- df[-quarter,]
test <- df[quarter,]

tree <- tree(improve ~ ., data = train)
plot(tree)
text(tree)
pred <- predict(tree, test, type = "class")
confusionMatrix(pred, test$improve)["table"]

cv <- cv.tree(tree, FUN = prune.tree)
plot(cv$size, cv$dev, type = "l")

pruned <- prune.tree(tree, best = 5)
plot(pruned)
text(pruned)
pred_pruned <- predict(pruned, test, type = "class")
confusionMatrix(pred_pruned, test$improve)["table"]

bag <- randomForest(improve ~ ., data = train, mtry = 6)
plot(bag)
pred_bag <- predict(bag, test)
confusionMatrix(pred_bag, test$improve)["table"]
barplot(importance(bag)[,1], col = "skyblue", 
        main = "Variables relative importance (Bagging)")

rf <- randomForest(improve ~ ., data = train, mtry = 3)
plot(rf)
pred_rf <- predict(rf, test)
print(confusionMatrix(pred_bag, test$improve)["table"])
barplot(importance(rf)[,1], col = "skyblue", 
        main = "Variables relative importance (Random Forest)")

boost <- boosting(improve ~ ., data = train, coeflearn = 'Breiman')
pred_boost <- predict(boost, test)
pred_boost$confusion
importanceplot(boost)

boost <- boosting(improve ~ ., data = train, coeflearn = 'Freund')
pred_boost <- predict(boost, test)
pred_boost$confusion
importanceplot(boost)

boost <- boosting(improve ~ ., data = train, coeflearn = 'Zhu')
pred_boost <- predict(boost, test)
pred_boost$confusion
importanceplot(boost)

knn_pred = knn(train, test, train$improve, k = 1)
confusionMatrix(knn_pred, test$improve)