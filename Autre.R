df2 <- read.table("C:/Users/dorya/Documents/Cour M2 ESA/Projets/Projet Big Data/Application 1/Big_Data/kaggle.txt")
df2 <- df2[-1]
df2 <- na.omit(df2)
df2

library(dplyr)
library(randomForest)
library(pROC)

# Division train et test

smp_size <- floor(0.70 * nrow(df2))
set.seed(123)
train_df2 <- sample(seq_len(nrow(df2)), size = smp_size)

train <- df2[train_df2, ]
test <- df2[-train_df2, ]

train$sep <- "train"
test$sep <- "test"

# Camembert pour expliquer la division

Nb_train <- count(train, vars = sep)
Nb_test <- count(test, vars = sep)

df2_sep <- rbind(Nb_train,Nb_test)

df2_sep <- df2_sep %>% 
  rename(
    Class = vars,
    N = n)

library(ggplot2)

ggplot(df2_sep, aes(x="", y=N, fill=Class)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) +
  theme_void()

summary(df2$V2)
attach(df2)
summary(V2)

## Random forest

set.seed(123456)
classifier_rf <- randomForest(as.factor(V2)~.,data = train[,-12], mtry=3, importance=T)
print(classifier_rf)

set.seed(123456)
rf.pred <- predict(classifier_rf,newdata=test[,-12])
table(test$V2,rf.pred)

set.seed(123456)
rf.pred <- predict(classifier_rf,newdata=test[,-12])
a=mean(test$V2!=rf.pred)
err="Le taux d'erreur est : "
paste(err,round(a,4))

set.seed(123456)
rf.probs=predict(classifier_rf, newdata=test[,-12], type="prob")
roc(test$V2,as.vector(rf.probs[,2]),plot=TRUE,legacy.axes=TRUE, lwd=2, col="red",auc.polygon=TRUE,print.auc=TRUE,grid=TRUE)

## Elastic net ##

install.packages("caret")
library(caret)

train = train[,-12]
test = test[,-12]

set.seed(123)
model <- train(
  V2 ~., data = train, method = "glmnet",
  trControl = trainControl("cv", number = 10),
  tuneLength = 10)

model$bestTune

coef(model$finalModel, model$bestTune$lambda)

x.test <- model.matrix(V2 ~., test)[,-1]
predictions <- model %>% predict(x.test)

cnf <- confusion.glmnet(
  model,
  newx = NULL,
  newy,
  family = "binomial"
)

roc.glmnet(model, newx = x_test, newy = test)






