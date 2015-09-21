train.data$Survived <- as.factor(train.data$Survived)
train.data$Pclass <- as.factor(train.data$Pclass)
train.data$SibSp <- as.factor(train.data$SibSp)
train.data$Cabin <- train.data$Cabin %>%
  substring(1, 1) %>%
  as.factor

train.data$Ticket <- train.data$Ticket %>%
  as.character %>% 
  gsub("\\D", "", .) %>% 
  as.numeric


fm <- Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Cabin + Embarked + Ticket

rf <- train(fm,
            data = train.data,
            method = "rf",
            trControl=trainControl(method="cv",number=5),
            importance = T, 
            ntree = 1000,
            verbose = T)

final.model <- rf$finalModel

predictor.importance <- varImp(final.model)
print(final.model)
plot(final.model)
varImpPlot(final.model)
