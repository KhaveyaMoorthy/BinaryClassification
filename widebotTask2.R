set.seed(123)
train<-read.csv("training.csv",header = T,sep=";")
#dim(train)
#View(train)
valid<-read.csv("validation.csv",header = T, sep=";")
train<-train[,c(2,3,9,14,15,16,18,19)]
#View(train)
valid<-valid[,c(2,3,9,14,15,16,18,19)]
#View(valid)
train<-na.omit(train)
#anyNA(train)
valid<-na.omit(valid)
# 
# lr<-glm(classLabel~.,data=train,family="binomial")
# 
# svm1<-svm(classLabel~.,train)
# random<-randomForest(classLabel~.,train)


ctrl <- trainControl(method="cv", 10)
set.seed(12358)
sms_model1 <- train(train, train$classLabel, method="nb",
                    trControl=ctrl)
sms_model1
sms_predict1 <- predict(sms_model1, valid)
cm1 <- confusionMatrix(sms_predict1, valid$classLabel)
cm1

# model<-naiveBayes(classLabel~.,train)
#  model
# 
# y_pred<-predict(model,valid$classLabel)
# y_pred
# y_pred<-ifelse(y_pred=="yes.",1,0)
# # y_pred
# cm<-table(y_pred,valid$classLabel)
# cm
# accuracy<-sum(diag(cm))/sum(cm)*100
# accuracy
