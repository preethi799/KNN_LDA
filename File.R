#Import Libraries:
library(caret)
library(pROC)
library(ggplot2)

#Import Dataset:
df<-read.table('newthyroid.txt', sep=',', header=TRUE)
sum(is.na(df)) #Check missing values

#Create AUC Vectors:
KNN<-c()
LDA<-c()

#Test-Train Split:
for (i in 1:10) {
  set.seed(i)
  trainIndex=createDataPartition(df$class,p=0.7,list=FALSE)
  train.feature=df[trainIndex,-1] ; train.label=df$class[trainIndex] #Train Data
  test.feature=df[-trainIndex,-1] ; test.label=df$class[-trainIndex] #Test Data

  #Set up Train Control:
  fitControl <- trainControl(method = "repeatedcv",number = 5, classProbs = TRUE, summaryFunction = twoClassSummary)

  #KNN:
  #Training Process:
  set.seed(i)
  knnFit=train(train.feature,train.label, method = "knn",
             trControl = fitControl,
             metric = "ROC",
             preProcess = c("center","scale"),
             tuneGrid=expand.grid(k=c(3, 5, 7, 9, 11, 13, 15)))
  
  #Predictions:
  knn.pred = predict(knnFit,test.feature, type="prob")
  knn.auc  = auc(test.label, knn.pred$h, levels=c("h", "n"))
  KNN<- c(KNN,knn.auc)
  
  #LDA:
  #Training Process:
  LdaFit=train(train.feature,train.label,
               method="lda",metric="ROC", 
               preProcess = c("center","scale"), trControl=trainControl(method = "none",classProbs = TRUE))
  
  #Predictions:
  Lda.pred = predict(LdaFit,test.feature, type="prob")
  Lda.auc  = auc(test.label, Lda.pred$h, levels=c("h", "n"))
  LDA<- c(LDA,Lda.auc)
  
}

#BOXPLOTS:
DF<-data.frame(AUC=c(KNN,LDA),Classifier=rep(c("KNN","LDA"),times=c(10,10))) #Combined Data frame
ggplot(DF, aes(x=Classifier, y=AUC, fill=Classifier)) + ggtitle('                           Classifier Performance Boxplot')+
  geom_boxplot(alpha=0.8) +
  theme(legend.position="none") +
  scale_fill_manual(values=c("deeppink4", "purple4"))


