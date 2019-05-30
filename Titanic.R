train <- read.csv('e:/kaggle/Titanic/train.csv')
test <- read.csv('e:/kaggle/Titanic/test.csv')
 test$Survived <- NA
 titanic <- rbind(train, test)
 lapply(full, function(x) length(unique(x))) 
 missing_values <- titanic %>% summarize_all(funs(sum(is.na(.))/n()))
 mice_mod<-mice(titanic[,c(6,10)],method = "rf")
 mice_output<-complete(mice_mod)
 titanic$Age<-mice_output$Age
 titanic$Fare<-mice_output$Fare
        titanic$title[titanic$title == ' Mlle']        <- 'Miss' 

titanic$title[titanic$title == ' Ms']          <- 'Miss'

titanic$title[titanic$title == ' Mme']         <- 'Mrs' 

titanic$title[titanic$title == ' Lady']          <- 'Miss'

titanic$title[titanic$title == ' Dona']          <- 'Miss'
titanic$title[titanic$title == 'Capt']        <- 'Officer' 

titanic$title[titanic$title == 'Col']        <- 'Officer' 

titanic$title[titanic$title == 'Major']   <- 'Officer'

titanic$title[titanic$title == 'Dr']   <- 'Officer'

titanic$title[titanic$title == 'Rev']   <- 'Officer'

titanic$title[titanic$title == 'Don']   <- 'Officer'

titanic$title[titanic$title == 'Sir']   <- 'Officer'

titanic$title[titanic$title == 'the Countess']   <- 'Officer'

titanic$title[titanic$title == 'Jonkheer']   <- 'Officer'  
ticket.unique <- rep(0, nrow(full))

tickets <- unique(titanic$Ticket)



for (i in 1:length(tickets)) {
  
  current.ticket <- tickets[i]
  
  party.indexes <- which(titanic$Ticket == current.ticket)
  
  
  for (k in 1:length(party.indexes)) {
    
    ticket.unique[party.indexes[k]] <- length(party.indexes)
    
  }
  
}



titanic$ticket.unique <- ticket.unique

titanic$ticket.size[titanic$ticket.unique == 1]   <- 'Single'

titanic$ticket.size[titanic$ticket.unique < 5 &titanic$ticket.unique>= 2]   <- 'Small'

titanic$ticket.size[titanic$ticket.unique >= 5]   <- 'Big'
 titanic <- titanic %>%
  mutate(
   Age = ifelse(is.na(Age), mean(full$Age, na.rm=TRUE), Age),
 `AgeGroup` = case_when(Age < 13 ~ "Age.0012", 
Age >= 13 & Age < 18 ~ "Age.1317",
     Age >= 18 & Age < 60 ~ "Age.1859",
Age >= 60 ~ "Age.60Ov"))
title <-  gsub("^.*, (.*?)\\..*$", "\\1", names)
titanic$FamilySize <-titanic$SibSp + titanic$Parch + 1 

titanic$FamilySized[titanic$FamilySize == 1] <- 'Single' 

titanic$FamilySized[titanic$FamilySize < 5 & titanic$FamilySize >= 2] <- 'Small' 

titanic$FamilySized[titanic$FamilySize >= 5] <- 'Big' 

titanic$FamilySized=as.factor(titanic$FamilySized)
cor.titanic<-cor(titanic[1:891,c(3,6,10,15,17)])
library(vcd)
titanic.mosaic<-titanic[1:891,]%>%select(Survived,Pclass,Sex)%>%mutate_all(as.factor )
mosaic(~Pclass+Sex+Survived,data=titanic.mosaic,shade=TRUE,legend=TRUE)
library(alluvial)
titanic.alluval<-titanic[1:891,]%>%group_by(Survived,Pclass,Sex,AgeGroup)%>%summarise(N=n())%>%ungroup%>%na.omit
alluvial(titanic.alluval,freq = titanic.alluval$N,border = NA)
alluvial(titanic.alluval,freq = titanic.alluval$N,border = NA,col = ifelse(titanic.alluval$Survived==1,"blue","red"))
library(RWeka)
tit.oner<-OneR(Survived~Pclass+Sex+Age+Embarked+AgeGroup+FamilySized+title,data = titanic[1:891,])
tit.oner
tit.jrip<-JRip(Survived~Pclass+Sex+Age+Embarked+AgeGroup+FamilySized+title,data = titanic[1:891,])
tit.jrip
library(caret)
tit.train<-titanic[1:891,c(2,3,5,12,13,15,16,18)]
index<-createDataPartition(tit.train$Survived,p=0.8,list = FALSE)
train<-tit.train[index,]
test<-tit.train[-index,]
library(randomForest)
tit.rf<-randomForest(Survived~.,data=train,ntree=1000)
tit.rf
varImpPlot(tit.rf)
which.min(tit.rf$err.rate[,1])
tit.rf.pred<-predict(tit.rf,test)
table(test$Survived,tit.rf.pred)
library(rpart)
tit.tree<-rpart(Survived~.,data=train)
tit.tree
tit.tree$cptable
tit.prune<-prune(tit.tree,cp=0.01)
library(rpart.plot
        )
library(partykit)
plot(as.party(tit.prune))
tit.rpart.pred<-predict(tit.prune,newdata = test,type="class")
table(test$Survived,tit.rpart.pred)
library(kknn)
grid1<-expand.grid(.k=seq(2,20,by=1))
control<-trainControl(method = "cv")
tit.knn<-train(Survived~.,data=train,method="knn",trControl=control,tuneGrid=grid1)
library(class)
knn.test<-knn(train[,-1],test[,-1],train[,1],k=20)

table(test1$Survived,knn.test)
