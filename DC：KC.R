kc.train<-read.csv("e:/kaggle/price/kc_train.csv",header = T,stringsAsFactors = F)
kc.test<-read.csv("e:/kaggle/price/kc_test.csv",header = T,stringsAsFactors = F)
kc.test$saleprice<-NA
kc<-rbind(kc.train,kc.test)
kc$repairyear<-ifelse(kc$repairyear==0,kc$builtyear,kc$repairyear)
kc1$builtyear<-NULL

library(tidyverse)
ggplot(kc[1:10000,],aes(x=constructionarea,y=saleprice))+geom_point()+geom_smooth(method = "lm")
ggplot(kc[1:10000,],aes(x=builtyear,y=saleprice))+geom_point()+geom_smooth(method = "lm")
ggplot(kc[1:10000,],aes(x=repairyear,y=saleprice))+geom_point()+geom_smooth(method = "lm")
ggplot(kc[1:10000,],aes(x=housearea,y=saleprice))+geom_point()+geom_smooth(method = "lm")
ggplot(kc[1:10000,],aes(x=Basementarea,y=saleprice))+geom_point()+geom_smooth(method = "lm")
library(corrplot)
kc.cor<-cor(kc[1:10000,])
corrplot(kc.cor)
corrplot.mixed(kc.cor)
library(lubridate)
kc$data<-ymd(kc$data)
kc$week<-format(kc$data,"%A")
ggplot(kc[1:10000,],aes(x=week,y=saleprice))+geom_point()
kc$month<-format(kc$data,"%m")
kc1$year<-format(kc$data,"%Y")
kc1$year<-as.numeric(kc1$year)
kc1$age<-kc1$year-kc1$builtyear
kc1$age<-ifelse(kc1$age==-1,0,kc1$age)
kc$month<-as.factor(kc$month)
normalize<-function(x){return((x-min(x))/(max(x)-min(x)))}
kc$housearea<-normalize(kc$housearea)
kc$parkarea<-normalize(kc$parkarea)
kc$constructionarea<-normalize(kc$constructionarea)
kc$Basementarea<-normalize(kc$Basementarea)
library(SoDA)
kc$xy<-geoXY(kc$latitude,kc$longitude,unit = 1)
kc1<-kc[,c(2:12,15,16,17)]
cor.kc<-cor(kc1[1:10000,c(4,5,8:11,14)])
corrplot.mixed(cor.kc)
kc.lm<-lm(saleprice~.,data=kc1[1:10000,])
kc.lm
summary(kc.lm)
plot(kc.lm)
kc.lm.pred<-predict(kc.lm,newdata = kc1[10001:13000,])
kc1.pca.lm<-lm(saleprice~.,data=kc1.scores[1:10000,])
summary(kc1.pca.lm)
kc1$year<-format(kc$data,"%Y")
kc1$isnew<-ifelse(kc1$builtyear-is.numeric(kc1$year)>=0,1,0)
ggplot(kc1[1:10000,],aes(x=age,y=saleprice))+geom_point()+geom_smooth(method = "lm")
cor(kc1$saleprice[1:10000],kc1$age[1:10000])
kc1$repairyear<-ifelse(kc1$repairyear==0,kc1$builtyear,kc1$repairyear)
ggplot(kc1[1:10000,],aes(x=repairyear,y=saleprice))+geom_point()+geom_smooth(method = "lm")
ggplot(kc1[1:10000,],aes(x=as.factor(floor),y=saleprice))+geom_boxplot()
ggplot(kc1[1:10000,],aes(x=age,y=saleprice))+geom_point()+geom_smooth(method="lm")
ggplot(kc1[1:10000,],aes(x=as.factor(bathroom),y=saleprice))+geom_boxplot()
ggplot(kc1[1:10000,],aes(x=as.factor(bedroom),y=saleprice))+geom_boxplot()
library(plyr)
lkl<-ddply(kc1[1:10000,],.(bathroom),summarize,mean=mean(saleprice),median=median(saleprice))
lkl<-ddply(kc1[1:10000,],.(bedroom),summarize,mean=mean(saleprice),median=median(saleprice),count=length(bedroom))
ggplot(kc1[1:10000,],aes(x=parkarea,y=saleprice))+geom_point()+geom_smooth(method = "lm")
kc1$bedroom[kc1$bedroom<=4]<-1
kc1$bedroom[kc1$bedroom<=6&kc1$bedroom>=5]<-2
kc1$bedroom[kc1$bedroom<=8&kc1$bedroom>=7]<-3
kc1$bedroom[kc1$bedroom<=10&kc1$bedroom>=9]<-4
kc1$bedroom[kc1$bedroom<=12&kc1$bedroom>=11]<-5
kc1$bedroom[kc1$bedroom<=14&kc1$bedroom>=13]<6
kc1$bedroom[kc1$bedroom<=14&kc1$bedroom>=13]<-6
kc1$bedroom[kc1$bedroom<=18&kc1$bedroom>=15]<7
kc1$bedroom[kc1$bedroom<=18&kc1$bedroom>=15]<-7
kc1$bedroom[kc1$bedroom<=23&kc1$bedroom>=19]<-8
kc1$bedroom[kc1$bedroom>=24]<-9
kc1.cor<-cor(kc1[1:10000,])
kc.rf<-randomForest(saleprice~.,data=kc1[1:10000,],ntree=1000)
varImpPlot(kc.rf)
library(caret)
ctrl<-trainControl(method = "cv",number = 10)
grid<-expand.grid(mtry=3:10)
kc.rf.train<-train(saleprice~.,data = kc1[1:10000,],metric="MAE",trControl=ctrl,tuneGrid=grid)
kc.rf2<-randomForest(saleprice~.,data=kc1[1:10000,],ntree=1000,mtry=10)
kc.rf2
kc.rf2.pred<-predict(kc.rf.train,newdata=kc1[10001:13000,])
library(xgboost)
grid1<-expand.grid(nrounds=c(750,1000,1250,1500),eta=c(0.1,0.05,0.01),max_depth=c(2,3,4,5,6),gamma=0,colsample_bytree=1,min_child_weight=c(1,2,3,4,5),subsample=1)
cntrl<-trainControl(method = "cv",number = 5)
kc.xgboost<-train(x=kc1[1:10000,2:17],y=kc1[1:10000,1],trControl = cntrl,tuneGrid = grid1,method = "xgbTree")
kc.xgboost
x<-as.matrix(kc1[1:10000,2:17])
y<-kc1[1:10000,1]
train.mat<-xgb.DMatrix(data=x,label=y)
param<-list(objective="reg:linear",booster="gbtree",eta=0.05,gamma=0,max_depth=5,min_child_weight=3,subsample=1,colsample_bytree=1)
kc.xgbcv<-xgb.cv(params = param,data=train.mat,nrounds = 1250,nfold = 5,showsd = TRUE,early_stopping_rounds = 10,print_every_n = 40,maximize = F)
kc.xgb.mod<-xgb.train(data=train.mat,params = param,nrounds = 216)
kc.xgb.mod
newdata<-as.matrix(kc1[10001:13000,2:17])
kc.xgb.pred<-predict(kc.xgb.mod,newdata = newdata)
head(kc.xgb.pred)
impmatrix<-xgb.importance(feature_names = dimnames(x)[[2]],model = kc.xgb.mod)
xgb.plot.importance(impmatrix)
