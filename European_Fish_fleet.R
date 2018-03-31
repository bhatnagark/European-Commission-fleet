library(caret)
library(gbm)
library(mlbench)
library(rpart)
library(rpart.plot)
library(ggplot2)
library(psych)
library(corrplot)
library(randomForest)
library(party)
library(grid)
library(plyr)

set.seed(1234)
#file loading
setwd("~/Desktop")
eco_df<-read.csv2("merged_data.csv",sep=",",stringsAsFactors = TRUE)
str(eco_df)
#Data Cleaning and converting to numeric
head(eco_df$Capacity..GT.)
eco_df$Capacity..GT. <-  sub("," ,"",as.character(eco_df$Capacity..GT.))
eco_df$Capacity..GT. <- as.numeric(as.character(eco_df$Capacity..GT.))
eco_df$Average.Length.for.Segment <-  sub("," ,"",as.character(eco_df$Average.Length.for.Segment))
eco_df$Average.Length.for.Segment<-as.numeric(eco_df$Average.Length.for.Segment)
eco_df$Capacity.Index<-  sub("," ,"",as.character(eco_df$Capacity.Index))
eco_df$Capacity.Index<-as.numeric(eco_df$Capacity.Index)
eco_df$Size.Index<-  sub("," ,"",as.character(eco_df$Size.Index))
eco_df$Size.Index<-as.numeric(eco_df$Size.Index)
eco_df$Length<-  sub("," ,"",as.character(eco_df$Length))
eco_df$Length<-as.numeric(eco_df$Length)

#converting factor to numeric
eco_df[,15:45] <- apply(eco_df[,15:45] ,2 ,function(x){(gsub(",","",x))})
eco_df[,15:45] <- apply(eco_df[,15:45] ,2 ,function(x){(gsub(",","",x))})
eco_df[,15:45] <- apply(eco_df[,15:45] ,2 ,as.numeric)

# data cleaning to remove garbage value
eco_df$Size.Category<-as.character(eco_df$Size.Category)
eco_df$Size.Category[eco_df$Size.Category== '43079']<-'10-12'
eco_df$Size.Category[eco_df$Size.Category== '43435']<-'12-18'
table(eco_df$Size.Category)
eco_df$Size.Category<-as.factor(eco_df$Size.Category)

#Removing NA values
eco_df[is.na(eco_df)]<-0
str(eco_df)

#feature cleaning
na_count <-sapply(eco_df, function(y) sum(length(which(is.na(y)))))
na_count <- data.frame(na_count)
print(na_count)
table(eco_df$.=='0')
eco_df<-subset(eco_df,select = -c(1))

#redundant feature removal with cost attriribute
eco_df<-eco_df[,c(2:44,64,65,66)]
str(eco_df)
eco_df$Total.Jobs<-as.numeric(eco_df$Total.Jobs)
eco_df$FTE<-as.numeric(eco_df$FTE)

#Dividing the data frame into different target variables
cost1<-eco_df[,c(1:43,44)]
cost2<-eco_df[,c(1:43,45)]
cost3<-eco_df[,c(1:43,46)]
str(cost1)
str(cost2)
str(cost3)

#variable importance and feature removal for gross profit
fit1<-rpart(GROSS.PROFIT~.,data=cost1)
fit2<-rpart(Depreciation~.,data=cost2)
fit3<-rpart(Sundry.receipts~.,data=cost3)
varimp1<-data.frame(varImp(fit1))
varimp2<-data.frame(varImp(fit2))
varimp3<-data.frame(varImp(fit3))
print(varimp1)
print(varimp2)
print(varimp3)
control <- rfeControl(functions=rfFuncs, method="cv", number=2)

# run the RFE algorithm
results1 <- rfe(cost1[,c(1:15)], cost1[,c(16)], sizes=c(1:16), rfeControl=control)
results2 <- rfe(cost2[,c(1:15)], cost2[,c(16)], sizes=c(1:16), rfeControl=control)
results3 <- rfe(cost3[,c(1:15)], cost3[,c(16)], sizes=c(1:16), rfeControl=control)

# summarize the results
print(results1)
print(results2)
print(results3)

# list the chosen features
predictors(results1)
predictors(results2)
predictors(results3)

# plot the results
plot(results1, type=c("g", "o"))
plot(results2, type=c("g", "o"))
plot(results3, type=c("g", "o"))

#random forest data splitting
split1 <- sample(nrow(cost1), floor(0.7*nrow(cost1)))
split2 <- sample(nrow(cost2), floor(0.7*nrow(cost1)))
split3 <- sample(nrow(cost3), floor(0.7*nrow(cost1)))
traindf1 <- cost1[split1,]
testdf1 <-  cost1[-split1,]
traindf2 <- cost2[split2,]
testdf2 <-  cost2[-split2,]
traindf3 <- cost3[split3,]
testdf3 <-  cost3[-split3,]


# Model Defination
forest.tree1 <- train(GROSS.PROFIT~ ., method = "rf",
                      data = traindf1, importance = T,
                      trControl = trainControl(method = "cv", number = 3))

forest.tree2 <- train(Depreciation~ ., method = "rf",
                      data = traindf2, importance = T,
                      trControl = trainControl(method = "cv", number = 3))

forest.tree3 <- train(Sundry.receipts~ ., method = "rf",
                      data = traindf3, importance = T,
                      trControl = trainControl(method = "cv", number = 3))

forest.pred1 <- predict(forest.tree1, testdf1)
forest.pred2 <- predict(forest.tree2, testdf2)
forest.pred3 <- predict(forest.tree3, testdf3)
print(forest.tree1)
print(forest.tree2)
print(forest.tree3)
plot(forest.tree1, type=c("g","o"))
plot(forest.tree2, type=c("g","o"))

rf_preds_df1 <- data.frame(cbind(actuals=testdf1$GROSS.PROFIT , predicteds=forest.pred1))
str(rf_preds_df1)

#confusionMatrix(cost1$GROSS.PROFIT, predicteds ,positive = "Yes")
rf_rmse1 <- (mean((testdf1$GROSS.PROFIT- forest.pred1)^2))**0.5
rf_sse1 <- sum( (rf_preds_df1$predicteds - rf_preds_df1$actuals)^2 )
print(mean(rf_preds_df1$actuals))
rf_sst1 <- sum( (mean(cost1$GROSS.PROFIT) - rf_preds_df1$actuals)^2)
print(rf_sst1)
rf_r21 <- 1- rf_sse1/rf_sst1
cat('FOR RANDOM FOREST \n')
cat('Squar Root of MSE',rf_rmse1,'\n')
cat('R squared value:', rf_r21*100, '%\n' )
rf1<- ggplot(rf_preds_df1, aes(predicteds, actuals))+ geom_abline(color='#E41A1C')+ geom_point(alpha=0.3) +
  geom_vline(xintercept = 0) + geom_hline(yintercept = 0)+geom_smooth(method = "loess", se=FALSE)+ggtitle('RANDOM FOREST')
print(rf1)
head(rf_preds_df1)

rf_preds_df2 <- data.frame(cbind(actuals=testdf2$Depreciation, predicteds=forest.pred2))

#confusionMatrix(cost1$GROSS.PROFIT, predicteds ,positive = "Yes")
rf_rmse2 <- (mean((testdf2$Depreciation- forest.pred2)^2))**0.5
rf_sse2 <- sum( (rf_preds_df2$predicteds - rf_preds_df2$actuals)^2 )
rf_sst2 <- sum( (mean(cost2$Depreciation) - rf_preds_df2$actuals)^2)
rf_r22 <- 1- rf_sse2/rf_sst2
cat('FOR RANDOM FOREST \n')
cat('Squar Root of MSE',rf_rmse2,'\n')
cat('R squared value:', rf_r22*100, '%\n' )
head(rf_preds_df2)
rf2<- ggplot(rf_preds_df2, aes(predicteds, actuals))+ geom_abline(color='#E41A1C')+ geom_point(alpha=0.3) +
  geom_vline(xintercept = 0) + geom_hline(yintercept = 0)+geom_smooth(method = "loess", se=FALSE)+ggtitle('RANDOM FOREST')
print(rf2)

rf_preds_df3 <- data.frame(cbind(actuals=testdf3$Sundry.receipts, predicteds=forest.pred3))
rf_rmse3 <- (mean((testdf3$Sundry.receipts - forest.pred3)^2))**0.5
rf_sse3 <- sum( (rf_preds_df3$predicteds - rf_preds_df3$actuals)^2 )
rf_sst3 <- sum( (mean(cost3$Sundry.receipts) - rf_preds_df3$actuals)^2)
rf_r23 <- 1- rf_sse3/rf_sst3
cat('FOR RANDOM FOREST \n')
cat('Squar Root of MSE',rf_rmse3,'\n')
cat('R squared value:', rf_r23*100, '%\n' )
head(rf_preds_df3)
tail(cost3$Sundry.receipts)

#due to large number of zeros sundry receipt column dropped
colSums(cost3 ==0)
str(cost1)


#graph plotting

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  
  library(grid)
  
  
  
  # Make a list from the ... arguments and plotlist
  
  plots <- c(list(...), plotlist)
  
  
  
  numPlots = length(plots)
  
  
  
  # If layout is NULL, then use 'cols' to determine layout
  
  if (is.null(layout)) {
    
    # Make the panel
    
    # ncol: Number of columns of plots
    
    # nrow: Number of rows needed, calculated from # of cols
    
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     
                     ncol = cols, nrow = ceiling(numPlots/cols))
    
  }
  
  
  
  if (numPlots==1) {
    
    print(plots[[1]])
    
    
    
  } else {
    
    # Set up the page
    
    grid.newpage()
    
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    
    
    # Make each plot, in the correct location
    
    for (i in 1:numPlots) {
      
      # Get the i,j matrix positions of the regions that contain this subplot
      
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      
                                      layout.pos.col = matchidx$col))
      
    }
    
  }
  
}



y1 <- ggplot(cost1, aes(value.S2, GROSS.PROFIT))+
  
  geom_point(aes(color=Segment), alpha = 0.6) + geom_vline(xintercept = 0) + geom_hline(yintercept = 0)

y2 <- ggplot(cost1, aes(Capacity..GT., GROSS.PROFIT))+
  
  geom_point(aes(color=Segment), alpha = 0.6) + geom_hline(yintercept = 0)

y3 <- ggplot(cost1, aes(Capacity.Index, GROSS.PROFIT))+
  
  geom_point(aes(color=Segment), alpha = 0.6) + geom_hline(yintercept = 0)

y4 <- ggplot(cost1, aes(Size.Category))+ geom_bar(aes(fill=Size.Category), alpha = 0.6)+ scale_y_continuous(breaks = seq(0,700, by=50))

multiplot(y1,y2,y3,y4 , cols=2)

z1 <- ggplot(cost2, aes(Length, Depreciation))+
  
  geom_point(aes(color=Segment), alpha = 0.6) + geom_vline(xintercept = 0) + geom_hline(yintercept = 0)

z2 <- ggplot(cost2, aes(Capacity..GT., Depreciation))+
  
  geom_point(aes(color=Segment), alpha = 0.6) + geom_hline(yintercept = 0)

z3 <- ggplot(cost2, aes(Capacity.Index, Depreciation))+
  
  geom_point(aes(color=Segment), alpha = 0.6) + geom_hline(yintercept = 0)


multiplot(z1,z2,z3, cols=2)

zz1 <- ggplot(cost3, aes(FTE, Sundry.receipts))+
  
  geom_point(aes(color=Segment), alpha = 0.6) + geom_vline(xintercept = 0) + geom_hline(yintercept = 0)

zz2 <- ggplot(cost3, aes(value.S44, Sundry.receipts))+
  
  geom_point(aes(color=Segment), alpha = 0.6) + geom_hline(yintercept = 0)

zz3 <- ggplot(cost3, aes(Total.Jobs, Sundry.receipts))+
  
  geom_point(aes(color=Segment), alpha = 0.6) + geom_hline(yintercept = 0)


multiplot(zz1,zz2,zz3, cols=2)

#single regression tree for gross profit

tree1 <- rpart(traindf1$GROSS.PROFIT ~ . ,method = "anova", data= traindf1, control = rpart.control(minsplit = 30, cp=0.001))
tree.pred1 <- predict(object = tree1, newdata= testdf1)

plot(tree1, uniform=TRUE,
     main="Regression Tree for Gross Profit ")
text(tree1, use.n=TRUE, all=TRUE, cex=.8)

tree_preds_df1 <- data.frame(cbind(actuals=testdf1$GROSS.PROFIT, predicteds=tree.pred1))
tree_rmse1 <- (mean((testdf1$GROSS.PROFIT- tree.pred1)^2))**0.5
tree_sse1 <- sum( (tree_preds_df1$predicted - tree_preds_df1$actuals)^2 )
tree_sst1 <- sum( (mean(eco_df$GROSS.PROFIT) - tree_preds_df1$actuals)^2)
tree_r21 <- 1- tree_sse1/tree_sst1

cat('FOR SINGLE REGRESSION TREE OF GROSS PROFIT \n')
cat('Squar Root of MSE',tree_rmse1,'\n')
cat('R squared value:', tree_r21*100, '%\n' )

head(tree_preds_df1)
printcp(tree1)
plotcp(tree1)
rsq.rpart(tree1)

x1<- ggplot(tree_preds_df1, aes(predicteds, actuals))+ geom_abline(color='#E41A1C')+ geom_point(alpha=0.3) +
  geom_vline(xintercept = 0) + geom_hline(yintercept = 0) +ggtitle('ACCURACY OF SINGLE TREE')
print(x1)

# regresssion tree for depriciation

tree2 <- rpart(traindf2$Depreciation~ . ,method = "anova", data= traindf2, control = rpart.control(minsplit = 30, cp=0.001))
tree.pred2 <- predict(object = tree2, newdata= testdf2)

plot(tree2, uniform=TRUE,
     main="Regression Tree for Gross Profit ")
text(tree2, use.n=TRUE, all=TRUE, cex=.8)

tree_preds_df2 <- data.frame(cbind(actuals=testdf2$Depreciation, predicteds=tree.pred2))
tree_rmse2 <- (mean((testdf2$Depreciation - tree.pred2)^2))**0.5
tree_sse2 <- sum( (tree_preds_df2$predicted - tree_preds_df2$actuals)^2 )
tree_sst2 <- sum( (mean(eco_df$Depreciation) - tree_preds_df2$actuals)^2)
tree_r22 <- 1- tree_sse2/tree_sst2

cat('FOR SINGLE TREE ON DEPRECIATION \n')
cat('Squar Root of MSE',tree_rmse2,'\n')
cat('R squared value:', tree_r22*100, '%\n' )

head(tree_preds_df2)
printcp(tree2)
plotcp(tree2)
rsq.rpart(tree2)

x2<- ggplot(tree_preds_df2, aes(predicteds, actuals))+ geom_abline(color='#E41A1C')+ geom_point(alpha=0.3) +
  geom_vline(xintercept = 0) + geom_hline(yintercept = 0) +ggtitle('ACCURACY OF SINGLE TREE ON DEPRECIATION')
print(x2)

#GBM model for Gross profit
boost.model1 <- train(GROSS.PROFIT~ ., method = "gbm",data = traindf1, verbose = F,
                      trControl = trainControl(method = "cv", number = 3))

boost.pred1 <- predict(boost.model1, testdf1)
print(boost.pred1)
print(boost.model1)
#plot(varImp(boost.model), top = 10)

boost_preds_df1 <- data.frame(cbind(actuals=testdf1$GROSS.PROFIT, predicteds=boost.pred1))
plot(boost_preds_df1, type=c("g","o"))
gbm1<- ggplot(boost_preds_df1, aes(predicteds, actuals))+ geom_abline(color='#E41A1C')+ geom_point(alpha=0.3) +
  geom_vline(xintercept = 0) + geom_hline(yintercept = 0)+geom_smooth(method = "loess", se=FALSE)+ggtitle('GBM for Gross Profit')
print(rf2)

boost_rmse1 <- (mean((testdf1$GROSS.PROFIT - boost.pred1)^2))**0.5
boost_sse1 <- sum( (boost_preds_df1$predicteds - boost_preds_df1$actuals)^2 )
boost_sst1 <- sum( (mean(eco_df$GROSS.PROFIT) - boost_preds_df1$actuals)^2)
boost_r21 <- 1- boost_sse1/boost_sst1

cat('FOR GBM \n')
cat('Squar Root of MSE',boost_rmse1,'\n')
cat('R squared value:', boost_r21*100, '%\n' )

#GBM model for Depreciation
boost.model2 <- train(Depreciation~ ., method = "gbm",data = traindf2, verbose = F,
                      trControl = trainControl(method = "cv", number = 3))

boost.pred2 <- predict(boost.model2, testdf2)


boost_preds_df2 <- data.frame(cbind(actuals=testdf2$Depreciation, predicteds=boost.pred2))

boost_rmse2 <- (mean((testdf2$Depreciation - boost.pred2)^2))**0.5
boost_sse2 <- sum( (boost_preds_df2$predicteds - boost_preds_df2$actuals)^2 )
boost_sst2 <- sum( (mean(eco_df$Depreciation) - boost_preds_df2$actuals)^2)
boost_r22 <- 1- boost_sse2/boost_sst2
gbm2<- ggplot(boost_preds_df1, aes(predicteds, actuals))+ geom_abline(color='#E41A1C')+ geom_point(alpha=0.3) +
  geom_vline(xintercept = 0) + geom_hline(yintercept = 0)+geom_smooth(method = "loess", se=FALSE)+ggtitle('GBM for Depreciation')
print(gbm2)
print(boost.model2)
cat('FOR GBM \n')
cat('Squar Root of MSE',boost_rmse2,'\n')
cat('R squared value:', boost_r22*100, '%\n' )
