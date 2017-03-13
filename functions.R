############################################################
# Supporting functions file for the Innovatos 2017 demo app

# Date: 08-Mar-2017
# Author: Marcel van den Bosch <marcel.vandenbosch@atos.net>
############################################################

library(sm)
library(pastecs)
library(DMwR)
library(corrplot)
library(caret)
library(xgboost)

options(scipen=100)
options(digits=2)

datadir <- 'secom_dataset';


loadData <- function()
{
  
  data <<- read.csv(paste0(getwd(),'/',datadir,'/','secom.data'),sep=' ',header=F);
  
  names(data) <<- paste0('MEAS_',rep(1:ncol(data)));
  
  data.labels <<- read.csv(paste0(getwd(),'/',datadir,'/','secom_labels.data'),sep=' ',header=F);
  names(data.labels) <<- c('FAIL','TIMESTAMP');
  
  data <<- cbind(data.labels,data);
  
  data$FAIL[data$FAIL == -1] <- 'OK';
  data$FAIL[data$FAIL == 1] <- 'FAIL';
  
  data$FAIL <<- as.factor(data$FAIL);
  
  data$TIMESTAMP <<- as.POSIXct(strptime(data$TIMESTAMP, "%d/%m/%Y %H:%M:%S"))

  
}

plotDensity <- function(colname)
{
  
  data_value <- subset(copy(data),select=c('FAIL',colname));
  names(data_value) <- c('FAIL','VALUE');
  data_value <- na.omit(data_value);
  
  par(mfrow=c(2,1));
  hist(data_value$VALUE,xlab=paste("Value:",colname),main = 'Histogram',xlim=c(min(data_value$VALUE),max(data_value$VALUE)));
  sm.density.compare(as.numeric(data_value$VALUE),group = data_value$FAIL, xlab=paste("Value:",colname),xlim=c(min(data_value$VALUE),max(data_value$VALUE)));
  title("Density compare");
  legend("topright",levels(as.factor(data_value$FAIL)), fill=c(2:(2+length(levels(as.factor(data_value$FAIL))))),cex=1);
  
  
}


plotCaseStats <- function()
{
  
  out <- table(data$FAIL)
  
  linch <-  max(strwidth(out, "inch")+0.7, na.rm = TRUE)
  par(mai=c(1.02,linch,0.82,0.42))
  x <- barplot(out,horiz = TRUE,cex.names=0.9,las=1,xlab=paste("# of Wafers"),xlim=c(0,max(out,na.rm=TRUE)+50),col="cornflowerblue")
  text(out+pmin((5+out*0.7),20),x,labels=round(out), col="black",cex=0.75)
  
}


createSummaryTable <- function()
{
  
  data_value <- copy(data[,3:ncol(data)]);
  
  return(t(stat.desc(data_value)))

}

plotCorrGram <- function(no_cols)
{
 
  # Exclude timestamp and FAIL, as they are not  numerical
  data_value <- subset(copy(data),select=c(-FAIL,-TIMESTAMP));
  
  
  columns.to.keep<-names(which(colMeans(is.na(data_value)) < 0.5)) # this removes those columns with more than 50% NULLs
  data_value<-subset(data_value,select = columns.to.keep) #the columns will stay which has less than 50% NAs
  
  nzv <- nearZeroVar(data_value)
  data_value <- data_value[,-nzv]
  
  if (no_cols > ncol(data_value))
  {
    no_cols <- ncol(data_value);
  }
  
  data_value <-  data_value[,1:no_cols];
  
  M <- cor(data_value,use = 'pairwise.complete.obs')
  col3 <- colorRampPalette(c("red", "white", "blue")) 
  corrplot(M, method="color",col = col3(20),tl.col="black",na.label=" ",tl.cex=0.5)
  
}

findTop100Features <- function(input_data,target_variable,SMOTE_BALANCE = TRUE, USE_RF = TRUE, USE_XGBOOST = TRUE, NO_RF_TREES = 500, RETURN_VAR_VECTOR = TRUE)
{
  # Description: Re-usable helper function to easily find relevant parameters in a dataset
  # Author: Marcel van den Bosch <marcel.vandenbosch@atos.net>
  
  # Find the TOP100 most interesting features using RandomForest & XGBoost and return it as a list of names
  
  # Smote only works with factor/numerics
  relevant_cols <- names(input_data)[unlist(lapply(seq(1:length(names(input_data))), function(x)  { if(paste(class(input_data[,x]),collapse=' ') %in% c('numeric','factor')) { return(x)} else { return(NULL)} } ))];
  data_balanced <- subset(input_data,select=relevant_cols);
  
  if (SMOTE_BALANCE == TRUE)
  {
    data_balanced <-SMOTE(form = FAIL ~., data = data_balanced, perc.over = 500,perc.under = 120)
  } 
  
  # Split out the data with target and input features
  data_balanced.target <- subset(data_balanced,select=target_variable);
  data_balanced.input <- subset(data_balanced,select=setdiff(names(data_balanced),target_variable));
  
  data_balanced.target.zero.one<-as.matrix(ifelse(data_balanced.target=='FAIL',yes=1,no=0))
  
  if (USE_XGBOOST == TRUE)
  {
  bst <- xgboost(data = as.matrix(data_balanced.input), label = data_balanced.target.zero.one, max_depth = 15,
                 eta = 0.1, nthread = 5, nrounds = 200,objective = "reg:logistic",  missing = NaN )
  
  # Now calculating the predictor importance
  importanceRaw <- xgb.importance(feature_names = colnames(data_balanced.input), model = bst, data = as.matrix(data_balanced.input), label = data_balanced.target.zero.one)
  
  #putting the data into dataframes
  importance.xgboost<-data.frame(importanceRaw$Feature,importanceRaw$Gain)
  importance.xgboost <- importance.xgboost[order(-importance.xgboost$importanceRaw.Gain),] #sorting the importances based on the gain
  
  importance.XG <- data.frame(Variable=importance.xgboost$importanceRaw.Feature[1:100],Importance=importance.xgboost$importanceRaw.Gain[1:100],Rank=seq(1:100),Type='XGBoost');
  }
  
  if (USE_RF == TRUE)
  {
    data_balanced <- rfImpute(FAIL ~ ., data=data_balanced, iter=3, ntree=50)
    
    fit.rf =randomForest(FAIL ~., data=data_balanced,importance=TRUE,ntree=NO_RF_TREES)
    
    importance.randomForest<-data.frame(row.names(varImp(fit.rf)),varImp(fit.rf))
    names(importance.randomForest)<-c("Variable","importanceRaw.Gain")
    importance.randomForest <- importance.randomForest[order(-importance.randomForest$importanceRaw.Gain),]
    
    importance.RF <- data.frame(Variable=importance.randomForest$Variable[1:100],Importance=importance.randomForest$importanceRaw.Gain[1:100],Rank=seq(1:100),Type='RF');
  }
  
  if (USE_RF == TRUE && USE_XGBOOST == TRUE)
  {
    # UNION/Rbind results
    result <- union(importance.xgboost$importanceRaw.Feature[1:100],importance.randomForest$Variable[1:100]);
    importance.ALL <- rbind(importance.XG,importance.RF)
  } else if (USE_RF == TRUE)
  {
    result <- importance.randomForest$Variable[1:100];
    importance.ALL <- importance.RF;
  } else if (USE_XGBOOST == TRUE)
  {
    result <- importance.xgboost$importanceRaw.Feature[1:100];
    importance.ALL <- importance.XG;
  }
  

  if (RETURN_VAR_VECTOR == TRUE)
  {
    return(result[1:100]);
    
  } else {
    
    importance.ALL <- importance.ALL[order(importance.ALL$Rank),];
    row.names(importance.ALL) <- NULL;
    return(head(importance.ALL,100));
  }
  
  
}




