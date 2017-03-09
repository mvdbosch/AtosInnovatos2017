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




