# Load required libraries (please install them with install.packages()  if missing )
library(shiny)
library(shinydashboard)
library(shinyjs)
library(data.table)
library(DT)
library(rpart)
library(rattle)
library(randomForest)
library(sampling)
library(e1071)
library(caTools)

# Include helper files
source('functions.R')

# Add URL prefix for loading additional resource, such as images
addResourcePath('resources',"www")

# Custom ShinyJS hack code to let the boxes collapse automatically
jscode <- "shinyjs.collapse = function(boxid) { $('#' + boxid).closest('.box').find('[data-widget=collapse]').click(); }";

# Define variables for having the datasat globally
assign("data", NULL, envir = .GlobalEnv);
assign("data.labels", NULL, envir = .GlobalEnv);


server <- function(input, output,session) {
  
  loadData()
  
  # Function that dynamically updates our dropdownlists
  observe({
    ListOfVars <- names(data)
    ListOfVars <- ListOfVars[!ListOfVars %in% c('FAIL','TIMESTAMP')];
    
    updateSelectInput(session, "selectVarNameForDensity", choices = ListOfVars);  
    updateSliderInput(session, "intNoCorrVars",max = ncol(data));
  })
  
  observe({
    
    
    # For performance reasons, only execute if the tab is actually opened by the user.
    # Logic behind the data table overview tab
    if (input$tab == 'Data')
    {
      output$dtViewDataset = renderDataTable({
        cat("Render data table");
        datatable(data, options = list(scrollX = TRUE,pageLength = 10))
      })
      
    }
    
    
    # Logic behind the summary statistcs tab
    if (input$tab == 'ExploreSummary')
    {
      output$dtSummary = renderDataTable({
        datatable(createSummaryTable(), options = list(scrollX = TRUE,pageLength = 20))
        
      })
      
    }
    
    # Logic behind the density compare tab
    if (input$tab == 'ExploreDensity')
    {
      output$DensityPlot <- renderPlot({ plotDensity(input$selectVarNameForDensity) });
    }
    
    if (input$tab == 'ExploreCorrelation')
    {
      output$CorrelogramPlot <- renderPlot({ 
        plotCorrGram(input$intNoCorrVars)
        
      });
    }
    
    # Logic behind the case stats overview tab
    if (input$tab == 'CaseOverview')
    {
      output$CaseStatsPlot <- renderPlot({ plotCaseStats() });
      
    }
    
    
  })
  
  # Below code is triggered once a button has been pressed.
  
  observeEvent(input$btnRunSMOTE, {
    
    # This code implemented the SMOTE example
    
    
    data_old <-data.table(copy(data)) 
    data_old <- subset(data_old,select=-TIMESTAMP);
    
    table(data_old$FAIL) #Statistics before 
    data_balanced <-SMOTE(form = FAIL ~., data = data_old, perc.over = 500,perc.under = 120)
    
    output$Plot_SMOTE_Old_CaseStats <- renderPlot({ 
      
      out <- table(data_old$FAIL)
      linch <-  max(strwidth(out, "inch")+0.7, na.rm = TRUE)
      par(mai=c(1.02,linch,0.82,0.42))
      x <- barplot(out,horiz = TRUE,cex.names=0.9,las=1,xlab=paste("# of cases"),xlim=c(0,max(out,na.rm=TRUE)+50),col="cornflowerblue",main = 'Before SMOTE')
      text(out+pmin((5+out*0.7),20),x,labels=round(out), col="black",cex=0.75)
      
    })
    
    output$Plot_SMOTE_New_CaseStats <- renderPlot({ 
      
      out <- table(data_balanced$FAIL)
      linch <-  max(strwidth(out, "inch")+0.7, na.rm = TRUE)
      par(mai=c(1.02,linch,0.82,0.42))
      x <- barplot(out,horiz = TRUE,cex.names=0.9,las=1,xlab=paste("# of cases"),xlim=c(0,max(out,na.rm=TRUE)+50),col="cornflowerblue",main = 'After SMOTE')
      text(out+pmin((5+out*0.7),20),x,labels=round(out), col="black",cex=0.75)
      
    })
    
    output$dtSMOTEresult = renderDataTable({
      datatable(data_balanced, options = list(scrollX = TRUE,pageLength = 20))
    })
    
    # Call our custom box collapse hack
    js$collapse("Box_SMOTE_CaseStats1")
    js$collapse("Box_SMOTE_Dataset")
    
  })
  
  
  observeEvent(input$btnRunRPart, {
    # Code below implements the decision tree functionality
    
    isolate({
      
      if (input$chkRPartWithSMOTE == TRUE )
      {
        data_old <-data.table(copy(data)) 
        data_old <- subset(data_old,select=-TIMESTAMP);
        data_balanced <-SMOTE(form = FAIL ~., data = data_old, perc.over = 500,perc.under = 120)
      } else {
        data_balanced <-data.table(copy(data))
        data_balanced <- subset(data_balanced,select=-TIMESTAMP);
      }
      
      
      # grow tree 
      fit <- rpart(FAIL ~ .,
                   method="class", data=data_balanced, control=rpart.control(minsplit=2))
      
      output$Out_rpart = renderPrint({
        summary(fit)
      })
      
      
      output$Plot_rpart = renderPlot({
        fancyRpartPlot(fit,main="Decision Tree",digits=10)
      })
      
    })
    js$collapse("Box_RPart_Results_plot");
    js$collapse("Box_RPart_Results_summary");
    
  })
  
  
  observeEvent(input$btnRunRF, {
    # Below code implements the RandomForest modelling functionality
    
    isolate({
      
      
      if (input$chkRFWithTest == TRUE)
      {
        # Do a 75/25 split
        sample = sample.split(data$FAIL, SplitRatio = .75)
        data_train = subset(copy(data), sample == TRUE)
        data_test = subset(copy(data), sample == FALSE)
      } else {
        # Just make both sets the same
        data_train <- copy(data);
        data_test <- copy(data);
      }
      
      
      if (input$chkRFWithSMOTE == TRUE )
      {
        data_old <- data_train
        data_old <- subset(data_old,select=-TIMESTAMP);
        data_balanced <-SMOTE(form = FAIL ~., data = data_old, perc.over = 500,perc.under = 120)
      } else {
        data_balanced <- data_train #speeding up by turning into data.table
        data_balanced <- subset(data_balanced,select=-TIMESTAMP);
      }
      
      columns.to.keep<-names(which(colMeans(is.na(data_balanced)) < 0.5)) # this removes those columns with more than 50% NULLs
      data_balanced<-subset(data_balanced,select = columns.to.keep) #the columns will stay which has less than 50% NAs
      
      nzv <- nearZeroVar(data_balanced)
      data_balanced <- data_balanced[,-nzv]
      
      if (input$chkRFWithImpute == TRUE )
      {
        # Since RandomForest cannot handle missing values, we will try to impute them
        data.imputed <- rfImpute(FAIL ~ ., data=data_balanced, iter=2, ntree=30)
      } else {
        
        # The very cheap version of setting the missing values to something very high
        data.imputed <- data_balanced;
        data.imputed[is.na(data.imputed)]<--9999 #just some random number that never happened in the data
      }
      
      #### Random Forest
      fit <- randomForest(FAIL ~ .,
                          data=data.imputed, 
                          importance=TRUE, 
                          ntree=input$intNoOfRF)
      
      
      output$plot_rf_varimp1 = renderPlot({
        varImpPlot(fit,type=1,main = "Variable Importance")
      })
      
      output$plot_rf_varimp2 = renderPlot({
        varImpPlot(fit,type=2,main = "Variable Importance")
      })        
      
      output$plot_rf_error = renderPlot({
        plot(fit,main = "Error")
      })
      
      
      test_data <- data_test
      test_result <- test_data$FAIL; # Make sure we have the results seaprate
      
      test_data <- subset(test_data,select=c(-TIMESTAMP,-FAIL))
      
      pred_data <- predict(object = fit, newdata = test_data)
      
      xtab <- table(pred_data, test_result)
      
      output$Out_rf = renderPrint({
        confusionMatrix(xtab)
      })
      
      
    })
    
    js$collapse("Box_RF_Results_plot");
    js$collapse("Box_RF_Results_conf");
    
    
  })
  
  
  observeEvent(input$btnRunFeature, {
    # Code below calls the function to find the TOP100 most promising features
    
    output$FeatureTable = renderDataTable({
      isolate({
        datatable(findTop100Features(copy(data),'FAIL',SMOTE_BALANCE = input$chkFeatureWithSMOTE, USE_RF = input$chkFeatureWithRF, USE_XGBOOST = input$chkFeatureWithXG,RETURN_VAR_VECTOR = FALSE), options = list(scrollX = TRUE,pageLength = 20))
      })
      
    })
    
    
  })
  
  
  observeEvent(input$btnRunSVM, {
    # This code implements the SVM modelling functionality
    
    isolate({
      
      if (input$chkSVMWithTest == TRUE)
      {
        # Do a 75/25 split
        sample = sample.split(data$FAIL, SplitRatio = .75)
        data_train = subset(copy(data), sample == TRUE)
        data_test = subset(copy(data), sample == FALSE)
      } else {
        # Just make both sets the same
        data_train <- copy(data);
        data_test <- copy(data);
      }
      
      if (input$chkSVMWithTOPFeatures == TRUE)
      {
        top100_cols <- findTop100Features(data_train,'FAIL');
      }
      
      if (input$chkSVMWithSMOTE == TRUE )
      {
        data_old <- data_train
        data_old <- subset(data_old,select=-TIMESTAMP);
        data_balanced <-SMOTE(form = FAIL ~., data = data_old, perc.over = 500,perc.under = 120)
      } else {
        data_balanced <- data_train 
        data_balanced <- subset(data_balanced,select=-TIMESTAMP);
      }
      
      if (input$chkSVMWithTOPFeatures == TRUE)
      {
        data_balanced <- subset(data_balanced,select=c(top100_cols,'FAIL'));
      }
      
      columns.to.keep<-names(which(colMeans(is.na(data_balanced)) < 0.5)) # this removes those columns with more than 50% NULLs
      data_balanced<-subset(data_balanced,select = columns.to.keep) #the columns will stay which has less than 50% NAs
      
      nzv <- nearZeroVar(data_balanced)
      data_balanced <- data_balanced[,-nzv]
      
      
      data_balanced[is.na(data_balanced)]<--9999
      
      xData <- subset(data_balanced,select=-FAIL);
      yData <- data_balanced$FAIL;
      
      
      fit.svm<-svm(x= xData,y= yData)
      
      # Get our test set from the original split
      test_data <- data_test
      test_result <- test_data$FAIL; # Make sure we have the results seaprate - as some functions like it separate
      
      # Remove the target and timestamp
      test_data <- subset(test_data,select=c(-TIMESTAMP,-FAIL))
      
      # NA's are not desired, as it hinders prediction.
      test_data[is.na(test_data)]<--9999 #just some random number that never happened in the data
      
      if (input$chkSVMWithTOPFeatures == TRUE)
      {
        test_data<-subset(test_data,select = c(top100_cols));
      }
      
      test_data<-subset(test_data,select = setdiff(columns.to.keep,'FAIL')) #the columns will stay which has less than 50% NAs
      test_data <- test_data[,-nzv]
      
      # Do the actual prediction with our previously trained model
      pred_data <- predict(object = fit.svm, newdata = test_data)
      
      xtab <- table(pred_data, test_result)
      
      # Write results back to the app
      output$Out_svm = renderPrint({
        isolate({
          cat("Started with options:\r\n")
          cat("* SMOTE : ", input$chkSVMWithSMOTE,"\r\n")
          cat("* TOP100 Features : ",input$chkSVMWithTOPFeatures,"\r\n")
          cat("* TEST/TRAINING SET : ",input$chkSVMWithTest,"\r\n")
          cat("---------------------------------------------------------\r\n")
          confusionMatrix(xtab)
        })
      })
      
      
    })
    
    
    
  })
  
}