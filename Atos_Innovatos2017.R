
############################################################
# Title: Data Analytics App Demo for the Atos Innovatos 2017 event
# 
# Author: Marcel van den Bosch <marcel.vandenbosch@atos.net>
# Date: 07-Mar-2017
#
# Copyright 2017: Atos Consulting & Atos Codex
# 
# Description: In this app we demonstrate the use of
# analytics in a semiconductor manufacturing case
############################################################




# Define variables for having the datasat globally
assign("data", NULL, envir = .GlobalEnv);
assign("data.labels", NULL, envir = .GlobalEnv);


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


## Code that defines the UI

ui <- dashboardPage(
  dashboardHeader(title = "Innovatos 2017"),
  dashboardSidebar(
    
    sidebarMenu(id = "tab",
                tags$div(tags$p(" ")),
      tags$div(align="center",
        img(src="resources/atos_codex_logo.png", align="center")
      ),tags$div(tags$p(" ")),

      # List of menu items in our app
      menuItem("Welcome", tabName = "welcome", icon = icon("home")),
      menuItem("Data", tabName = "Data", icon = icon("database")),
      menuItem("Case Overview", tabName = "CaseOverview", icon = icon("bar-chart")),
      menuItem("EDA Summary", tabName = "ExploreSummary", icon = icon("list-ol")),
      menuItem("EDA Density", tabName = "ExploreDensity", icon = icon("bar-chart")),
      menuItem("EDA Correlation", tabName = "ExploreCorrelation", icon = icon("area-chart")),
      menuItem("Rebalancing", tabName = "Rebalance", icon = icon("pie-chart")),
      menuItem("Feature Select", tabName = "Feature", icon = icon("list-ol")),
      menuItem("Decision Tree", tabName = "RPartModel", icon = icon("leaf")),
      menuItem("RandomForest", tabName = "RFmodel", icon = icon("tree")),
      menuItem("SVM", tabName = "SVMmodel", icon = icon("expand"))
          
    )
    
  ),
  dashboardBody(
    useShinyjs(),
    extendShinyjs(text = jscode),
    tabItems(
      
    # Start of welcome tab
    tabItem(tabName = "welcome",
            fluidRow(
    
      box(title = "Welcome to Innovatos 2017", status = "primary", solidHeader = TRUE,width = 12,
      tags$div(class="header", checked=NA,
               tags$p("Innovatos Plaza 2017, our yearly event for all Atos employees and their direct customer relations. The theme of this year is ‘Digital Shockwaves in Business’. It is the title of the trends report Journey 2020 of the Atos Scientific Community with Atos’ vision on the changing world and its impact on business, society and technology. This informal evening is to challenge your thinking about the forces that will shape business during the next few years."),
               tags$div(tags$p(" ")),
               tags$div(align="center",
                        img(src="resources/Innovatos2017_Banner.jpg", align="center")
               ),tags$div(tags$p(" "))
      )
    )
            ),
    fluidRow(
      box(title = "About this demo", status = "primary", solidHeader = TRUE,width = 12,
          tags$p("A complex modern semi-conductor manufacturing process is normally under consistent surveillance via the monitoring of signals/variables collected from sensors and or process measurement points. The sensors monitor the process and production lines which sometimes lead to failure."), 
          tags$p("Therefore, the question is how to predict the failures in early stages in production line using sensory information?"),
          tags$div(tags$p(" ")),
          tags$div(align="center",
                   img(src="resources/semicon_mfg_picture.jpg", align="center")
          ),tags$div(tags$p(" "))
      )
    ),
    fluidRow(
      box(title = "About the dataset", status = "primary", solidHeader = TRUE,width = 12,
          tags$p("The demo is based on the SECOM dataset (McCann & Johnston, 2008). This is publicly available data from a semi-conductor manufacturing process."),
          tags$p("A complex modern semi-conductor manufacturing process is normally under consistent surveillance via the monitoring of signals/variables collected from sensors and or process measurement points. However, not all of these signals are equally valuable in a specific monitoring system. 
                  The measured signals contain a combination of useful information, irrelevant information as well as noise."),
          tags$p("The data consists of 2 files the dataset file SECOM consisting of 1567 examples each with 591 features a 1567 x 591 matrix and a labels file containing the classifications and date time stamp for each example.")
                  
      )
    )
    ),
    ## End of Welcome tab
    
    
    # Begin of the Data tab.
    tabItem(tabName = "Data",
            fluidRow(
              box(title = "What is this?", status = "primary", solidHeader = TRUE,width = 10,
                  tags$p("In the table below you see the source data that is used in this analysis case")
              )),
            fluidRow(
              box(title = "Source Data", status = "primary", solidHeader = TRUE, width = 10,
            dataTableOutput('dtViewDataset')
              )
            )
    ),
    # End of the data tab
    
    # Begin of the case overview tab
    tabItem(tabName = "CaseOverview",
            fluidRow(
            box(title = "What is this?", status = "primary", solidHeader = TRUE,width = 10,
                tags$p("In the graph below the number of OK and FAILED wafers are shown for this analysis case")
    )),
            fluidRow(
            box(title = "Case Statistics", status = "primary", solidHeader = TRUE, width = 10,
              plotOutput("CaseStatsPlot") 
            )
            )
                
    ),
    # End of the case overview tab
    
    # Begin of the summary statistics tab.
    tabItem(tabName = "ExploreSummary",
            fluidRow(
              box(title = "What is this?", status = "primary", solidHeader = TRUE,width = 10,
                  tags$p("Below we explore the spread of the selected measurement. The first graph is a standard histogram of all the values. In the second graph, the
                         density between the OK and FAILed wafers are compared.")
                  )
              ),
            fluidRow(
              box(title = "Summary Statistics", status = "primary", solidHeader = TRUE, width = 10,
                  dataTableOutput('dtSummary')
              )
            )
            
            
            ),
    # End of the summary statistics tab
    
    # Begin of the density compare tab
    tabItem(tabName = "ExploreDensity",
            fluidRow(
            box(title = "What is this?", status = "primary", solidHeader = TRUE,width = 10,
                tags$p("Below we explore the spread of the selected measurement. The first graph is a standard histogram of all the values. In the second graph, the
                       density between the OK and FAILed wafers are compared.")
            )
            ),
            fluidRow(
              box(title = "Selection variable", status = "primary", solidHeader = TRUE,width = 10,
              selectInput("selectVarNameForDensity",label = "Variable:",choices = NULL)
              )
            ),
            fluidRow(
              box(title = "Plot", status = "primary", solidHeader = TRUE,width = 10,
               plotOutput("DensityPlot",height="750px") 
              )
            )
              
            
    ),
    # End of the density compare tab
    
    # Begin of the correlation plot tab
    tabItem(tabName = "ExploreCorrelation",
            fluidRow(
              box(title = "What is this?", status = "primary", solidHeader = TRUE,width = 10,
                  tags$p("One of the key activities of Exploratory Data Analysis (EDA) is to investigate the correlation between variables. In the correlogram
                         below we compared the correlation all all variables in our dataset.")
                  )
              ),
            fluidRow(
              box(title = "Number of variables", status = "primary", solidHeader = TRUE, width = 10,
                  sliderInput("intNoCorrVars","Number of variables:",min = 2,max = 600,value = 30,step = 1)
              )
            ),
            fluidRow(
              box(title = "Correlogram", status = "primary", solidHeader = TRUE, width = 10,
                  plotOutput("CorrelogramPlot",height="500px") 
              )
            )
            
            
            ),
    # End of the correlation plot tab
    
    # Begin tab for SMOTE example
    tabItem(tabName = "Rebalance",
            fluidRow(
              box(title = "What is this?", status = "primary", solidHeader = TRUE,width = 10,
                  tags$p("In the example below we will rebalance our dataset, so it has enough FAILed cases. This is done with the help of SMOTE (a Synthetic Minority Over-sampling Technique)")
                  )
              ),
            fluidRow(
              box(title = "Running SMOTE", status = "primary", solidHeader = TRUE,width = 10,
                  actionButton('btnRunSMOTE', label="Rebalance", icon = icon("ok", lib = "glyphicon"))
              )
            ),
            fluidRow(
              box(id = "Box_SMOTE_CaseStats1",title = "Class Balance", status = "primary", solidHeader = TRUE,width = 10,collapsible = TRUE,collapsed = TRUE,
                  plotOutput("Plot_SMOTE_Old_CaseStats",height = '200px'),
                  plotOutput("Plot_SMOTE_New_CaseStats",height = '200px')
              )
            ),
            fluidRow(
              box(id = "Box_SMOTE_Dataset", title = "New Dataset", status = "primary", solidHeader = TRUE,collapsible = TRUE, collapsed = TRUE,width = 10,
                  dataTableOutput('dtSMOTEresult')
              )
            )
            
    ),
    # End of the SMOTE example tab
    
    # Begin decision tree tab
    tabItem(tabName = "RPartModel",
            fluidRow(
              box(title = "What is this?", status = "primary", solidHeader = TRUE,width = 12,
                  tags$p("For building a very basic prediction model, we can use decision trees. The algorithm we used for this is RPart (Recursive partitioning for classification,
                          regression and survival trees.")
              )
            ),
            fluidRow(
              box(title = "Running RPart", status = "primary", solidHeader = TRUE,width = 12,
                  checkboxInput("chkRPartWithSMOTE",label="First run SMOTE to balance classes",value = FALSE),
                  actionButton('btnRunRPart', label="Run RPart", icon = icon("ok", lib = "glyphicon"))
              )
            ),
            fluidRow(
              box(id = "Box_RPart_Results_plot",title = "Tree Model", status = "primary", solidHeader = TRUE,width = 12,collapsible = TRUE,collapsed = TRUE,
                  plotOutput('Plot_rpart', width='100%', height=800)
              )),
            fluidRow(
              box(id = "Box_RPart_Results_summary",title = "Results", status = "primary", solidHeader = TRUE,width = 12,collapsible = TRUE,collapsed = TRUE,
                  verbatimTextOutput("Out_rpart")
              )
              
            )
            
    ),
    # End of the decision tree tab
    
    # Begin of Random Forest modelling tab
    tabItem(tabName = "RFmodel",
            fluidRow(
              box(title = "What is this?", status = "primary", solidHeader = TRUE,width = 12,
                  tags$p("In order to move towards a more advanced model, we use RandomForest. This is classification and regression based on a forest of trees using random inputs.
                          This example uses the Breiman and Cutler's Random Forests algorithm implementation.")
              )
            ),
            fluidRow(
              box(title = "Running RandomForest", status = "primary", solidHeader = TRUE,width = 12,
                  sliderInput("intNoOfRF","Number of tree models:",min = 2,max = 3500,value = 50,step = 1,width = '300px'),
                  checkboxInput("chkRFWithSMOTE",label="First run SMOTE to balance classes",value = FALSE),
                  checkboxInput("chkRFWithImpute",label="Use RF Impute for more advanced missing value imputation",value = FALSE),
                  checkboxInput("chkRFWithTest",label="Split in Training/Test set",value = FALSE),
                  actionButton('btnRunRF', label="Run RandomForest", icon = icon("ok", lib = "glyphicon"))
              )
            ),
            fluidRow(
              box(id = "Box_RF_Results_plot",title = "RandomForest results", status = "primary", solidHeader = TRUE,width = 12,collapsible = TRUE,collapsed = TRUE,
                  plotOutput('plot_rf_varimp1', width='98%', height=600),
                  plotOutput('plot_rf_varimp2', width='98%', height=600),
                  plotOutput('plot_rf_error', width='98%', height=600)
              )),
            fluidRow(
              box(id = "Box_RF_Results_conf",title = "Confusion Matrix", status = "primary", solidHeader = TRUE,width = 12,collapsible = TRUE,collapsed = TRUE,
                  verbatimTextOutput("Out_rf")
              ))
            
    ),
    # End of the Random Forest modelling tab
    
    
    # Begin of the feature selection example tab
    tabItem(tabName = "Feature",
            fluidRow(
              box(title = "What is this?", status = "primary", solidHeader = TRUE,width = 10,
                  tags$p("In the example below we will select the most relevant features (i.e. columns) from our dataset. This is done with a combination of XGBoost and RandomForest. The best
                         100 features are returned")
              )
            ),
            fluidRow(
              box(title = "Running Feature Selection", status = "primary", solidHeader = TRUE,width = 10,
                  checkboxInput("chkFeatureWithSMOTE",label="First run SMOTE to balance classes",value = TRUE),
                  checkboxInput("chkFeatureWithRF",label="Use RF to find  features",value = TRUE),
                  checkboxInput("chkFeatureWithXG",label="Use XG Boost to find  features",value = TRUE),
                  actionButton('btnRunFeature', label="Run", icon = icon("ok", lib = "glyphicon"))
              )
            ),
            fluidRow(
              box(id = "Box_Feature_Table",title = "Most promising features", status = "primary", solidHeader = TRUE,width = 10,collapsible = TRUE,collapsed = FALSE,
                  dataTableOutput("FeatureTable")
              )
            )
     
            
    ),
    # End of the feature selection tab
    
    
    # Begin of the SVM modelling tab
    tabItem(tabName = "SVMmodel",
            fluidRow(
              box(title = "What is this?", status = "primary", solidHeader = TRUE,width = 12,
                  tags$p("In order to move towards a more advanced model, we use SVM (Support Vector Machines).")
              )
            ),
            fluidRow(
              box(title = "Running SVM", status = "primary", solidHeader = TRUE,width = 12,
                  checkboxInput("chkSVMWithSMOTE",label="First run SMOTE to balance classes",value = FALSE),
                  checkboxInput("chkSVMWithTOPFeatures",label="Only use the 100 best features in model",value = FALSE),
                  checkboxInput("chkSVMWithTest",label="Split in Training/Test set",value = FALSE),
                  actionButton('btnRunSVM', label="Run SVM", icon = icon("ok", lib = "glyphicon"))
              )
              ),
            fluidRow(
              box(id = "Box_SVM_Results_conf",title = "SVM Results", status = "primary", solidHeader = TRUE,width = 12,collapsible = TRUE,collapsed = FALSE,
                  verbatimTextOutput("Out_svm")
              ))
            
    )
    # End of the SVM modelling tab
    
  )
  # End of the dashboard body
  
  )
)

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

# This is used to start the app
shinyApp(ui, server)