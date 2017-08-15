############################################################
# Title: Data Analytics App Demo for the Atos Innovatos 2017 event
# File: UI.R
# 
# Author: Marcel van den Bosch <marcel.vandenbosch@atos.net>
# Date: 07-Mar-2017
#
# Copyright 2017: Atos Consulting & Atos Codex
# 
# Description: In this app we demonstrate the use of
# analytics in a semiconductor manufacturing case
############################################################

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
