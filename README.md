# AtosInnovatos2017
This repository contains the source code of the Data Analytics App used at the Atos Innovatos Plaza 2017 event.
The app is based on a public dataset and is used for demo purposes.

# Installation instructions
This app is based on R & Shiny. It has been designed to run from a local Rstudio installation, as it is presented in the demo. 
In case you want to run this app on a Shiny server, some small modifications will need to happen.

* Please clone this github repository to your local machine

* Open the  project from your Rstudio environment.

* Open the file "Atos_Innovatos2017.R" and click on the "Run App" button on the top right.

![How to run the app]((https://github.com/mvdbosch/AtosInnovatos2017/blob/master/www/HowToRun.jpg)

# Missing packages?

Execute the following line, to install all required packages into your local R environment

```R
install.packages(c('shiny','shinydashboard','shinyjs','data.table','DT','rpart','rattle','randomForest','sampling','e1071','caTools','sm','pastecs','DMwR','corrplot','caret','xgboost'))
```

# Porting this application to Shiny server

Currently the UI and Server code has been placed in 1 file (Atos_Innovatos2017.R). 
When running this app on a Shiny-server, splitting up the UI and Server functions into UI.R and Server.R is needed.

# About the Innovation Plaza 2017 event

Innovatos Plaza 2017, our yearly event for all Atos employees and their direct customer relations. The theme of this year is ‘Digital Shockwaves in Business’. 
It is the title of the trends report Journey 2020 of the Atos Scientific Community with Atos’ vision on the changing world and its impact on business, society and technology. 
This informal evening is to challenge your thinking about the forces that will shape business during the next few years.

# More information

* Check out the Atos Innovatos Plaza 2017 [slides](https://github.com/mvdbosch/AtosInnovatos2017/blob/master/www/Innovatos 2017 - Codex and Data Analytics in Manufacturing v0.2.pdf)

* Contact me at: marcel.vandenbosch@atos.net
