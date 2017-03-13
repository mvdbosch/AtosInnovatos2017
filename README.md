# AtosInnovatos2017
This repository contains the source code of the Data Analytics App used at the Atos Innovatos Plaza 2017 event.
The app is based on a public dataset and is used for demo purposes.

# Installation instructions
This app is based on R & Shiny. It has been designed to run from a local Rstudio installation, as it is presented in the demo. 
In case you want to run this app on a Shiny server, some small modifications will need to happen.

* Please clone this github repository to your local machine

* Open the  project from your Rstudio environment.

* Open the file "Atos_Innovatos2017.R" and click on the "Run App" button on the top right.


# Missing packages?

Execute the following line, to install all required packages into your local R environment

```R
install.packages(c('shiny','shinydashboard','shinyjs','data.table','DT','rpart','rattle','randomForest','sampling','e1071','caTools','sm','pastecs','DMwR','corrplot','caret','xgboost'))
```

# More information

* Check out the Atos Innovatos Plaza 2017 [slides](https://github.com/mvdbosch/AtosInnovatos2017/blob/master/www/Innovatos 2017 - Codex and Data Analytics in Manufacturing v0.2.pdf)

* Contact me at: marcel.vandenbosch@atos.net
