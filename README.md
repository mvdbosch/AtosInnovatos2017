# AtosInnovatos2017
This repository contains the source code of the Data Analytics App used at the Atos Innovatos Plaza 2017 event.
The app is based on a public dataset and is ONLY used for (public) demo purposes.

# Installation instructions
This app is based on R & Shiny. It has been designed to run from a local Rstudio installation on a laptop, as it is presented in the demo. 
In case you want to run this app on a Shiny server, some small modifications will need to happen.

* Please clone this github repository to your local machine ( https://github.com/mvdbosch/AtosInnovatos2017.git )

* Open the  project from your Rstudio environment.

* Open the file **"Atos_Innovatos2017.R"** and click on the **"Run App"** button on the top right.

![How to run the app](https://github.com/mvdbosch/AtosInnovatos2017/blob/master/www/HowToRun.jpg)

# Running the docker container image
You can pull the latest docker image for this demo app from my private registry. In addition, you can
also build you own docker image, using the Dockerfile provided in the docker directory.

```Bash
docker pull registry.marcel.im/atosanalyticsapp
docker run -p 3838:3838 registry.marcel.im/atosanalyticsapp
```

You can now point your browser to the http://*ipadres_or_hostname*:3838/AtosInnovatos2017/

# Missing packages?

Execute the following line, to install all required packages into your local R environment

```R
install.packages(c('shiny','shinydashboard','shinyjs','data.table','DT','rpart','rattle','randomForest','sampling','e1071','caTools','sm','pastecs','DMwR','corrplot','caret','xgboost','V8','ggplot2','assertthat'))
```

# Running this application on shiny-server

You can use the server.R and ui.R files, to host the application on a shiny-server.

# About the Innovation Plaza 2017 event

Innovatos Plaza 2017, our yearly event for all Atos employees and their direct customer relations. The theme of this year is ‘Digital Shockwaves in Business’. 
It is the title of the trends report Journey 2020 of the Atos Scientific Community with Atos’ vision on the changing world and its impact on business, society and technology. 
This informal evening is to challenge your thinking about the forces that will shape business during the next few years.

# More information

* Check out the Atos Innovatos Plaza 2017 [slides](https://github.com/mvdbosch/AtosInnovatos2017/blob/master/www/Innovatos%202017%20-%20Codex%20and%20Data%20Analytics%20in%20Manufacturing%20v0.2.pdf)

* Contact me at: marcel.vandenbosch@atos.net

# Some screenshots

![Screenshot 1](https://github.com/mvdbosch/AtosInnovatos2017/blob/master/www/app_screenshot1.jpg)

![Screenshot 2](https://github.com/mvdbosch/AtosInnovatos2017/blob/master/www/app_screenshot2.jpg)

![Screenshot 3](https://github.com/mvdbosch/AtosInnovatos2017/blob/master/www/app_screenshot3.jpg)

![Screenshot 4](https://github.com/mvdbosch/AtosInnovatos2017/blob/master/www/app_screenshot4.jpg)
