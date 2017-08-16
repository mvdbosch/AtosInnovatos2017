# Instructions for building the docker image for this demo

```Bash
docker build -t *your.registry.here*/atosanalyticsapp:latest .
```

# Pushing it to a private registry
```Bash
docker push *your.registry.here*/atosanalyticsapp
```

# Running the docker container
```Bash
docker run -d -p 3838:3838 *your.registry.here*/atosanalyticsapp
```

# Opening the app

You can now point your browser to the http://*ipadres_or_hostname*:3838/AtosInnovatos2017/
 

 