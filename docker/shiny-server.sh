#!/bin/sh

# Make sure the directory for individual app logs exists
mkdir -p /var/log/shiny-server
chown shiny.shiny /var/log/shiny-server
chown shiny:shiny /srv/shiny-server -R

#exec shiny-server 2>&1
exec shiny-server
