#!/bin/sh

# Make sure the directory for individual app logs exists
mkdir -p /var/log/shiny-server
chown shiny.shiny /var/log/shiny-server
chown -R shiny.shiny /srv/shiny-server

export SHINY_LOG_LEVEL=DEBUG
export SHINY_LOG_STDERR=1

# Output environment variables to .Renviron file for shiny server to read.
env > /home/shiny/.Renviron
chown shiny.shiny /home/shiny/.Renviron

exec shiny-server 2>&1
