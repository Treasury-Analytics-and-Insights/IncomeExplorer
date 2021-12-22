FROM rocker/shiny-verse:4.0
LABEL maintainer="iMSD_CBI_Help@msd.govt.nz"

EXPOSE 3838
# In case you are behind a firewall
ARG https_proxy
ARG http_proxy

RUN apt-get update && apt-get upgrade -y \
    && rm -rf /var/lib/apt/lists/*

# The shiny-server.conf file sets up Shiny Server to serve a single app
COPY ./docker/shiny-server.conf /etc/shiny-server/

# The shell file doesn't live with the app code 
COPY ./docker/shiny-server.sh /usr/bin/
# RUN chmod o+x /usr/bin/shiny-server.sh

# Create the folder to serve the single app with shiny server
RUN rm -rf /srv/shiny-server \
    && mkdir -p /srv/shiny-server/incomeexplorer \
    && chown shiny:shiny /srv/shiny-server/incomeexplorer \
                         /usr/bin/shiny-server.sh \
    && chmod u+x /usr/bin/shiny-server.sh

WORKDIR /srv/shiny-server/incomeexplorer

# Install the dependencies first since they likely change less often
COPY ./DESCRIPTION .
RUN R -e 'devtools::install()' 

# Get the code last since this is likely to change the most often
COPY . .

CMD ["/usr/bin/shiny-server.sh"]
