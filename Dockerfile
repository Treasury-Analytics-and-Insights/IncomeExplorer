FROM rocker/shiny:3.6.3
LABEL maintainer="iMSD_CBI_Help@msd.govt.nz"


EXPOSE 3838

ENV https_proxy "http://10.174.8.26:3128"
ENV http_proxy "http://10.174.8.26:3128"

RUN apt-get update \
        && apt-get install -y libssl-dev \
        && rm -rf /var/lib/apt/lists/*

# Get renv which will install the required dependencies
ENV RENV_VERSION 0.12.0-3
RUN R -e "install.packages('remotes', repos = c(CRAN = 'https://cloud.r-project.org'))"
RUN R -e "remotes::install_github('rstudio/renv@${RENV_VERSION}')"

# Copy across just the dependency file -- mostly this won't change so build will be faster
RUN rm -rf /srv/shiny-server \
 && mkdir -p /srv/shiny-server/outcomes
COPY ./outcomes/renv.lock /srv/shiny-server/outcomes/

# Install the dependencies
WORKDIR /srv/shiny-server/outcomes
RUN R -e 'renv::restore()'

# Patching is done last because it will often create a new layer and
# so we try and keep the churn near the top of the stack.
RUN apt-get update && apt-get upgrade -y && rm -rf /var/lib/apt/lists/*

# The shiny-server.conf file sets up Shiny Server to serve a single app
COPY ./shiny-server.conf /etc/shiny-server/
# The shell file doesn't live with the app code 
COPY ./shiny-server.sh /usr/bin/
# Grab all the code for the app
COPY ./outcomes /srv/shiny-server/outcomes

CMD ["/usr/bin/shiny-server.sh"]
