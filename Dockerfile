## Anastasiya Prymolenna, PNNL
## Last Updated: 2022_06_28
## nmRanalysis BASE IMAGE Version: 0.0.1
## Tag: nmranalysisbase:0.0.1


## BASE IMAGE 

# Install R Version 4.1.3
FROM rocker/shiny:4.1.3

#Install Ubuntu dependencies
RUN apt-get update && apt-get install -y --no-install-recommends \
    sudo \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    libssl-dev \
    libssh2-1-dev \
    libgsl0-dev \
    r-cran-xml \
    libxml2-dev \
    xdg-utils \
    && rm -rf /var/lib/apt/lists/*


#install renv on the docker image
ENV RENV_VERSION 0.15.4

#ENV RENV_PATHS_CACHE /usr/local/lib/R/site-library
RUN R -e "install.packages('remotes', repos = c(CRAN = 'https://cloud.r-project.org'), dependencies = TRUE)"
RUN R -e "remotes::install_github('rstudio/renv@${RENV_VERSION}')"


# Install R packages from renv
#COPY ../nmranalysis/* /srv/shiny/nmranalysis/
WORKDIR /srv/shiny/
COPY renv.lock renv.lock
RUN R -e 'renv::restore()'
#RUN R -e 'renv::deactivate()'
#RUN R -e "remotes::install_local(path = '../nmranalysis', lib = '/usr/local/lib/R/site-library')"
#RUN R -e "remotes::install_github('r-lib/devtools')"
#RUN R -e "devtools::install_local('/srv/shiny/nmranalysis', force = TRUE)"


# set Rprofile in dir
RUN echo "local(options(shiny.port = 3838, shiny.host = '0.0.0.0'))" > /usr/lib/R/etc/Rprofile.site
# creates a Linux group and user, both called app
# user will have access to the app instead of the default root user
RUN addgroup --system app \
    && adduser --system --ingroup app app

# nmrApp IMAGE  (move to separate script from here)
# copy the app directory into the image

COPY * /srv/shiny/

RUN chmod -R 755 /srv/shiny/
#RUN R -e 'remotes::install_local(upgrade="never", lib = "/usr/local/lib/R/site-library")'
RUN R -e "remotes::install_github('EMSL-Computing/nmRanalysis')"
#RUN R -e "library(devtools)"
#RUN R -e "devtools::load_all('.')"
# Make the ShinyApp available at port 3838
EXPOSE 3838

# run app
CMD ["R", "-e", "options('shiny.port' = 3838,shiny.host='0.0.0.0');nmRanalysisApp::nmRapp()"]
#CMD ["R", "-e", "nmRapp()"]
#CMD Rscript R/app_nmRapp.R