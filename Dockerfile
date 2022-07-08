## Anastasiya Prymolenna, PNNL
## Last Updated: 2022_07_06
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
ENV RENV_VERSION 0.15.5

#RUN R -e 'renv::install("remotes");remotes::install_local(upgrade="never")'
RUN R -e "install.packages('remotes', repos = c(CRAN = 'https://cloud.r-project.org'), dependencies = TRUE)"
RUN R -e "remotes::install_github('rstudio/renv@${RENV_VERSION}')"


# Install R packages from renv
WORKDIR /srv/shiny/
COPY renv.lock renv.lock
RUN R -e 'renv::restore()'

# set Rprofile in dir
RUN echo "local(options(shiny.port = 3838, shiny.host = '0.0.0.0'))" > /usr/lib/R/etc/Rprofile.site
# creates a Linux group and user, both called app
# user will have access to the app instead of the default root user
RUN addgroup --system app \
    && adduser --system --ingroup app app

# nmrApp IMAGE  --end of base--
# copy the app directory into the image

COPY * /srv/shiny/

RUN chmod -R 755 /srv/shiny/
RUN R -e "remotes::install_github('EMSL-Computing/nmRanalysis')"

# Make the ShinyApp available at port 3838
EXPOSE 3838

# run app
CMD ["R", "-e", "options('shiny.port' = 3838,shiny.host='0.0.0.0');nmRanalysisApp::nmRapp()"]