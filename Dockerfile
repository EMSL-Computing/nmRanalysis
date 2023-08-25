FROM code-registry.emsl.pnl.gov/emslc60117/nmranalysisappbaseimage:main-0.0.4

COPY . /srv/shiny/nmRanalysis/

RUN \
  chmod -R 755 /srv/shiny/ && \
  R -e "install.packages(c('DBI', 'RPostgres'))" && \
  R -e "install.packages('/srv/shiny/nmRanalysis', repos = NULL, type='source')" && \
  rm -rf /srv/shiny/nmRanalysis

#uncomment next line for local build
#RUN export GIT_SSL_NO_VERIFY=1

# RUN R -e "remotes::install_github('EMSL-Computing/nmRanalysis')"

# Make the ShinyApp available at port 3838
EXPOSE 3838

# run app
CMD ["R", "-e", "options('shiny.port' = 3838,shiny.host='0.0.0.0', launch.browser = FALSE);nmRanalysis::nmRapp()"]
