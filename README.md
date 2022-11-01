# nmRanalysis
## Current Version
  
###  `0.0.1`

### Services:

- Semi-automated NMR profiling software and UI


## Set up

1. Optional: If you do not already have it, install [Git Bash for Windows](https://git-scm.com/download/win)

2. If you don't have docker installed, the easiest way is to [install docker for desktop](https://docs.docker.com/engine/install/)

3. Open a bash terminal and clone into the nmRanalysis repository by copying the HTTPS link available in the dropdown menu of the green 'Code' button.
    ```bash
    git clone https://github.com/EMSL-Computing/nmRanalysis.git

    ```

4. Navigate to the nmRanalysis directory with ```cd nmRanalysis```

5. Build images with the following command

    ```bash
    docker-compose up --build
   
    ```
    ** Note: The build will take ~30 minutes the very first time.
    ** Subsequent updates where only nmRanalysis and not its dependencies are loaded should take only a couple of minutes.

    Once the image is built and the container is running you should get the terminal message: "Listening on http://0.0.0.0:3838"

6. Open a web browser and go to "localhost:3838"

7. You are now free to do your analyses through the app!


## Troubleshooting

- `renv` failed to install a dependency

## User Manual

A complete user's manual for nmRanalysis can be found here:
https://htmlpreview.github.io/?https://github.com/EMSL-Computing/nmRanalysis/blob/master/docs/nmRanalysis_Manual.html

If you have trouble with installing dependencies through `renv`. You might need to change the CRAN respository.

 1. Open the Dockerfile in a text editor
 2. Change line 31 from 
 `RUN R -e "install.packages('remotes', repos = c(CRAN = 'http://cran.us.r-project.org'), dependencies = TRUE)"` 
 to 
 `RUN R -e "install.packages('remotes', repos = c(CRAN = 'http://cran.rstudio.com/'), dependencies = TRUE)"`
 3. Save the Dockerfile changes and re-run the build command.
