# Load bmse_associations.rda
load("bmse_associations.rda")

# library("readxl")
# refmets <- read.csv("C:/Users/prym311/Documents/NMRdatabase/bmrb_metabolomics/refmets_fitting_info.csv")
refmets <- read.csv("refmets_fitting_info.csv")


connect_db <- function(){
  # Set environmental variables

  # dsn_hostname = "localhost"
  # dsn_uid = "developer"
  # dsn_pwd = "developer"
  # dsn_database = "nmRanalysis"
  # dsn_port = "5432"

  dsn_database = Sys.getenv(c("DATABASE_NAME"))
  dsn_hostname = Sys.getenv(c("DATABASE_HOST"))
  dsn_port = Sys.getenv(c("DATABASE_PORT"))
  dsn_uid = Sys.getenv(c("DATABASE_USER"))
  dsn_pwd = Sys.getenv(c("DATABASE_PASSWORD"))

  # Connect to Database
  tryCatch({
    #drv <- RPostgres::Postgres()
    print("Connecting to Database…")
    connec <- DBI::dbConnect(RPostgres::Postgres(),
                             dbname = dsn_database,
                             host=dsn_hostname,
                             port=dsn_port,
                             user=dsn_uid,
                             password=dsn_pwd)
    print("Database Connected!")
  },
  error=function(cond) {
    print("Unable to connect to Database.")
    print("Here's the original error message:")
    print(cond)
  })
  return(connec)
}
conn = connect_db()

#create table
create_new_table <- function(db_connection, table_name, df_object){
  DBI::dbCreateTable(db_connection, toString(table_name), df_object)
}

#fill table
append_table <- function(db_connection, table_name, df_object){
  DBI::dbAppendTable(conn = db_connection,
                     name = DBI::SQL(toString(table_name)),
                     value = df_object,
                     copy = NULL,
                     row.names = NULL)
}


#create bmse_associations
create_new_table(conn, "bmse_associations", bmse_associations)
append_table(conn, "bmse_associations", bmse_associations)

# add refmets
create_new_table(conn, "refmets_fitting_info", refmets)
append_table(conn, "refmets_fitting_info", refmets)

# define user_profiling table
user_profile_df <- data.frame("id"= character(),
                              "ROI left edge (ppm)"=double(),
                              "ROI right edge (ppm)"=double(),
                              "Quantification Mode"=character(),
                              "Metabolite"=character(),
                              "Quantification Signal"=character(),
                              "Chemical shift(ppm)"=double(),
                              "Chemical shift tolerance (ppm)"=double(),
                              "Half bandwidth (Hz)"=double(),
                              "Multiplicity"=character(),
                              "J coupling (Hz)"=double(),
                              "Roof effect"=double(),
                              "J coupling 2 (Hz)"=double(),
                              "Roof effect 2"=double(),
                              "Quantify"=double(),
                              "Frequency (MHz)"=double(),
                              "pH"=double(),
                              "Concentration (mM)"=double(),
                              "Temperature (K)"=double(),
                              "Solvent"=character(),
                              "rowid"=character(),
                              "user"=character(),
                              "session"=character(),
                              'proposal_number'=character(),
                              'PI_name'=character(),
                              'project_name'=character(),
                              stringsAsFactors=FALSE,
                              check.names = FALSE)

# create user profiling save session table
create_new_table(conn, "profiling_parameters", user_profile_df)

# define recommended saved metabolites tables
recomender_metabs<- data.frame("metabolite"=character(),
                              "user"=character(),
                              "session"=character(),
                              'proposal_number'=character(),
                              'pi_name'=character(),
                              'project_name'=character(),
                              stringsAsFactors=FALSE,
                              check.names = FALSE)

# create recommended saved metabolites table in db
create_new_table(conn, "recommended_metabs", recomender_metabs)
