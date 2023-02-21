# Load bmse_associations.rda
load("data/bmse_associations.rda")

# connect db
connect_db <- function(){
  # Set environmental variables

  dsn_hostname = "localhost"
  dsn_uid = "developer"
  dsn_pwd = "developer"
  dsn_database = "nmRanalysis"
  dsn_port = "5432"

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

create_new_table(conn, "bmse_associations", bmse_associations)

#fill table
append_table <- function(db_connection, table_name, df_object){
  DBI::dbAppendTable(conn = db_connection,
                     name = DBI::SQL(toString(table_name)),
                     value = df_object,
                     copy = NULL,
                     row.names = NULL)
}

append_table(conn, "bmse_associations", bmse_associations)

# create user_profiling table
user_profile_df <- data.frame("id"= integer(),
                              "ROI left edge (ppm)"=double(),
                              "ROI right edge (ppm)"=integer(),
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
                              stringsAsFactors=FALSE)
user_profiling_parameters <- data.frame("id"= integer(),
                                        "Signal"=character(),
                                        "Quantification Mode"=character(),
                                        "Chemical shift(ppm)"=double(),
                                        "Half bandwidth (Hz)"=double(),
                                        "Multiplicity"=character(),
                                        "J coupling (Hz)"=double(),
                                        "J coupling 2 (Hz)"=double(),
                                        "Roof effect"=double(),
                                        "Roof effect 2"=double(),
                                        "Frequency (MHz)"=double(),
                                        "pH"=double(),
                                        "Concentration (mM)"=double(),
                                        "Temperature (K)"=double(),
                                        "Solvent"=character(),
                                        "rowid"=character(),
                                        "user"=character(),
                                        "session"=character(),
                                        stringsAsFactors=FALSE)
create_new_table(conn, "profiling_parameters", user_profiling_parameters)

